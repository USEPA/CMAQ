      SUBROUTINE JAC( C, PD, RK )                                                                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C                                                                                                                       
C Automatically generated Jacobian for DDM-3D                                                                           
C                                                                                                                       
C =-=-= MECHANISM: CB6R3_AE7_AQ                     =-=-=                                                               
C                                                                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Use RXNS_DATA                                                                                                     
      Use UTILIO_DEFN                                                                                                   

      IMPLICIT NONE                                                                                                     

      INTEGER, PARAMETER :: NJACSPC = 147                                                                               
      INTEGER, PARAMETER :: NJACRXN = 343                                                                               

      CHARACTER( 16 ), SAVE :: PNAME = 'JAC'                                                                            
      CHARACTER( 132 ) :: MSG                                                                                           

      REAL, DIMENSION( NJACSPC ) :: C                                                                                   
      REAL, DIMENSION( NJACSPC, NJACSPC ) :: PD                                                                         
      REAL(KIND=8), DIMENSION( NJACRXN ) :: RK                                                                          

      INTEGER, SAVE :: LOGDEV                                                                                           
      LOGICAL, SAVE :: FIRSTIME = .TRUE.                                                                                

      INTEGER, PARAMETER :: NO2              =   1                                                                      
      INTEGER, PARAMETER :: NO               =   2                                                                      
      INTEGER, PARAMETER :: O                =   3                                                                      
      INTEGER, PARAMETER :: O3               =   4                                                                      
      INTEGER, PARAMETER :: NO3              =   5                                                                      
      INTEGER, PARAMETER :: O1D              =   6                                                                      
      INTEGER, PARAMETER :: OH               =   7                                                                      
      INTEGER, PARAMETER :: HO2              =   8                                                                      
      INTEGER, PARAMETER :: H2O2             =   9                                                                      
      INTEGER, PARAMETER :: N2O5             =  10                                                                      
      INTEGER, PARAMETER :: HNO3             =  11                                                                      
      INTEGER, PARAMETER :: HONO             =  12                                                                      
      INTEGER, PARAMETER :: PNA              =  13                                                                      
      INTEGER, PARAMETER :: SO2              =  14                                                                      
      INTEGER, PARAMETER :: SULF             =  15                                                                      
      INTEGER, PARAMETER :: SULRXN           =  16                                                                      
      INTEGER, PARAMETER :: C2O3             =  17                                                                      
      INTEGER, PARAMETER :: MEO2             =  18                                                                      
      INTEGER, PARAMETER :: RO2              =  19                                                                      
      INTEGER, PARAMETER :: PAN              =  20                                                                      
      INTEGER, PARAMETER :: PACD             =  21                                                                      
      INTEGER, PARAMETER :: AACD             =  22                                                                      
      INTEGER, PARAMETER :: CXO3             =  23                                                                      
      INTEGER, PARAMETER :: ALD2             =  24                                                                      
      INTEGER, PARAMETER :: XO2H             =  25                                                                      
      INTEGER, PARAMETER :: PANX             =  26                                                                      
      INTEGER, PARAMETER :: FORM             =  27                                                                      
      INTEGER, PARAMETER :: MEPX             =  28                                                                      
      INTEGER, PARAMETER :: MEOH             =  29                                                                      
      INTEGER, PARAMETER :: ROOH             =  30                                                                      
      INTEGER, PARAMETER :: XO2              =  31                                                                      
      INTEGER, PARAMETER :: XO2N             =  32                                                                      
      INTEGER, PARAMETER :: NTR1             =  33                                                                      
      INTEGER, PARAMETER :: NTR2             =  34                                                                      
      INTEGER, PARAMETER :: FACD             =  35                                                                      
      INTEGER, PARAMETER :: CO               =  36                                                                      
      INTEGER, PARAMETER :: HCO3             =  37                                                                      
      INTEGER, PARAMETER :: ALDX             =  38                                                                      
      INTEGER, PARAMETER :: GLYD             =  39                                                                      
      INTEGER, PARAMETER :: GLY              =  40                                                                      
      INTEGER, PARAMETER :: MGLY             =  41                                                                      
      INTEGER, PARAMETER :: ETHA             =  42                                                                      
      INTEGER, PARAMETER :: ETOH             =  43                                                                      
      INTEGER, PARAMETER :: KET              =  44                                                                      
      INTEGER, PARAMETER :: PAR              =  45                                                                      
      INTEGER, PARAMETER :: ACET             =  46                                                                      
      INTEGER, PARAMETER :: PRPA             =  47                                                                      
      INTEGER, PARAMETER :: XPRP             =  48                                                                      
      INTEGER, PARAMETER :: XPAR             =  49                                                                      
      INTEGER, PARAMETER :: ROR              =  50                                                                      
      INTEGER, PARAMETER :: ETHY             =  51                                                                      
      INTEGER, PARAMETER :: ETH              =  52                                                                      
      INTEGER, PARAMETER :: OLE              =  53                                                                      
      INTEGER, PARAMETER :: IOLE             =  54                                                                      
      INTEGER, PARAMETER :: ISOP             =  55                                                                      
      INTEGER, PARAMETER :: ISO2             =  56                                                                      
      INTEGER, PARAMETER :: ISOPRXN          =  57                                                                      
      INTEGER, PARAMETER :: ISPD             =  58                                                                      
      INTEGER, PARAMETER :: INTR             =  59                                                                      
      INTEGER, PARAMETER :: ISPX             =  60                                                                      
      INTEGER, PARAMETER :: HPLD             =  61                                                                      
      INTEGER, PARAMETER :: OPO3             =  62                                                                      
      INTEGER, PARAMETER :: EPOX             =  63                                                                      
      INTEGER, PARAMETER :: EPX2             =  64                                                                      
      INTEGER, PARAMETER :: TERP             =  65                                                                      
      INTEGER, PARAMETER :: TRPRXN           =  66                                                                      
      INTEGER, PARAMETER :: TERPNRO2         =  67                                                                      
      INTEGER, PARAMETER :: APIN             =  68                                                                      
      INTEGER, PARAMETER :: BENZENE          =  69                                                                      
      INTEGER, PARAMETER :: CRES             =  70                                                                      
      INTEGER, PARAMETER :: BZO2             =  71                                                                      
      INTEGER, PARAMETER :: OPEN             =  72                                                                      
      INTEGER, PARAMETER :: BENZRO2          =  73                                                                      
      INTEGER, PARAMETER :: TOL              =  74                                                                      
      INTEGER, PARAMETER :: TO2              =  75                                                                      
      INTEGER, PARAMETER :: TOLRO2           =  76                                                                      
      INTEGER, PARAMETER :: XOPN             =  77                                                                      
      INTEGER, PARAMETER :: XYLMN            =  78                                                                      
      INTEGER, PARAMETER :: XLO2             =  79                                                                      
      INTEGER, PARAMETER :: XYLRO2           =  80                                                                      
      INTEGER, PARAMETER :: NAPH             =  81                                                                      
      INTEGER, PARAMETER :: PAHRO2           =  82                                                                      
      INTEGER, PARAMETER :: CRO              =  83                                                                      
      INTEGER, PARAMETER :: CAT1             =  84                                                                      
      INTEGER, PARAMETER :: CRON             =  85                                                                      
      INTEGER, PARAMETER :: OPAN             =  86                                                                      
      INTEGER, PARAMETER :: ECH4             =  87                                                                      
      INTEGER, PARAMETER :: CL2              =  88                                                                      
      INTEGER, PARAMETER :: CL               =  89                                                                      
      INTEGER, PARAMETER :: HOCL             =  90                                                                      
      INTEGER, PARAMETER :: CLO              =  91                                                                      
      INTEGER, PARAMETER :: FMCL             =  92                                                                      
      INTEGER, PARAMETER :: HCL              =  93                                                                      
      INTEGER, PARAMETER :: CLNO2            =  94                                                                      
      INTEGER, PARAMETER :: CLNO3            =  95                                                                      
      INTEGER, PARAMETER :: SVAVB2           =  96                                                                      
      INTEGER, PARAMETER :: SVAVB3           =  97                                                                      
      INTEGER, PARAMETER :: SVAVB4           =  98                                                                      
      INTEGER, PARAMETER :: SVAVB1           =  99                                                                      
      INTEGER, PARAMETER :: SESQ             = 100                                                                      
      INTEGER, PARAMETER :: SESQRXN          = 101                                                                      
      INTEGER, PARAMETER :: SOAALK           = 102                                                                      
      INTEGER, PARAMETER :: H2NO3PIJ         = 103                                                                      
      INTEGER, PARAMETER :: H2NO3PK          = 104                                                                      
      INTEGER, PARAMETER :: ACLI             = 105                                                                      
      INTEGER, PARAMETER :: ACLJ             = 106                                                                      
      INTEGER, PARAMETER :: ACLK             = 107                                                                      
      INTEGER, PARAMETER :: IEPOXP           = 108                                                                      
      INTEGER, PARAMETER :: ASO4J            = 109                                                                      
      INTEGER, PARAMETER :: AISO3J           = 110                                                                      
      INTEGER, PARAMETER :: AGLYJ            = 111                                                                      
      INTEGER, PARAMETER :: MTNO3            = 112                                                                      
      INTEGER, PARAMETER :: AMTNO3J          = 113                                                                      
      INTEGER, PARAMETER :: AMTHYDJ          = 114                                                                      
      INTEGER, PARAMETER :: AAVB2J           = 115                                                                      
      INTEGER, PARAMETER :: AOLGAJ           = 116                                                                      
      INTEGER, PARAMETER :: AAVB3J           = 117                                                                      
      INTEGER, PARAMETER :: AAVB4J           = 118                                                                      
      INTEGER, PARAMETER :: AISO1J           = 119                                                                      
      INTEGER, PARAMETER :: AOLGBJ           = 120                                                                      
      INTEGER, PARAMETER :: AISO2J           = 121                                                                      
      INTEGER, PARAMETER :: ASQTJ            = 122                                                                      
      INTEGER, PARAMETER :: APOCI            = 123                                                                      
      INTEGER, PARAMETER :: APNCOMI          = 124                                                                      
      INTEGER, PARAMETER :: APOCJ            = 125                                                                      
      INTEGER, PARAMETER :: APNCOMJ          = 126                                                                      
      INTEGER, PARAMETER :: PCVOC            = 127                                                                      
      INTEGER, PARAMETER :: PCSOARXN         = 128                                                                      
      INTEGER, PARAMETER :: VLVPO1           = 129                                                                      
      INTEGER, PARAMETER :: VSVPO1           = 130                                                                      
      INTEGER, PARAMETER :: VSVPO2           = 131                                                                      
      INTEGER, PARAMETER :: VSVPO3           = 132                                                                      
      INTEGER, PARAMETER :: VIVPO1           = 133                                                                      
      INTEGER, PARAMETER :: VLVOO1           = 134                                                                      
      INTEGER, PARAMETER :: VLVOO2           = 135                                                                      
      INTEGER, PARAMETER :: VSVOO2           = 136                                                                      
      INTEGER, PARAMETER :: VSVOO3           = 137                                                                      
      INTEGER, PARAMETER :: VSVOO1           = 138                                                                      
      INTEGER, PARAMETER :: FORM_PRIMARY     = 139                                                                      
      INTEGER, PARAMETER :: ALD2_PRIMARY     = 140                                                                      
      INTEGER, PARAMETER :: BUTADIENE13      = 141                                                                      
      INTEGER, PARAMETER :: ACROLEIN         = 142                                                                      
      INTEGER, PARAMETER :: ACRO_PRIMARY     = 143                                                                      
      INTEGER, PARAMETER :: TOLU             = 144                                                                      
      INTEGER, PARAMETER :: HG               = 145                                                                      
      INTEGER, PARAMETER :: HGIIAER          = 146                                                                      
      INTEGER, PARAMETER :: HGIIGAS          = 147                                                                      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF( FIRSTIME ) THEN                                                                                               

         IF( MECHNAME .NE. 'CB6R3_AE7_AQ                    ' ) THEN                                                    
            MSG =  'This Jacobian was built for CB6R3_AE7_AQ                     '                                      
     &            // MECHNAME // ' is in use. Exiting.'                                                                 
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )                                                                     
         ENDIF                                                                                                          

         FIRSTIME = .FALSE.                                                                                             

      ENDIF                                                                                                             

      PD = 0.0                                                                                                          

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   1: NO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO2         ,NO2          ) = -           RK( 001 )                                                           
     &                                  -           RK( 005 ) * C( O            )                                       
     &                                  -           RK( 006 ) * C( O            )                                       
     &                                  -           RK( 026 ) * C( O3           )                                       
     &                                  -           RK( 036 ) * C( NO3          )                                       
     &                                  -           RK( 041 ) * C( NO           )                                       
     &                                  -           RK( 045 ) * C( OH           )                                       
     &                                  -           RK( 048 ) * C( HO2          )                                       
     &                                  -           RK( 054 ) * C( C2O3         )                                       
     &                                  -           RK( 062 ) * C( CXO3         )                                       
     &                                  -           RK( 135 ) * C( ROR          )                                       
     &                                  -           RK( 197 ) * C( CRO          )                                       
     &                                  -           RK( 213 ) * C( OPO3         )                                       
     &                                  -           RK( 252 ) * C( CLO          )                                       
     &                                  -           RK( 276 )                                                           
      PD( NO2         ,NO           ) =             RK( 003 ) * C( O3           )                                       
     &                                  +           RK( 004 ) * C( O            )                                       
     &                                  + 4.00000 * RK( 024 ) * C( NO           )                                       
     &                                  +           RK( 025 ) * C( HO2          )                                       
     &                                  + 2.00000 * RK( 029 ) * C( NO3          )                                       
     &                                  -           RK( 041 ) * C( NO2          )                                       
     &                                  +           RK( 053 ) * C( C2O3         )                                       
     &                                  +           RK( 061 ) * C( CXO3         )                                       
     &                                  +           RK( 071 ) * C( MEO2         )                                       
     &                                  +           RK( 075 ) * C( XO2H         )                                       
     &                                  +           RK( 079 ) * C( XO2          )                                       
     &                                  +           RK( 103 ) * C( HCO3         )                                       
     &                                  + 0.90000 * RK( 151 ) * C( ISO2         )                                       
     &                                  +           RK( 167 ) * C( EPX2         )                                       
     &                                  + 0.91800 * RK( 180 ) * C( BZO2         )                                       
     &                                  + 0.86000 * RK( 185 ) * C( TO2          )                                       
     &                                  + 0.86000 * RK( 191 ) * C( XLO2         )                                       
     &                                  +           RK( 212 ) * C( OPO3         )                                       
     &                                  +           RK( 229 ) * C( CLO          )                                       
      PD( NO2         ,O            ) =             RK( 004 ) * C( NO           )                                       
     &                                  -           RK( 005 ) * C( NO2          )                                       
     &                                  -           RK( 006 ) * C( NO2          )                                       
     &                                  +           RK( 031 ) * C( NO3          )                                       
      PD( NO2         ,O3           ) =             RK( 003 ) * C( NO           )                                       
     &                                  -           RK( 026 ) * C( NO2          )                                       
     &                                  +           RK( 034 ) * C( NO3          )                                       
      PD( NO2         ,NO3          ) =             RK( 027 )                                                           
     &                                  + 2.00000 * RK( 029 ) * C( NO           )                                       
     &                                  +           RK( 031 ) * C( O            )                                       
     &                                  +           RK( 032 ) * C( OH           )                                       
     &                                  +           RK( 033 ) * C( HO2          )                                       
     &                                  +           RK( 034 ) * C( O3           )                                       
     &                                  + 4.00000 * RK( 035 ) * C( NO3          )                                       
     &                                  -           RK( 036 ) * C( NO2          )                                       
     &                                  + 0.50000 * RK( 140 ) * C( ETH          )                                       
     &                                  + 0.50000 * RK( 144 ) * C( OLE          )                                       
     &                                  + 0.50000 * RK( 148 ) * C( IOLE         )                                       
     &                                  + 0.35000 * RK( 157 ) * C( ISOP         )                                       
     &                                  + 0.14200 * RK( 160 ) * C( ISPD         )                                       
     &                                  + 0.47000 * RK( 174 ) * C( TERP         )                                       
     &                                  + 0.47000 * RK( 178 ) * C( APIN         )                                       
     &                                  + 0.50000 * RK( 205 ) * C( XOPN         )                                       
      PD( NO2         ,OH           ) =             RK( 032 ) * C( NO3          )                                       
     &                                  +           RK( 044 ) * C( HONO         )                                       
     &                                  -           RK( 045 ) * C( NO2          )                                       
     &                                  +           RK( 051 ) * C( PNA          )                                       
     &                                  + 0.44400 * RK( 170 ) * C( INTR         )                                       
     &                                  + 0.50000 * RK( 218 ) * C( OPAN         )                                       
     &                                  +           RK( 219 ) * C( PANX         )                                       
      PD( NO2         ,HO2          ) =             RK( 025 ) * C( NO           )                                       
     &                                  +           RK( 033 ) * C( NO3          )                                       
     &                                  -           RK( 048 ) * C( NO2          )                                       
      PD( NO2         ,N2O5         ) =             RK( 037 )                                                           
     &                                  +           RK( 038 )                                                           
      PD( NO2         ,HNO3         ) =             RK( 047 )                                                           
      PD( NO2         ,HONO         ) =   4.00000 * RK( 042 ) * C( HONO         )                                       
     &                                  +           RK( 044 ) * C( OH           )                                       
      PD( NO2         ,PNA          ) =             RK( 049 )                                                           
     &                                  + 0.59000 * RK( 050 )                                                           
     &                                  +           RK( 051 ) * C( OH           )                                       
      PD( NO2         ,C2O3         ) =             RK( 053 ) * C( NO           )                                       
     &                                  -           RK( 054 ) * C( NO2          )                                       
      PD( NO2         ,MEO2         ) =             RK( 071 ) * C( NO           )                                       
      PD( NO2         ,PAN          ) =             RK( 055 )                                                           
     &                                  + 0.60000 * RK( 056 )                                                           
      PD( NO2         ,CXO3         ) =             RK( 061 ) * C( NO           )                                       
     &                                  -           RK( 062 ) * C( NO2          )                                       
      PD( NO2         ,XO2H         ) =             RK( 075 ) * C( NO           )                                       
      PD( NO2         ,PANX         ) =             RK( 063 )                                                           
     &                                  + 0.60000 * RK( 064 )                                                           
     &                                  +           RK( 219 ) * C( OH           )                                       
      PD( NO2         ,XO2          ) =             RK( 079 ) * C( NO           )                                       
      PD( NO2         ,NTR1         ) =             RK( 092 )                                                           
      PD( NO2         ,HCO3         ) =             RK( 103 ) * C( NO           )                                       
      PD( NO2         ,ROR          ) = -           RK( 135 ) * C( NO2          )                                       
      PD( NO2         ,ETH          ) =   0.50000 * RK( 140 ) * C( NO3          )                                       
      PD( NO2         ,OLE          ) =   0.50000 * RK( 144 ) * C( NO3          )                                       
      PD( NO2         ,IOLE         ) =   0.50000 * RK( 148 ) * C( NO3          )                                       
      PD( NO2         ,ISOP         ) =   0.35000 * RK( 157 ) * C( NO3          )                                       
      PD( NO2         ,ISO2         ) =   0.90000 * RK( 151 ) * C( NO           )                                       
      PD( NO2         ,ISPD         ) =   0.14200 * RK( 160 ) * C( NO3          )                                       
      PD( NO2         ,INTR         ) =   0.44400 * RK( 170 ) * C( OH           )                                       
      PD( NO2         ,OPO3         ) =             RK( 212 ) * C( NO           )                                       
     &                                  -           RK( 213 ) * C( NO2          )                                       
      PD( NO2         ,EPX2         ) =             RK( 167 ) * C( NO           )                                       
      PD( NO2         ,TERP         ) =   0.47000 * RK( 174 ) * C( NO3          )                                       
      PD( NO2         ,APIN         ) =   0.47000 * RK( 178 ) * C( NO3          )                                       
      PD( NO2         ,BZO2         ) =   0.91800 * RK( 180 ) * C( NO           )                                       
      PD( NO2         ,TO2          ) =   0.86000 * RK( 185 ) * C( NO           )                                       
      PD( NO2         ,XOPN         ) =   0.50000 * RK( 205 ) * C( NO3          )                                       
      PD( NO2         ,XLO2         ) =   0.86000 * RK( 191 ) * C( NO           )                                       
      PD( NO2         ,CRO          ) = -           RK( 197 ) * C( NO2          )                                       
      PD( NO2         ,OPAN         ) =             RK( 214 )                                                           
     &                                  + 0.50000 * RK( 218 ) * C( OH           )                                       
      PD( NO2         ,CLO          ) =             RK( 229 ) * C( NO           )                                       
     &                                  -           RK( 252 ) * C( NO2          )                                       
      PD( NO2         ,CLNO2        ) =             RK( 251 )                                                           
      PD( NO2         ,CLNO3        ) =             RK( 253 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   2: NO                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO          ,NO2          ) =             RK( 001 )                                                           
     &                                  +           RK( 005 ) * C( O            )                                       
     &                                  +           RK( 030 ) * C( NO3          )                                       
     &                                  -           RK( 041 ) * C( NO           )                                       
      PD( NO          ,NO           ) = -           RK( 003 ) * C( O3           )                                       
     &                                  -           RK( 004 ) * C( O            )                                       
     &                                  - 4.00000 * RK( 024 ) * C( NO           )                                       
     &                                  -           RK( 025 ) * C( HO2          )                                       
     &                                  -           RK( 029 ) * C( NO3          )                                       
     &                                  -           RK( 040 ) * C( OH           )                                       
     &                                  -           RK( 041 ) * C( NO2          )                                       
     &                                  -           RK( 053 ) * C( C2O3         )                                       
     &                                  -           RK( 061 ) * C( CXO3         )                                       
     &                                  -           RK( 071 ) * C( MEO2         )                                       
     &                                  -           RK( 075 ) * C( XO2H         )                                       
     &                                  -           RK( 079 ) * C( XO2          )                                       
     &                                  -           RK( 083 ) * C( XO2N         )                                       
     &                                  -           RK( 103 ) * C( HCO3         )                                       
     &                                  -           RK( 151 ) * C( ISO2         )                                       
     &                                  -           RK( 167 ) * C( EPX2         )                                       
     &                                  -           RK( 180 ) * C( BZO2         )                                       
     &                                  -           RK( 185 ) * C( TO2          )                                       
     &                                  -           RK( 191 ) * C( XLO2         )                                       
     &                                  -           RK( 212 ) * C( OPO3         )                                       
     &                                  -           RK( 229 ) * C( CLO          )                                       
      PD( NO          ,O            ) = -           RK( 004 ) * C( NO           )                                       
     &                                  +           RK( 005 ) * C( NO2          )                                       
      PD( NO          ,O3           ) = -           RK( 003 ) * C( NO           )                                       
      PD( NO          ,NO3          ) =             RK( 028 )                                                           
     &                                  -           RK( 029 ) * C( NO           )                                       
     &                                  +           RK( 030 ) * C( NO2          )                                       
      PD( NO          ,OH           ) = -           RK( 040 ) * C( NO           )                                       
      PD( NO          ,HO2          ) = -           RK( 025 ) * C( NO           )                                       
      PD( NO          ,HONO         ) =   4.00000 * RK( 042 ) * C( HONO         )                                       
     &                                  +           RK( 043 )                                                           
      PD( NO          ,C2O3         ) = -           RK( 053 ) * C( NO           )                                       
      PD( NO          ,MEO2         ) = -           RK( 071 ) * C( NO           )                                       
      PD( NO          ,CXO3         ) = -           RK( 061 ) * C( NO           )                                       
      PD( NO          ,XO2H         ) = -           RK( 075 ) * C( NO           )                                       
      PD( NO          ,XO2          ) = -           RK( 079 ) * C( NO           )                                       
      PD( NO          ,XO2N         ) = -           RK( 083 ) * C( NO           )                                       
      PD( NO          ,HCO3         ) = -           RK( 103 ) * C( NO           )                                       
      PD( NO          ,ISO2         ) = -           RK( 151 ) * C( NO           )                                       
      PD( NO          ,OPO3         ) = -           RK( 212 ) * C( NO           )                                       
      PD( NO          ,EPX2         ) = -           RK( 167 ) * C( NO           )                                       
      PD( NO          ,BZO2         ) = -           RK( 180 ) * C( NO           )                                       
      PD( NO          ,TO2          ) = -           RK( 185 ) * C( NO           )                                       
      PD( NO          ,XLO2         ) = -           RK( 191 ) * C( NO           )                                       
      PD( NO          ,CLO          ) = -           RK( 229 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   3: O                                                                          
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O           ,NO2          ) =             RK( 001 )                                                           
     &                                  -           RK( 005 ) * C( O            )                                       
     &                                  -           RK( 006 ) * C( O            )                                       
      PD( O           ,NO           ) = -           RK( 004 ) * C( O            )                                       
      PD( O           ,O            ) = -           RK( 002 )                                                           
     &                                  -           RK( 004 ) * C( NO           )                                       
     &                                  -           RK( 005 ) * C( NO2          )                                       
     &                                  -           RK( 006 ) * C( NO2          )                                       
     &                                  -           RK( 007 ) * C( O3           )                                       
     &                                  -           RK( 014 ) * C( OH           )                                       
     &                                  -           RK( 015 ) * C( HO2          )                                       
     &                                  -           RK( 023 ) * C( H2O2         )                                       
     &                                  -           RK( 031 ) * C( NO3          )                                       
     &                                  -           RK( 099 ) * C( FORM         )                                       
     &                                  -           RK( 105 ) * C( ALD2         )                                       
     &                                  -           RK( 109 ) * C( ALDX         )                                       
     &                                  -           RK( 137 ) * C( ETH          )                                       
     &                                  -           RK( 141 ) * C( OLE          )                                       
     &                                  -           RK( 145 ) * C( IOLE         )                                       
     &                                  -           RK( 150 ) * C( ISOP         )                                       
     &                                  -           RK( 171 ) * C( TERP         )                                       
     &                                  -           RK( 175 ) * C( APIN         )                                       
      PD( O           ,O3           ) = -           RK( 007 ) * C( O            )                                       
     &                                  +           RK( 008 )                                                           
      PD( O           ,NO3          ) =             RK( 027 )                                                           
     &                                  -           RK( 031 ) * C( O            )                                       
      PD( O           ,O1D          ) =             RK( 010 )                                                           
      PD( O           ,OH           ) = -           RK( 014 ) * C( O            )                                       
     &                                  + 4.00000 * RK( 016 ) * C( OH           )                                       
      PD( O           ,HO2          ) = -           RK( 015 ) * C( O            )                                       
      PD( O           ,H2O2         ) = -           RK( 023 ) * C( O            )                                       
      PD( O           ,ALD2         ) = -           RK( 105 ) * C( O            )                                       
      PD( O           ,FORM         ) = -           RK( 099 ) * C( O            )                                       
      PD( O           ,ALDX         ) = -           RK( 109 ) * C( O            )                                       
      PD( O           ,ETH          ) = -           RK( 137 ) * C( O            )                                       
      PD( O           ,OLE          ) = -           RK( 141 ) * C( O            )                                       
      PD( O           ,IOLE         ) = -           RK( 145 ) * C( O            )                                       
      PD( O           ,ISOP         ) = -           RK( 150 ) * C( O            )                                       
      PD( O           ,TERP         ) = -           RK( 171 ) * C( O            )                                       
      PD( O           ,APIN         ) = -           RK( 175 ) * C( O            )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   4: O3                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O3          ,NO2          ) = -           RK( 026 ) * C( O3           )                                       
      PD( O3          ,NO           ) = -           RK( 003 ) * C( O3           )                                       
      PD( O3          ,O            ) =             RK( 002 )                                                           
     &                                  -           RK( 007 ) * C( O3           )                                       
      PD( O3          ,O3           ) = -           RK( 003 ) * C( NO           )                                       
     &                                  -           RK( 007 ) * C( O            )                                       
     &                                  -           RK( 008 )                                                           
     &                                  -           RK( 009 )                                                           
     &                                  -           RK( 012 ) * C( OH           )                                       
     &                                  -           RK( 013 ) * C( HO2          )                                       
     &                                  -           RK( 026 ) * C( NO2          )                                       
     &                                  -           RK( 034 ) * C( NO3          )                                       
     &                                  -           RK( 139 ) * C( ETH          )                                       
     &                                  -           RK( 143 ) * C( OLE          )                                       
     &                                  -           RK( 147 ) * C( IOLE         )                                       
     &                                  -           RK( 156 ) * C( ISOP         )                                       
     &                                  -           RK( 159 ) * C( ISPD         )                                       
     &                                  -           RK( 173 ) * C( TERP         )                                       
     &                                  -           RK( 177 ) * C( APIN         )                                       
     &                                  -           RK( 204 ) * C( XOPN         )                                       
     &                                  -           RK( 208 ) * C( OPEN         )                                       
     &                                  -           RK( 227 ) * C( CL           )                                       
     &                                  -           RK( 277 )                                                           
      PD( O3          ,NO3          ) = -           RK( 034 ) * C( O3           )                                       
      PD( O3          ,OH           ) = -           RK( 012 ) * C( O3           )                                       
      PD( O3          ,HO2          ) = -           RK( 013 ) * C( O3           )                                       
     &                                  + 0.15000 * RK( 057 ) * C( C2O3         )                                       
     &                                  + 0.15000 * RK( 065 ) * C( CXO3         )                                       
     &                                  + 0.15000 * RK( 215 ) * C( OPO3         )                                       
      PD( O3          ,C2O3         ) =   0.15000 * RK( 057 ) * C( HO2          )                                       
      PD( O3          ,CXO3         ) =   0.15000 * RK( 065 ) * C( HO2          )                                       
      PD( O3          ,ETH          ) = -           RK( 139 ) * C( O3           )                                       
      PD( O3          ,OLE          ) = -           RK( 143 ) * C( O3           )                                       
      PD( O3          ,IOLE         ) = -           RK( 147 ) * C( O3           )                                       
      PD( O3          ,ISOP         ) = -           RK( 156 ) * C( O3           )                                       
      PD( O3          ,ISPD         ) = -           RK( 159 ) * C( O3           )                                       
      PD( O3          ,OPO3         ) =   0.15000 * RK( 215 ) * C( HO2          )                                       
      PD( O3          ,TERP         ) = -           RK( 173 ) * C( O3           )                                       
      PD( O3          ,APIN         ) = -           RK( 177 ) * C( O3           )                                       
      PD( O3          ,OPEN         ) = -           RK( 208 ) * C( O3           )                                       
      PD( O3          ,XOPN         ) = -           RK( 204 ) * C( O3           )                                       
      PD( O3          ,CL           ) = -           RK( 227 ) * C( O3           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   5: NO3                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO3         ,NO2          ) =             RK( 006 ) * C( O            )                                       
     &                                  +           RK( 026 ) * C( O3           )                                       
     &                                  -           RK( 030 ) * C( NO3          )                                       
     &                                  -           RK( 036 ) * C( NO3          )                                       
      PD( NO3         ,NO           ) = -           RK( 029 ) * C( NO3          )                                       
      PD( NO3         ,O            ) =             RK( 006 ) * C( NO2          )                                       
     &                                  -           RK( 031 ) * C( NO3          )                                       
      PD( NO3         ,O3           ) =             RK( 026 ) * C( NO2          )                                       
     &                                  -           RK( 034 ) * C( NO3          )                                       
      PD( NO3         ,NO3          ) = -           RK( 027 )                                                           
     &                                  -           RK( 028 )                                                           
     &                                  -           RK( 029 ) * C( NO           )                                       
     &                                  -           RK( 030 ) * C( NO2          )                                       
     &                                  -           RK( 031 ) * C( O            )                                       
     &                                  -           RK( 032 ) * C( OH           )                                       
     &                                  -           RK( 033 ) * C( HO2          )                                       
     &                                  -           RK( 034 ) * C( O3           )                                       
     &                                  - 4.00000 * RK( 035 ) * C( NO3          )                                       
     &                                  -           RK( 036 ) * C( NO2          )                                       
     &                                  -           RK( 100 ) * C( FORM         )                                       
     &                                  -           RK( 107 ) * C( ALD2         )                                       
     &                                  -           RK( 111 ) * C( ALDX         )                                       
     &                                  -           RK( 115 ) * C( GLYD         )                                       
     &                                  -           RK( 118 ) * C( GLY          )                                       
     &                                  -           RK( 120 ) * C( MGLY         )                                       
     &                                  -           RK( 140 ) * C( ETH          )                                       
     &                                  -           RK( 144 ) * C( OLE          )                                       
     &                                  -           RK( 148 ) * C( IOLE         )                                       
     &                                  -           RK( 157 ) * C( ISOP         )                                       
     &                                  -           RK( 160 ) * C( ISPD         )                                       
     &                                  -           RK( 164 ) * C( HPLD         )                                       
     &                                  -           RK( 174 ) * C( TERP         )                                       
     &                                  -           RK( 178 ) * C( APIN         )                                       
     &                                  -           RK( 196 ) * C( CRES         )                                       
     &                                  -           RK( 200 ) * C( CRON         )                                       
     &                                  -           RK( 205 ) * C( XOPN         )                                       
     &                                  -           RK( 209 ) * C( OPEN         )                                       
     &                                  -           RK( 211 ) * C( CAT1         )                                       
      PD( NO3         ,OH           ) = -           RK( 032 ) * C( NO3          )                                       
     &                                  +           RK( 046 ) * C( HNO3         )                                       
     &                                  + 0.18500 * RK( 170 ) * C( INTR         )                                       
      PD( NO3         ,HO2          ) = -           RK( 033 ) * C( NO3          )                                       
      PD( NO3         ,N2O5         ) =             RK( 037 )                                                           
     &                                  +           RK( 038 )                                                           
      PD( NO3         ,HNO3         ) =             RK( 046 ) * C( OH           )                                       
      PD( NO3         ,PNA          ) =   0.41000 * RK( 050 )                                                           
      PD( NO3         ,PAN          ) =   0.40000 * RK( 056 )                                                           
      PD( NO3         ,ALD2         ) = -           RK( 107 ) * C( NO3          )                                       
      PD( NO3         ,PANX         ) =   0.40000 * RK( 064 )                                                           
      PD( NO3         ,FORM         ) = -           RK( 100 ) * C( NO3          )                                       
      PD( NO3         ,ALDX         ) = -           RK( 111 ) * C( NO3          )                                       
      PD( NO3         ,GLYD         ) = -           RK( 115 ) * C( NO3          )                                       
      PD( NO3         ,GLY          ) = -           RK( 118 ) * C( NO3          )                                       
      PD( NO3         ,MGLY         ) = -           RK( 120 ) * C( NO3          )                                       
      PD( NO3         ,ETH          ) = -           RK( 140 ) * C( NO3          )                                       
      PD( NO3         ,OLE          ) = -           RK( 144 ) * C( NO3          )                                       
      PD( NO3         ,IOLE         ) = -           RK( 148 ) * C( NO3          )                                       
      PD( NO3         ,ISOP         ) = -           RK( 157 ) * C( NO3          )                                       
      PD( NO3         ,ISPD         ) = -           RK( 160 ) * C( NO3          )                                       
      PD( NO3         ,INTR         ) =   0.18500 * RK( 170 ) * C( OH           )                                       
      PD( NO3         ,HPLD         ) = -           RK( 164 ) * C( NO3          )                                       
      PD( NO3         ,TERP         ) = -           RK( 174 ) * C( NO3          )                                       
      PD( NO3         ,APIN         ) = -           RK( 178 ) * C( NO3          )                                       
      PD( NO3         ,CRES         ) = -           RK( 196 ) * C( NO3          )                                       
      PD( NO3         ,OPEN         ) = -           RK( 209 ) * C( NO3          )                                       
      PD( NO3         ,XOPN         ) = -           RK( 205 ) * C( NO3          )                                       
      PD( NO3         ,CAT1         ) = -           RK( 211 ) * C( NO3          )                                       
      PD( NO3         ,CRON         ) = -           RK( 200 ) * C( NO3          )                                       
      PD( NO3         ,CLNO3        ) =             RK( 254 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   6: O1D                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O1D         ,O3           ) =             RK( 009 )                                                           
      PD( O1D         ,O1D          ) = -           RK( 010 )                                                           
     &                                  -           RK( 011 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   7: OH                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OH          ,NO2          ) = -           RK( 045 ) * C( OH           )                                       
      PD( OH          ,NO           ) =             RK( 025 ) * C( HO2          )                                       
     &                                  -           RK( 040 ) * C( OH           )                                       
     &                                  + 0.12500 * RK( 167 ) * C( EPX2         )                                       
      PD( OH          ,O            ) = -           RK( 014 ) * C( OH           )                                       
     &                                  +           RK( 015 ) * C( HO2          )                                       
     &                                  +           RK( 023 ) * C( H2O2         )                                       
     &                                  +           RK( 099 ) * C( FORM         )                                       
     &                                  +           RK( 105 ) * C( ALD2         )                                       
     &                                  +           RK( 109 ) * C( ALDX         )                                       
     &                                  + 0.30000 * RK( 137 ) * C( ETH          )                                       
     &                                  + 0.10000 * RK( 141 ) * C( OLE          )                                       
      PD( OH          ,O3           ) = -           RK( 012 ) * C( OH           )                                       
     &                                  +           RK( 013 ) * C( HO2          )                                       
     &                                  + 0.16000 * RK( 139 ) * C( ETH          )                                       
     &                                  + 0.33400 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.50000 * RK( 147 ) * C( IOLE         )                                       
     &                                  + 0.26600 * RK( 156 ) * C( ISOP         )                                       
     &                                  + 0.46100 * RK( 159 ) * C( ISPD         )                                       
     &                                  + 0.57000 * RK( 173 ) * C( TERP         )                                       
     &                                  + 0.57000 * RK( 177 ) * C( APIN         )                                       
     &                                  + 0.50000 * RK( 204 ) * C( XOPN         )                                       
     &                                  + 0.50000 * RK( 208 ) * C( OPEN         )                                       
      PD( OH          ,NO3          ) = -           RK( 032 ) * C( OH           )                                       
     &                                  +           RK( 033 ) * C( HO2          )                                       
      PD( OH          ,O1D          ) =   2.00000 * RK( 011 )                                                           
      PD( OH          ,OH           ) = -           RK( 012 ) * C( O3           )                                       
     &                                  -           RK( 014 ) * C( O            )                                       
     &                                  - 4.00000 * RK( 016 ) * C( OH           )                                       
     &                                  - 4.00000 * RK( 017 ) * C( OH           )                                       
     &                                  -           RK( 018 ) * C( HO2          )                                       
     &                                  -           RK( 022 ) * C( H2O2         )                                       
     &                                  -           RK( 032 ) * C( NO3          )                                       
     &                                  -           RK( 040 ) * C( NO           )                                       
     &                                  -           RK( 044 ) * C( HONO         )                                       
     &                                  -           RK( 045 ) * C( NO2          )                                       
     &                                  -           RK( 046 ) * C( HNO3         )                                       
     &                                  -           RK( 051 ) * C( PNA          )                                       
     &                                  -           RK( 052 ) * C( SO2          )                                       
     &                                  - 0.60000 * RK( 087 ) * C( MEPX         )                                       
     &                                  - 0.60000 * RK( 089 ) * C( ROOH         )                                       
     &                                  -           RK( 091 ) * C( NTR1         )                                       
     &                                  -           RK( 093 ) * C( FACD         )                                       
     &                                  -           RK( 094 ) * C( AACD         )                                       
     &                                  -           RK( 095 ) * C( PACD         )                                       
     &                                  -           RK( 096 ) * C( FORM         )                                       
     &                                  -           RK( 106 ) * C( ALD2         )                                       
     &                                  -           RK( 110 ) * C( ALDX         )                                       
     &                                  -           RK( 113 ) * C( GLYD         )                                       
     &                                  -           RK( 116 ) * C( GLY          )                                       
     &                                  -           RK( 121 ) * C( MGLY         )                                       
     &                                  -           RK( 122 )                                                           
     &                                  -           RK( 123 ) * C( CO           )                                       
     &                                  -           RK( 124 )                                                           
     &                                  -           RK( 125 ) * C( ETHA         )                                       
     &                                  -           RK( 126 ) * C( MEOH         )                                       
     &                                  -           RK( 127 ) * C( ETOH         )                                       
     &                                  -           RK( 130 ) * C( ACET         )                                       
     &                                  -           RK( 131 ) * C( PRPA         )                                       
     &                                  -           RK( 132 ) * C( PAR          )                                       
     &                                  - 0.30000 * RK( 136 ) * C( ETHY         )                                       
     &                                  -           RK( 138 ) * C( ETH          )                                       
     &                                  -           RK( 142 ) * C( OLE          )                                       
     &                                  -           RK( 146 ) * C( IOLE         )                                       
     &                                  -           RK( 149 ) * C( ISOP         )                                       
     &                                  -           RK( 158 ) * C( ISPD         )                                       
     &                                  - 0.06700 * RK( 162 ) * C( ISPX         )                                       
     &                                  -           RK( 165 ) * C( EPOX         )                                       
     &                                  -           RK( 170 ) * C( INTR         )                                       
     &                                  -           RK( 172 ) * C( TERP         )                                       
     &                                  -           RK( 176 ) * C( APIN         )                                       
     &                                  - 0.88200 * RK( 179 ) * C( BENZENE      )                                       
     &                                  - 0.90000 * RK( 184 ) * C( TOL          )                                       
     &                                  - 0.75600 * RK( 189 ) * C( XYLMN        )                                       
     &                                  - 0.75600 * RK( 190 ) * C( NAPH         )                                       
     &                                  -           RK( 195 ) * C( CRES         )                                       
     &                                  -           RK( 199 ) * C( CRON         )                                       
     &                                  -           RK( 203 ) * C( XOPN         )                                       
     &                                  -           RK( 207 ) * C( OPEN         )                                       
     &                                  -           RK( 210 ) * C( CAT1         )                                       
     &                                  -           RK( 218 ) * C( OPAN         )                                       
     &                                  -           RK( 219 ) * C( PANX         )                                       
     &                                  -           RK( 220 ) * C( ECH4         )                                       
     &                                  -           RK( 232 ) * C( FMCL         )                                       
     &                                  -           RK( 247 ) * C( HCL          )                                       
      PD( OH          ,HO2          ) =             RK( 013 ) * C( O3           )                                       
     &                                  +           RK( 015 ) * C( O            )                                       
     &                                  -           RK( 018 ) * C( OH           )                                       
     &                                  +           RK( 025 ) * C( NO           )                                       
     &                                  +           RK( 033 ) * C( NO3          )                                       
     &                                  + 0.44000 * RK( 057 ) * C( C2O3         )                                       
     &                                  + 0.44000 * RK( 065 ) * C( CXO3         )                                       
     &                                  + 0.20000 * RK( 104 ) * C( HCO3         )                                       
     &                                  + 0.12000 * RK( 152 ) * C( ISO2         )                                       
     &                                  + 1.12500 * RK( 166 ) * C( EPX2         )                                       
     &                                  + 0.44000 * RK( 215 ) * C( OPO3         )                                       
      PD( OH          ,H2O2         ) =   2.00000 * RK( 021 )                                                           
     &                                  -           RK( 022 ) * C( OH           )                                       
     &                                  +           RK( 023 ) * C( O            )                                       
      PD( OH          ,HNO3         ) = -           RK( 046 ) * C( OH           )                                       
     &                                  +           RK( 047 )                                                           
      PD( OH          ,HONO         ) =             RK( 043 )                                                           
     &                                  -           RK( 044 ) * C( OH           )                                       
      PD( OH          ,PNA          ) =   0.41000 * RK( 050 )                                                           
     &                                  -           RK( 051 ) * C( OH           )                                       
      PD( OH          ,SO2          ) = -           RK( 052 ) * C( OH           )                                       
      PD( OH          ,C2O3         ) =   0.44000 * RK( 057 ) * C( HO2          )                                       
     &                                  + 0.10000 * RK( 168 ) * C( EPX2         )                                       
      PD( OH          ,RO2          ) =   0.12500 * RK( 169 ) * C( EPX2         )                                       
      PD( OH          ,PACD         ) = -           RK( 095 ) * C( OH           )                                       
      PD( OH          ,AACD         ) = -           RK( 094 ) * C( OH           )                                       
      PD( OH          ,CXO3         ) =   0.44000 * RK( 065 ) * C( HO2          )                                       
      PD( OH          ,ALD2         ) =             RK( 105 ) * C( O            )                                       
     &                                  -           RK( 106 ) * C( OH           )                                       
      PD( OH          ,PANX         ) = -           RK( 219 ) * C( OH           )                                       
      PD( OH          ,FORM         ) = -           RK( 096 ) * C( OH           )                                       
     &                                  +           RK( 099 ) * C( O            )                                       
      PD( OH          ,MEPX         ) = - 0.60000 * RK( 087 ) * C( OH           )                                       
     &                                  +           RK( 088 )                                                           
      PD( OH          ,MEOH         ) = -           RK( 126 ) * C( OH           )                                       
      PD( OH          ,ROOH         ) = - 0.60000 * RK( 089 ) * C( OH           )                                       
     &                                  +           RK( 090 )                                                           
      PD( OH          ,NTR1         ) = -           RK( 091 ) * C( OH           )                                       
      PD( OH          ,FACD         ) = -           RK( 093 ) * C( OH           )                                       
      PD( OH          ,CO           ) = -           RK( 123 ) * C( OH           )                                       
      PD( OH          ,HCO3         ) =   0.20000 * RK( 104 ) * C( HO2          )                                       
      PD( OH          ,ALDX         ) =             RK( 109 ) * C( O            )                                       
     &                                  -           RK( 110 ) * C( OH           )                                       
      PD( OH          ,GLYD         ) = -           RK( 113 ) * C( OH           )                                       
     &                                  + 0.19000 * RK( 114 )                                                           
      PD( OH          ,GLY          ) = -           RK( 116 ) * C( OH           )                                       
      PD( OH          ,MGLY         ) = -           RK( 121 ) * C( OH           )                                       
      PD( OH          ,ETHA         ) = -           RK( 125 ) * C( OH           )                                       
      PD( OH          ,ETOH         ) = -           RK( 127 ) * C( OH           )                                       
      PD( OH          ,PAR          ) = -           RK( 132 ) * C( OH           )                                       
      PD( OH          ,ACET         ) = -           RK( 130 ) * C( OH           )                                       
      PD( OH          ,PRPA         ) = -           RK( 131 ) * C( OH           )                                       
      PD( OH          ,ETHY         ) = - 0.30000 * RK( 136 ) * C( OH           )                                       
      PD( OH          ,ETH          ) =   0.30000 * RK( 137 ) * C( O            )                                       
     &                                  -           RK( 138 ) * C( OH           )                                       
     &                                  + 0.16000 * RK( 139 ) * C( O3           )                                       
      PD( OH          ,OLE          ) =   0.10000 * RK( 141 ) * C( O            )                                       
     &                                  -           RK( 142 ) * C( OH           )                                       
     &                                  + 0.33400 * RK( 143 ) * C( O3           )                                       
      PD( OH          ,IOLE         ) = -           RK( 146 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 147 ) * C( O3           )                                       
      PD( OH          ,ISOP         ) = -           RK( 149 ) * C( OH           )                                       
     &                                  + 0.26600 * RK( 156 ) * C( O3           )                                       
      PD( OH          ,ISO2         ) =   0.12000 * RK( 152 ) * C( HO2          )                                       
      PD( OH          ,ISPD         ) = -           RK( 158 ) * C( OH           )                                       
     &                                  + 0.46100 * RK( 159 ) * C( O3           )                                       
      PD( OH          ,INTR         ) = -           RK( 170 ) * C( OH           )                                       
      PD( OH          ,ISPX         ) = - 0.06700 * RK( 162 ) * C( OH           )                                       
      PD( OH          ,HPLD         ) =             RK( 163 )                                                           
      PD( OH          ,OPO3         ) =   0.44000 * RK( 215 ) * C( HO2          )                                       
      PD( OH          ,EPOX         ) = -           RK( 165 ) * C( OH           )                                       
      PD( OH          ,EPX2         ) =   1.12500 * RK( 166 ) * C( HO2          )                                       
     &                                  + 0.12500 * RK( 167 ) * C( NO           )                                       
     &                                  + 0.10000 * RK( 168 ) * C( C2O3         )                                       
     &                                  + 0.12500 * RK( 169 ) * C( RO2          )                                       
      PD( OH          ,TERP         ) = -           RK( 172 ) * C( OH           )                                       
     &                                  + 0.57000 * RK( 173 ) * C( O3           )                                       
      PD( OH          ,APIN         ) = -           RK( 176 ) * C( OH           )                                       
     &                                  + 0.57000 * RK( 177 ) * C( O3           )                                       
      PD( OH          ,BENZENE      ) = - 0.88200 * RK( 179 ) * C( OH           )                                       
      PD( OH          ,CRES         ) = -           RK( 195 ) * C( OH           )                                       
      PD( OH          ,OPEN         ) = -           RK( 207 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 208 ) * C( O3           )                                       
      PD( OH          ,TOL          ) = - 0.90000 * RK( 184 ) * C( OH           )                                       
     &                                  + 0.10000 * RK( 248 ) * C( CL           )                                       
      PD( OH          ,XOPN         ) = -           RK( 203 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 204 ) * C( O3           )                                       
      PD( OH          ,XYLMN        ) = - 0.75600 * RK( 189 ) * C( OH           )                                       
     &                                  + 0.24400 * RK( 249 ) * C( CL           )                                       
      PD( OH          ,NAPH         ) = - 0.75600 * RK( 190 ) * C( OH           )                                       
     &                                  + 0.24400 * RK( 250 ) * C( CL           )                                       
      PD( OH          ,CAT1         ) = -           RK( 210 ) * C( OH           )                                       
      PD( OH          ,CRON         ) = -           RK( 199 ) * C( OH           )                                       
      PD( OH          ,OPAN         ) = -           RK( 218 ) * C( OH           )                                       
      PD( OH          ,ECH4         ) = -           RK( 220 ) * C( OH           )                                       
      PD( OH          ,CL           ) =   0.10000 * RK( 248 ) * C( TOL          )                                       
     &                                  + 0.24400 * RK( 249 ) * C( XYLMN        )                                       
     &                                  + 0.24400 * RK( 250 ) * C( NAPH         )                                       
      PD( OH          ,HOCL         ) =             RK( 226 )                                                           
      PD( OH          ,FMCL         ) = -           RK( 232 ) * C( OH           )                                       
      PD( OH          ,HCL          ) = -           RK( 247 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   8: HO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HO2         ,NO2          ) = -           RK( 048 ) * C( HO2          )                                       
      PD( HO2         ,NO           ) = -           RK( 025 ) * C( HO2          )                                       
     &                                  +           RK( 071 ) * C( MEO2         )                                       
     &                                  +           RK( 075 ) * C( XO2H         )                                       
     &                                  +           RK( 103 ) * C( HCO3         )                                       
     &                                  + 0.81800 * RK( 151 ) * C( ISO2         )                                       
     &                                  + 0.82500 * RK( 167 ) * C( EPX2         )                                       
     &                                  + 0.91800 * RK( 180 ) * C( BZO2         )                                       
     &                                  + 0.86000 * RK( 185 ) * C( TO2          )                                       
     &                                  + 0.86000 * RK( 191 ) * C( XLO2         )                                       
     &                                  + 0.80000 * RK( 212 ) * C( OPO3         )                                       
      PD( HO2         ,O            ) =             RK( 014 ) * C( OH           )                                       
     &                                  -           RK( 015 ) * C( HO2          )                                       
     &                                  +           RK( 023 ) * C( H2O2         )                                       
     &                                  +           RK( 099 ) * C( FORM         )                                       
     &                                  +           RK( 137 ) * C( ETH          )                                       
     &                                  + 0.10000 * RK( 141 ) * C( OLE          )                                       
     &                                  + 0.25000 * RK( 150 ) * C( ISOP         )                                       
      PD( HO2         ,O3           ) =             RK( 012 ) * C( OH           )                                       
     &                                  -           RK( 013 ) * C( HO2          )                                       
     &                                  + 0.16000 * RK( 139 ) * C( ETH          )                                       
     &                                  + 0.08000 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.06600 * RK( 156 ) * C( ISOP         )                                       
     &                                  + 0.39800 * RK( 159 ) * C( ISPD         )                                       
     &                                  + 0.56000 * RK( 208 ) * C( OPEN         )                                       
      PD( HO2         ,NO3          ) =             RK( 032 ) * C( OH           )                                       
     &                                  -           RK( 033 ) * C( HO2          )                                       
     &                                  +           RK( 100 ) * C( FORM         )                                       
     &                                  +           RK( 118 ) * C( GLY          )                                       
      PD( HO2         ,OH           ) =             RK( 012 ) * C( O3           )                                       
     &                                  +           RK( 014 ) * C( O            )                                       
     &                                  -           RK( 018 ) * C( HO2          )                                       
     &                                  +           RK( 022 ) * C( H2O2         )                                       
     &                                  +           RK( 032 ) * C( NO3          )                                       
     &                                  +           RK( 052 ) * C( SO2          )                                       
     &                                  +           RK( 093 ) * C( FACD         )                                       
     &                                  +           RK( 096 ) * C( FORM         )                                       
     &                                  + 0.20000 * RK( 113 ) * C( GLYD         )                                       
     &                                  +           RK( 116 ) * C( GLY          )                                       
     &                                  +           RK( 122 )                                                           
     &                                  +           RK( 123 ) * C( CO           )                                       
     &                                  +           RK( 126 ) * C( MEOH         )                                       
     &                                  + 0.90000 * RK( 127 ) * C( ETOH         )                                       
     &                                  + 0.30000 * RK( 136 ) * C( ETHY         )                                       
     &                                  + 0.13700 * RK( 158 ) * C( ISPD         )                                       
     &                                  + 0.53000 * RK( 179 ) * C( BENZENE      )                                       
     &                                  + 0.18000 * RK( 184 ) * C( TOL          )                                       
     &                                  + 0.15500 * RK( 189 ) * C( XYLMN        )                                       
     &                                  + 0.15500 * RK( 190 ) * C( NAPH         )                                       
     &                                  +           RK( 195 ) * C( CRES         )                                       
     &                                  + 0.20000 * RK( 210 ) * C( CAT1         )                                       
      PD( HO2         ,HO2          ) = -           RK( 013 ) * C( O3           )                                       
     &                                  -           RK( 015 ) * C( O            )                                       
     &                                  -           RK( 018 ) * C( OH           )                                       
     &                                  - 4.00000 * RK( 019 ) * C( HO2          )                                       
     &                                  - 4.00000 * RK( 020 ) * C( HO2          )                                       
     &                                  -           RK( 025 ) * C( NO           )                                       
     &                                  -           RK( 033 ) * C( NO3          )                                       
     &                                  -           RK( 048 ) * C( NO2          )                                       
     &                                  -           RK( 057 ) * C( C2O3         )                                       
     &                                  -           RK( 065 ) * C( CXO3         )                                       
     &                                  -           RK( 072 ) * C( MEO2         )                                       
     &                                  -           RK( 076 ) * C( XO2H         )                                       
     &                                  -           RK( 080 ) * C( XO2          )                                       
     &                                  -           RK( 084 ) * C( XO2N         )                                       
     &                                  -           RK( 101 ) * C( FORM         )                                       
     &                                  - 0.80000 * RK( 104 ) * C( HCO3         )                                       
     &                                  - 0.88000 * RK( 152 ) * C( ISO2         )                                       
     &                                  - 0.17500 * RK( 166 ) * C( EPX2         )                                       
     &                                  -           RK( 182 ) * C( BZO2         )                                       
     &                                  -           RK( 187 ) * C( TO2          )                                       
     &                                  -           RK( 192 ) * C( XLO2         )                                       
     &                                  -           RK( 198 ) * C( CRO          )                                       
     &                                  -           RK( 215 ) * C( OPO3         )                                       
     &                                  -           RK( 230 ) * C( CLO          )                                       
      PD( HO2         ,H2O2         ) =             RK( 022 ) * C( OH           )                                       
     &                                  +           RK( 023 ) * C( O            )                                       
      PD( HO2         ,PNA          ) =             RK( 049 )                                                           
     &                                  + 0.59000 * RK( 050 )                                                           
      PD( HO2         ,SO2          ) =             RK( 052 ) * C( OH           )                                       
      PD( HO2         ,C2O3         ) = -           RK( 057 ) * C( HO2          )                                       
     &                                  + 0.90000 * RK( 073 ) * C( MEO2         )                                       
     &                                  + 0.80000 * RK( 077 ) * C( XO2H         )                                       
     &                                  + 0.80000 * RK( 085 ) * C( XO2N         )                                       
     &                                  + 0.72800 * RK( 153 ) * C( ISO2         )                                       
     &                                  + 0.66000 * RK( 168 ) * C( EPX2         )                                       
     &                                  +           RK( 181 ) * C( BZO2         )                                       
     &                                  +           RK( 186 ) * C( TO2          )                                       
     &                                  +           RK( 193 ) * C( XLO2         )                                       
      PD( HO2         ,MEO2         ) =             RK( 071 ) * C( NO           )                                       
     &                                  -           RK( 072 ) * C( HO2          )                                       
     &                                  + 0.90000 * RK( 073 ) * C( C2O3         )                                       
     &                                  + 0.37000 * RK( 074 ) * C( RO2          )                                       
     &                                  +           RK( 231 ) * C( CLO          )                                       
      PD( HO2         ,RO2          ) =   0.37000 * RK( 074 ) * C( MEO2         )                                       
     &                                  + 0.60000 * RK( 078 ) * C( XO2H         )                                       
     &                                  + 0.72800 * RK( 154 ) * C( ISO2         )                                       
     &                                  + 0.82500 * RK( 169 ) * C( EPX2         )                                       
     &                                  +           RK( 183 ) * C( BZO2         )                                       
     &                                  +           RK( 188 ) * C( TO2          )                                       
     &                                  +           RK( 194 ) * C( XLO2         )                                       
      PD( HO2         ,CXO3         ) = -           RK( 065 ) * C( HO2          )                                       
      PD( HO2         ,ALD2         ) =             RK( 108 )                                                           
      PD( HO2         ,XO2H         ) =             RK( 075 ) * C( NO           )                                       
     &                                  -           RK( 076 ) * C( HO2          )                                       
     &                                  + 0.80000 * RK( 077 ) * C( C2O3         )                                       
     &                                  + 0.60000 * RK( 078 ) * C( RO2          )                                       
      PD( HO2         ,FORM         ) =             RK( 096 ) * C( OH           )                                       
     &                                  + 2.00000 * RK( 097 )                                                           
     &                                  +           RK( 099 ) * C( O            )                                       
     &                                  +           RK( 100 ) * C( NO3          )                                       
     &                                  -           RK( 101 ) * C( HO2          )                                       
     &                                  +           RK( 242 ) * C( CL           )                                       
      PD( HO2         ,MEOH         ) =             RK( 126 ) * C( OH           )                                       
     &                                  +           RK( 245 ) * C( CL           )                                       
      PD( HO2         ,ROOH         ) =             RK( 090 )                                                           
      PD( HO2         ,XO2          ) = -           RK( 080 ) * C( HO2          )                                       
      PD( HO2         ,XO2N         ) = -           RK( 084 ) * C( HO2          )                                       
     &                                  + 0.80000 * RK( 085 ) * C( C2O3         )                                       
      PD( HO2         ,FACD         ) =             RK( 093 ) * C( OH           )                                       
      PD( HO2         ,CO           ) =             RK( 123 ) * C( OH           )                                       
      PD( HO2         ,HCO3         ) =             RK( 102 )                                                           
     &                                  +           RK( 103 ) * C( NO           )                                       
     &                                  - 0.80000 * RK( 104 ) * C( HO2          )                                       
      PD( HO2         ,ALDX         ) =             RK( 112 )                                                           
      PD( HO2         ,GLYD         ) =   0.20000 * RK( 113 ) * C( OH           )                                       
     &                                  + 1.40000 * RK( 114 )                                                           
      PD( HO2         ,GLY          ) =             RK( 116 ) * C( OH           )                                       
     &                                  + 2.00000 * RK( 117 )                                                           
     &                                  +           RK( 118 ) * C( NO3          )                                       
      PD( HO2         ,MGLY         ) =             RK( 119 )                                                           
      PD( HO2         ,ETOH         ) =   0.90000 * RK( 127 ) * C( OH           )                                       
     &                                  +           RK( 246 ) * C( CL           )                                       
      PD( HO2         ,ROR          ) =             RK( 134 )                                                           
      PD( HO2         ,ETHY         ) =   0.30000 * RK( 136 ) * C( OH           )                                       
      PD( HO2         ,ETH          ) =             RK( 137 ) * C( O            )                                       
     &                                  + 0.16000 * RK( 139 ) * C( O3           )                                       
     &                                  +           RK( 238 ) * C( CL           )                                       
      PD( HO2         ,OLE          ) =   0.10000 * RK( 141 ) * C( O            )                                       
     &                                  + 0.08000 * RK( 143 ) * C( O3           )                                       
     &                                  +           RK( 239 ) * C( CL           )                                       
      PD( HO2         ,IOLE         ) =             RK( 240 ) * C( CL           )                                       
      PD( HO2         ,ISOP         ) =   0.25000 * RK( 150 ) * C( O            )                                       
     &                                  + 0.06600 * RK( 156 ) * C( O3           )                                       
      PD( HO2         ,ISO2         ) =   0.81800 * RK( 151 ) * C( NO           )                                       
     &                                  - 0.88000 * RK( 152 ) * C( HO2          )                                       
     &                                  + 0.72800 * RK( 153 ) * C( C2O3         )                                       
     &                                  + 0.72800 * RK( 154 ) * C( RO2          )                                       
     &                                  +           RK( 155 )                                                           
      PD( HO2         ,ISPD         ) =   0.13700 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.39800 * RK( 159 ) * C( O3           )                                       
     &                                  + 0.76000 * RK( 161 )                                                           
      PD( HO2         ,OPO3         ) =   0.80000 * RK( 212 ) * C( NO           )                                       
     &                                  -           RK( 215 ) * C( HO2          )                                       
      PD( HO2         ,EPX2         ) = - 0.17500 * RK( 166 ) * C( HO2          )                                       
     &                                  + 0.82500 * RK( 167 ) * C( NO           )                                       
     &                                  + 0.66000 * RK( 168 ) * C( C2O3         )                                       
     &                                  + 0.82500 * RK( 169 ) * C( RO2          )                                       
      PD( HO2         ,BENZENE      ) =   0.53000 * RK( 179 ) * C( OH           )                                       
      PD( HO2         ,CRES         ) =             RK( 195 ) * C( OH           )                                       
      PD( HO2         ,BZO2         ) =   0.91800 * RK( 180 ) * C( NO           )                                       
     &                                  +           RK( 181 ) * C( C2O3         )                                       
     &                                  -           RK( 182 ) * C( HO2          )                                       
     &                                  +           RK( 183 ) * C( RO2          )                                       
      PD( HO2         ,OPEN         ) =             RK( 206 )                                                           
     &                                  + 0.56000 * RK( 208 ) * C( O3           )                                       
      PD( HO2         ,TOL          ) =   0.18000 * RK( 184 ) * C( OH           )                                       
     &                                  + 0.18000 * RK( 248 ) * C( CL           )                                       
      PD( HO2         ,TO2          ) =   0.86000 * RK( 185 ) * C( NO           )                                       
     &                                  +           RK( 186 ) * C( C2O3         )                                       
     &                                  -           RK( 187 ) * C( HO2          )                                       
     &                                  +           RK( 188 ) * C( RO2          )                                       
      PD( HO2         ,XOPN         ) =   0.70000 * RK( 202 )                                                           
      PD( HO2         ,XYLMN        ) =   0.15500 * RK( 189 ) * C( OH           )                                       
     &                                  + 0.15500 * RK( 249 ) * C( CL           )                                       
      PD( HO2         ,XLO2         ) =   0.86000 * RK( 191 ) * C( NO           )                                       
     &                                  -           RK( 192 ) * C( HO2          )                                       
     &                                  +           RK( 193 ) * C( C2O3         )                                       
     &                                  +           RK( 194 ) * C( RO2          )                                       
      PD( HO2         ,NAPH         ) =   0.15500 * RK( 190 ) * C( OH           )                                       
     &                                  + 0.15500 * RK( 250 ) * C( CL           )                                       
      PD( HO2         ,CRO          ) = -           RK( 198 ) * C( HO2          )                                       
      PD( HO2         ,CAT1         ) =   0.20000 * RK( 210 ) * C( OH           )                                       
      PD( HO2         ,CRON         ) =             RK( 201 )                                                           
      PD( HO2         ,CL           ) =             RK( 238 ) * C( ETH          )                                       
     &                                  +           RK( 239 ) * C( OLE          )                                       
     &                                  +           RK( 240 ) * C( IOLE         )                                       
     &                                  +           RK( 242 ) * C( FORM         )                                       
     &                                  +           RK( 245 ) * C( MEOH         )                                       
     &                                  +           RK( 246 ) * C( ETOH         )                                       
     &                                  + 0.18000 * RK( 248 ) * C( TOL          )                                       
     &                                  + 0.15500 * RK( 249 ) * C( XYLMN        )                                       
     &                                  + 0.15500 * RK( 250 ) * C( NAPH         )                                       
      PD( HO2         ,CLO          ) = -           RK( 230 ) * C( HO2          )                                       
     &                                  +           RK( 231 ) * C( MEO2         )                                       
      PD( HO2         ,FMCL         ) =             RK( 233 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   9: H2O2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( H2O2        ,O            ) = -           RK( 023 ) * C( H2O2         )                                       
      PD( H2O2        ,O3           ) =   0.04000 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.08000 * RK( 147 ) * C( IOLE         )                                       
      PD( H2O2        ,OH           ) =   4.00000 * RK( 017 ) * C( OH           )                                       
     &                                  -           RK( 022 ) * C( H2O2         )                                       
      PD( H2O2        ,HO2          ) =   4.00000 * RK( 019 ) * C( HO2          )                                       
     &                                  + 4.00000 * RK( 020 ) * C( HO2          )                                       
      PD( H2O2        ,H2O2         ) = -           RK( 021 )                                                           
     &                                  -           RK( 022 ) * C( OH           )                                       
     &                                  -           RK( 023 ) * C( O            )                                       
      PD( H2O2        ,OLE          ) =   0.04000 * RK( 143 ) * C( O3           )                                       
      PD( H2O2        ,IOLE         ) =   0.08000 * RK( 147 ) * C( O3           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  10: N2O5                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( N2O5        ,NO2          ) =             RK( 036 ) * C( NO3          )                                       
      PD( N2O5        ,NO3          ) =             RK( 036 ) * C( NO2          )                                       
      PD( N2O5        ,N2O5         ) = -           RK( 037 )                                                           
     &                                  -           RK( 038 )                                                           
     &                                  -           RK( 039 )                                                           
     &                                  -           RK( 269 )                                                           
     &                                  -           RK( 270 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  11: HNO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HNO3        ,NO2          ) =             RK( 045 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 276 )                                                           
      PD( HNO3        ,NO3          ) =             RK( 100 ) * C( FORM         )                                       
     &                                  +           RK( 107 ) * C( ALD2         )                                       
     &                                  +           RK( 111 ) * C( ALDX         )                                       
     &                                  +           RK( 115 ) * C( GLYD         )                                       
     &                                  +           RK( 118 ) * C( GLY          )                                       
     &                                  +           RK( 120 ) * C( MGLY         )                                       
     &                                  + 0.71700 * RK( 160 ) * C( ISPD         )                                       
     &                                  +           RK( 164 ) * C( HPLD         )                                       
     &                                  +           RK( 196 ) * C( CRES         )                                       
     &                                  +           RK( 200 ) * C( CRON         )                                       
     &                                  +           RK( 209 ) * C( OPEN         )                                       
     &                                  +           RK( 211 ) * C( CAT1         )                                       
      PD( HNO3        ,OH           ) =             RK( 045 ) * C( NO2          )                                       
     &                                  -           RK( 046 ) * C( HNO3         )                                       
      PD( HNO3        ,N2O5         ) =   2.00000 * RK( 039 )                                                           
     &                                  +           RK( 269 )                                                           
     &                                  +           RK( 270 )                                                           
      PD( HNO3        ,HNO3         ) = -           RK( 046 ) * C( OH           )                                       
     &                                  -           RK( 047 )                                                           
      PD( HNO3        ,ALD2         ) =             RK( 107 ) * C( NO3          )                                       
      PD( HNO3        ,FORM         ) =             RK( 100 ) * C( NO3          )                                       
      PD( HNO3        ,NTR2         ) =             RK( 268 )                                                           
      PD( HNO3        ,ALDX         ) =             RK( 111 ) * C( NO3          )                                       
      PD( HNO3        ,GLYD         ) =             RK( 115 ) * C( NO3          )                                       
      PD( HNO3        ,GLY          ) =             RK( 118 ) * C( NO3          )                                       
      PD( HNO3        ,MGLY         ) =             RK( 120 ) * C( NO3          )                                       
      PD( HNO3        ,ISPD         ) =   0.71700 * RK( 160 ) * C( NO3          )                                       
      PD( HNO3        ,HPLD         ) =             RK( 164 ) * C( NO3          )                                       
      PD( HNO3        ,CRES         ) =             RK( 196 ) * C( NO3          )                                       
      PD( HNO3        ,OPEN         ) =             RK( 209 ) * C( NO3          )                                       
      PD( HNO3        ,CAT1         ) =             RK( 211 ) * C( NO3          )                                       
      PD( HNO3        ,CRON         ) =             RK( 200 ) * C( NO3          )                                       
      PD( HNO3        ,CLNO3        ) =             RK( 255 )                                                           
      PD( HNO3        ,H2NO3PIJ     ) =             RK( 271 )                                                           
      PD( HNO3        ,H2NO3PK      ) =             RK( 272 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  12: HONO                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HONO        ,NO2          ) =   2.00000 * RK( 041 ) * C( NO           )                                       
     &                                  + 0.50000 * RK( 276 )                                                           
      PD( HONO        ,NO           ) =             RK( 040 ) * C( OH           )                                       
     &                                  + 2.00000 * RK( 041 ) * C( NO2          )                                       
      PD( HONO        ,OH           ) =             RK( 040 ) * C( NO           )                                       
     &                                  -           RK( 044 ) * C( HONO         )                                       
      PD( HONO        ,HONO         ) = - 4.00000 * RK( 042 ) * C( HONO         )                                       
     &                                  -           RK( 043 )                                                           
     &                                  -           RK( 044 ) * C( OH           )                                       
      PD( HONO        ,CRON         ) =             RK( 201 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  13: PNA                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PNA         ,NO2          ) =             RK( 048 ) * C( HO2          )                                       
      PD( PNA         ,OH           ) = -           RK( 051 ) * C( PNA          )                                       
      PD( PNA         ,HO2          ) =             RK( 048 ) * C( NO2          )                                       
      PD( PNA         ,PNA          ) = -           RK( 049 )                                                           
     &                                  -           RK( 050 )                                                           
     &                                  -           RK( 051 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  14: SO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SO2         ,OH           ) = -           RK( 052 ) * C( SO2          )                                       
      PD( SO2         ,SO2          ) = -           RK( 052 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  15: SULF                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SULF        ,OH           ) =             RK( 052 ) * C( SO2          )                                       
      PD( SULF        ,SO2          ) =             RK( 052 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  16: SULRXN                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SULRXN      ,OH           ) =             RK( 052 ) * C( SO2          )                                       
      PD( SULRXN      ,SO2          ) =             RK( 052 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  17: C2O3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( C2O3        ,NO2          ) = -           RK( 054 ) * C( C2O3         )                                       
      PD( C2O3        ,NO           ) = -           RK( 053 ) * C( C2O3         )                                       
      PD( C2O3        ,O            ) =             RK( 105 ) * C( ALD2         )                                       
      PD( C2O3        ,O3           ) =   0.14300 * RK( 159 ) * C( ISPD         )                                       
     &                                  + 0.60000 * RK( 204 ) * C( XOPN         )                                       
     &                                  + 0.12000 * RK( 208 ) * C( OPEN         )                                       
      PD( C2O3        ,NO3          ) =             RK( 107 ) * C( ALD2         )                                       
     &                                  +           RK( 115 ) * C( GLYD         )                                       
     &                                  +           RK( 120 ) * C( MGLY         )                                       
      PD( C2O3        ,OH           ) =             RK( 095 ) * C( PACD         )                                       
     &                                  +           RK( 106 ) * C( ALD2         )                                       
     &                                  + 0.80000 * RK( 113 ) * C( GLYD         )                                       
     &                                  +           RK( 121 ) * C( MGLY         )                                       
     &                                  +           RK( 130 ) * C( ACET         )                                       
     &                                  + 0.26900 * RK( 158 ) * C( ISPD         )                                       
      PD( C2O3        ,HO2          ) = -           RK( 057 ) * C( C2O3         )                                       
      PD( C2O3        ,C2O3         ) = -           RK( 053 ) * C( NO           )                                       
     &                                  -           RK( 054 ) * C( NO2          )                                       
     &                                  -           RK( 057 ) * C( HO2          )                                       
     &                                  - 4.00000 * RK( 059 ) * C( C2O3         )                                       
     &                                  -           RK( 060 ) * C( CXO3         )                                       
     &                                  -           RK( 073 ) * C( MEO2         )                                       
     &                                  -           RK( 077 ) * C( XO2H         )                                       
     &                                  -           RK( 081 ) * C( XO2          )                                       
     &                                  -           RK( 085 ) * C( XO2N         )                                       
     &                                  -           RK( 153 ) * C( ISO2         )                                       
     &                                  -           RK( 168 ) * C( EPX2         )                                       
     &                                  -           RK( 181 ) * C( BZO2         )                                       
     &                                  -           RK( 186 ) * C( TO2          )                                       
     &                                  -           RK( 193 ) * C( XLO2         )                                       
     &                                  -           RK( 216 ) * C( OPO3         )                                       
      PD( C2O3        ,MEO2         ) = -           RK( 073 ) * C( C2O3         )                                       
      PD( C2O3        ,PAN          ) =             RK( 055 )                                                           
     &                                  + 0.60000 * RK( 056 )                                                           
      PD( C2O3        ,PACD         ) =             RK( 095 ) * C( OH           )                                       
      PD( C2O3        ,CXO3         ) = -           RK( 060 ) * C( C2O3         )                                       
      PD( C2O3        ,ALD2         ) =             RK( 105 ) * C( O            )                                       
     &                                  +           RK( 106 ) * C( OH           )                                       
     &                                  +           RK( 107 ) * C( NO3          )                                       
     &                                  +           RK( 243 ) * C( CL           )                                       
      PD( C2O3        ,XO2H         ) = -           RK( 077 ) * C( C2O3         )                                       
      PD( C2O3        ,XO2          ) = -           RK( 081 ) * C( C2O3         )                                       
      PD( C2O3        ,XO2N         ) = -           RK( 085 ) * C( C2O3         )                                       
      PD( C2O3        ,GLYD         ) =   0.80000 * RK( 113 ) * C( OH           )                                       
     &                                  +           RK( 115 ) * C( NO3          )                                       
      PD( C2O3        ,MGLY         ) =             RK( 119 )                                                           
     &                                  +           RK( 120 ) * C( NO3          )                                       
     &                                  +           RK( 121 ) * C( OH           )                                       
      PD( C2O3        ,KET          ) =   0.50000 * RK( 128 )                                                           
      PD( C2O3        ,ACET         ) =   0.62000 * RK( 129 )                                                           
     &                                  +           RK( 130 ) * C( OH           )                                       
      PD( C2O3        ,ISO2         ) = -           RK( 153 ) * C( C2O3         )                                       
      PD( C2O3        ,ISPD         ) =   0.26900 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.14300 * RK( 159 ) * C( O3           )                                       
     &                                  + 0.20800 * RK( 161 )                                                           
      PD( C2O3        ,OPO3         ) = -           RK( 216 ) * C( C2O3         )                                       
      PD( C2O3        ,EPX2         ) = -           RK( 168 ) * C( C2O3         )                                       
      PD( C2O3        ,BZO2         ) = -           RK( 181 ) * C( C2O3         )                                       
      PD( C2O3        ,OPEN         ) =   0.12000 * RK( 208 ) * C( O3           )                                       
      PD( C2O3        ,TO2          ) = -           RK( 186 ) * C( C2O3         )                                       
      PD( C2O3        ,XOPN         ) =   0.30000 * RK( 202 )                                                           
     &                                  + 0.60000 * RK( 204 ) * C( O3           )                                       
      PD( C2O3        ,XLO2         ) = -           RK( 193 ) * C( C2O3         )                                       
      PD( C2O3        ,CL           ) =             RK( 243 ) * C( ALD2         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  18: MEO2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEO2        ,NO           ) =             RK( 053 ) * C( C2O3         )                                       
     &                                  -           RK( 071 ) * C( MEO2         )                                       
      PD( MEO2        ,OH           ) =   0.60000 * RK( 087 ) * C( MEPX         )                                       
     &                                  +           RK( 094 ) * C( AACD         )                                       
     &                                  +           RK( 124 )                                                           
     &                                  + 0.11500 * RK( 158 ) * C( ISPD         )                                       
     &                                  +           RK( 220 ) * C( ECH4         )                                       
      PD( MEO2        ,HO2          ) =   0.44000 * RK( 057 ) * C( C2O3         )                                       
     &                                  -           RK( 072 ) * C( MEO2         )                                       
      PD( MEO2        ,C2O3         ) =             RK( 053 ) * C( NO           )                                       
     &                                  + 0.44000 * RK( 057 ) * C( HO2          )                                       
     &                                  + 4.00000 * RK( 059 ) * C( C2O3         )                                       
     &                                  +           RK( 060 ) * C( CXO3         )                                       
     &                                  - 0.10000 * RK( 073 ) * C( MEO2         )                                       
     &                                  + 0.80000 * RK( 077 ) * C( XO2H         )                                       
     &                                  + 0.80000 * RK( 081 ) * C( XO2          )                                       
     &                                  + 0.80000 * RK( 085 ) * C( XO2N         )                                       
     &                                  + 0.80000 * RK( 153 ) * C( ISO2         )                                       
     &                                  + 0.80000 * RK( 168 ) * C( EPX2         )                                       
     &                                  +           RK( 181 ) * C( BZO2         )                                       
     &                                  +           RK( 186 ) * C( TO2          )                                       
     &                                  +           RK( 193 ) * C( XLO2         )                                       
     &                                  +           RK( 216 ) * C( OPO3         )                                       
      PD( MEO2        ,MEO2         ) = -           RK( 071 ) * C( NO           )                                       
     &                                  -           RK( 072 ) * C( HO2          )                                       
     &                                  - 0.10000 * RK( 073 ) * C( C2O3         )                                       
     &                                  -           RK( 074 ) * C( RO2          )                                       
     &                                  -           RK( 231 ) * C( CLO          )                                       
      PD( MEO2        ,RO2          ) = -           RK( 074 ) * C( MEO2         )                                       
      PD( MEO2        ,PAN          ) =   0.40000 * RK( 056 )                                                           
      PD( MEO2        ,AACD         ) =             RK( 094 ) * C( OH           )                                       
      PD( MEO2        ,CXO3         ) =             RK( 060 ) * C( C2O3         )                                       
      PD( MEO2        ,ALD2         ) =             RK( 108 )                                                           
      PD( MEO2        ,XO2H         ) =   0.80000 * RK( 077 ) * C( C2O3         )                                       
      PD( MEO2        ,MEPX         ) =   0.60000 * RK( 087 ) * C( OH           )                                       
     &                                  +           RK( 088 )                                                           
      PD( MEO2        ,XO2          ) =   0.80000 * RK( 081 ) * C( C2O3         )                                       
      PD( MEO2        ,XO2N         ) =   0.80000 * RK( 085 ) * C( C2O3         )                                       
      PD( MEO2        ,KET          ) =   0.50000 * RK( 128 )                                                           
      PD( MEO2        ,ACET         ) =   1.38000 * RK( 129 )                                                           
      PD( MEO2        ,ISO2         ) =   0.80000 * RK( 153 ) * C( C2O3         )                                       
      PD( MEO2        ,ISPD         ) =   0.11500 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.34000 * RK( 161 )                                                           
      PD( MEO2        ,OPO3         ) =             RK( 216 ) * C( C2O3         )                                       
      PD( MEO2        ,EPX2         ) =   0.80000 * RK( 168 ) * C( C2O3         )                                       
      PD( MEO2        ,BZO2         ) =             RK( 181 ) * C( C2O3         )                                       
      PD( MEO2        ,TO2          ) =             RK( 186 ) * C( C2O3         )                                       
      PD( MEO2        ,XLO2         ) =             RK( 193 ) * C( C2O3         )                                       
      PD( MEO2        ,ECH4         ) =             RK( 220 ) * C( OH           )                                       
      PD( MEO2        ,CL           ) =             RK( 234 )                                                           
      PD( MEO2        ,CLO          ) = -           RK( 231 ) * C( MEO2         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  19: RO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( RO2         ,NO           ) =             RK( 053 ) * C( C2O3         )                                       
     &                                  +           RK( 061 ) * C( CXO3         )                                       
     &                                  -           RK( 068 ) * C( RO2          )                                       
     &                                  + 0.08200 * RK( 151 ) * C( ISO2         )                                       
      PD( RO2         ,O            ) =   0.70000 * RK( 137 ) * C( ETH          )                                       
     &                                  + 0.21000 * RK( 141 ) * C( OLE          )                                       
     &                                  + 0.10000 * RK( 145 ) * C( IOLE         )                                       
     &                                  + 0.25000 * RK( 150 ) * C( ISOP         )                                       
      PD( RO2         ,O3           ) =   0.15000 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.30000 * RK( 147 ) * C( IOLE         )                                       
     &                                  + 0.20000 * RK( 156 ) * C( ISOP         )                                       
     &                                  + 0.94000 * RK( 173 ) * C( TERP         )                                       
     &                                  + 0.94000 * RK( 177 ) * C( APIN         )                                       
     &                                  + 0.30000 * RK( 204 ) * C( XOPN         )                                       
      PD( RO2         ,NO3          ) =   0.50000 * RK( 118 ) * C( GLY          )                                       
     &                                  +           RK( 120 ) * C( MGLY         )                                       
     &                                  +           RK( 140 ) * C( ETH          )                                       
     &                                  +           RK( 144 ) * C( OLE          )                                       
     &                                  +           RK( 148 ) * C( IOLE         )                                       
     &                                  +           RK( 157 ) * C( ISOP         )                                       
     &                                  + 0.28400 * RK( 160 ) * C( ISPD         )                                       
     &                                  + 1.28000 * RK( 174 ) * C( TERP         )                                       
     &                                  + 1.28000 * RK( 178 ) * C( APIN         )                                       
     &                                  + 0.70000 * RK( 196 ) * C( CRES         )                                       
     &                                  +           RK( 205 ) * C( XOPN         )                                       
      PD( RO2         ,OH           ) =   0.60000 * RK( 087 ) * C( MEPX         )                                       
     &                                  + 0.60000 * RK( 089 ) * C( ROOH         )                                       
     &                                  +           RK( 094 ) * C( AACD         )                                       
     &                                  + 0.20000 * RK( 116 ) * C( GLY          )                                       
     &                                  +           RK( 124 )                                                           
     &                                  +           RK( 125 ) * C( ETHA         )                                       
     &                                  + 0.10000 * RK( 127 ) * C( ETOH         )                                       
     &                                  +           RK( 130 ) * C( ACET         )                                       
     &                                  +           RK( 138 ) * C( ETH          )                                       
     &                                  + 1.19500 * RK( 142 ) * C( OLE          )                                       
     &                                  +           RK( 146 ) * C( IOLE         )                                       
     &                                  +           RK( 149 ) * C( ISOP         )                                       
     &                                  + 0.65800 * RK( 158 ) * C( ISPD         )                                       
     &                                  + 0.06700 * RK( 162 ) * C( ISPX         )                                       
     &                                  +           RK( 165 ) * C( EPOX         )                                       
     &                                  +           RK( 170 ) * C( INTR         )                                       
     &                                  + 1.50000 * RK( 172 ) * C( TERP         )                                       
     &                                  + 1.50000 * RK( 176 ) * C( APIN         )                                       
     &                                  + 0.35200 * RK( 179 ) * C( BENZENE      )                                       
     &                                  + 0.72000 * RK( 184 ) * C( TOL          )                                       
     &                                  + 0.60200 * RK( 189 ) * C( XYLMN        )                                       
     &                                  + 0.60200 * RK( 190 ) * C( NAPH         )                                       
     &                                  + 0.02000 * RK( 195 ) * C( CRES         )                                       
     &                                  + 2.00000 * RK( 203 ) * C( XOPN         )                                       
     &                                  + 0.40000 * RK( 207 ) * C( OPEN         )                                       
     &                                  +           RK( 220 ) * C( ECH4         )                                       
      PD( RO2         ,HO2          ) =   0.44000 * RK( 057 ) * C( C2O3         )                                       
     &                                  + 0.44000 * RK( 065 ) * C( CXO3         )                                       
     &                                  -           RK( 069 ) * C( RO2          )                                       
     &                                  + 0.44000 * RK( 215 ) * C( OPO3         )                                       
      PD( RO2         ,C2O3         ) =             RK( 053 ) * C( NO           )                                       
     &                                  + 0.44000 * RK( 057 ) * C( HO2          )                                       
     &                                  -           RK( 058 ) * C( RO2          )                                       
     &                                  + 4.00000 * RK( 059 ) * C( C2O3         )                                       
     &                                  + 2.00000 * RK( 060 ) * C( CXO3         )                                       
     &                                  + 0.90000 * RK( 073 ) * C( MEO2         )                                       
     &                                  + 0.80000 * RK( 077 ) * C( XO2H         )                                       
     &                                  + 0.80000 * RK( 081 ) * C( XO2          )                                       
     &                                  + 0.80000 * RK( 085 ) * C( XO2N         )                                       
     &                                  + 0.87200 * RK( 153 ) * C( ISO2         )                                       
     &                                  + 0.80000 * RK( 168 ) * C( EPX2         )                                       
     &                                  +           RK( 181 ) * C( BZO2         )                                       
     &                                  +           RK( 186 ) * C( TO2          )                                       
     &                                  +           RK( 193 ) * C( XLO2         )                                       
     &                                  + 2.00000 * RK( 216 ) * C( OPO3         )                                       
      PD( RO2         ,MEO2         ) =   0.90000 * RK( 073 ) * C( C2O3         )                                       
      PD( RO2         ,RO2          ) = -           RK( 058 ) * C( C2O3         )                                       
     &                                  - 0.20000 * RK( 066 ) * C( CXO3         )                                       
     &                                  -           RK( 068 ) * C( NO           )                                       
     &                                  -           RK( 069 ) * C( HO2          )                                       
     &                                  - 4.00000 * RK( 070 ) * C( RO2          )                                       
     &                                  + 0.07200 * RK( 154 ) * C( ISO2         )                                       
     &                                  + 0.80000 * RK( 217 ) * C( OPO3         )                                       
      PD( RO2         ,PAN          ) =   0.40000 * RK( 056 )                                                           
      PD( RO2         ,AACD         ) =             RK( 094 ) * C( OH           )                                       
      PD( RO2         ,CXO3         ) =   2.00000 * RK( 060 ) * C( C2O3         )                                       
     &                                  +           RK( 061 ) * C( NO           )                                       
     &                                  + 0.44000 * RK( 065 ) * C( HO2          )                                       
     &                                  - 0.20000 * RK( 066 ) * C( RO2          )                                       
     &                                  + 4.00000 * RK( 067 ) * C( CXO3         )                                       
      PD( RO2         ,ALD2         ) =             RK( 108 )                                                           
      PD( RO2         ,XO2H         ) =   0.80000 * RK( 077 ) * C( C2O3         )                                       
      PD( RO2         ,PANX         ) =   0.40000 * RK( 064 )                                                           
      PD( RO2         ,MEPX         ) =   0.60000 * RK( 087 ) * C( OH           )                                       
     &                                  +           RK( 088 )                                                           
      PD( RO2         ,ROOH         ) =   0.60000 * RK( 089 ) * C( OH           )                                       
      PD( RO2         ,XO2          ) =   0.80000 * RK( 081 ) * C( C2O3         )                                       
      PD( RO2         ,XO2N         ) =   0.80000 * RK( 085 ) * C( C2O3         )                                       
      PD( RO2         ,ALDX         ) =             RK( 112 )                                                           
      PD( RO2         ,GLYD         ) =   0.11000 * RK( 114 )                                                           
      PD( RO2         ,GLY          ) =   0.20000 * RK( 116 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 118 ) * C( NO3          )                                       
      PD( RO2         ,MGLY         ) =             RK( 120 ) * C( NO3          )                                       
      PD( RO2         ,ETHA         ) =             RK( 125 ) * C( OH           )                                       
     &                                  +           RK( 237 ) * C( CL           )                                       
      PD( RO2         ,ETOH         ) =   0.10000 * RK( 127 ) * C( OH           )                                       
      PD( RO2         ,KET          ) =             RK( 128 )                                                           
      PD( RO2         ,ACET         ) =   1.38000 * RK( 129 )                                                           
     &                                  +           RK( 130 ) * C( OH           )                                       
      PD( RO2         ,PRPA         ) =             RK( 236 ) * C( CL           )                                       
      PD( RO2         ,XPRP         ) =             RK( 221 )                                                           
     &                                  +           RK( 222 )                                                           
      PD( RO2         ,XPAR         ) =             RK( 223 )                                                           
     &                                  +           RK( 224 )                                                           
      PD( RO2         ,ROR          ) =   0.98000 * RK( 133 )                                                           
      PD( RO2         ,ETH          ) =   0.70000 * RK( 137 ) * C( O            )                                       
     &                                  +           RK( 138 ) * C( OH           )                                       
     &                                  +           RK( 140 ) * C( NO3          )                                       
      PD( RO2         ,OLE          ) =   0.21000 * RK( 141 ) * C( O            )                                       
     &                                  + 1.19500 * RK( 142 ) * C( OH           )                                       
     &                                  + 0.15000 * RK( 143 ) * C( O3           )                                       
     &                                  +           RK( 144 ) * C( NO3          )                                       
      PD( RO2         ,IOLE         ) =   0.10000 * RK( 145 ) * C( O            )                                       
     &                                  +           RK( 146 ) * C( OH           )                                       
     &                                  + 0.30000 * RK( 147 ) * C( O3           )                                       
     &                                  +           RK( 148 ) * C( NO3          )                                       
      PD( RO2         ,ISOP         ) =             RK( 149 ) * C( OH           )                                       
     &                                  + 0.25000 * RK( 150 ) * C( O            )                                       
     &                                  + 0.20000 * RK( 156 ) * C( O3           )                                       
     &                                  +           RK( 157 ) * C( NO3          )                                       
     &                                  +           RK( 241 ) * C( CL           )                                       
      PD( RO2         ,ISO2         ) =   0.08200 * RK( 151 ) * C( NO           )                                       
     &                                  + 0.87200 * RK( 153 ) * C( C2O3         )                                       
     &                                  + 0.07200 * RK( 154 ) * C( RO2          )                                       
      PD( RO2         ,ISPD         ) =   0.65800 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.28400 * RK( 160 ) * C( NO3          )                                       
     &                                  + 0.84000 * RK( 161 )                                                           
      PD( RO2         ,INTR         ) =             RK( 170 ) * C( OH           )                                       
      PD( RO2         ,ISPX         ) =   0.06700 * RK( 162 ) * C( OH           )                                       
      PD( RO2         ,OPO3         ) =   0.44000 * RK( 215 ) * C( HO2          )                                       
     &                                  + 2.00000 * RK( 216 ) * C( C2O3         )                                       
     &                                  + 0.80000 * RK( 217 ) * C( RO2          )                                       
      PD( RO2         ,EPOX         ) =             RK( 165 ) * C( OH           )                                       
      PD( RO2         ,EPX2         ) =   0.80000 * RK( 168 ) * C( C2O3         )                                       
      PD( RO2         ,TERP         ) =   1.50000 * RK( 172 ) * C( OH           )                                       
     &                                  + 0.94000 * RK( 173 ) * C( O3           )                                       
     &                                  + 1.28000 * RK( 174 ) * C( NO3          )                                       
      PD( RO2         ,APIN         ) =   1.50000 * RK( 176 ) * C( OH           )                                       
     &                                  + 0.94000 * RK( 177 ) * C( O3           )                                       
     &                                  + 1.28000 * RK( 178 ) * C( NO3          )                                       
      PD( RO2         ,BENZENE      ) =   0.35200 * RK( 179 ) * C( OH           )                                       
      PD( RO2         ,CRES         ) =   0.02000 * RK( 195 ) * C( OH           )                                       
     &                                  + 0.70000 * RK( 196 ) * C( NO3          )                                       
      PD( RO2         ,BZO2         ) =             RK( 181 ) * C( C2O3         )                                       
      PD( RO2         ,OPEN         ) =   0.40000 * RK( 207 ) * C( OH           )                                       
      PD( RO2         ,TOL          ) =   0.72000 * RK( 184 ) * C( OH           )                                       
     &                                  + 0.72000 * RK( 248 ) * C( CL           )                                       
      PD( RO2         ,TO2          ) =             RK( 186 ) * C( C2O3         )                                       
      PD( RO2         ,XOPN         ) =   2.00000 * RK( 203 ) * C( OH           )                                       
     &                                  + 0.30000 * RK( 204 ) * C( O3           )                                       
     &                                  +           RK( 205 ) * C( NO3          )                                       
      PD( RO2         ,XYLMN        ) =   0.60200 * RK( 189 ) * C( OH           )                                       
     &                                  + 0.60200 * RK( 249 ) * C( CL           )                                       
      PD( RO2         ,XLO2         ) =             RK( 193 ) * C( C2O3         )                                       
      PD( RO2         ,NAPH         ) =   0.60200 * RK( 190 ) * C( OH           )                                       
     &                                  + 0.60200 * RK( 250 ) * C( CL           )                                       
      PD( RO2         ,ECH4         ) =             RK( 220 ) * C( OH           )                                       
      PD( RO2         ,CL           ) =             RK( 234 )                                                           
     &                                  +           RK( 236 ) * C( PRPA         )                                       
     &                                  +           RK( 237 ) * C( ETHA         )                                       
     &                                  +           RK( 241 ) * C( ISOP         )                                       
     &                                  + 0.72000 * RK( 248 ) * C( TOL          )                                       
     &                                  + 0.60200 * RK( 249 ) * C( XYLMN        )                                       
     &                                  + 0.60200 * RK( 250 ) * C( NAPH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  20: PAN                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAN         ,NO2          ) =             RK( 054 ) * C( C2O3         )                                       
      PD( PAN         ,C2O3         ) =             RK( 054 ) * C( NO2          )                                       
      PD( PAN         ,PAN          ) = -           RK( 055 )                                                           
     &                                  -           RK( 056 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  21: PACD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PACD        ,OH           ) = -           RK( 095 ) * C( PACD         )                                       
      PD( PACD        ,HO2          ) =   0.41000 * RK( 057 ) * C( C2O3         )                                       
     &                                  + 0.41000 * RK( 065 ) * C( CXO3         )                                       
     &                                  + 0.41000 * RK( 215 ) * C( OPO3         )                                       
      PD( PACD        ,C2O3         ) =   0.41000 * RK( 057 ) * C( HO2          )                                       
      PD( PACD        ,PACD         ) = -           RK( 095 ) * C( OH           )                                       
      PD( PACD        ,CXO3         ) =   0.41000 * RK( 065 ) * C( HO2          )                                       
      PD( PACD        ,OPO3         ) =   0.41000 * RK( 215 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  22: AACD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AACD        ,O3           ) =   0.13000 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.08000 * RK( 147 ) * C( IOLE         )                                       
      PD( AACD        ,OH           ) = -           RK( 094 ) * C( AACD         )                                       
      PD( AACD        ,HO2          ) =   0.15000 * RK( 057 ) * C( C2O3         )                                       
     &                                  + 0.15000 * RK( 065 ) * C( CXO3         )                                       
     &                                  + 0.15000 * RK( 215 ) * C( OPO3         )                                       
      PD( AACD        ,C2O3         ) =   0.15000 * RK( 057 ) * C( HO2          )                                       
     &                                  + 0.10000 * RK( 073 ) * C( MEO2         )                                       
     &                                  + 0.20000 * RK( 077 ) * C( XO2H         )                                       
     &                                  + 0.20000 * RK( 081 ) * C( XO2          )                                       
     &                                  + 0.20000 * RK( 085 ) * C( XO2N         )                                       
     &                                  + 0.20000 * RK( 153 ) * C( ISO2         )                                       
     &                                  + 0.20000 * RK( 168 ) * C( EPX2         )                                       
      PD( AACD        ,MEO2         ) =   0.10000 * RK( 073 ) * C( C2O3         )                                       
      PD( AACD        ,RO2          ) =   0.20000 * RK( 217 ) * C( OPO3         )                                       
      PD( AACD        ,AACD         ) = -           RK( 094 ) * C( OH           )                                       
      PD( AACD        ,CXO3         ) =   0.15000 * RK( 065 ) * C( HO2          )                                       
      PD( AACD        ,XO2H         ) =   0.20000 * RK( 077 ) * C( C2O3         )                                       
      PD( AACD        ,XO2          ) =   0.20000 * RK( 081 ) * C( C2O3         )                                       
      PD( AACD        ,XO2N         ) =   0.20000 * RK( 085 ) * C( C2O3         )                                       
      PD( AACD        ,OLE          ) =   0.13000 * RK( 143 ) * C( O3           )                                       
      PD( AACD        ,IOLE         ) =   0.08000 * RK( 147 ) * C( O3           )                                       
      PD( AACD        ,ISO2         ) =   0.20000 * RK( 153 ) * C( C2O3         )                                       
      PD( AACD        ,OPO3         ) =   0.15000 * RK( 215 ) * C( HO2          )                                       
     &                                  + 0.20000 * RK( 217 ) * C( RO2          )                                       
      PD( AACD        ,EPX2         ) =   0.20000 * RK( 168 ) * C( C2O3         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  23: CXO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CXO3        ,NO2          ) = -           RK( 062 ) * C( CXO3         )                                       
      PD( CXO3        ,NO           ) = -           RK( 061 ) * C( CXO3         )                                       
     &                                  + 0.20000 * RK( 212 ) * C( OPO3         )                                       
      PD( CXO3        ,O            ) =             RK( 109 ) * C( ALDX         )                                       
     &                                  + 0.25000 * RK( 150 ) * C( ISOP         )                                       
      PD( CXO3        ,O3           ) =   0.20000 * RK( 156 ) * C( ISOP         )                                       
     &                                  + 0.39000 * RK( 173 ) * C( TERP         )                                       
     &                                  + 0.39000 * RK( 177 ) * C( APIN         )                                       
      PD( CXO3        ,NO3          ) =             RK( 111 ) * C( ALDX         )                                       
     &                                  + 0.71700 * RK( 160 ) * C( ISPD         )                                       
      PD( CXO3        ,OH           ) =             RK( 110 ) * C( ALDX         )                                       
      PD( CXO3        ,HO2          ) = -           RK( 065 ) * C( CXO3         )                                       
      PD( CXO3        ,C2O3         ) = -           RK( 060 ) * C( CXO3         )                                       
      PD( CXO3        ,RO2          ) = -           RK( 066 ) * C( CXO3         )                                       
      PD( CXO3        ,CXO3         ) = -           RK( 060 ) * C( C2O3         )                                       
     &                                  -           RK( 061 ) * C( NO           )                                       
     &                                  -           RK( 062 ) * C( NO2          )                                       
     &                                  -           RK( 065 ) * C( HO2          )                                       
     &                                  -           RK( 066 ) * C( RO2          )                                       
     &                                  - 4.00000 * RK( 067 ) * C( CXO3         )                                       
      PD( CXO3        ,PANX         ) =             RK( 063 )                                                           
     &                                  + 0.60000 * RK( 064 )                                                           
      PD( CXO3        ,ALDX         ) =             RK( 109 ) * C( O            )                                       
     &                                  +           RK( 110 ) * C( OH           )                                       
     &                                  +           RK( 111 ) * C( NO3          )                                       
     &                                  +           RK( 244 ) * C( CL           )                                       
      PD( CXO3        ,KET          ) =   0.50000 * RK( 128 )                                                           
      PD( CXO3        ,ISOP         ) =   0.25000 * RK( 150 ) * C( O            )                                       
     &                                  + 0.20000 * RK( 156 ) * C( O3           )                                       
      PD( CXO3        ,ISPD         ) =   0.71700 * RK( 160 ) * C( NO3          )                                       
      PD( CXO3        ,OPO3         ) =   0.20000 * RK( 212 ) * C( NO           )                                       
      PD( CXO3        ,TERP         ) =   0.39000 * RK( 173 ) * C( O3           )                                       
      PD( CXO3        ,APIN         ) =   0.39000 * RK( 177 ) * C( O3           )                                       
      PD( CXO3        ,CL           ) =             RK( 244 ) * C( ALDX         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  24: ALD2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALD2        ,NO           ) =             RK( 061 ) * C( CXO3         )                                       
      PD( ALD2        ,O            ) = -           RK( 105 ) * C( ALD2         )                                       
     &                                  + 0.20000 * RK( 141 ) * C( OLE          )                                       
     &                                  + 1.24000 * RK( 145 ) * C( IOLE         )                                       
      PD( ALD2        ,O3           ) =   0.29500 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.73200 * RK( 147 ) * C( IOLE         )                                       
     &                                  + 0.04000 * RK( 159 ) * C( ISPD         )                                       
     &                                  + 0.10000 * RK( 204 ) * C( XOPN         )                                       
     &                                  + 0.02000 * RK( 208 ) * C( OPEN         )                                       
      PD( ALD2        ,NO3          ) = -           RK( 107 ) * C( ALD2         )                                       
     &                                  + 0.25000 * RK( 144 ) * C( OLE          )                                       
     &                                  + 0.50000 * RK( 148 ) * C( IOLE         )                                       
      PD( ALD2        ,OH           ) = -           RK( 106 ) * C( ALD2         )                                       
     &                                  + 0.99100 * RK( 125 ) * C( ETHA         )                                       
     &                                  + 0.95000 * RK( 127 ) * C( ETOH         )                                       
     &                                  + 0.48800 * RK( 142 ) * C( OLE          )                                       
     &                                  + 1.30000 * RK( 146 ) * C( IOLE         )                                       
     &                                  +           RK( 219 ) * C( PANX         )                                       
      PD( ALD2        ,HO2          ) =   0.44000 * RK( 065 ) * C( CXO3         )                                       
      PD( ALD2        ,C2O3         ) =             RK( 060 ) * C( CXO3         )                                       
      PD( ALD2        ,RO2          ) =   0.80000 * RK( 066 ) * C( CXO3         )                                       
      PD( ALD2        ,CXO3         ) =             RK( 060 ) * C( C2O3         )                                       
     &                                  +           RK( 061 ) * C( NO           )                                       
     &                                  + 0.44000 * RK( 065 ) * C( HO2          )                                       
     &                                  + 0.80000 * RK( 066 ) * C( RO2          )                                       
     &                                  + 4.00000 * RK( 067 ) * C( CXO3         )                                       
      PD( ALD2        ,ALD2         ) = -           RK( 105 ) * C( O            )                                       
     &                                  -           RK( 106 ) * C( OH           )                                       
     &                                  -           RK( 107 ) * C( NO3          )                                       
     &                                  -           RK( 108 )                                                           
     &                                  -           RK( 243 ) * C( CL           )                                       
      PD( ALD2        ,PANX         ) =   0.40000 * RK( 064 )                                                           
     &                                  +           RK( 219 ) * C( OH           )                                       
      PD( ALD2        ,ALDX         ) =             RK( 112 )                                                           
      PD( ALD2        ,ETHA         ) =   0.99100 * RK( 125 ) * C( OH           )                                       
     &                                  + 0.99100 * RK( 237 ) * C( CL           )                                       
      PD( ALD2        ,ETOH         ) =   0.95000 * RK( 127 ) * C( OH           )                                       
     &                                  +           RK( 246 ) * C( CL           )                                       
      PD( ALD2        ,KET          ) =   0.50000 * RK( 128 )                                                           
      PD( ALD2        ,ROR          ) =   0.74000 * RK( 133 )                                                           
      PD( ALD2        ,OLE          ) =   0.20000 * RK( 141 ) * C( O            )                                       
     &                                  + 0.48800 * RK( 142 ) * C( OH           )                                       
     &                                  + 0.29500 * RK( 143 ) * C( O3           )                                       
     &                                  + 0.25000 * RK( 144 ) * C( NO3          )                                       
     &                                  + 0.33000 * RK( 239 ) * C( CL           )                                       
      PD( ALD2        ,IOLE         ) =   1.24000 * RK( 145 ) * C( O            )                                       
     &                                  + 1.30000 * RK( 146 ) * C( OH           )                                       
     &                                  + 0.73200 * RK( 147 ) * C( O3           )                                       
     &                                  + 0.50000 * RK( 148 ) * C( NO3          )                                       
     &                                  + 0.45000 * RK( 240 ) * C( CL           )                                       
      PD( ALD2        ,ISPD         ) =   0.04000 * RK( 159 ) * C( O3           )                                       
      PD( ALD2        ,OPEN         ) =   0.02000 * RK( 208 ) * C( O3           )                                       
      PD( ALD2        ,XOPN         ) =   0.10000 * RK( 204 ) * C( O3           )                                       
      PD( ALD2        ,CL           ) =   0.99100 * RK( 237 ) * C( ETHA         )                                       
     &                                  + 0.33000 * RK( 239 ) * C( OLE          )                                       
     &                                  + 0.45000 * RK( 240 ) * C( IOLE         )                                       
     &                                  -           RK( 243 ) * C( ALD2         )                                       
     &                                  +           RK( 246 ) * C( ETOH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  25: XO2H                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XO2H        ,NO           ) =             RK( 061 ) * C( CXO3         )                                       
     &                                  -           RK( 075 ) * C( XO2H         )                                       
     &                                  + 0.08200 * RK( 151 ) * C( ISO2         )                                       
      PD( XO2H        ,O            ) =   0.70000 * RK( 137 ) * C( ETH          )                                       
     &                                  + 0.20000 * RK( 141 ) * C( OLE          )                                       
     &                                  + 0.10000 * RK( 145 ) * C( IOLE         )                                       
      PD( XO2H        ,O3           ) =   0.15000 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.30000 * RK( 147 ) * C( IOLE         )                                       
     &                                  + 0.07000 * RK( 173 ) * C( TERP         )                                       
     &                                  + 0.07000 * RK( 177 ) * C( APIN         )                                       
     &                                  + 0.30000 * RK( 204 ) * C( XOPN         )                                       
      PD( XO2H        ,NO3          ) =   0.50000 * RK( 140 ) * C( ETH          )                                       
     &                                  + 0.48000 * RK( 144 ) * C( OLE          )                                       
     &                                  + 0.48000 * RK( 148 ) * C( IOLE         )                                       
     &                                  + 0.64000 * RK( 157 ) * C( ISOP         )                                       
     &                                  + 0.14200 * RK( 160 ) * C( ISPD         )                                       
     &                                  + 0.28000 * RK( 174 ) * C( TERP         )                                       
     &                                  + 0.28000 * RK( 178 ) * C( APIN         )                                       
     &                                  + 0.12000 * RK( 196 ) * C( CRES         )                                       
     &                                  + 0.45000 * RK( 205 ) * C( XOPN         )                                       
      PD( XO2H        ,OH           ) =   0.54000 * RK( 089 ) * C( ROOH         )                                       
     &                                  + 0.99100 * RK( 125 ) * C( ETHA         )                                       
     &                                  + 0.10000 * RK( 127 ) * C( ETOH         )                                       
     &                                  +           RK( 138 ) * C( ETH          )                                       
     &                                  + 0.97600 * RK( 142 ) * C( OLE          )                                       
     &                                  +           RK( 146 ) * C( IOLE         )                                       
     &                                  + 0.37000 * RK( 170 ) * C( INTR         )                                       
     &                                  + 0.75000 * RK( 172 ) * C( TERP         )                                       
     &                                  + 0.75000 * RK( 176 ) * C( APIN         )                                       
     &                                  + 0.07000 * RK( 184 ) * C( TOL          )                                       
     &                                  + 0.05800 * RK( 189 ) * C( XYLMN        )                                       
     &                                  + 0.05800 * RK( 190 ) * C( NAPH         )                                       
     &                                  + 2.00000 * RK( 203 ) * C( XOPN         )                                       
     &                                  + 0.40000 * RK( 207 ) * C( OPEN         )                                       
      PD( XO2H        ,HO2          ) =   0.44000 * RK( 065 ) * C( CXO3         )                                       
     &                                  -           RK( 076 ) * C( XO2H         )                                       
     &                                  + 0.44000 * RK( 215 ) * C( OPO3         )                                       
      PD( XO2H        ,C2O3         ) =             RK( 060 ) * C( CXO3         )                                       
     &                                  -           RK( 077 ) * C( XO2H         )                                       
     &                                  + 0.07200 * RK( 153 ) * C( ISO2         )                                       
      PD( XO2H        ,RO2          ) =   0.80000 * RK( 066 ) * C( CXO3         )                                       
     &                                  -           RK( 078 ) * C( XO2H         )                                       
     &                                  + 0.07200 * RK( 154 ) * C( ISO2         )                                       
     &                                  + 0.80000 * RK( 217 ) * C( OPO3         )                                       
      PD( XO2H        ,CXO3         ) =             RK( 060 ) * C( C2O3         )                                       
     &                                  +           RK( 061 ) * C( NO           )                                       
     &                                  + 0.44000 * RK( 065 ) * C( HO2          )                                       
     &                                  + 0.80000 * RK( 066 ) * C( RO2          )                                       
     &                                  + 4.00000 * RK( 067 ) * C( CXO3         )                                       
      PD( XO2H        ,XO2H         ) = -           RK( 075 ) * C( NO           )                                       
     &                                  -           RK( 076 ) * C( HO2          )                                       
     &                                  -           RK( 077 ) * C( C2O3         )                                       
     &                                  -           RK( 078 ) * C( RO2          )                                       
      PD( XO2H        ,PANX         ) =   0.40000 * RK( 064 )                                                           
      PD( XO2H        ,ROOH         ) =   0.54000 * RK( 089 ) * C( OH           )                                       
      PD( XO2H        ,ALDX         ) =             RK( 112 )                                                           
      PD( XO2H        ,GLYD         ) =   0.11000 * RK( 114 )                                                           
      PD( XO2H        ,ETHA         ) =   0.99100 * RK( 125 ) * C( OH           )                                       
     &                                  + 0.99100 * RK( 237 ) * C( CL           )                                       
      PD( XO2H        ,ETOH         ) =   0.10000 * RK( 127 ) * C( OH           )                                       
      PD( XO2H        ,KET          ) =   0.50000 * RK( 128 )                                                           
      PD( XO2H        ,PRPA         ) =   0.97000 * RK( 236 ) * C( CL           )                                       
      PD( XO2H        ,XPRP         ) =             RK( 222 )                                                           
      PD( XO2H        ,XPAR         ) =   0.12600 * RK( 224 )                                                           
      PD( XO2H        ,ROR          ) =   0.94000 * RK( 133 )                                                           
      PD( XO2H        ,ETH          ) =   0.70000 * RK( 137 ) * C( O            )                                       
     &                                  +           RK( 138 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 140 ) * C( NO3          )                                       
      PD( XO2H        ,OLE          ) =   0.20000 * RK( 141 ) * C( O            )                                       
     &                                  + 0.97600 * RK( 142 ) * C( OH           )                                       
     &                                  + 0.15000 * RK( 143 ) * C( O3           )                                       
     &                                  + 0.48000 * RK( 144 ) * C( NO3          )                                       
      PD( XO2H        ,IOLE         ) =   0.10000 * RK( 145 ) * C( O            )                                       
     &                                  +           RK( 146 ) * C( OH           )                                       
     &                                  + 0.30000 * RK( 147 ) * C( O3           )                                       
     &                                  + 0.48000 * RK( 148 ) * C( NO3          )                                       
      PD( XO2H        ,ISOP         ) =   0.64000 * RK( 157 ) * C( NO3          )                                       
     &                                  + 0.96000 * RK( 241 ) * C( CL           )                                       
      PD( XO2H        ,ISO2         ) =   0.08200 * RK( 151 ) * C( NO           )                                       
     &                                  + 0.07200 * RK( 153 ) * C( C2O3         )                                       
     &                                  + 0.07200 * RK( 154 ) * C( RO2          )                                       
      PD( XO2H        ,ISPD         ) =   0.14200 * RK( 160 ) * C( NO3          )                                       
     &                                  + 0.34000 * RK( 161 )                                                           
      PD( XO2H        ,INTR         ) =   0.37000 * RK( 170 ) * C( OH           )                                       
      PD( XO2H        ,OPO3         ) =   0.44000 * RK( 215 ) * C( HO2          )                                       
     &                                  + 0.80000 * RK( 217 ) * C( RO2          )                                       
      PD( XO2H        ,TERP         ) =   0.75000 * RK( 172 ) * C( OH           )                                       
     &                                  + 0.07000 * RK( 173 ) * C( O3           )                                       
     &                                  + 0.28000 * RK( 174 ) * C( NO3          )                                       
      PD( XO2H        ,APIN         ) =   0.75000 * RK( 176 ) * C( OH           )                                       
     &                                  + 0.07000 * RK( 177 ) * C( O3           )                                       
     &                                  + 0.28000 * RK( 178 ) * C( NO3          )                                       
      PD( XO2H        ,CRES         ) =   0.12000 * RK( 196 ) * C( NO3          )                                       
      PD( XO2H        ,OPEN         ) =   0.40000 * RK( 207 ) * C( OH           )                                       
      PD( XO2H        ,TOL          ) =   0.07000 * RK( 184 ) * C( OH           )                                       
     &                                  + 0.07000 * RK( 248 ) * C( CL           )                                       
      PD( XO2H        ,XOPN         ) =             RK( 202 )                                                           
     &                                  + 2.00000 * RK( 203 ) * C( OH           )                                       
     &                                  + 0.30000 * RK( 204 ) * C( O3           )                                       
     &                                  + 0.45000 * RK( 205 ) * C( NO3          )                                       
      PD( XO2H        ,XYLMN        ) =   0.05800 * RK( 189 ) * C( OH           )                                       
     &                                  + 0.05800 * RK( 249 ) * C( CL           )                                       
      PD( XO2H        ,NAPH         ) =   0.05800 * RK( 190 ) * C( OH           )                                       
     &                                  + 0.05800 * RK( 250 ) * C( CL           )                                       
      PD( XO2H        ,CL           ) =   0.97000 * RK( 236 ) * C( PRPA         )                                       
     &                                  + 0.99100 * RK( 237 ) * C( ETHA         )                                       
     &                                  + 0.96000 * RK( 241 ) * C( ISOP         )                                       
     &                                  + 0.07000 * RK( 248 ) * C( TOL          )                                       
     &                                  + 0.05800 * RK( 249 ) * C( XYLMN        )                                       
     &                                  + 0.05800 * RK( 250 ) * C( NAPH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  26: PANX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PANX        ,NO2          ) =             RK( 062 ) * C( CXO3         )                                       
      PD( PANX        ,OH           ) = -           RK( 219 ) * C( PANX         )                                       
      PD( PANX        ,CXO3         ) =             RK( 062 ) * C( NO2          )                                       
      PD( PANX        ,PANX         ) = -           RK( 063 )                                                           
     &                                  -           RK( 064 )                                                           
     &                                  -           RK( 219 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  27: FORM                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FORM        ,NO           ) =             RK( 071 ) * C( MEO2         )                                       
     &                                  + 0.67300 * RK( 151 ) * C( ISO2         )                                       
     &                                  + 0.37500 * RK( 167 ) * C( EPX2         )                                       
      PD( FORM        ,O            ) = -           RK( 099 ) * C( FORM         )                                       
     &                                  +           RK( 137 ) * C( ETH          )                                       
     &                                  + 0.20000 * RK( 141 ) * C( OLE          )                                       
     &                                  + 0.50000 * RK( 150 ) * C( ISOP         )                                       
      PD( FORM        ,O3           ) =             RK( 139 ) * C( ETH          )                                       
     &                                  + 0.55500 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.12800 * RK( 147 ) * C( IOLE         )                                       
     &                                  + 0.60000 * RK( 156 ) * C( ISOP         )                                       
     &                                  + 0.23100 * RK( 159 ) * C( ISPD         )                                       
     &                                  + 0.24000 * RK( 173 ) * C( TERP         )                                       
     &                                  + 0.24000 * RK( 177 ) * C( APIN         )                                       
     &                                  + 0.08000 * RK( 208 ) * C( OPEN         )                                       
      PD( FORM        ,NO3          ) = -           RK( 100 ) * C( FORM         )                                       
     &                                  + 1.12500 * RK( 140 ) * C( ETH          )                                       
     &                                  + 0.50000 * RK( 144 ) * C( OLE          )                                       
     &                                  + 0.35000 * RK( 157 ) * C( ISOP         )                                       
      PD( FORM        ,OH           ) =   0.40000 * RK( 087 ) * C( MEPX         )                                       
     &                                  -           RK( 096 ) * C( FORM         )                                       
     &                                  +           RK( 126 ) * C( MEOH         )                                       
     &                                  + 0.07800 * RK( 127 ) * C( ETOH         )                                       
     &                                  +           RK( 130 ) * C( ACET         )                                       
     &                                  + 1.56000 * RK( 138 ) * C( ETH          )                                       
     &                                  + 0.78100 * RK( 142 ) * C( OLE          )                                       
     &                                  + 0.59200 * RK( 170 ) * C( INTR         )                                       
     &                                  + 0.28000 * RK( 172 ) * C( TERP         )                                       
     &                                  + 0.28000 * RK( 176 ) * C( APIN         )                                       
     &                                  + 0.14000 * RK( 210 ) * C( CAT1         )                                       
      PD( FORM        ,HO2          ) =   0.10000 * RK( 072 ) * C( MEO2         )                                       
     &                                  -           RK( 101 ) * C( FORM         )                                       
     &                                  + 0.12000 * RK( 152 ) * C( ISO2         )                                       
     &                                  + 0.37500 * RK( 166 ) * C( EPX2         )                                       
      PD( FORM        ,C2O3         ) =             RK( 073 ) * C( MEO2         )                                       
     &                                  + 0.59800 * RK( 153 ) * C( ISO2         )                                       
     &                                  + 0.30000 * RK( 168 ) * C( EPX2         )                                       
      PD( FORM        ,MEO2         ) =             RK( 071 ) * C( NO           )                                       
     &                                  + 0.10000 * RK( 072 ) * C( HO2          )                                       
     &                                  +           RK( 073 ) * C( C2O3         )                                       
     &                                  + 0.68500 * RK( 074 ) * C( RO2          )                                       
     &                                  +           RK( 231 ) * C( CLO          )                                       
      PD( FORM        ,RO2          ) =   0.68500 * RK( 074 ) * C( MEO2         )                                       
     &                                  + 0.59800 * RK( 154 ) * C( ISO2         )                                       
     &                                  + 0.37500 * RK( 169 ) * C( EPX2         )                                       
      PD( FORM        ,FORM         ) = -           RK( 096 ) * C( OH           )                                       
     &                                  -           RK( 097 )                                                           
     &                                  -           RK( 098 )                                                           
     &                                  -           RK( 099 ) * C( O            )                                       
     &                                  -           RK( 100 ) * C( NO3          )                                       
     &                                  -           RK( 101 ) * C( HO2          )                                       
     &                                  -           RK( 242 ) * C( CL           )                                       
      PD( FORM        ,MEPX         ) =   0.40000 * RK( 087 ) * C( OH           )                                       
      PD( FORM        ,MEOH         ) =             RK( 126 ) * C( OH           )                                       
     &                                  +           RK( 245 ) * C( CL           )                                       
      PD( FORM        ,HCO3         ) =             RK( 102 )                                                           
      PD( FORM        ,GLYD         ) =   0.74000 * RK( 114 )                                                           
      PD( FORM        ,ETOH         ) =   0.07800 * RK( 127 ) * C( OH           )                                       
      PD( FORM        ,ACET         ) =             RK( 130 ) * C( OH           )                                       
      PD( FORM        ,ETH          ) =             RK( 137 ) * C( O            )                                       
     &                                  + 1.56000 * RK( 138 ) * C( OH           )                                       
     &                                  +           RK( 139 ) * C( O3           )                                       
     &                                  + 1.12500 * RK( 140 ) * C( NO3          )                                       
     &                                  +           RK( 238 ) * C( CL           )                                       
      PD( FORM        ,OLE          ) =   0.20000 * RK( 141 ) * C( O            )                                       
     &                                  + 0.78100 * RK( 142 ) * C( OH           )                                       
     &                                  + 0.55500 * RK( 143 ) * C( O3           )                                       
     &                                  + 0.50000 * RK( 144 ) * C( NO3          )                                       
      PD( FORM        ,IOLE         ) =   0.12800 * RK( 147 ) * C( O3           )                                       
      PD( FORM        ,ISOP         ) =   0.50000 * RK( 150 ) * C( O            )                                       
     &                                  + 0.60000 * RK( 156 ) * C( O3           )                                       
     &                                  + 0.35000 * RK( 157 ) * C( NO3          )                                       
      PD( FORM        ,ISO2         ) =   0.67300 * RK( 151 ) * C( NO           )                                       
     &                                  + 0.12000 * RK( 152 ) * C( HO2          )                                       
     &                                  + 0.59800 * RK( 153 ) * C( C2O3         )                                       
     &                                  + 0.59800 * RK( 154 ) * C( RO2          )                                       
      PD( FORM        ,ISPD         ) =   0.23100 * RK( 159 ) * C( O3           )                                       
     &                                  + 0.26000 * RK( 161 )                                                           
      PD( FORM        ,INTR         ) =   0.59200 * RK( 170 ) * C( OH           )                                       
      PD( FORM        ,EPX2         ) =   0.37500 * RK( 166 ) * C( HO2          )                                       
     &                                  + 0.37500 * RK( 167 ) * C( NO           )                                       
     &                                  + 0.30000 * RK( 168 ) * C( C2O3         )                                       
     &                                  + 0.37500 * RK( 169 ) * C( RO2          )                                       
      PD( FORM        ,TERP         ) =   0.28000 * RK( 172 ) * C( OH           )                                       
     &                                  + 0.24000 * RK( 173 ) * C( O3           )                                       
      PD( FORM        ,APIN         ) =   0.28000 * RK( 176 ) * C( OH           )                                       
     &                                  + 0.24000 * RK( 177 ) * C( O3           )                                       
      PD( FORM        ,OPEN         ) =   0.08000 * RK( 208 ) * C( O3           )                                       
      PD( FORM        ,CAT1         ) =   0.14000 * RK( 210 ) * C( OH           )                                       
      PD( FORM        ,CRON         ) =             RK( 201 )                                                           
      PD( FORM        ,CL           ) =             RK( 238 ) * C( ETH          )                                       
     &                                  -           RK( 242 ) * C( FORM         )                                       
     &                                  +           RK( 245 ) * C( MEOH         )                                       
      PD( FORM        ,CLO          ) =             RK( 231 ) * C( MEO2         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  28: MEPX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEPX        ,OH           ) = -           RK( 087 ) * C( MEPX         )                                       
      PD( MEPX        ,HO2          ) =   0.90000 * RK( 072 ) * C( MEO2         )                                       
     &                                  + 0.50000 * RK( 104 ) * C( HCO3         )                                       
      PD( MEPX        ,MEO2         ) =   0.90000 * RK( 072 ) * C( HO2          )                                       
      PD( MEPX        ,MEPX         ) = -           RK( 087 ) * C( OH           )                                       
     &                                  -           RK( 088 )                                                           
      PD( MEPX        ,HCO3         ) =   0.50000 * RK( 104 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  29: MEOH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEOH        ,OH           ) = -           RK( 126 ) * C( MEOH         )                                       
      PD( MEOH        ,MEO2         ) =   0.31500 * RK( 074 ) * C( RO2          )                                       
      PD( MEOH        ,RO2          ) =   0.31500 * RK( 074 ) * C( MEO2         )                                       
      PD( MEOH        ,MEOH         ) = -           RK( 126 ) * C( OH           )                                       
     &                                  -           RK( 245 ) * C( CL           )                                       
      PD( MEOH        ,GLYD         ) =   0.15000 * RK( 114 )                                                           
      PD( MEOH        ,CL           ) = -           RK( 245 ) * C( MEOH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  30: ROOH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ROOH        ,OH           ) = -           RK( 089 ) * C( ROOH         )                                       
      PD( ROOH        ,HO2          ) =             RK( 076 ) * C( XO2H         )                                       
     &                                  +           RK( 080 ) * C( XO2          )                                       
     &                                  +           RK( 084 ) * C( XO2N         )                                       
      PD( ROOH        ,XO2H         ) =             RK( 076 ) * C( HO2          )                                       
      PD( ROOH        ,ROOH         ) = -           RK( 089 ) * C( OH           )                                       
     &                                  -           RK( 090 )                                                           
      PD( ROOH        ,XO2          ) =             RK( 080 ) * C( HO2          )                                       
      PD( ROOH        ,XO2N         ) =             RK( 084 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  31: XO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XO2         ,NO           ) = -           RK( 079 ) * C( XO2          )                                       
      PD( XO2         ,O            ) =   0.25000 * RK( 150 ) * C( ISOP         )                                       
      PD( XO2         ,O3           ) =   0.20000 * RK( 156 ) * C( ISOP         )                                       
     &                                  + 0.69000 * RK( 173 ) * C( TERP         )                                       
     &                                  + 0.69000 * RK( 177 ) * C( APIN         )                                       
      PD( XO2         ,NO3          ) =   0.50000 * RK( 118 ) * C( GLY          )                                       
     &                                  +           RK( 120 ) * C( MGLY         )                                       
     &                                  + 0.50000 * RK( 140 ) * C( ETH          )                                       
     &                                  + 0.48000 * RK( 144 ) * C( OLE          )                                       
     &                                  + 0.48000 * RK( 148 ) * C( IOLE         )                                       
     &                                  + 0.33000 * RK( 157 ) * C( ISOP         )                                       
     &                                  + 0.14200 * RK( 160 ) * C( ISPD         )                                       
     &                                  + 0.75000 * RK( 174 ) * C( TERP         )                                       
     &                                  + 0.75000 * RK( 178 ) * C( APIN         )                                       
     &                                  + 0.48000 * RK( 196 ) * C( CRES         )                                       
     &                                  + 0.45000 * RK( 205 ) * C( XOPN         )                                       
      PD( XO2         ,OH           ) =   0.20000 * RK( 116 ) * C( GLY          )                                       
     &                                  +           RK( 130 ) * C( ACET         )                                       
     &                                  + 0.19500 * RK( 142 ) * C( OLE          )                                       
     &                                  + 0.52100 * RK( 158 ) * C( ISPD         )                                       
     &                                  + 0.63000 * RK( 170 ) * C( INTR         )                                       
     &                                  + 0.50000 * RK( 172 ) * C( TERP         )                                       
     &                                  + 0.50000 * RK( 176 ) * C( APIN         )                                       
      PD( XO2         ,HO2          ) = -           RK( 080 ) * C( XO2          )                                       
      PD( XO2         ,C2O3         ) = -           RK( 081 ) * C( XO2          )                                       
     &                                  +           RK( 216 ) * C( OPO3         )                                       
      PD( XO2         ,RO2          ) = -           RK( 082 ) * C( XO2          )                                       
      PD( XO2         ,XO2          ) = -           RK( 079 ) * C( NO           )                                       
     &                                  -           RK( 080 ) * C( HO2          )                                       
     &                                  -           RK( 081 ) * C( C2O3         )                                       
     &                                  -           RK( 082 ) * C( RO2          )                                       
      PD( XO2         ,GLY          ) =   0.20000 * RK( 116 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 118 ) * C( NO3          )                                       
      PD( XO2         ,MGLY         ) =             RK( 120 ) * C( NO3          )                                       
      PD( XO2         ,ACET         ) =             RK( 130 ) * C( OH           )                                       
      PD( XO2         ,XPAR         ) =   0.87400 * RK( 224 )                                                           
      PD( XO2         ,ETH          ) =   0.50000 * RK( 140 ) * C( NO3          )                                       
     &                                  + 2.00000 * RK( 238 ) * C( CL           )                                       
      PD( XO2         ,OLE          ) =   0.19500 * RK( 142 ) * C( OH           )                                       
     &                                  + 0.48000 * RK( 144 ) * C( NO3          )                                       
     &                                  + 2.00000 * RK( 239 ) * C( CL           )                                       
      PD( XO2         ,IOLE         ) =   0.48000 * RK( 148 ) * C( NO3          )                                       
     &                                  + 1.70000 * RK( 240 ) * C( CL           )                                       
      PD( XO2         ,ISOP         ) =   0.25000 * RK( 150 ) * C( O            )                                       
     &                                  + 0.20000 * RK( 156 ) * C( O3           )                                       
     &                                  + 0.33000 * RK( 157 ) * C( NO3          )                                       
      PD( XO2         ,ISPD         ) =   0.52100 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.14200 * RK( 160 ) * C( NO3          )                                       
     &                                  + 0.16000 * RK( 161 )                                                           
      PD( XO2         ,INTR         ) =   0.63000 * RK( 170 ) * C( OH           )                                       
      PD( XO2         ,OPO3         ) =             RK( 216 ) * C( C2O3         )                                       
      PD( XO2         ,TERP         ) =   0.50000 * RK( 172 ) * C( OH           )                                       
     &                                  + 0.69000 * RK( 173 ) * C( O3           )                                       
     &                                  + 0.75000 * RK( 174 ) * C( NO3          )                                       
      PD( XO2         ,APIN         ) =   0.50000 * RK( 176 ) * C( OH           )                                       
     &                                  + 0.69000 * RK( 177 ) * C( O3           )                                       
     &                                  + 0.75000 * RK( 178 ) * C( NO3          )                                       
      PD( XO2         ,CRES         ) =   0.48000 * RK( 196 ) * C( NO3          )                                       
      PD( XO2         ,XOPN         ) =   0.45000 * RK( 205 ) * C( NO3          )                                       
      PD( XO2         ,CL           ) =   2.00000 * RK( 238 ) * C( ETH          )                                       
     &                                  + 2.00000 * RK( 239 ) * C( OLE          )                                       
     &                                  + 1.70000 * RK( 240 ) * C( IOLE         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  32: XO2N                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XO2N        ,NO           ) = -           RK( 083 ) * C( XO2N         )                                       
      PD( XO2N        ,O            ) =   0.01000 * RK( 141 ) * C( OLE          )                                       
      PD( XO2N        ,O3           ) =   0.18000 * RK( 173 ) * C( TERP         )                                       
     &                                  + 0.18000 * RK( 177 ) * C( APIN         )                                       
      PD( XO2N        ,NO3          ) =   0.04000 * RK( 144 ) * C( OLE          )                                       
     &                                  + 0.04000 * RK( 148 ) * C( IOLE         )                                       
     &                                  + 0.03000 * RK( 157 ) * C( ISOP         )                                       
     &                                  + 0.25000 * RK( 174 ) * C( TERP         )                                       
     &                                  + 0.25000 * RK( 178 ) * C( APIN         )                                       
     &                                  + 0.10000 * RK( 196 ) * C( CRES         )                                       
     &                                  + 0.10000 * RK( 205 ) * C( XOPN         )                                       
      PD( XO2N        ,OH           ) =   0.06000 * RK( 089 ) * C( ROOH         )                                       
     &                                  + 0.00900 * RK( 125 ) * C( ETHA         )                                       
     &                                  + 0.02400 * RK( 142 ) * C( OLE          )                                       
     &                                  + 0.02200 * RK( 158 ) * C( ISPD         )                                       
     &                                  + 0.25000 * RK( 172 ) * C( TERP         )                                       
     &                                  + 0.25000 * RK( 176 ) * C( APIN         )                                       
     &                                  + 0.02000 * RK( 195 ) * C( CRES         )                                       
      PD( XO2N        ,HO2          ) = -           RK( 084 ) * C( XO2N         )                                       
      PD( XO2N        ,C2O3         ) = -           RK( 085 ) * C( XO2N         )                                       
      PD( XO2N        ,RO2          ) = -           RK( 086 ) * C( XO2N         )                                       
      PD( XO2N        ,ROOH         ) =   0.06000 * RK( 089 ) * C( OH           )                                       
      PD( XO2N        ,XO2N         ) = -           RK( 083 ) * C( NO           )                                       
     &                                  -           RK( 084 ) * C( HO2          )                                       
     &                                  -           RK( 085 ) * C( C2O3         )                                       
     &                                  -           RK( 086 ) * C( RO2          )                                       
      PD( XO2N        ,ETHA         ) =   0.00900 * RK( 125 ) * C( OH           )                                       
     &                                  + 0.00900 * RK( 237 ) * C( CL           )                                       
      PD( XO2N        ,PRPA         ) =   0.03000 * RK( 236 ) * C( CL           )                                       
      PD( XO2N        ,XPRP         ) =             RK( 221 )                                                           
      PD( XO2N        ,XPAR         ) =             RK( 223 )                                                           
      PD( XO2N        ,ROR          ) =   0.04000 * RK( 133 )                                                           
      PD( XO2N        ,OLE          ) =   0.01000 * RK( 141 ) * C( O            )                                       
     &                                  + 0.02400 * RK( 142 ) * C( OH           )                                       
     &                                  + 0.04000 * RK( 144 ) * C( NO3          )                                       
      PD( XO2N        ,IOLE         ) =   0.04000 * RK( 148 ) * C( NO3          )                                       
      PD( XO2N        ,ISOP         ) =   0.03000 * RK( 157 ) * C( NO3          )                                       
     &                                  + 0.04000 * RK( 241 ) * C( CL           )                                       
      PD( XO2N        ,ISPD         ) =   0.02200 * RK( 158 ) * C( OH           )                                       
      PD( XO2N        ,TERP         ) =   0.25000 * RK( 172 ) * C( OH           )                                       
     &                                  + 0.18000 * RK( 173 ) * C( O3           )                                       
     &                                  + 0.25000 * RK( 174 ) * C( NO3          )                                       
      PD( XO2N        ,APIN         ) =   0.25000 * RK( 176 ) * C( OH           )                                       
     &                                  + 0.18000 * RK( 177 ) * C( O3           )                                       
     &                                  + 0.25000 * RK( 178 ) * C( NO3          )                                       
      PD( XO2N        ,CRES         ) =   0.02000 * RK( 195 ) * C( OH           )                                       
     &                                  + 0.10000 * RK( 196 ) * C( NO3          )                                       
      PD( XO2N        ,XOPN         ) =   0.10000 * RK( 205 ) * C( NO3          )                                       
      PD( XO2N        ,CL           ) =   0.03000 * RK( 236 ) * C( PRPA         )                                       
     &                                  + 0.00900 * RK( 237 ) * C( ETHA         )                                       
     &                                  + 0.04000 * RK( 241 ) * C( ISOP         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  33: NTR1                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTR1        ,NO2          ) =             RK( 135 ) * C( ROR          )                                       
      PD( NTR1        ,NO           ) =   0.50000 * RK( 083 ) * C( XO2N         )                                       
      PD( NTR1        ,NO3          ) =   0.50000 * RK( 140 ) * C( ETH          )                                       
     &                                  + 0.50000 * RK( 144 ) * C( OLE          )                                       
     &                                  + 0.50000 * RK( 148 ) * C( IOLE         )                                       
      PD( NTR1        ,OH           ) = -           RK( 091 ) * C( NTR1         )                                       
      PD( NTR1        ,XO2N         ) =   0.50000 * RK( 083 ) * C( NO           )                                       
      PD( NTR1        ,NTR1         ) = -           RK( 091 ) * C( OH           )                                       
     &                                  -           RK( 092 )                                                           
      PD( NTR1        ,ROR          ) =             RK( 135 ) * C( NO2          )                                       
      PD( NTR1        ,ETH          ) =   0.50000 * RK( 140 ) * C( NO3          )                                       
      PD( NTR1        ,OLE          ) =   0.50000 * RK( 144 ) * C( NO3          )                                       
      PD( NTR1        ,IOLE         ) =   0.50000 * RK( 148 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  34: NTR2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTR2        ,NO           ) =   0.50000 * RK( 083 ) * C( XO2N         )                                       
     &                                  + 0.08200 * RK( 180 ) * C( BZO2         )                                       
     &                                  + 0.14000 * RK( 185 ) * C( TO2          )                                       
     &                                  + 0.14000 * RK( 191 ) * C( XLO2         )                                       
      PD( NTR2        ,NO3          ) =   0.65000 * RK( 157 ) * C( ISOP         )                                       
     &                                  + 0.14200 * RK( 160 ) * C( ISPD         )                                       
     &                                  + 0.53000 * RK( 174 ) * C( TERP         )                                       
     &                                  + 0.53000 * RK( 178 ) * C( APIN         )                                       
     &                                  +           RK( 200 ) * C( CRON         )                                       
     &                                  + 0.50000 * RK( 205 ) * C( XOPN         )                                       
      PD( NTR2        ,OH           ) =             RK( 091 ) * C( NTR1         )                                       
     &                                  + 0.26600 * RK( 170 ) * C( INTR         )                                       
     &                                  +           RK( 199 ) * C( CRON         )                                       
     &                                  + 0.50000 * RK( 218 ) * C( OPAN         )                                       
      PD( NTR2        ,XO2N         ) =   0.50000 * RK( 083 ) * C( NO           )                                       
      PD( NTR2        ,NTR1         ) =             RK( 091 ) * C( OH           )                                       
      PD( NTR2        ,NTR2         ) = -           RK( 268 )                                                           
      PD( NTR2        ,ISOP         ) =   0.65000 * RK( 157 ) * C( NO3          )                                       
      PD( NTR2        ,ISPD         ) =   0.14200 * RK( 160 ) * C( NO3          )                                       
      PD( NTR2        ,INTR         ) =   0.26600 * RK( 170 ) * C( OH           )                                       
      PD( NTR2        ,TERP         ) =   0.53000 * RK( 174 ) * C( NO3          )                                       
      PD( NTR2        ,APIN         ) =   0.53000 * RK( 178 ) * C( NO3          )                                       
      PD( NTR2        ,BZO2         ) =   0.08200 * RK( 180 ) * C( NO           )                                       
      PD( NTR2        ,TO2          ) =   0.14000 * RK( 185 ) * C( NO           )                                       
      PD( NTR2        ,XOPN         ) =   0.50000 * RK( 205 ) * C( NO3          )                                       
      PD( NTR2        ,XLO2         ) =   0.14000 * RK( 191 ) * C( NO           )                                       
      PD( NTR2        ,CRON         ) =             RK( 199 ) * C( OH           )                                       
     &                                  +           RK( 200 ) * C( NO3          )                                       
      PD( NTR2        ,OPAN         ) =   0.50000 * RK( 218 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  35: FACD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FACD        ,NO           ) =             RK( 103 ) * C( HCO3         )                                       
      PD( FACD        ,O3           ) =   0.37000 * RK( 139 ) * C( ETH          )                                       
     &                                  + 0.09000 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.15000 * RK( 159 ) * C( ISPD         )                                       
      PD( FACD        ,OH           ) = -           RK( 093 ) * C( FACD         )                                       
     &                                  + 0.30000 * RK( 136 ) * C( ETHY         )                                       
     &                                  + 0.18500 * RK( 170 ) * C( INTR         )                                       
      PD( FACD        ,HO2          ) =   0.50000 * RK( 104 ) * C( HCO3         )                                       
     &                                  + 0.07400 * RK( 166 ) * C( EPX2         )                                       
      PD( FACD        ,FACD         ) = -           RK( 093 ) * C( OH           )                                       
      PD( FACD        ,HCO3         ) =             RK( 103 ) * C( NO           )                                       
     &                                  + 0.50000 * RK( 104 ) * C( HO2          )                                       
      PD( FACD        ,ETHY         ) =   0.30000 * RK( 136 ) * C( OH           )                                       
      PD( FACD        ,ETH          ) =   0.37000 * RK( 139 ) * C( O3           )                                       
      PD( FACD        ,OLE          ) =   0.09000 * RK( 143 ) * C( O3           )                                       
      PD( FACD        ,ISPD         ) =   0.15000 * RK( 159 ) * C( O3           )                                       
      PD( FACD        ,INTR         ) =   0.18500 * RK( 170 ) * C( OH           )                                       
      PD( FACD        ,EPX2         ) =   0.07400 * RK( 166 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  36: CO                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CO          ,NO           ) =   0.25100 * RK( 167 ) * C( EPX2         )                                       
     &                                  + 0.50000 * RK( 212 ) * C( OPO3         )                                       
      PD( CO          ,O            ) =             RK( 099 ) * C( FORM         )                                       
     &                                  +           RK( 137 ) * C( ETH          )                                       
     &                                  + 0.20000 * RK( 141 ) * C( OLE          )                                       
     &                                  + 0.10000 * RK( 145 ) * C( IOLE         )                                       
      PD( CO          ,O3           ) =   0.51000 * RK( 139 ) * C( ETH          )                                       
     &                                  + 0.37800 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.24500 * RK( 147 ) * C( IOLE         )                                       
     &                                  + 0.06600 * RK( 156 ) * C( ISOP         )                                       
     &                                  + 0.54300 * RK( 159 ) * C( ISPD         )                                       
     &                                  + 0.00100 * RK( 173 ) * C( TERP         )                                       
     &                                  + 0.00100 * RK( 177 ) * C( APIN         )                                       
     &                                  + 0.50000 * RK( 204 ) * C( XOPN         )                                       
     &                                  + 1.98000 * RK( 208 ) * C( OPEN         )                                       
      PD( CO          ,NO3          ) =             RK( 100 ) * C( FORM         )                                       
     &                                  + 1.50000 * RK( 118 ) * C( GLY          )                                       
      PD( CO          ,OH           ) =             RK( 096 ) * C( FORM         )                                       
     &                                  + 1.80000 * RK( 116 ) * C( GLY          )                                       
     &                                  +           RK( 121 ) * C( MGLY         )                                       
     &                                  -           RK( 123 ) * C( CO           )                                       
     &                                  + 0.30000 * RK( 136 ) * C( ETHY         )                                       
     &                                  + 0.13700 * RK( 158 ) * C( ISPD         )                                       
     &                                  +           RK( 218 ) * C( OPAN         )                                       
     &                                  +           RK( 232 ) * C( FMCL         )                                       
      PD( CO          ,HO2          ) =   0.25100 * RK( 166 ) * C( EPX2         )                                       
      PD( CO          ,C2O3         ) =   0.20000 * RK( 168 ) * C( EPX2         )                                       
      PD( CO          ,RO2          ) =   0.25100 * RK( 169 ) * C( EPX2         )                                       
      PD( CO          ,ALD2         ) =             RK( 108 )                                                           
      PD( CO          ,FORM         ) =             RK( 096 ) * C( OH           )                                       
     &                                  +           RK( 097 )                                                           
     &                                  +           RK( 098 )                                                           
     &                                  +           RK( 099 ) * C( O            )                                       
     &                                  +           RK( 100 ) * C( NO3          )                                       
     &                                  +           RK( 242 ) * C( CL           )                                       
      PD( CO          ,CO           ) = -           RK( 123 ) * C( OH           )                                       
      PD( CO          ,ALDX         ) =             RK( 112 )                                                           
      PD( CO          ,GLYD         ) =   0.89000 * RK( 114 )                                                           
      PD( CO          ,GLY          ) =   1.80000 * RK( 116 ) * C( OH           )                                       
     &                                  + 2.00000 * RK( 117 )                                                           
     &                                  + 1.50000 * RK( 118 ) * C( NO3          )                                       
      PD( CO          ,MGLY         ) =             RK( 119 )                                                           
     &                                  +           RK( 121 ) * C( OH           )                                       
      PD( CO          ,ACET         ) =   0.38000 * RK( 129 )                                                           
      PD( CO          ,ETHY         ) =   0.30000 * RK( 136 ) * C( OH           )                                       
      PD( CO          ,ETH          ) =             RK( 137 ) * C( O            )                                       
     &                                  + 0.51000 * RK( 139 ) * C( O3           )                                       
      PD( CO          ,OLE          ) =   0.20000 * RK( 141 ) * C( O            )                                       
     &                                  + 0.37800 * RK( 143 ) * C( O3           )                                       
      PD( CO          ,IOLE         ) =   0.10000 * RK( 145 ) * C( O            )                                       
     &                                  + 0.24500 * RK( 147 ) * C( O3           )                                       
      PD( CO          ,ISOP         ) =   0.06600 * RK( 156 ) * C( O3           )                                       
      PD( CO          ,ISPD         ) =   0.13700 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.54300 * RK( 159 ) * C( O3           )                                       
      PD( CO          ,OPO3         ) =   0.50000 * RK( 212 ) * C( NO           )                                       
      PD( CO          ,EPX2         ) =   0.25100 * RK( 166 ) * C( HO2          )                                       
     &                                  + 0.25100 * RK( 167 ) * C( NO           )                                       
     &                                  + 0.20000 * RK( 168 ) * C( C2O3         )                                       
     &                                  + 0.25100 * RK( 169 ) * C( RO2          )                                       
      PD( CO          ,TERP         ) =   0.00100 * RK( 173 ) * C( O3           )                                       
      PD( CO          ,APIN         ) =   0.00100 * RK( 177 ) * C( O3           )                                       
      PD( CO          ,OPEN         ) =             RK( 206 )                                                           
     &                                  + 1.98000 * RK( 208 ) * C( O3           )                                       
      PD( CO          ,XOPN         ) =   0.70000 * RK( 202 )                                                           
     &                                  + 0.50000 * RK( 204 ) * C( O3           )                                       
      PD( CO          ,OPAN         ) =             RK( 218 ) * C( OH           )                                       
      PD( CO          ,CL           ) =             RK( 242 ) * C( FORM         )                                       
      PD( CO          ,FMCL         ) =             RK( 232 ) * C( OH           )                                       
     &                                  +           RK( 233 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  37: HCO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HCO3        ,NO           ) = -           RK( 103 ) * C( HCO3         )                                       
      PD( HCO3        ,HO2          ) =             RK( 101 ) * C( FORM         )                                       
     &                                  -           RK( 104 ) * C( HCO3         )                                       
      PD( HCO3        ,FORM         ) =             RK( 101 ) * C( HO2          )                                       
      PD( HCO3        ,HCO3         ) = -           RK( 102 )                                                           
     &                                  -           RK( 103 ) * C( NO           )                                       
     &                                  -           RK( 104 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  38: ALDX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALDX        ,O            ) = -           RK( 109 ) * C( ALDX         )                                       
     &                                  + 0.30000 * RK( 141 ) * C( OLE          )                                       
     &                                  + 0.66000 * RK( 145 ) * C( IOLE         )                                       
     &                                  + 0.15000 * RK( 171 ) * C( TERP         )                                       
     &                                  + 0.15000 * RK( 175 ) * C( APIN         )                                       
      PD( ALDX        ,O3           ) =   0.27000 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.44200 * RK( 147 ) * C( IOLE         )                                       
     &                                  + 0.15000 * RK( 156 ) * C( ISOP         )                                       
     &                                  + 0.21000 * RK( 173 ) * C( TERP         )                                       
     &                                  + 0.21000 * RK( 177 ) * C( APIN         )                                       
      PD( ALDX        ,NO3          ) = -           RK( 111 ) * C( ALDX         )                                       
     &                                  + 0.37500 * RK( 144 ) * C( OLE          )                                       
     &                                  + 0.62500 * RK( 148 ) * C( IOLE         )                                       
     &                                  + 0.47000 * RK( 174 ) * C( TERP         )                                       
     &                                  + 0.47000 * RK( 178 ) * C( APIN         )                                       
      PD( ALDX        ,OH           ) = -           RK( 110 ) * C( ALDX         )                                       
     &                                  + 0.48800 * RK( 142 ) * C( OLE          )                                       
     &                                  + 0.70000 * RK( 146 ) * C( IOLE         )                                       
     &                                  + 0.02900 * RK( 162 ) * C( ISPX         )                                       
     &                                  + 0.07800 * RK( 170 ) * C( INTR         )                                       
     &                                  + 0.47000 * RK( 172 ) * C( TERP         )                                       
     &                                  + 0.47000 * RK( 176 ) * C( APIN         )                                       
      PD( ALDX        ,HO2          ) =   0.44000 * RK( 215 ) * C( OPO3         )                                       
      PD( ALDX        ,C2O3         ) =             RK( 216 ) * C( OPO3         )                                       
      PD( ALDX        ,RO2          ) =   0.80000 * RK( 217 ) * C( OPO3         )                                       
      PD( ALDX        ,ALDX         ) = -           RK( 109 ) * C( O            )                                       
     &                                  -           RK( 110 ) * C( OH           )                                       
     &                                  -           RK( 111 ) * C( NO3          )                                       
     &                                  -           RK( 112 )                                                           
     &                                  -           RK( 244 ) * C( CL           )                                       
      PD( ALDX        ,XPRP         ) =   0.26800 * RK( 222 )                                                           
      PD( ALDX        ,XPAR         ) =   0.12600 * RK( 224 )                                                           
      PD( ALDX        ,ROR          ) =   0.37000 * RK( 133 )                                                           
      PD( ALDX        ,OLE          ) =   0.30000 * RK( 141 ) * C( O            )                                       
     &                                  + 0.48800 * RK( 142 ) * C( OH           )                                       
     &                                  + 0.27000 * RK( 143 ) * C( O3           )                                       
     &                                  + 0.37500 * RK( 144 ) * C( NO3          )                                       
     &                                  + 0.67000 * RK( 239 ) * C( CL           )                                       
      PD( ALDX        ,IOLE         ) =   0.66000 * RK( 145 ) * C( O            )                                       
     &                                  + 0.70000 * RK( 146 ) * C( OH           )                                       
     &                                  + 0.44200 * RK( 147 ) * C( O3           )                                       
     &                                  + 0.62500 * RK( 148 ) * C( NO3          )                                       
     &                                  + 0.55000 * RK( 240 ) * C( CL           )                                       
      PD( ALDX        ,ISOP         ) =   0.15000 * RK( 156 ) * C( O3           )                                       
      PD( ALDX        ,INTR         ) =   0.07800 * RK( 170 ) * C( OH           )                                       
      PD( ALDX        ,ISPX         ) =   0.02900 * RK( 162 ) * C( OH           )                                       
      PD( ALDX        ,OPO3         ) =   0.44000 * RK( 215 ) * C( HO2          )                                       
     &                                  +           RK( 216 ) * C( C2O3         )                                       
     &                                  + 0.80000 * RK( 217 ) * C( RO2          )                                       
      PD( ALDX        ,TERP         ) =   0.15000 * RK( 171 ) * C( O            )                                       
     &                                  + 0.47000 * RK( 172 ) * C( OH           )                                       
     &                                  + 0.21000 * RK( 173 ) * C( O3           )                                       
     &                                  + 0.47000 * RK( 174 ) * C( NO3          )                                       
      PD( ALDX        ,APIN         ) =   0.15000 * RK( 175 ) * C( O            )                                       
     &                                  + 0.47000 * RK( 176 ) * C( OH           )                                       
     &                                  + 0.21000 * RK( 177 ) * C( O3           )                                       
     &                                  + 0.47000 * RK( 178 ) * C( NO3          )                                       
      PD( ALDX        ,CL           ) =   0.67000 * RK( 239 ) * C( OLE          )                                       
     &                                  + 0.55000 * RK( 240 ) * C( IOLE         )                                       
     &                                  -           RK( 244 ) * C( ALDX         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  39: GLYD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( GLYD        ,NO           ) =   0.27500 * RK( 167 ) * C( EPX2         )                                       
      PD( GLYD        ,NO3          ) = -           RK( 115 ) * C( GLYD         )                                       
     &                                  + 0.11300 * RK( 160 ) * C( ISPD         )                                       
      PD( GLYD        ,OH           ) = -           RK( 113 ) * C( GLYD         )                                       
     &                                  + 0.01100 * RK( 127 ) * C( ETOH         )                                       
     &                                  + 0.22000 * RK( 138 ) * C( ETH          )                                       
     &                                  + 0.26900 * RK( 158 ) * C( ISPD         )                                       
     &                                  + 0.33100 * RK( 170 ) * C( INTR         )                                       
      PD( GLYD        ,HO2          ) =   0.27500 * RK( 166 ) * C( EPX2         )                                       
      PD( GLYD        ,C2O3         ) =   0.22000 * RK( 168 ) * C( EPX2         )                                       
      PD( GLYD        ,RO2          ) =   0.27500 * RK( 169 ) * C( EPX2         )                                       
      PD( GLYD        ,GLYD         ) = -           RK( 113 ) * C( OH           )                                       
     &                                  -           RK( 114 )                                                           
     &                                  -           RK( 115 ) * C( NO3          )                                       
      PD( GLYD        ,ETOH         ) =   0.01100 * RK( 127 ) * C( OH           )                                       
      PD( GLYD        ,ETH          ) =   0.22000 * RK( 138 ) * C( OH           )                                       
      PD( GLYD        ,ISPD         ) =   0.26900 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.11300 * RK( 160 ) * C( NO3          )                                       
     &                                  + 0.12800 * RK( 161 )                                                           
      PD( GLYD        ,INTR         ) =   0.33100 * RK( 170 ) * C( OH           )                                       
      PD( GLYD        ,EPX2         ) =   0.27500 * RK( 166 ) * C( HO2          )                                       
     &                                  + 0.27500 * RK( 167 ) * C( NO           )                                       
     &                                  + 0.22000 * RK( 168 ) * C( C2O3         )                                       
     &                                  + 0.27500 * RK( 169 ) * C( RO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  40: GLY                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( GLY         ,NO           ) =   0.27500 * RK( 167 ) * C( EPX2         )                                       
     &                                  + 0.91800 * RK( 180 ) * C( BZO2         )                                       
     &                                  + 0.41700 * RK( 185 ) * C( TO2          )                                       
     &                                  + 0.22100 * RK( 191 ) * C( XLO2         )                                       
     &                                  + 0.50000 * RK( 212 ) * C( OPO3         )                                       
      PD( GLY         ,O3           ) =   0.07500 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.24000 * RK( 147 ) * C( IOLE         )                                       
     &                                  + 0.17000 * RK( 159 ) * C( ISPD         )                                       
     &                                  + 1.40000 * RK( 208 ) * C( OPEN         )                                       
      PD( GLY         ,NO3          ) = -           RK( 118 ) * C( GLY          )                                       
     &                                  + 0.24000 * RK( 196 ) * C( CRES         )                                       
      PD( GLY         ,OH           ) =   0.20000 * RK( 113 ) * C( GLYD         )                                       
     &                                  -           RK( 116 ) * C( GLY          )                                       
     &                                  + 0.70000 * RK( 136 ) * C( ETHY         )                                       
     &                                  + 0.02500 * RK( 195 ) * C( CRES         )                                       
     &                                  + 0.40000 * RK( 203 ) * C( XOPN         )                                       
     &                                  + 0.40000 * RK( 207 ) * C( OPEN         )                                       
     &                                  + 0.50000 * RK( 218 ) * C( OPAN         )                                       
      PD( GLY         ,HO2          ) =   0.27500 * RK( 166 ) * C( EPX2         )                                       
      PD( GLY         ,C2O3         ) =   0.22000 * RK( 168 ) * C( EPX2         )                                       
     &                                  +           RK( 181 ) * C( BZO2         )                                       
     &                                  + 0.48000 * RK( 186 ) * C( TO2          )                                       
     &                                  + 0.26000 * RK( 193 ) * C( XLO2         )                                       
      PD( GLY         ,RO2          ) =   0.27500 * RK( 169 ) * C( EPX2         )                                       
     &                                  +           RK( 183 ) * C( BZO2         )                                       
     &                                  + 0.48000 * RK( 188 ) * C( TO2          )                                       
     &                                  + 0.26000 * RK( 194 ) * C( XLO2         )                                       
      PD( GLY         ,GLYD         ) =   0.20000 * RK( 113 ) * C( OH           )                                       
     &                                  + 0.11000 * RK( 114 )                                                           
      PD( GLY         ,GLY          ) = -           RK( 116 ) * C( OH           )                                       
     &                                  -           RK( 117 )                                                           
     &                                  -           RK( 118 ) * C( NO3          )                                       
     &                                  -           RK( 281 )                                                           
      PD( GLY         ,ETHY         ) =   0.70000 * RK( 136 ) * C( OH           )                                       
      PD( GLY         ,OLE          ) =   0.07500 * RK( 143 ) * C( O3           )                                       
      PD( GLY         ,IOLE         ) =   0.24000 * RK( 147 ) * C( O3           )                                       
      PD( GLY         ,ISPD         ) =   0.17000 * RK( 159 ) * C( O3           )                                       
      PD( GLY         ,OPO3         ) =   0.50000 * RK( 212 ) * C( NO           )                                       
      PD( GLY         ,EPX2         ) =   0.27500 * RK( 166 ) * C( HO2          )                                       
     &                                  + 0.27500 * RK( 167 ) * C( NO           )                                       
     &                                  + 0.22000 * RK( 168 ) * C( C2O3         )                                       
     &                                  + 0.27500 * RK( 169 ) * C( RO2          )                                       
      PD( GLY         ,CRES         ) =   0.02500 * RK( 195 ) * C( OH           )                                       
     &                                  + 0.24000 * RK( 196 ) * C( NO3          )                                       
      PD( GLY         ,BZO2         ) =   0.91800 * RK( 180 ) * C( NO           )                                       
     &                                  +           RK( 181 ) * C( C2O3         )                                       
     &                                  +           RK( 183 ) * C( RO2          )                                       
      PD( GLY         ,OPEN         ) =   0.40000 * RK( 207 ) * C( OH           )                                       
     &                                  + 1.40000 * RK( 208 ) * C( O3           )                                       
      PD( GLY         ,TO2          ) =   0.41700 * RK( 185 ) * C( NO           )                                       
     &                                  + 0.48000 * RK( 186 ) * C( C2O3         )                                       
     &                                  + 0.48000 * RK( 188 ) * C( RO2          )                                       
      PD( GLY         ,XOPN         ) =   0.40000 * RK( 202 )                                                           
     &                                  + 0.40000 * RK( 203 ) * C( OH           )                                       
      PD( GLY         ,XLO2         ) =   0.22100 * RK( 191 ) * C( NO           )                                       
     &                                  + 0.26000 * RK( 193 ) * C( C2O3         )                                       
     &                                  + 0.26000 * RK( 194 ) * C( RO2          )                                       
      PD( GLY         ,OPAN         ) =   0.50000 * RK( 218 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  41: MGLY                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MGLY        ,NO           ) =   0.27500 * RK( 167 ) * C( EPX2         )                                       
     &                                  + 0.44300 * RK( 185 ) * C( TO2          )                                       
     &                                  + 0.67500 * RK( 191 ) * C( XLO2         )                                       
      PD( MGLY        ,O3           ) =   0.07500 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.06000 * RK( 147 ) * C( IOLE         )                                       
     &                                  + 0.53100 * RK( 159 ) * C( ISPD         )                                       
     &                                  + 1.20000 * RK( 204 ) * C( XOPN         )                                       
     &                                  + 0.24000 * RK( 208 ) * C( OPEN         )                                       
      PD( MGLY        ,NO3          ) = -           RK( 120 ) * C( MGLY         )                                       
     &                                  + 0.11300 * RK( 160 ) * C( ISPD         )                                       
     &                                  + 0.24000 * RK( 196 ) * C( CRES         )                                       
     &                                  + 0.25000 * RK( 205 ) * C( XOPN         )                                       
      PD( MGLY        ,OH           ) = -           RK( 121 ) * C( MGLY         )                                       
     &                                  + 0.11500 * RK( 158 ) * C( ISPD         )                                       
     &                                  +           RK( 203 ) * C( XOPN         )                                       
      PD( MGLY        ,HO2          ) =   0.27500 * RK( 166 ) * C( EPX2         )                                       
      PD( MGLY        ,C2O3         ) =   0.22000 * RK( 168 ) * C( EPX2         )                                       
     &                                  + 0.52000 * RK( 186 ) * C( TO2          )                                       
     &                                  + 0.77000 * RK( 193 ) * C( XLO2         )                                       
      PD( MGLY        ,RO2          ) =   0.27500 * RK( 169 ) * C( EPX2         )                                       
     &                                  + 0.52000 * RK( 188 ) * C( TO2          )                                       
     &                                  + 0.77000 * RK( 194 ) * C( XLO2         )                                       
      PD( MGLY        ,MGLY         ) = -           RK( 119 )                                                           
     &                                  -           RK( 120 ) * C( NO3          )                                       
     &                                  -           RK( 121 ) * C( OH           )                                       
     &                                  -           RK( 282 )                                                           
      PD( MGLY        ,OLE          ) =   0.07500 * RK( 143 ) * C( O3           )                                       
      PD( MGLY        ,IOLE         ) =   0.06000 * RK( 147 ) * C( O3           )                                       
      PD( MGLY        ,ISPD         ) =   0.11500 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.53100 * RK( 159 ) * C( O3           )                                       
     &                                  + 0.11300 * RK( 160 ) * C( NO3          )                                       
      PD( MGLY        ,EPX2         ) =   0.27500 * RK( 166 ) * C( HO2          )                                       
     &                                  + 0.27500 * RK( 167 ) * C( NO           )                                       
     &                                  + 0.22000 * RK( 168 ) * C( C2O3         )                                       
     &                                  + 0.27500 * RK( 169 ) * C( RO2          )                                       
      PD( MGLY        ,CRES         ) =   0.24000 * RK( 196 ) * C( NO3          )                                       
      PD( MGLY        ,OPEN         ) =   0.24000 * RK( 208 ) * C( O3           )                                       
      PD( MGLY        ,TO2          ) =   0.44300 * RK( 185 ) * C( NO           )                                       
     &                                  + 0.52000 * RK( 186 ) * C( C2O3         )                                       
     &                                  + 0.52000 * RK( 188 ) * C( RO2          )                                       
      PD( MGLY        ,XOPN         ) =             RK( 203 ) * C( OH           )                                       
     &                                  + 1.20000 * RK( 204 ) * C( O3           )                                       
     &                                  + 0.25000 * RK( 205 ) * C( NO3          )                                       
      PD( MGLY        ,XLO2         ) =   0.67500 * RK( 191 ) * C( NO           )                                       
     &                                  + 0.77000 * RK( 193 ) * C( C2O3         )                                       
     &                                  + 0.77000 * RK( 194 ) * C( RO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  42: ETHA                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETHA        ,OH           ) = -           RK( 125 ) * C( ETHA         )                                       
      PD( ETHA        ,ETHA         ) = -           RK( 125 ) * C( OH           )                                       
     &                                  -           RK( 237 ) * C( CL           )                                       
      PD( ETHA        ,CL           ) = -           RK( 237 ) * C( ETHA         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  43: ETOH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETOH        ,OH           ) = -           RK( 127 ) * C( ETOH         )                                       
      PD( ETOH        ,ETOH         ) = -           RK( 127 ) * C( OH           )                                       
     &                                  -           RK( 246 ) * C( CL           )                                       
      PD( ETOH        ,CL           ) = -           RK( 246 ) * C( ETOH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  44: KET                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( KET         ,KET          ) = -           RK( 128 )                                                           
      PD( KET         ,ROR          ) =   0.20000 * RK( 133 )                                                           
     &                                  +           RK( 134 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  45: PAR                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAR         ,NO           ) =   2.17500 * RK( 167 ) * C( EPX2         )                                       
      PD( PAR         ,O            ) =   0.20000 * RK( 141 ) * C( OLE          )                                       
     &                                  + 0.10000 * RK( 145 ) * C( IOLE         )                                       
     &                                  + 0.25000 * RK( 150 ) * C( ISOP         )                                       
     &                                  + 5.12000 * RK( 171 ) * C( TERP         )                                       
     &                                  + 5.12000 * RK( 175 ) * C( APIN         )                                       
      PD( PAR         ,O3           ) = - 0.79000 * RK( 143 ) * C( OLE          )                                       
     &                                  + 0.29000 * RK( 147 ) * C( IOLE         )                                       
     &                                  + 0.35000 * RK( 156 ) * C( ISOP         )                                       
     &                                  + 7.00000 * RK( 173 ) * C( TERP         )                                       
     &                                  + 7.00000 * RK( 177 ) * C( APIN         )                                       
      PD( PAR         ,NO3          ) = -           RK( 144 ) * C( OLE          )                                       
     &                                  +           RK( 148 ) * C( IOLE         )                                       
     &                                  + 0.71700 * RK( 160 ) * C( ISPD         )                                       
      PD( PAR         ,OH           ) = -           RK( 132 ) * C( PAR          )                                       
     &                                  - 0.73000 * RK( 142 ) * C( OLE          )                                       
     &                                  + 0.11700 * RK( 158 ) * C( ISPD         )                                       
     &                                  + 2.70000 * RK( 170 ) * C( INTR         )                                       
     &                                  + 1.66000 * RK( 172 ) * C( TERP         )                                       
     &                                  + 1.66000 * RK( 176 ) * C( APIN         )                                       
      PD( PAR         ,HO2          ) =   2.17500 * RK( 166 ) * C( EPX2         )                                       
      PD( PAR         ,C2O3         ) =   1.74000 * RK( 168 ) * C( EPX2         )                                       
      PD( PAR         ,RO2          ) =   2.17500 * RK( 169 ) * C( EPX2         )                                       
      PD( PAR         ,KET          ) = - 2.50000 * RK( 128 )                                                           
      PD( PAR         ,PAR          ) = -           RK( 132 ) * C( OH           )                                       
     &                                  -           RK( 235 ) * C( CL           )                                       
      PD( PAR         ,XPRP         ) =   0.26800 * RK( 222 )                                                           
      PD( PAR         ,XPAR         ) = - 0.12600 * RK( 224 )                                                           
      PD( PAR         ,ROR          ) = - 2.70000 * RK( 133 )                                                           
      PD( PAR         ,OLE          ) =   0.20000 * RK( 141 ) * C( O            )                                       
     &                                  - 0.73000 * RK( 142 ) * C( OH           )                                       
     &                                  - 0.79000 * RK( 143 ) * C( O3           )                                       
     &                                  -           RK( 144 ) * C( NO3          )                                       
     &                                  -           RK( 239 ) * C( CL           )                                       
      PD( PAR         ,IOLE         ) =   0.10000 * RK( 145 ) * C( O            )                                       
     &                                  + 0.29000 * RK( 147 ) * C( O3           )                                       
     &                                  +           RK( 148 ) * C( NO3          )                                       
     &                                  + 0.30000 * RK( 240 ) * C( CL           )                                       
      PD( PAR         ,ISOP         ) =   0.25000 * RK( 150 ) * C( O            )                                       
     &                                  + 0.35000 * RK( 156 ) * C( O3           )                                       
      PD( PAR         ,ISPD         ) =   0.11700 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.71700 * RK( 160 ) * C( NO3          )                                       
     &                                  + 0.24000 * RK( 161 )                                                           
      PD( PAR         ,INTR         ) =   2.70000 * RK( 170 ) * C( OH           )                                       
      PD( PAR         ,EPX2         ) =   2.17500 * RK( 166 ) * C( HO2          )                                       
     &                                  + 2.17500 * RK( 167 ) * C( NO           )                                       
     &                                  + 1.74000 * RK( 168 ) * C( C2O3         )                                       
     &                                  + 2.17500 * RK( 169 ) * C( RO2          )                                       
      PD( PAR         ,TERP         ) =   5.12000 * RK( 171 ) * C( O            )                                       
     &                                  + 1.66000 * RK( 172 ) * C( OH           )                                       
     &                                  + 7.00000 * RK( 173 ) * C( O3           )                                       
      PD( PAR         ,APIN         ) =   5.12000 * RK( 175 ) * C( O            )                                       
     &                                  + 1.66000 * RK( 176 ) * C( OH           )                                       
     &                                  + 7.00000 * RK( 177 ) * C( O3           )                                       
      PD( PAR         ,CL           ) = -           RK( 235 ) * C( PAR          )                                       
     &                                  -           RK( 239 ) * C( OLE          )                                       
     &                                  + 0.30000 * RK( 240 ) * C( IOLE         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  46: ACET                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACET        ,O3           ) =   0.17000 * RK( 159 ) * C( ISPD         )                                       
      PD( ACET        ,OH           ) = -           RK( 130 ) * C( ACET         )                                       
     &                                  + 0.13700 * RK( 158 ) * C( ISPD         )                                       
      PD( ACET        ,ACET         ) = -           RK( 129 )                                                           
     &                                  -           RK( 130 ) * C( OH           )                                       
      PD( ACET        ,PRPA         ) =             RK( 236 ) * C( CL           )                                       
      PD( ACET        ,XPRP         ) =   0.73200 * RK( 222 )                                                           
      PD( ACET        ,ROR          ) =   0.42000 * RK( 133 )                                                           
      PD( ACET        ,ISPD         ) =   0.13700 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.17000 * RK( 159 ) * C( O3           )                                       
     &                                  + 0.17000 * RK( 161 )                                                           
      PD( ACET        ,CL           ) =             RK( 236 ) * C( PRPA         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  47: PRPA                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PRPA        ,OH           ) = -           RK( 131 ) * C( PRPA         )                                       
      PD( PRPA        ,PRPA         ) = -           RK( 131 ) * C( OH           )                                       
     &                                  -           RK( 236 ) * C( CL           )                                       
      PD( PRPA        ,CL           ) = -           RK( 236 ) * C( PRPA         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  48: XPRP                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XPRP        ,OH           ) =             RK( 131 ) * C( PRPA         )                                       
      PD( XPRP        ,PRPA         ) =             RK( 131 ) * C( OH           )                                       
      PD( XPRP        ,XPRP         ) = -           RK( 221 )                                                           
     &                                  -           RK( 222 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  49: XPAR                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XPAR        ,OH           ) =             RK( 132 ) * C( PAR          )                                       
      PD( XPAR        ,PAR          ) =             RK( 132 ) * C( OH           )                                       
     &                                  +           RK( 235 ) * C( CL           )                                       
      PD( XPAR        ,XPAR         ) = -           RK( 223 )                                                           
     &                                  -           RK( 224 )                                                           
      PD( XPAR        ,CL           ) =             RK( 235 ) * C( PAR          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  50: ROR                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ROR         ,NO2          ) = -           RK( 135 ) * C( ROR          )                                       
      PD( ROR         ,XPAR         ) =   0.87400 * RK( 224 )                                                           
      PD( ROR         ,ROR          ) = - 0.98000 * RK( 133 )                                                           
     &                                  -           RK( 134 )                                                           
     &                                  -           RK( 135 ) * C( NO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  51: ETHY                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETHY        ,OH           ) = -           RK( 136 ) * C( ETHY         )                                       
      PD( ETHY        ,ETHY         ) = -           RK( 136 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  52: ETH                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETH         ,O            ) = -           RK( 137 ) * C( ETH          )                                       
      PD( ETH         ,O3           ) = -           RK( 139 ) * C( ETH          )                                       
      PD( ETH         ,NO3          ) = -           RK( 140 ) * C( ETH          )                                       
      PD( ETH         ,OH           ) = -           RK( 138 ) * C( ETH          )                                       
      PD( ETH         ,ETH          ) = -           RK( 137 ) * C( O            )                                       
     &                                  -           RK( 138 ) * C( OH           )                                       
     &                                  -           RK( 139 ) * C( O3           )                                       
     &                                  -           RK( 140 ) * C( NO3          )                                       
     &                                  -           RK( 238 ) * C( CL           )                                       
      PD( ETH         ,CL           ) = -           RK( 238 ) * C( ETH          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  53: OLE                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OLE         ,O            ) = -           RK( 141 ) * C( OLE          )                                       
      PD( OLE         ,O3           ) = -           RK( 143 ) * C( OLE          )                                       
      PD( OLE         ,NO3          ) = -           RK( 144 ) * C( OLE          )                                       
      PD( OLE         ,OH           ) = -           RK( 142 ) * C( OLE          )                                       
     &                                  + 0.09800 * RK( 170 ) * C( INTR         )                                       
      PD( OLE         ,OLE          ) = -           RK( 141 ) * C( O            )                                       
     &                                  -           RK( 142 ) * C( OH           )                                       
     &                                  -           RK( 143 ) * C( O3           )                                       
     &                                  -           RK( 144 ) * C( NO3          )                                       
     &                                  -           RK( 239 ) * C( CL           )                                       
      PD( OLE         ,IOLE         ) =   0.30000 * RK( 240 ) * C( CL           )                                       
      PD( OLE         ,ISPD         ) =   0.24000 * RK( 161 )                                                           
      PD( OLE         ,INTR         ) =   0.09800 * RK( 170 ) * C( OH           )                                       
      PD( OLE         ,CL           ) = -           RK( 239 ) * C( OLE          )                                       
     &                                  + 0.30000 * RK( 240 ) * C( IOLE         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  54: IOLE                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( IOLE        ,O            ) = -           RK( 145 ) * C( IOLE         )                                       
      PD( IOLE        ,O3           ) = -           RK( 147 ) * C( IOLE         )                                       
      PD( IOLE        ,NO3          ) = -           RK( 148 ) * C( IOLE         )                                       
      PD( IOLE        ,OH           ) = -           RK( 146 ) * C( IOLE         )                                       
     &                                  + 0.02900 * RK( 162 ) * C( ISPX         )                                       
      PD( IOLE        ,IOLE         ) = -           RK( 145 ) * C( O            )                                       
     &                                  -           RK( 146 ) * C( OH           )                                       
     &                                  -           RK( 147 ) * C( O3           )                                       
     &                                  -           RK( 148 ) * C( NO3          )                                       
     &                                  -           RK( 240 ) * C( CL           )                                       
      PD( IOLE        ,ISPX         ) =   0.02900 * RK( 162 ) * C( OH           )                                       
      PD( IOLE        ,CL           ) = -           RK( 240 ) * C( IOLE         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  55: ISOP                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISOP        ,O            ) = -           RK( 150 ) * C( ISOP         )                                       
      PD( ISOP        ,O3           ) = -           RK( 156 ) * C( ISOP         )                                       
      PD( ISOP        ,NO3          ) = -           RK( 157 ) * C( ISOP         )                                       
      PD( ISOP        ,OH           ) = -           RK( 149 ) * C( ISOP         )                                       
      PD( ISOP        ,ISOP         ) = -           RK( 149 ) * C( OH           )                                       
     &                                  -           RK( 150 ) * C( O            )                                       
     &                                  -           RK( 156 ) * C( O3           )                                       
     &                                  -           RK( 157 ) * C( NO3          )                                       
     &                                  -           RK( 241 ) * C( CL           )                                       
      PD( ISOP        ,CL           ) = -           RK( 241 ) * C( ISOP         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  56: ISO2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISO2        ,NO           ) = -           RK( 151 ) * C( ISO2         )                                       
      PD( ISO2        ,OH           ) =             RK( 149 ) * C( ISOP         )                                       
     &                                  + 0.06700 * RK( 162 ) * C( ISPX         )                                       
      PD( ISO2        ,HO2          ) = -           RK( 152 ) * C( ISO2         )                                       
      PD( ISO2        ,C2O3         ) = -           RK( 153 ) * C( ISO2         )                                       
      PD( ISO2        ,RO2          ) = -           RK( 154 ) * C( ISO2         )                                       
      PD( ISO2        ,ISOP         ) =             RK( 149 ) * C( OH           )                                       
      PD( ISO2        ,ISO2         ) = -           RK( 151 ) * C( NO           )                                       
     &                                  -           RK( 152 ) * C( HO2          )                                       
     &                                  -           RK( 153 ) * C( C2O3         )                                       
     &                                  -           RK( 154 ) * C( RO2          )                                       
     &                                  -           RK( 155 )                                                           
      PD( ISO2        ,ISPX         ) =   0.06700 * RK( 162 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  57: ISOPRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISOPRXN     ,NO3          ) =             RK( 157 ) * C( ISOP         )                                       
      PD( ISOPRXN     ,OH           ) =             RK( 149 ) * C( ISOP         )                                       
      PD( ISOPRXN     ,ISOP         ) =             RK( 149 ) * C( OH           )                                       
     &                                  +           RK( 157 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  58: ISPD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISPD        ,NO           ) =   0.90000 * RK( 151 ) * C( ISO2         )                                       
      PD( ISPD        ,O            ) =   0.75000 * RK( 150 ) * C( ISOP         )                                       
      PD( ISPD        ,O3           ) =   0.65000 * RK( 156 ) * C( ISOP         )                                       
     &                                  -           RK( 159 ) * C( ISPD         )                                       
      PD( ISPD        ,NO3          ) =   0.35000 * RK( 157 ) * C( ISOP         )                                       
     &                                  -           RK( 160 ) * C( ISPD         )                                       
     &                                  +           RK( 164 ) * C( HPLD         )                                       
      PD( ISPD        ,OH           ) = -           RK( 158 ) * C( ISPD         )                                       
      PD( ISPD        ,HO2          ) =   0.12000 * RK( 152 ) * C( ISO2         )                                       
      PD( ISPD        ,C2O3         ) =             RK( 153 ) * C( ISO2         )                                       
      PD( ISPD        ,RO2          ) =             RK( 154 ) * C( ISO2         )                                       
      PD( ISPD        ,ISOP         ) =   0.75000 * RK( 150 ) * C( O            )                                       
     &                                  + 0.65000 * RK( 156 ) * C( O3           )                                       
     &                                  + 0.35000 * RK( 157 ) * C( NO3          )                                       
     &                                  +           RK( 241 ) * C( CL           )                                       
      PD( ISPD        ,ISO2         ) =   0.90000 * RK( 151 ) * C( NO           )                                       
     &                                  + 0.12000 * RK( 152 ) * C( HO2          )                                       
     &                                  +           RK( 153 ) * C( C2O3         )                                       
     &                                  +           RK( 154 ) * C( RO2          )                                       
      PD( ISPD        ,ISPD         ) = -           RK( 158 ) * C( OH           )                                       
     &                                  -           RK( 159 ) * C( O3           )                                       
     &                                  -           RK( 160 ) * C( NO3          )                                       
     &                                  -           RK( 161 )                                                           
      PD( ISPD        ,HPLD         ) =             RK( 163 )                                                           
     &                                  +           RK( 164 ) * C( NO3          )                                       
      PD( ISPD        ,CL           ) =             RK( 241 ) * C( ISOP         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  59: INTR                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( INTR        ,NO           ) =   0.10000 * RK( 151 ) * C( ISO2         )                                       
      PD( INTR        ,OH           ) = - 0.89600 * RK( 170 ) * C( INTR         )                                       
      PD( INTR        ,ISO2         ) =   0.10000 * RK( 151 ) * C( NO           )                                       
      PD( INTR        ,INTR         ) = - 0.89600 * RK( 170 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  60: ISPX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISPX        ,OH           ) = -           RK( 162 ) * C( ISPX         )                                       
      PD( ISPX        ,HO2          ) =   0.88000 * RK( 152 ) * C( ISO2         )                                       
      PD( ISPX        ,ISO2         ) =   0.88000 * RK( 152 ) * C( HO2          )                                       
      PD( ISPX        ,ISPX         ) = -           RK( 162 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  61: HPLD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HPLD        ,NO3          ) = -           RK( 164 ) * C( HPLD         )                                       
      PD( HPLD        ,ISO2         ) =             RK( 155 )                                                           
      PD( HPLD        ,HPLD         ) = -           RK( 163 )                                                           
     &                                  -           RK( 164 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  62: OPO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OPO3        ,NO2          ) = -           RK( 213 ) * C( OPO3         )                                       
      PD( OPO3        ,NO           ) = -           RK( 212 ) * C( OPO3         )                                       
      PD( OPO3        ,NO3          ) =   0.48000 * RK( 196 ) * C( CRES         )                                       
     &                                  +           RK( 209 ) * C( OPEN         )                                       
      PD( OPO3        ,OH           ) =   0.45700 * RK( 158 ) * C( ISPD         )                                       
     &                                  + 0.60000 * RK( 207 ) * C( OPEN         )                                       
      PD( OPO3        ,HO2          ) = -           RK( 215 ) * C( OPO3         )                                       
      PD( OPO3        ,C2O3         ) = -           RK( 216 ) * C( OPO3         )                                       
      PD( OPO3        ,RO2          ) = -           RK( 217 ) * C( OPO3         )                                       
      PD( OPO3        ,ISPD         ) =   0.45700 * RK( 158 ) * C( OH           )                                       
      PD( OPO3        ,OPO3         ) = -           RK( 212 ) * C( NO           )                                       
     &                                  -           RK( 213 ) * C( NO2          )                                       
     &                                  -           RK( 215 ) * C( HO2          )                                       
     &                                  -           RK( 216 ) * C( C2O3         )                                       
     &                                  -           RK( 217 ) * C( RO2          )                                       
      PD( OPO3        ,CRES         ) =   0.48000 * RK( 196 ) * C( NO3          )                                       
      PD( OPO3        ,OPEN         ) =             RK( 206 )                                                           
     &                                  + 0.60000 * RK( 207 ) * C( OH           )                                       
     &                                  +           RK( 209 ) * C( NO3          )                                       
      PD( OPO3        ,OPAN         ) =             RK( 214 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  63: EPOX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( EPOX        ,OH           ) =   0.90400 * RK( 162 ) * C( ISPX         )                                       
     &                                  -           RK( 165 ) * C( EPOX         )                                       
      PD( EPOX        ,ISPX         ) =   0.90400 * RK( 162 ) * C( OH           )                                       
      PD( EPOX        ,EPOX         ) = -           RK( 165 ) * C( OH           )                                       
     &                                  -           RK( 278 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  64: EPX2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( EPX2        ,NO           ) = -           RK( 167 ) * C( EPX2         )                                       
      PD( EPX2        ,OH           ) =             RK( 165 ) * C( EPOX         )                                       
      PD( EPX2        ,HO2          ) = -           RK( 166 ) * C( EPX2         )                                       
      PD( EPX2        ,C2O3         ) = -           RK( 168 ) * C( EPX2         )                                       
      PD( EPX2        ,RO2          ) = -           RK( 169 ) * C( EPX2         )                                       
      PD( EPX2        ,EPOX         ) =             RK( 165 ) * C( OH           )                                       
      PD( EPX2        ,EPX2         ) = -           RK( 166 ) * C( HO2          )                                       
     &                                  -           RK( 167 ) * C( NO           )                                       
     &                                  -           RK( 168 ) * C( C2O3         )                                       
     &                                  -           RK( 169 ) * C( RO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  65: TERP                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TERP        ,O            ) = -           RK( 171 ) * C( TERP         )                                       
      PD( TERP        ,O3           ) = -           RK( 173 ) * C( TERP         )                                       
      PD( TERP        ,NO3          ) = -           RK( 174 ) * C( TERP         )                                       
      PD( TERP        ,OH           ) = -           RK( 172 ) * C( TERP         )                                       
      PD( TERP        ,TERP         ) = -           RK( 171 ) * C( O            )                                       
     &                                  -           RK( 172 ) * C( OH           )                                       
     &                                  -           RK( 173 ) * C( O3           )                                       
     &                                  -           RK( 174 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  66: TRPRXN                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TRPRXN      ,O            ) =             RK( 171 ) * C( TERP         )                                       
     &                                  +           RK( 175 ) * C( APIN         )                                       
      PD( TRPRXN      ,O3           ) =             RK( 173 ) * C( TERP         )                                       
     &                                  +           RK( 177 ) * C( APIN         )                                       
      PD( TRPRXN      ,OH           ) =             RK( 172 ) * C( TERP         )                                       
     &                                  +           RK( 176 ) * C( APIN         )                                       
      PD( TRPRXN      ,TERP         ) =             RK( 171 ) * C( O            )                                       
     &                                  +           RK( 172 ) * C( OH           )                                       
     &                                  +           RK( 173 ) * C( O3           )                                       
      PD( TRPRXN      ,APIN         ) =             RK( 175 ) * C( O            )                                       
     &                                  +           RK( 176 ) * C( OH           )                                       
     &                                  +           RK( 177 ) * C( O3           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  67: TERPNRO2                                                                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TERPNRO2    ,NO           ) = -           RK( 283 ) * C( TERPNRO2     )                                       
      PD( TERPNRO2    ,NO3          ) =             RK( 174 ) * C( TERP         )                                       
     &                                  -           RK( 285 ) * C( TERPNRO2     )                                       
      PD( TERPNRO2    ,HO2          ) = -           RK( 284 ) * C( TERPNRO2     )                                       
      PD( TERPNRO2    ,RO2          ) = -           RK( 286 ) * C( TERPNRO2     )                                       
      PD( TERPNRO2    ,TERP         ) =             RK( 174 ) * C( NO3          )                                       
      PD( TERPNRO2    ,TERPNRO2     ) = -           RK( 283 ) * C( NO           )                                       
     &                                  -           RK( 284 ) * C( HO2          )                                       
     &                                  -           RK( 285 ) * C( NO3          )                                       
     &                                  -           RK( 286 ) * C( RO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  68: APIN                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( APIN        ,O            ) = -           RK( 175 ) * C( APIN         )                                       
      PD( APIN        ,O3           ) = -           RK( 177 ) * C( APIN         )                                       
      PD( APIN        ,NO3          ) = -           RK( 178 ) * C( APIN         )                                       
      PD( APIN        ,OH           ) = -           RK( 176 ) * C( APIN         )                                       
      PD( APIN        ,APIN         ) = -           RK( 175 ) * C( O            )                                       
     &                                  -           RK( 176 ) * C( OH           )                                       
     &                                  -           RK( 177 ) * C( O3           )                                       
     &                                  -           RK( 178 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  69: BENZENE                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BENZENE     ,OH           ) = -           RK( 179 ) * C( BENZENE      )                                       
      PD( BENZENE     ,BENZENE      ) = -           RK( 179 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  70: CRES                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRES        ,NO3          ) = -           RK( 196 ) * C( CRES         )                                       
      PD( CRES        ,OH           ) =   0.53000 * RK( 179 ) * C( BENZENE      )                                       
     &                                  + 0.18000 * RK( 184 ) * C( TOL          )                                       
     &                                  + 0.15500 * RK( 189 ) * C( XYLMN        )                                       
     &                                  + 0.15500 * RK( 190 ) * C( NAPH         )                                       
     &                                  -           RK( 195 ) * C( CRES         )                                       
      PD( CRES        ,HO2          ) =             RK( 198 ) * C( CRO          )                                       
      PD( CRES        ,BENZENE      ) =   0.53000 * RK( 179 ) * C( OH           )                                       
      PD( CRES        ,CRES         ) = -           RK( 195 ) * C( OH           )                                       
     &                                  -           RK( 196 ) * C( NO3          )                                       
      PD( CRES        ,TOL          ) =   0.18000 * RK( 184 ) * C( OH           )                                       
     &                                  + 0.18000 * RK( 248 ) * C( CL           )                                       
      PD( CRES        ,XYLMN        ) =   0.15500 * RK( 189 ) * C( OH           )                                       
     &                                  + 0.15500 * RK( 249 ) * C( CL           )                                       
      PD( CRES        ,NAPH         ) =   0.15500 * RK( 190 ) * C( OH           )                                       
     &                                  + 0.15500 * RK( 250 ) * C( CL           )                                       
      PD( CRES        ,CRO          ) =             RK( 198 ) * C( HO2          )                                       
      PD( CRES        ,CL           ) =   0.18000 * RK( 248 ) * C( TOL          )                                       
     &                                  + 0.15500 * RK( 249 ) * C( XYLMN        )                                       
     &                                  + 0.15500 * RK( 250 ) * C( NAPH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  71: BZO2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BZO2        ,NO           ) = -           RK( 180 ) * C( BZO2         )                                       
      PD( BZO2        ,OH           ) =   0.35200 * RK( 179 ) * C( BENZENE      )                                       
      PD( BZO2        ,HO2          ) = -           RK( 182 ) * C( BZO2         )                                       
      PD( BZO2        ,C2O3         ) = -           RK( 181 ) * C( BZO2         )                                       
      PD( BZO2        ,RO2          ) = -           RK( 183 ) * C( BZO2         )                                       
      PD( BZO2        ,BENZENE      ) =   0.35200 * RK( 179 ) * C( OH           )                                       
      PD( BZO2        ,BZO2         ) = -           RK( 180 ) * C( NO           )                                       
     &                                  -           RK( 181 ) * C( C2O3         )                                       
     &                                  -           RK( 182 ) * C( HO2          )                                       
     &                                  -           RK( 183 ) * C( RO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  72: OPEN                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OPEN        ,NO           ) =   0.91800 * RK( 180 ) * C( BZO2         )                                       
     &                                  + 0.66000 * RK( 185 ) * C( TO2          )                                       
     &                                  + 0.30000 * RK( 191 ) * C( XLO2         )                                       
      PD( OPEN        ,O3           ) = -           RK( 208 ) * C( OPEN         )                                       
      PD( OPEN        ,NO3          ) =   0.25000 * RK( 205 ) * C( XOPN         )                                       
     &                                  -           RK( 209 ) * C( OPEN         )                                       
      PD( OPEN        ,OH           ) =   0.11800 * RK( 179 ) * C( BENZENE      )                                       
     &                                  + 0.10000 * RK( 184 ) * C( TOL          )                                       
     &                                  + 0.02500 * RK( 195 ) * C( CRES         )                                       
     &                                  -           RK( 207 ) * C( OPEN         )                                       
      PD( OPEN        ,C2O3         ) =             RK( 181 ) * C( BZO2         )                                       
     &                                  + 0.77000 * RK( 186 ) * C( TO2          )                                       
     &                                  + 0.35000 * RK( 193 ) * C( XLO2         )                                       
      PD( OPEN        ,RO2          ) =             RK( 183 ) * C( BZO2         )                                       
     &                                  + 0.77000 * RK( 188 ) * C( TO2          )                                       
     &                                  + 0.35000 * RK( 194 ) * C( XLO2         )                                       
      PD( OPEN        ,BENZENE      ) =   0.11800 * RK( 179 ) * C( OH           )                                       
      PD( OPEN        ,CRES         ) =   0.02500 * RK( 195 ) * C( OH           )                                       
      PD( OPEN        ,BZO2         ) =   0.91800 * RK( 180 ) * C( NO           )                                       
     &                                  +           RK( 181 ) * C( C2O3         )                                       
     &                                  +           RK( 183 ) * C( RO2          )                                       
      PD( OPEN        ,OPEN         ) = -           RK( 206 )                                                           
     &                                  -           RK( 207 ) * C( OH           )                                       
     &                                  -           RK( 208 ) * C( O3           )                                       
     &                                  -           RK( 209 ) * C( NO3          )                                       
      PD( OPEN        ,TOL          ) =   0.10000 * RK( 184 ) * C( OH           )                                       
     &                                  + 0.10000 * RK( 248 ) * C( CL           )                                       
      PD( OPEN        ,TO2          ) =   0.66000 * RK( 185 ) * C( NO           )                                       
     &                                  + 0.77000 * RK( 186 ) * C( C2O3         )                                       
     &                                  + 0.77000 * RK( 188 ) * C( RO2          )                                       
      PD( OPEN        ,XOPN         ) =   0.25000 * RK( 205 ) * C( NO3          )                                       
      PD( OPEN        ,XLO2         ) =   0.30000 * RK( 191 ) * C( NO           )                                       
     &                                  + 0.35000 * RK( 193 ) * C( C2O3         )                                       
     &                                  + 0.35000 * RK( 194 ) * C( RO2          )                                       
      PD( OPEN        ,CRON         ) =             RK( 201 )                                                           
      PD( OPEN        ,CL           ) =   0.10000 * RK( 248 ) * C( TOL          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  73: BENZRO2                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BENZRO2     ,NO           ) = -           RK( 260 ) * C( BENZRO2      )                                       
      PD( BENZRO2     ,OH           ) =             RK( 179 ) * C( BENZENE      )                                       
      PD( BENZRO2     ,HO2          ) = -           RK( 261 ) * C( BENZRO2      )                                       
      PD( BENZRO2     ,BENZENE      ) =             RK( 179 ) * C( OH           )                                       
      PD( BENZRO2     ,BENZRO2      ) = -           RK( 260 ) * C( NO           )                                       
     &                                  -           RK( 261 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  74: TOL                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOL         ,OH           ) = -           RK( 184 ) * C( TOL          )                                       
      PD( TOL         ,TOL          ) = -           RK( 184 ) * C( OH           )                                       
     &                                  -           RK( 248 ) * C( CL           )                                       
      PD( TOL         ,CL           ) = -           RK( 248 ) * C( TOL          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  75: TO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TO2         ,NO           ) = -           RK( 185 ) * C( TO2          )                                       
      PD( TO2         ,OH           ) =   0.65000 * RK( 184 ) * C( TOL          )                                       
      PD( TO2         ,HO2          ) = -           RK( 187 ) * C( TO2          )                                       
      PD( TO2         ,C2O3         ) = -           RK( 186 ) * C( TO2          )                                       
      PD( TO2         ,RO2          ) = -           RK( 188 ) * C( TO2          )                                       
      PD( TO2         ,TOL          ) =   0.65000 * RK( 184 ) * C( OH           )                                       
     &                                  + 0.65000 * RK( 248 ) * C( CL           )                                       
      PD( TO2         ,TO2          ) = -           RK( 185 ) * C( NO           )                                       
     &                                  -           RK( 186 ) * C( C2O3         )                                       
     &                                  -           RK( 187 ) * C( HO2          )                                       
     &                                  -           RK( 188 ) * C( RO2          )                                       
      PD( TO2         ,CL           ) =   0.65000 * RK( 248 ) * C( TOL          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  76: TOLRO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLRO2      ,NO           ) = -           RK( 256 ) * C( TOLRO2       )                                       
      PD( TOLRO2      ,OH           ) =             RK( 184 ) * C( TOL          )                                       
      PD( TOLRO2      ,HO2          ) = -           RK( 257 ) * C( TOLRO2       )                                       
      PD( TOLRO2      ,TOL          ) =             RK( 184 ) * C( OH           )                                       
     &                                  +           RK( 248 ) * C( CL           )                                       
      PD( TOLRO2      ,TOLRO2       ) = -           RK( 256 ) * C( NO           )                                       
     &                                  -           RK( 257 ) * C( HO2          )                                       
      PD( TOLRO2      ,CL           ) =             RK( 248 ) * C( TOL          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  77: XOPN                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XOPN        ,NO           ) =   0.20000 * RK( 185 ) * C( TO2          )                                       
     &                                  + 0.56000 * RK( 191 ) * C( XLO2         )                                       
      PD( XOPN        ,O3           ) = -           RK( 204 ) * C( XOPN         )                                       
      PD( XOPN        ,NO3          ) = -           RK( 205 ) * C( XOPN         )                                       
      PD( XOPN        ,OH           ) =   0.24400 * RK( 189 ) * C( XYLMN        )                                       
     &                                  + 0.24400 * RK( 190 ) * C( NAPH         )                                       
     &                                  -           RK( 203 ) * C( XOPN         )                                       
      PD( XOPN        ,C2O3         ) =   0.23000 * RK( 186 ) * C( TO2          )                                       
     &                                  + 0.65000 * RK( 193 ) * C( XLO2         )                                       
      PD( XOPN        ,RO2          ) =   0.23000 * RK( 188 ) * C( TO2          )                                       
     &                                  + 0.65000 * RK( 194 ) * C( XLO2         )                                       
      PD( XOPN        ,TO2          ) =   0.20000 * RK( 185 ) * C( NO           )                                       
     &                                  + 0.23000 * RK( 186 ) * C( C2O3         )                                       
     &                                  + 0.23000 * RK( 188 ) * C( RO2          )                                       
      PD( XOPN        ,XOPN         ) = -           RK( 202 )                                                           
     &                                  -           RK( 203 ) * C( OH           )                                       
     &                                  -           RK( 204 ) * C( O3           )                                       
     &                                  -           RK( 205 ) * C( NO3          )                                       
      PD( XOPN        ,XYLMN        ) =   0.24400 * RK( 189 ) * C( OH           )                                       
     &                                  + 0.24400 * RK( 249 ) * C( CL           )                                       
      PD( XOPN        ,XLO2         ) =   0.56000 * RK( 191 ) * C( NO           )                                       
     &                                  + 0.65000 * RK( 193 ) * C( C2O3         )                                       
     &                                  + 0.65000 * RK( 194 ) * C( RO2          )                                       
      PD( XOPN        ,NAPH         ) =   0.24400 * RK( 190 ) * C( OH           )                                       
     &                                  + 0.24400 * RK( 250 ) * C( CL           )                                       
      PD( XOPN        ,CL           ) =   0.24400 * RK( 249 ) * C( XYLMN        )                                       
     &                                  + 0.24400 * RK( 250 ) * C( NAPH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  78: XYLMN                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLMN       ,OH           ) = -           RK( 189 ) * C( XYLMN        )                                       
      PD( XYLMN       ,XYLMN        ) = -           RK( 189 ) * C( OH           )                                       
     &                                  -           RK( 249 ) * C( CL           )                                       
      PD( XYLMN       ,CL           ) = -           RK( 249 ) * C( XYLMN        )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  79: XLO2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XLO2        ,NO           ) = -           RK( 191 ) * C( XLO2         )                                       
      PD( XLO2        ,OH           ) =   0.54400 * RK( 189 ) * C( XYLMN        )                                       
     &                                  + 0.54400 * RK( 190 ) * C( NAPH         )                                       
      PD( XLO2        ,HO2          ) = -           RK( 192 ) * C( XLO2         )                                       
      PD( XLO2        ,C2O3         ) = -           RK( 193 ) * C( XLO2         )                                       
      PD( XLO2        ,RO2          ) = -           RK( 194 ) * C( XLO2         )                                       
      PD( XLO2        ,XYLMN        ) =   0.54400 * RK( 189 ) * C( OH           )                                       
     &                                  + 0.54400 * RK( 249 ) * C( CL           )                                       
      PD( XLO2        ,XLO2         ) = -           RK( 191 ) * C( NO           )                                       
     &                                  -           RK( 192 ) * C( HO2          )                                       
     &                                  -           RK( 193 ) * C( C2O3         )                                       
     &                                  -           RK( 194 ) * C( RO2          )                                       
      PD( XLO2        ,NAPH         ) =   0.54400 * RK( 190 ) * C( OH           )                                       
     &                                  + 0.54400 * RK( 250 ) * C( CL           )                                       
      PD( XLO2        ,CL           ) =   0.54400 * RK( 249 ) * C( XYLMN        )                                       
     &                                  + 0.54400 * RK( 250 ) * C( NAPH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  80: XYLRO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLRO2      ,NO           ) = -           RK( 258 ) * C( XYLRO2       )                                       
      PD( XYLRO2      ,OH           ) =             RK( 189 ) * C( XYLMN        )                                       
      PD( XYLRO2      ,HO2          ) = -           RK( 259 ) * C( XYLRO2       )                                       
      PD( XYLRO2      ,XYLMN        ) =             RK( 189 ) * C( OH           )                                       
     &                                  +           RK( 249 ) * C( CL           )                                       
      PD( XYLRO2      ,XYLRO2       ) = -           RK( 258 ) * C( NO           )                                       
     &                                  -           RK( 259 ) * C( HO2          )                                       
      PD( XYLRO2      ,CL           ) =             RK( 249 ) * C( XYLMN        )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  81: NAPH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NAPH        ,OH           ) = -           RK( 190 ) * C( NAPH         )                                       
      PD( NAPH        ,NAPH         ) = -           RK( 190 ) * C( OH           )                                       
     &                                  -           RK( 250 ) * C( CL           )                                       
      PD( NAPH        ,CL           ) = -           RK( 250 ) * C( NAPH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  82: PAHRO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAHRO2      ,NO           ) = -           RK( 265 ) * C( PAHRO2       )                                       
      PD( PAHRO2      ,OH           ) =             RK( 190 ) * C( NAPH         )                                       
      PD( PAHRO2      ,HO2          ) = -           RK( 266 ) * C( PAHRO2       )                                       
      PD( PAHRO2      ,NAPH         ) =             RK( 190 ) * C( OH           )                                       
     &                                  +           RK( 250 ) * C( CL           )                                       
      PD( PAHRO2      ,PAHRO2       ) = -           RK( 265 ) * C( NO           )                                       
     &                                  -           RK( 266 ) * C( HO2          )                                       
      PD( PAHRO2      ,CL           ) =             RK( 250 ) * C( NAPH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  83: CRO                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRO         ,NO2          ) = -           RK( 197 ) * C( CRO          )                                       
      PD( CRO         ,NO3          ) =   0.30000 * RK( 196 ) * C( CRES         )                                       
     &                                  + 0.50000 * RK( 200 ) * C( CRON         )                                       
     &                                  +           RK( 211 ) * C( CAT1         )                                       
      PD( CRO         ,OH           ) =   0.20000 * RK( 195 ) * C( CRES         )                                       
     &                                  + 0.50000 * RK( 199 ) * C( CRON         )                                       
     &                                  + 0.50000 * RK( 210 ) * C( CAT1         )                                       
      PD( CRO         ,HO2          ) = -           RK( 198 ) * C( CRO          )                                       
      PD( CRO         ,CRES         ) =   0.20000 * RK( 195 ) * C( OH           )                                       
     &                                  + 0.30000 * RK( 196 ) * C( NO3          )                                       
      PD( CRO         ,CRO          ) = -           RK( 197 ) * C( NO2          )                                       
     &                                  -           RK( 198 ) * C( HO2          )                                       
      PD( CRO         ,CAT1         ) =   0.50000 * RK( 210 ) * C( OH           )                                       
     &                                  +           RK( 211 ) * C( NO3          )                                       
      PD( CRO         ,CRON         ) =   0.50000 * RK( 199 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 200 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  84: CAT1                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CAT1        ,NO3          ) = -           RK( 211 ) * C( CAT1         )                                       
      PD( CAT1        ,OH           ) =   0.73200 * RK( 195 ) * C( CRES         )                                       
     &                                  -           RK( 210 ) * C( CAT1         )                                       
      PD( CAT1        ,CRES         ) =   0.73200 * RK( 195 ) * C( OH           )                                       
      PD( CAT1        ,CAT1         ) = -           RK( 210 ) * C( OH           )                                       
     &                                  -           RK( 211 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  85: CRON                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRON        ,NO2          ) =             RK( 197 ) * C( CRO          )                                       
      PD( CRON        ,NO3          ) = -           RK( 200 ) * C( CRON         )                                       
      PD( CRON        ,OH           ) = -           RK( 199 ) * C( CRON         )                                       
      PD( CRON        ,CRO          ) =             RK( 197 ) * C( NO2          )                                       
      PD( CRON        ,CRON         ) = -           RK( 199 ) * C( OH           )                                       
     &                                  -           RK( 200 ) * C( NO3          )                                       
     &                                  -           RK( 201 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  86: OPAN                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OPAN        ,NO2          ) =             RK( 213 ) * C( OPO3         )                                       
      PD( OPAN        ,OH           ) = -           RK( 218 ) * C( OPAN         )                                       
      PD( OPAN        ,OPO3         ) =             RK( 213 ) * C( NO2          )                                       
      PD( OPAN        ,OPAN         ) = -           RK( 214 )                                                           
     &                                  -           RK( 218 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  87: ECH4                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ECH4        ,OH           ) = -           RK( 220 ) * C( ECH4         )                                       
      PD( ECH4        ,ECH4         ) = -           RK( 220 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  88: CL2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CL2         ,CL2          ) = -           RK( 225 )                                                           
      PD( CL2         ,CLO          ) =   4.00000 * RK( 228 ) * C( CLO          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  89: CL                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CL          ,NO           ) =             RK( 229 ) * C( CLO          )                                       
      PD( CL          ,O3           ) = -           RK( 227 ) * C( CL           )                                       
      PD( CL          ,OH           ) =             RK( 232 ) * C( FMCL         )                                       
     &                                  +           RK( 247 ) * C( HCL          )                                       
      PD( CL          ,MEO2         ) =             RK( 231 ) * C( CLO          )                                       
      PD( CL          ,ALD2         ) = -           RK( 243 ) * C( CL           )                                       
      PD( CL          ,FORM         ) = -           RK( 242 ) * C( CL           )                                       
      PD( CL          ,MEOH         ) = -           RK( 245 ) * C( CL           )                                       
      PD( CL          ,ALDX         ) = -           RK( 244 ) * C( CL           )                                       
      PD( CL          ,ETHA         ) = -           RK( 237 ) * C( CL           )                                       
      PD( CL          ,ETOH         ) = -           RK( 246 ) * C( CL           )                                       
      PD( CL          ,PAR          ) = -           RK( 235 ) * C( CL           )                                       
      PD( CL          ,PRPA         ) = -           RK( 236 ) * C( CL           )                                       
      PD( CL          ,ETH          ) = -           RK( 238 ) * C( CL           )                                       
      PD( CL          ,OLE          ) = -           RK( 239 ) * C( CL           )                                       
      PD( CL          ,IOLE         ) = -           RK( 240 ) * C( CL           )                                       
      PD( CL          ,ISOP         ) = -           RK( 241 ) * C( CL           )                                       
      PD( CL          ,TOL          ) = -           RK( 248 ) * C( CL           )                                       
      PD( CL          ,XYLMN        ) = -           RK( 249 ) * C( CL           )                                       
      PD( CL          ,NAPH         ) = -           RK( 250 ) * C( CL           )                                       
      PD( CL          ,CL2          ) =   2.00000 * RK( 225 )                                                           
      PD( CL          ,CL           ) = -           RK( 227 ) * C( O3           )                                       
     &                                  -           RK( 234 )                                                           
     &                                  -           RK( 235 ) * C( PAR          )                                       
     &                                  -           RK( 236 ) * C( PRPA         )                                       
     &                                  -           RK( 237 ) * C( ETHA         )                                       
     &                                  -           RK( 238 ) * C( ETH          )                                       
     &                                  -           RK( 239 ) * C( OLE          )                                       
     &                                  -           RK( 240 ) * C( IOLE         )                                       
     &                                  -           RK( 241 ) * C( ISOP         )                                       
     &                                  -           RK( 242 ) * C( FORM         )                                       
     &                                  -           RK( 243 ) * C( ALD2         )                                       
     &                                  -           RK( 244 ) * C( ALDX         )                                       
     &                                  -           RK( 245 ) * C( MEOH         )                                       
     &                                  -           RK( 246 ) * C( ETOH         )                                       
     &                                  -           RK( 248 ) * C( TOL          )                                       
     &                                  -           RK( 249 ) * C( XYLMN        )                                       
     &                                  -           RK( 250 ) * C( NAPH         )                                       
      PD( CL          ,HOCL         ) =             RK( 226 )                                                           
      PD( CL          ,CLO          ) =   4.00000 * RK( 228 ) * C( CLO          )                                       
     &                                  +           RK( 229 ) * C( NO           )                                       
     &                                  +           RK( 231 ) * C( MEO2         )                                       
      PD( CL          ,FMCL         ) =             RK( 232 ) * C( OH           )                                       
     &                                  +           RK( 233 )                                                           
      PD( CL          ,HCL          ) =             RK( 247 ) * C( OH           )                                       
      PD( CL          ,CLNO2        ) =             RK( 251 )                                                           
      PD( CL          ,CLNO3        ) =             RK( 254 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  90: HOCL                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HOCL        ,HO2          ) =             RK( 230 ) * C( CLO          )                                       
      PD( HOCL        ,HOCL         ) = -           RK( 226 )                                                           
      PD( HOCL        ,CLO          ) =             RK( 230 ) * C( HO2          )                                       
      PD( HOCL        ,CLNO3        ) =             RK( 255 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  91: CLO                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLO         ,NO2          ) = -           RK( 252 ) * C( CLO          )                                       
      PD( CLO         ,NO           ) = -           RK( 229 ) * C( CLO          )                                       
      PD( CLO         ,O3           ) =             RK( 227 ) * C( CL           )                                       
      PD( CLO         ,HO2          ) = -           RK( 230 ) * C( CLO          )                                       
      PD( CLO         ,MEO2         ) = -           RK( 231 ) * C( CLO          )                                       
      PD( CLO         ,CL           ) =             RK( 227 ) * C( O3           )                                       
      PD( CLO         ,CLO          ) = - 4.00000 * RK( 228 ) * C( CLO          )                                       
     &                                  -           RK( 229 ) * C( NO           )                                       
     &                                  -           RK( 230 ) * C( HO2          )                                       
     &                                  -           RK( 231 ) * C( MEO2         )                                       
     &                                  -           RK( 252 ) * C( NO2          )                                       
      PD( CLO         ,CLNO3        ) =             RK( 253 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  92: FMCL                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FMCL        ,OH           ) = -           RK( 232 ) * C( FMCL         )                                       
      PD( FMCL        ,ETH          ) =             RK( 238 ) * C( CL           )                                       
      PD( FMCL        ,OLE          ) =             RK( 239 ) * C( CL           )                                       
      PD( FMCL        ,IOLE         ) =   0.70000 * RK( 240 ) * C( CL           )                                       
      PD( FMCL        ,ISOP         ) =             RK( 241 ) * C( CL           )                                       
      PD( FMCL        ,CL           ) =             RK( 238 ) * C( ETH          )                                       
     &                                  +           RK( 239 ) * C( OLE          )                                       
     &                                  + 0.70000 * RK( 240 ) * C( IOLE         )                                       
     &                                  +           RK( 241 ) * C( ISOP         )                                       
      PD( FMCL        ,FMCL         ) = -           RK( 232 ) * C( OH           )                                       
     &                                  -           RK( 233 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  93: HCL                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HCL         ,OH           ) = -           RK( 247 ) * C( HCL          )                                       
      PD( HCL         ,ALD2         ) =             RK( 243 ) * C( CL           )                                       
      PD( HCL         ,FORM         ) =             RK( 242 ) * C( CL           )                                       
      PD( HCL         ,MEOH         ) =             RK( 245 ) * C( CL           )                                       
      PD( HCL         ,ALDX         ) =             RK( 244 ) * C( CL           )                                       
      PD( HCL         ,ETHA         ) =             RK( 237 ) * C( CL           )                                       
      PD( HCL         ,ETOH         ) =             RK( 246 ) * C( CL           )                                       
      PD( HCL         ,PAR          ) =             RK( 235 ) * C( CL           )                                       
      PD( HCL         ,PRPA         ) =             RK( 236 ) * C( CL           )                                       
      PD( HCL         ,IOLE         ) =   0.30000 * RK( 240 ) * C( CL           )                                       
      PD( HCL         ,TOL          ) =             RK( 248 ) * C( CL           )                                       
      PD( HCL         ,XYLMN        ) =             RK( 249 ) * C( CL           )                                       
      PD( HCL         ,NAPH         ) =             RK( 250 ) * C( CL           )                                       
      PD( HCL         ,CL           ) =             RK( 234 )                                                           
     &                                  +           RK( 235 ) * C( PAR          )                                       
     &                                  +           RK( 236 ) * C( PRPA         )                                       
     &                                  +           RK( 237 ) * C( ETHA         )                                       
     &                                  + 0.30000 * RK( 240 ) * C( IOLE         )                                       
     &                                  +           RK( 242 ) * C( FORM         )                                       
     &                                  +           RK( 243 ) * C( ALD2         )                                       
     &                                  +           RK( 244 ) * C( ALDX         )                                       
     &                                  +           RK( 245 ) * C( MEOH         )                                       
     &                                  +           RK( 246 ) * C( ETOH         )                                       
     &                                  +           RK( 248 ) * C( TOL          )                                       
     &                                  +           RK( 249 ) * C( XYLMN        )                                       
     &                                  +           RK( 250 ) * C( NAPH         )                                       
      PD( HCL         ,HCL          ) = -           RK( 247 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  94: CLNO2                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLNO2       ,CLNO2        ) = -           RK( 251 )                                                           
      PD( CLNO2       ,H2NO3PIJ     ) =             RK( 273 ) * C( ACLI         )                                       
     &                                  +           RK( 274 ) * C( ACLJ         )                                       
      PD( CLNO2       ,H2NO3PK      ) =             RK( 275 ) * C( ACLK         )                                       
      PD( CLNO2       ,ACLI         ) =             RK( 273 ) * C( H2NO3PIJ     )                                       
      PD( CLNO2       ,ACLJ         ) =             RK( 274 ) * C( H2NO3PIJ     )                                       
      PD( CLNO2       ,ACLK         ) =             RK( 275 ) * C( H2NO3PK      )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  95: CLNO3                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLNO3       ,NO2          ) =             RK( 252 ) * C( CLO          )                                       
      PD( CLNO3       ,CLO          ) =             RK( 252 ) * C( NO2          )                                       
      PD( CLNO3       ,CLNO3        ) = -           RK( 253 )                                                           
     &                                  -           RK( 254 )                                                           
     &                                  -           RK( 255 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  96: SVAVB2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SVAVB2      ,NO           ) =   0.01600 * RK( 256 ) * C( TOLRO2       )                                       
     &                                  + 0.01500 * RK( 258 ) * C( XYLRO2       )                                       
     &                                  + 0.03400 * RK( 260 ) * C( BENZRO2      )                                       
     &                                  + 0.02800 * RK( 265 ) * C( PAHRO2       )                                       
      PD( SVAVB2      ,OH           ) =   0.00600 * RK( 267 ) * C( SOAALK       )                                       
      PD( SVAVB2      ,BENZRO2      ) =   0.03400 * RK( 260 ) * C( NO           )                                       
      PD( SVAVB2      ,TOLRO2       ) =   0.01600 * RK( 256 ) * C( NO           )                                       
      PD( SVAVB2      ,XYLRO2       ) =   0.01500 * RK( 258 ) * C( NO           )                                       
      PD( SVAVB2      ,PAHRO2       ) =   0.02800 * RK( 265 ) * C( NO           )                                       
      PD( SVAVB2      ,SOAALK       ) =   0.00600 * RK( 267 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  97: SVAVB3                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SVAVB3      ,NO           ) =   0.05100 * RK( 256 ) * C( TOLRO2       )                                       
     &                                  + 0.02300 * RK( 258 ) * C( XYLRO2       )                                       
     &                                  + 0.22500 * RK( 265 ) * C( PAHRO2       )                                       
      PD( SVAVB3      ,OH           ) =   0.05200 * RK( 267 ) * C( SOAALK       )                                       
      PD( SVAVB3      ,TOLRO2       ) =   0.05100 * RK( 256 ) * C( NO           )                                       
      PD( SVAVB3      ,XYLRO2       ) =   0.02300 * RK( 258 ) * C( NO           )                                       
      PD( SVAVB3      ,PAHRO2       ) =   0.22500 * RK( 265 ) * C( NO           )                                       
      PD( SVAVB3      ,SOAALK       ) =   0.05200 * RK( 267 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  98: SVAVB4                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SVAVB4      ,NO           ) =   0.04700 * RK( 256 ) * C( TOLRO2       )                                       
     &                                  + 0.06000 * RK( 258 ) * C( XYLRO2       )                                       
     &                                  + 0.39200 * RK( 260 ) * C( BENZRO2      )                                       
     &                                  + 0.19100 * RK( 265 ) * C( PAHRO2       )                                       
      PD( SVAVB4      ,OH           ) =   0.08100 * RK( 267 ) * C( SOAALK       )                                       
      PD( SVAVB4      ,BENZRO2      ) =   0.39200 * RK( 260 ) * C( NO           )                                       
      PD( SVAVB4      ,TOLRO2       ) =   0.04700 * RK( 256 ) * C( NO           )                                       
      PD( SVAVB4      ,XYLRO2       ) =   0.06000 * RK( 258 ) * C( NO           )                                       
      PD( SVAVB4      ,PAHRO2       ) =   0.19100 * RK( 265 ) * C( NO           )                                       
      PD( SVAVB4      ,SOAALK       ) =   0.08100 * RK( 267 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  99: SVAVB1                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SVAVB1      ,HO2          ) =   0.14000 * RK( 257 ) * C( TOLRO2       )                                       
     &                                  + 0.19300 * RK( 259 ) * C( XYLRO2       )                                       
     &                                  + 0.14600 * RK( 261 ) * C( BENZRO2      )                                       
     &                                  + 0.47300 * RK( 266 ) * C( PAHRO2       )                                       
      PD( SVAVB1      ,BENZRO2      ) =   0.14600 * RK( 261 ) * C( HO2          )                                       
      PD( SVAVB1      ,TOLRO2       ) =   0.14000 * RK( 257 ) * C( HO2          )                                       
      PD( SVAVB1      ,XYLRO2       ) =   0.19300 * RK( 259 ) * C( HO2          )                                       
      PD( SVAVB1      ,PAHRO2       ) =   0.47300 * RK( 266 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 100: SESQ                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SESQ        ,O3           ) = -           RK( 262 ) * C( SESQ         )                                       
      PD( SESQ        ,NO3          ) = -           RK( 264 ) * C( SESQ         )                                       
      PD( SESQ        ,OH           ) = -           RK( 263 ) * C( SESQ         )                                       
      PD( SESQ        ,SESQ         ) = -           RK( 262 ) * C( O3           )                                       
     &                                  -           RK( 263 ) * C( OH           )                                       
     &                                  -           RK( 264 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 101: SESQRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SESQRXN     ,O3           ) =             RK( 262 ) * C( SESQ         )                                       
      PD( SESQRXN     ,NO3          ) =             RK( 264 ) * C( SESQ         )                                       
      PD( SESQRXN     ,OH           ) =             RK( 263 ) * C( SESQ         )                                       
      PD( SESQRXN     ,SESQ         ) =             RK( 262 ) * C( O3           )                                       
     &                                  +           RK( 263 ) * C( OH           )                                       
     &                                  +           RK( 264 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 102: SOAALK                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SOAALK      ,OH           ) = -           RK( 267 ) * C( SOAALK       )                                       
      PD( SOAALK      ,SOAALK       ) = -           RK( 267 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 103: H2NO3PIJ                                                                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( H2NO3PIJ    ,N2O5         ) =             RK( 269 )                                                           
      PD( H2NO3PIJ    ,H2NO3PIJ     ) = -           RK( 271 )                                                           
     &                                  -           RK( 273 ) * C( ACLI         )                                       
     &                                  -           RK( 274 ) * C( ACLJ         )                                       
      PD( H2NO3PIJ    ,ACLI         ) = -           RK( 273 ) * C( H2NO3PIJ     )                                       
      PD( H2NO3PIJ    ,ACLJ         ) = -           RK( 274 ) * C( H2NO3PIJ     )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 104: H2NO3PK                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( H2NO3PK     ,N2O5         ) =             RK( 270 )                                                           
      PD( H2NO3PK     ,H2NO3PK      ) = -           RK( 272 )                                                           
     &                                  -           RK( 275 ) * C( ACLK         )                                       
      PD( H2NO3PK     ,ACLK         ) = -           RK( 275 ) * C( H2NO3PK      )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 105: ACLI                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACLI        ,H2NO3PIJ     ) = -           RK( 273 ) * C( ACLI         )                                       
      PD( ACLI        ,ACLI         ) = -           RK( 273 ) * C( H2NO3PIJ     )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 106: ACLJ                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACLJ        ,H2NO3PIJ     ) = -           RK( 274 ) * C( ACLJ         )                                       
      PD( ACLJ        ,ACLJ         ) = -           RK( 274 ) * C( H2NO3PIJ     )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 107: ACLK                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACLK        ,H2NO3PK      ) = -           RK( 275 ) * C( ACLK         )                                       
      PD( ACLK        ,ACLK         ) = -           RK( 275 ) * C( H2NO3PK      )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 108: IEPOXP                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( IEPOXP      ,EPOX         ) =             RK( 278 )                                                           
      PD( IEPOXP      ,IEPOXP       ) = -           RK( 279 ) * C( ASO4J        )                                       
     &                                  -           RK( 280 )                                                           
      PD( IEPOXP      ,ASO4J        ) = -           RK( 279 ) * C( IEPOXP       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 109: ASO4J                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ASO4J       ,IEPOXP       ) = -           RK( 279 ) * C( ASO4J        )                                       
      PD( ASO4J       ,ASO4J        ) = -           RK( 279 ) * C( IEPOXP       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 110: AISO3J                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AISO3J      ,IEPOXP       ) =             RK( 279 ) * C( ASO4J        )                                       
     &                                  +           RK( 280 )                                                           
      PD( AISO3J      ,ASO4J        ) =             RK( 279 ) * C( IEPOXP       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 111: AGLYJ                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AGLYJ       ,GLY          ) =             RK( 281 )                                                           
      PD( AGLYJ       ,MGLY         ) =             RK( 282 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 112: MTNO3                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MTNO3       ,NO           ) =   0.68800 * RK( 283 ) * C( TERPNRO2     )                                       
      PD( MTNO3       ,NO3          ) =   0.42200 * RK( 285 ) * C( TERPNRO2     )                                       
      PD( MTNO3       ,OH           ) = - 0.76000 * RK( 288 ) * C( MTNO3        )                                       
      PD( MTNO3       ,HO2          ) =             RK( 284 ) * C( TERPNRO2     )                                       
      PD( MTNO3       ,RO2          ) =   0.71100 * RK( 286 ) * C( TERPNRO2     )                                       
      PD( MTNO3       ,TERPNRO2     ) =   0.68800 * RK( 283 ) * C( NO           )                                       
     &                                  +           RK( 284 ) * C( HO2          )                                       
     &                                  + 0.42200 * RK( 285 ) * C( NO3          )                                       
     &                                  + 0.71100 * RK( 286 ) * C( RO2          )                                       
      PD( MTNO3       ,CL           ) = - 0.63000 * RK( 287 ) * C( MTNO3        )                                       
      PD( MTNO3       ,MTNO3        ) = - 0.63000 * RK( 287 ) * C( CL           )                                       
     &                                  - 0.76000 * RK( 288 ) * C( OH           )                                       
     &                                  -           RK( 289 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 113: AMTNO3J                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AMTNO3J     ,AMTNO3J      ) = -           RK( 290 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 114: AMTHYDJ                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AMTHYDJ     ,AMTNO3J      ) =             RK( 290 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 115: AAVB2J                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AAVB2J      ,AAVB2J       ) = -           RK( 291 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 116: AOLGAJ                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AOLGAJ      ,AAVB2J       ) =   0.90700 * RK( 291 )                                                           
      PD( AOLGAJ      ,AAVB3J       ) =   0.92500 * RK( 292 )                                                           
      PD( AOLGAJ      ,AAVB4J       ) =   0.94300 * RK( 293 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 117: AAVB3J                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AAVB3J      ,AAVB3J       ) = -           RK( 292 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 118: AAVB4J                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AAVB4J      ,AAVB4J       ) = -           RK( 293 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 119: AISO1J                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AISO1J      ,AISO1J       ) = -           RK( 294 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 120: AOLGBJ                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AOLGBJ      ,AISO1J       ) =   0.50000 * RK( 294 )                                                           
      PD( AOLGBJ      ,AISO2J       ) =   0.50000 * RK( 295 )                                                           
      PD( AOLGBJ      ,ASQTJ        ) =   1.50000 * RK( 296 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 121: AISO2J                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AISO2J      ,AISO2J       ) = -           RK( 295 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 122: ASQTJ                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ASQTJ       ,ASQTJ        ) = -           RK( 296 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 123: APOCI                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 124: APNCOMI                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( APNCOMI     ,OH           ) =   1.25000 * RK( 297 ) * C( APOCI        )                                       
     &                                  -           RK( 298 ) * C( APNCOMI      )                                       
      PD( APNCOMI     ,APOCI        ) =   1.25000 * RK( 297 ) * C( OH           )                                       
      PD( APNCOMI     ,APNCOMI      ) = -           RK( 298 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 125: APOCJ                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 126: APNCOMJ                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( APNCOMJ     ,OH           ) =   1.25000 * RK( 299 ) * C( APOCJ        )                                       
     &                                  -           RK( 300 ) * C( APNCOMJ      )                                       
      PD( APNCOMJ     ,APOCJ        ) =   1.25000 * RK( 299 ) * C( OH           )                                       
      PD( APNCOMJ     ,APNCOMJ      ) = -           RK( 300 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 127: PCVOC                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PCVOC       ,OH           ) = -           RK( 301 ) * C( PCVOC        )                                       
      PD( PCVOC       ,PCVOC        ) = -           RK( 301 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 128: PCSOARXN                                                                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PCSOARXN    ,OH           ) =             RK( 301 ) * C( PCVOC        )                                       
      PD( PCSOARXN    ,PCVOC        ) =             RK( 301 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 129: VLVPO1                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( VLVPO1      ,OH           ) = - 0.51430 * RK( 302 ) * C( VLVPO1       )                                       
     &                                  + 0.30030 * RK( 303 ) * C( VSVPO1       )                                       
     &                                  + 0.38560 * RK( 304 ) * C( VSVPO2       )                                       
     &                                  + 0.21810 * RK( 305 ) * C( VSVPO3       )                                       
     &                                  + 0.24120 * RK( 306 ) * C( VIVPO1       )                                       
      PD( VLVPO1      ,VLVPO1       ) = - 0.51430 * RK( 302 ) * C( OH           )                                       
      PD( VLVPO1      ,VSVPO1       ) =   0.30030 * RK( 303 ) * C( OH           )                                       
      PD( VLVPO1      ,VSVPO2       ) =   0.38560 * RK( 304 ) * C( OH           )                                       
      PD( VLVPO1      ,VSVPO3       ) =   0.21810 * RK( 305 ) * C( OH           )                                       
      PD( VLVPO1      ,VIVPO1       ) =   0.24120 * RK( 306 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 130: VSVPO1                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( VSVPO1      ,OH           ) =   0.00620 * RK( 302 ) * C( VLVPO1       )                                       
     &                                  - 0.71380 * RK( 303 ) * C( VSVPO1       )                                       
     &                                  + 0.09500 * RK( 304 ) * C( VSVPO2       )                                       
     &                                  + 0.30630 * RK( 305 ) * C( VSVPO3       )                                       
     &                                  + 0.20890 * RK( 306 ) * C( VIVPO1       )                                       
      PD( VSVPO1      ,VLVPO1       ) =   0.00620 * RK( 302 ) * C( OH           )                                       
      PD( VSVPO1      ,VSVPO1       ) = - 0.71380 * RK( 303 ) * C( OH           )                                       
      PD( VSVPO1      ,VSVPO2       ) =   0.09500 * RK( 304 ) * C( OH           )                                       
      PD( VSVPO1      ,VSVPO3       ) =   0.30630 * RK( 305 ) * C( OH           )                                       
      PD( VSVPO1      ,VIVPO1       ) =   0.20890 * RK( 306 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 131: VSVPO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( VSVPO2      ,OH           ) =   0.00250 * RK( 302 ) * C( VLVPO1       )                                       
     &                                  + 0.00410 * RK( 303 ) * C( VSVPO1       )                                       
     &                                  - 0.86270 * RK( 304 ) * C( VSVPO2       )                                       
     &                                  + 0.01530 * RK( 305 ) * C( VSVPO3       )                                       
     &                                  + 0.30000 * RK( 306 ) * C( VIVPO1       )                                       
      PD( VSVPO2      ,VLVPO1       ) =   0.00250 * RK( 302 ) * C( OH           )                                       
      PD( VSVPO2      ,VSVPO1       ) =   0.00410 * RK( 303 ) * C( OH           )                                       
      PD( VSVPO2      ,VSVPO2       ) = - 0.86270 * RK( 304 ) * C( OH           )                                       
      PD( VSVPO2      ,VSVPO3       ) =   0.01530 * RK( 305 ) * C( OH           )                                       
      PD( VSVPO2      ,VIVPO1       ) =   0.30000 * RK( 306 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 132: VSVPO3                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( VSVPO3      ,OH           ) =   0.00260 * RK( 302 ) * C( VLVPO1       )                                       
     &                                  + 0.00350 * RK( 303 ) * C( VSVPO1       )                                       
     &                                  + 0.00050 * RK( 304 ) * C( VSVPO2       )                                       
     &                                  - 0.89570 * RK( 305 ) * C( VSVPO3       )                                       
      PD( VSVPO3      ,VLVPO1       ) =   0.00260 * RK( 302 ) * C( OH           )                                       
      PD( VSVPO3      ,VSVPO1       ) =   0.00350 * RK( 303 ) * C( OH           )                                       
      PD( VSVPO3      ,VSVPO2       ) =   0.00050 * RK( 304 ) * C( OH           )                                       
      PD( VSVPO3      ,VSVPO3       ) = - 0.89570 * RK( 305 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 133: VIVPO1                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( VIVPO1      ,OH           ) =   0.00230 * RK( 302 ) * C( VLVPO1       )                                       
     &                                  -           RK( 306 ) * C( VIVPO1       )                                       
      PD( VIVPO1      ,VLVPO1       ) =   0.00230 * RK( 302 ) * C( OH           )                                       
      PD( VIVPO1      ,VIVPO1       ) = -           RK( 306 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 134: VLVOO1                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( VLVOO1      ,OH           ) =   0.29440 * RK( 302 ) * C( VLVPO1       )                                       
     &                                  + 0.22390 * RK( 303 ) * C( VSVPO1       )                                       
     &                                  + 0.20510 * RK( 304 ) * C( VSVPO2       )                                       
     &                                  + 0.18930 * RK( 305 ) * C( VSVPO3       )                                       
     &                                  + 0.20280 * RK( 306 ) * C( VIVPO1       )                                       
     &                                  - 0.33360 * RK( 307 ) * C( VLVOO1       )                                       
     &                                  + 0.28580 * RK( 308 ) * C( VLVOO2       )                                       
     &                                  + 0.33030 * RK( 309 ) * C( VSVOO1       )                                       
     &                                  + 0.34440 * RK( 310 ) * C( VSVOO2       )                                       
     &                                  + 0.38860 * RK( 311 ) * C( VSVOO3       )                                       
      PD( VLVOO1      ,VLVPO1       ) =   0.29440 * RK( 302 ) * C( OH           )                                       
      PD( VLVOO1      ,VSVPO1       ) =   0.22390 * RK( 303 ) * C( OH           )                                       
      PD( VLVOO1      ,VSVPO2       ) =   0.20510 * RK( 304 ) * C( OH           )                                       
      PD( VLVOO1      ,VSVPO3       ) =   0.18930 * RK( 305 ) * C( OH           )                                       
      PD( VLVOO1      ,VIVPO1       ) =   0.20280 * RK( 306 ) * C( OH           )                                       
      PD( VLVOO1      ,VLVOO1       ) = - 0.33360 * RK( 307 ) * C( OH           )                                       
      PD( VLVOO1      ,VLVOO2       ) =   0.28580 * RK( 308 ) * C( OH           )                                       
      PD( VLVOO1      ,VSVOO2       ) =   0.34440 * RK( 310 ) * C( OH           )                                       
      PD( VLVOO1      ,VSVOO3       ) =   0.38860 * RK( 311 ) * C( OH           )                                       
      PD( VLVOO1      ,VSVOO1       ) =   0.33030 * RK( 309 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 135: VLVOO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( VLVOO2      ,OH           ) =   0.20210 * RK( 302 ) * C( VLVPO1       )                                       
     &                                  + 0.18200 * RK( 303 ) * C( VSVPO1       )                                       
     &                                  + 0.17640 * RK( 304 ) * C( VSVPO2       )                                       
     &                                  + 0.16680 * RK( 305 ) * C( VSVPO3       )                                       
     &                                  + 0.04710 * RK( 306 ) * C( VIVPO1       )                                       
     &                                  + 0.01430 * RK( 307 ) * C( VLVOO1       )                                       
     &                                  - 0.60690 * RK( 308 ) * C( VLVOO2       )                                       
     &                                  + 0.22720 * RK( 309 ) * C( VSVOO1       )                                       
     &                                  + 0.27490 * RK( 310 ) * C( VSVOO2       )                                       
     &                                  + 0.24210 * RK( 311 ) * C( VSVOO3       )                                       
      PD( VLVOO2      ,VLVPO1       ) =   0.20210 * RK( 302 ) * C( OH           )                                       
      PD( VLVOO2      ,VSVPO1       ) =   0.18200 * RK( 303 ) * C( OH           )                                       
      PD( VLVOO2      ,VSVPO2       ) =   0.17640 * RK( 304 ) * C( OH           )                                       
      PD( VLVOO2      ,VSVPO3       ) =   0.16680 * RK( 305 ) * C( OH           )                                       
      PD( VLVOO2      ,VIVPO1       ) =   0.04710 * RK( 306 ) * C( OH           )                                       
      PD( VLVOO2      ,VLVOO1       ) =   0.01430 * RK( 307 ) * C( OH           )                                       
      PD( VLVOO2      ,VLVOO2       ) = - 0.60690 * RK( 308 ) * C( OH           )                                       
      PD( VLVOO2      ,VSVOO2       ) =   0.27490 * RK( 310 ) * C( OH           )                                       
      PD( VLVOO2      ,VSVOO3       ) =   0.24210 * RK( 311 ) * C( OH           )                                       
      PD( VLVOO2      ,VSVOO1       ) =   0.22720 * RK( 309 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 136: VSVOO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( VSVOO2      ,OH           ) =   0.00190 * RK( 302 ) * C( VLVPO1       )                                       
     &                                  + 0.12390 * RK( 307 ) * C( VLVOO1       )                                       
     &                                  + 0.10270 * RK( 308 ) * C( VLVOO2       )                                       
     &                                  + 0.07020 * RK( 309 ) * C( VSVOO1       )                                       
     &                                  - 0.74230 * RK( 310 ) * C( VSVOO2       )                                       
     &                                  + 0.03850 * RK( 311 ) * C( VSVOO3       )                                       
      PD( VSVOO2      ,VLVPO1       ) =   0.00190 * RK( 302 ) * C( OH           )                                       
      PD( VSVOO2      ,VLVOO1       ) =   0.12390 * RK( 307 ) * C( OH           )                                       
      PD( VSVOO2      ,VLVOO2       ) =   0.10270 * RK( 308 ) * C( OH           )                                       
      PD( VSVOO2      ,VSVOO2       ) = - 0.74230 * RK( 310 ) * C( OH           )                                       
      PD( VSVOO2      ,VSVOO3       ) =   0.03850 * RK( 311 ) * C( OH           )                                       
      PD( VSVOO2      ,VSVOO1       ) =   0.07020 * RK( 309 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 137: VSVOO3                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( VSVOO3      ,OH           ) =   0.00230 * RK( 302 ) * C( VLVPO1       )                                       
     &                                  + 0.18310 * RK( 307 ) * C( VLVOO1       )                                       
     &                                  + 0.20450 * RK( 308 ) * C( VLVOO2       )                                       
     &                                  + 0.11160 * RK( 309 ) * C( VSVOO1       )                                       
     &                                  + 0.07390 * RK( 310 ) * C( VSVOO2       )                                       
     &                                  - 0.73330 * RK( 311 ) * C( VSVOO3       )                                       
      PD( VSVOO3      ,VLVPO1       ) =   0.00230 * RK( 302 ) * C( OH           )                                       
      PD( VSVOO3      ,VLVOO1       ) =   0.18310 * RK( 307 ) * C( OH           )                                       
      PD( VSVOO3      ,VLVOO2       ) =   0.20450 * RK( 308 ) * C( OH           )                                       
      PD( VSVOO3      ,VSVOO2       ) =   0.07390 * RK( 310 ) * C( OH           )                                       
      PD( VSVOO3      ,VSVOO3       ) = - 0.73330 * RK( 311 ) * C( OH           )                                       
      PD( VSVOO3      ,VSVOO1       ) =   0.11160 * RK( 309 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 138: VSVOO1                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( VSVOO1      ,OH           ) =   0.01230 * RK( 307 ) * C( VLVOO1       )                                       
     &                                  + 0.01390 * RK( 308 ) * C( VLVOO2       )                                       
     &                                  - 0.73930 * RK( 309 ) * C( VSVOO1       )                                       
     &                                  + 0.04910 * RK( 310 ) * C( VSVOO2       )                                       
     &                                  + 0.06400 * RK( 311 ) * C( VSVOO3       )                                       
      PD( VSVOO1      ,VLVOO1       ) =   0.01230 * RK( 307 ) * C( OH           )                                       
      PD( VSVOO1      ,VLVOO2       ) =   0.01390 * RK( 308 ) * C( OH           )                                       
      PD( VSVOO1      ,VSVOO2       ) =   0.04910 * RK( 310 ) * C( OH           )                                       
      PD( VSVOO1      ,VSVOO3       ) =   0.06400 * RK( 311 ) * C( OH           )                                       
      PD( VSVOO1      ,VSVOO1       ) = - 0.73930 * RK( 309 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 139: FORM_PRIMARY                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FORM_PRIMARY,O            ) = -           RK( 314 ) * C( FORM_PRIMARY )                                       
      PD( FORM_PRIMARY,NO3          ) = -           RK( 313 ) * C( FORM_PRIMARY )                                       
      PD( FORM_PRIMARY,OH           ) = -           RK( 312 ) * C( FORM_PRIMARY )                                       
      PD( FORM_PRIMARY,CL           ) = -           RK( 317 ) * C( FORM_PRIMARY )                                       
      PD( FORM_PRIMARY,FORM_PRIMARY ) = -           RK( 312 ) * C( OH           )                                       
     &                                  -           RK( 313 ) * C( NO3          )                                       
     &                                  -           RK( 314 ) * C( O            )                                       
     &                                  -           RK( 315 )                                                           
     &                                  -           RK( 316 )                                                           
     &                                  -           RK( 317 ) * C( CL           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 140: ALD2_PRIMARY                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALD2_PRIMARY,O            ) = -           RK( 320 ) * C( ALD2_PRIMARY )                                       
      PD( ALD2_PRIMARY,NO3          ) = -           RK( 319 ) * C( ALD2_PRIMARY )                                       
      PD( ALD2_PRIMARY,OH           ) = -           RK( 318 ) * C( ALD2_PRIMARY )                                       
      PD( ALD2_PRIMARY,CL           ) = -           RK( 322 ) * C( ALD2_PRIMARY )                                       
      PD( ALD2_PRIMARY,ALD2_PRIMARY ) = -           RK( 318 ) * C( OH           )                                       
     &                                  -           RK( 319 ) * C( NO3          )                                       
     &                                  -           RK( 320 ) * C( O            )                                       
     &                                  -           RK( 321 )                                                           
     &                                  -           RK( 322 ) * C( CL           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 141: BUTADIENE13                                                                
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BUTADIENE13 ,O3           ) = -           RK( 324 ) * C( BUTADIENE13  )                                       
      PD( BUTADIENE13 ,NO3          ) = -           RK( 325 ) * C( BUTADIENE13  )                                       
      PD( BUTADIENE13 ,OH           ) = -           RK( 323 ) * C( BUTADIENE13  )                                       
      PD( BUTADIENE13 ,CL           ) = -           RK( 326 ) * C( BUTADIENE13  )                                       
      PD( BUTADIENE13 ,BUTADIENE13  ) = -           RK( 323 ) * C( OH           )                                       
     &                                  -           RK( 324 ) * C( O3           )                                       
     &                                  -           RK( 325 ) * C( NO3          )                                       
     &                                  -           RK( 326 ) * C( CL           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 142: ACROLEIN                                                                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACROLEIN    ,O3           ) =   0.52000 * RK( 324 ) * C( BUTADIENE13  )                                       
     &                                  -           RK( 333 ) * C( ACROLEIN     )                                       
      PD( ACROLEIN    ,NO3          ) =   0.04500 * RK( 325 ) * C( BUTADIENE13  )                                       
     &                                  -           RK( 334 ) * C( ACROLEIN     )                                       
      PD( ACROLEIN    ,OH           ) =   0.58000 * RK( 323 ) * C( BUTADIENE13  )                                       
     &                                  -           RK( 332 ) * C( ACROLEIN     )                                       
      PD( ACROLEIN    ,CL           ) =   0.58000 * RK( 326 ) * C( BUTADIENE13  )                                       
     &                                  -           RK( 336 ) * C( ACROLEIN     )                                       
      PD( ACROLEIN    ,BUTADIENE13  ) =   0.58000 * RK( 323 ) * C( OH           )                                       
     &                                  + 0.52000 * RK( 324 ) * C( O3           )                                       
     &                                  + 0.04500 * RK( 325 ) * C( NO3          )                                       
     &                                  + 0.58000 * RK( 326 ) * C( CL           )                                       
      PD( ACROLEIN    ,ACROLEIN     ) = -           RK( 332 ) * C( OH           )                                       
     &                                  -           RK( 333 ) * C( O3           )                                       
     &                                  -           RK( 334 ) * C( NO3          )                                       
     &                                  -           RK( 335 )                                                           
     &                                  -           RK( 336 ) * C( CL           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 143: ACRO_PRIMARY                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACRO_PRIMARY,O3           ) = -           RK( 328 ) * C( ACRO_PRIMARY )                                       
      PD( ACRO_PRIMARY,NO3          ) = -           RK( 329 ) * C( ACRO_PRIMARY )                                       
      PD( ACRO_PRIMARY,OH           ) = -           RK( 327 ) * C( ACRO_PRIMARY )                                       
      PD( ACRO_PRIMARY,CL           ) = -           RK( 331 ) * C( ACRO_PRIMARY )                                       
      PD( ACRO_PRIMARY,ACRO_PRIMARY ) = -           RK( 327 ) * C( OH           )                                       
     &                                  -           RK( 328 ) * C( O3           )                                       
     &                                  -           RK( 329 ) * C( NO3          )                                       
     &                                  -           RK( 330 )                                                           
     &                                  -           RK( 331 ) * C( CL           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 144: TOLU                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLU        ,OH           ) = -           RK( 337 ) * C( TOLU         )                                       
      PD( TOLU        ,CL           ) = -           RK( 338 ) * C( TOLU         )                                       
      PD( TOLU        ,TOLU         ) = -           RK( 337 ) * C( OH           )                                       
     &                                  -           RK( 338 ) * C( CL           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 145: HG                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HG          ,O3           ) = -           RK( 339 ) * C( HG           )                                       
      PD( HG          ,OH           ) = -           RK( 342 ) * C( HG           )                                       
      PD( HG          ,H2O2         ) = -           RK( 341 ) * C( HG           )                                       
      PD( HG          ,CL2          ) = -           RK( 340 ) * C( HG           )                                       
      PD( HG          ,CL           ) = - 0.50000 * RK( 343 ) * C( HG           )                                       
      PD( HG          ,HG           ) = -           RK( 339 ) * C( O3           )                                       
     &                                  -           RK( 340 ) * C( CL2          )                                       
     &                                  -           RK( 341 ) * C( H2O2         )                                       
     &                                  -           RK( 342 ) * C( OH           )                                       
     &                                  - 0.50000 * RK( 343 ) * C( CL           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 146: HGIIAER                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HGIIAER     ,O3           ) =   0.50000 * RK( 339 ) * C( HG           )                                       
      PD( HGIIAER     ,OH           ) =   0.50000 * RK( 342 ) * C( HG           )                                       
      PD( HGIIAER     ,HG           ) =   0.50000 * RK( 339 ) * C( O3           )                                       
     &                                  + 0.50000 * RK( 342 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 147: HGIIGAS                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HGIIGAS     ,O3           ) =   0.50000 * RK( 339 ) * C( HG           )                                       
      PD( HGIIGAS     ,OH           ) =   0.50000 * RK( 342 ) * C( HG           )                                       
      PD( HGIIGAS     ,H2O2         ) =             RK( 341 ) * C( HG           )                                       
      PD( HGIIGAS     ,CL2          ) =             RK( 340 ) * C( HG           )                                       
      PD( HGIIGAS     ,CL           ) =   0.50000 * RK( 343 ) * C( HG           )                                       
      PD( HGIIGAS     ,HG           ) =   0.50000 * RK( 339 ) * C( O3           )                                       
     &                                  +           RK( 340 ) * C( CL2          )                                       
     &                                  +           RK( 341 ) * C( H2O2         )                                       
     &                                  + 0.50000 * RK( 342 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 343 ) * C( CL           )                                       


      RETURN                                                                                                            
      END                                                                                                               
