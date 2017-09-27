      SUBROUTINE JAC( C, PD, RK )                                                                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C                                                                                                                       
C Automatically generated Jacobian for DDM-3D                                                                           
C                                                                                                                       
C =-=-= MECHANISM: CB05E51_AE6_AQ                   =-=-=                                                               
C                                                                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Use RXNS_DATA, Only: NRXNS, MECHNAME
      Use CGRID_SPCS, Only: N_GC_SPC
      Use UTILIO_DEFN                   ! IOAPI parameters and functions declarations

      IMPLICIT NONE                                                                                                     

      INTEGER, PARAMETER :: NJACSPC = 122                                                                               
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
      INTEGER, PARAMETER :: N2O5             =   9                                                                      
      INTEGER, PARAMETER :: HNO3             =  10                                                                      
      INTEGER, PARAMETER :: HONO             =  11                                                                      
      INTEGER, PARAMETER :: PNA              =  12                                                                      
      INTEGER, PARAMETER :: H2O2             =  13                                                                      
      INTEGER, PARAMETER :: XO2              =  14                                                                      
      INTEGER, PARAMETER :: XO2N             =  15                                                                      
      INTEGER, PARAMETER :: NTROH            =  16                                                                      
      INTEGER, PARAMETER :: NTRALK           =  17                                                                      
      INTEGER, PARAMETER :: ROOH             =  18                                                                      
      INTEGER, PARAMETER :: ALD2             =  19                                                                      
      INTEGER, PARAMETER :: ALDX             =  20                                                                      
      INTEGER, PARAMETER :: ISOPX            =  21                                                                      
      INTEGER, PARAMETER :: IEPOX            =  22                                                                      
      INTEGER, PARAMETER :: ISOPO2           =  23                                                                      
      INTEGER, PARAMETER :: IOLE             =  24                                                                      
      INTEGER, PARAMETER :: IEPXO2           =  25                                                                      
      INTEGER, PARAMETER :: MGLY             =  26                                                                      
      INTEGER, PARAMETER :: FORM             =  27                                                                      
      INTEGER, PARAMETER :: FACD             =  28                                                                      
      INTEGER, PARAMETER :: CO               =  29                                                                      
      INTEGER, PARAMETER :: PAR              =  30                                                                      
      INTEGER, PARAMETER :: C2O3             =  31                                                                      
      INTEGER, PARAMETER :: MEO2             =  32                                                                      
      INTEGER, PARAMETER :: AACD             =  33                                                                      
      INTEGER, PARAMETER :: MEPX             =  34                                                                      
      INTEGER, PARAMETER :: MEOH             =  35                                                                      
      INTEGER, PARAMETER :: HCO3             =  36                                                                      
      INTEGER, PARAMETER :: PAN              =  37                                                                      
      INTEGER, PARAMETER :: PACD             =  38                                                                      
      INTEGER, PARAMETER :: CXO3             =  39                                                                      
      INTEGER, PARAMETER :: PANX             =  40                                                                      
      INTEGER, PARAMETER :: ROR              =  41                                                                      
      INTEGER, PARAMETER :: OLE              =  42                                                                      
      INTEGER, PARAMETER :: NALKO2           =  43                                                                      
      INTEGER, PARAMETER :: ETH              =  44                                                                      
      INTEGER, PARAMETER :: TOL              =  45                                                                      
      INTEGER, PARAMETER :: CRES             =  46                                                                      
      INTEGER, PARAMETER :: TO2              =  47                                                                      
      INTEGER, PARAMETER :: TOLRO2           =  48                                                                      
      INTEGER, PARAMETER :: OPEN             =  49                                                                      
      INTEGER, PARAMETER :: CRO              =  50                                                                      
      INTEGER, PARAMETER :: CAT1             =  51                                                                      
      INTEGER, PARAMETER :: CRON             =  52                                                                      
      INTEGER, PARAMETER :: CRNO             =  53                                                                      
      INTEGER, PARAMETER :: CRN2             =  54                                                                      
      INTEGER, PARAMETER :: CRPX             =  55                                                                      
      INTEGER, PARAMETER :: OPO3             =  56                                                                      
      INTEGER, PARAMETER :: CAO2             =  57                                                                      
      INTEGER, PARAMETER :: OPAN             =  58                                                                      
      INTEGER, PARAMETER :: XYLMN            =  59                                                                      
      INTEGER, PARAMETER :: XYLRO2           =  60                                                                      
      INTEGER, PARAMETER :: NAPH             =  61                                                                      
      INTEGER, PARAMETER :: PAHRO2           =  62                                                                      
      INTEGER, PARAMETER :: ISOP             =  63                                                                      
      INTEGER, PARAMETER :: ISPD             =  64                                                                      
      INTEGER, PARAMETER :: ISOPRXN          =  65                                                                      
      INTEGER, PARAMETER :: NTRM             =  66                                                                      
      INTEGER, PARAMETER :: MACO3            =  67                                                                      
      INTEGER, PARAMETER :: NTRI             =  68                                                                      
      INTEGER, PARAMETER :: TERP             =  69                                                                      
      INTEGER, PARAMETER :: TRPRXN           =  70                                                                      
      INTEGER, PARAMETER :: XO2T             =  71                                                                      
      INTEGER, PARAMETER :: SO2              =  72                                                                      
      INTEGER, PARAMETER :: SULF             =  73                                                                      
      INTEGER, PARAMETER :: SULRXN           =  74                                                                      
      INTEGER, PARAMETER :: ETOH             =  75                                                                      
      INTEGER, PARAMETER :: ETHA             =  76                                                                      
      INTEGER, PARAMETER :: CL2              =  77                                                                      
      INTEGER, PARAMETER :: CL               =  78                                                                      
      INTEGER, PARAMETER :: HOCL             =  79                                                                      
      INTEGER, PARAMETER :: CLO              =  80                                                                      
      INTEGER, PARAMETER :: FMCL             =  81                                                                      
      INTEGER, PARAMETER :: HCL              =  82                                                                      
      INTEGER, PARAMETER :: CLNO2            =  83                                                                      
      INTEGER, PARAMETER :: TOLNRXN          =  84                                                                      
      INTEGER, PARAMETER :: TOLHRXN          =  85                                                                      
      INTEGER, PARAMETER :: XYLNRXN          =  86                                                                      
      INTEGER, PARAMETER :: XYLHRXN          =  87                                                                      
      INTEGER, PARAMETER :: BENZENE          =  88                                                                      
      INTEGER, PARAMETER :: BENZRO2          =  89                                                                      
      INTEGER, PARAMETER :: BNZNRXN          =  90                                                                      
      INTEGER, PARAMETER :: BNZHRXN          =  91                                                                      
      INTEGER, PARAMETER :: SESQ             =  92                                                                      
      INTEGER, PARAMETER :: SESQRXN          =  93                                                                      
      INTEGER, PARAMETER :: PAHNRXN          =  94                                                                      
      INTEGER, PARAMETER :: PAHHRXN          =  95                                                                      
      INTEGER, PARAMETER :: SOAALK           =  96                                                                      
      INTEGER, PARAMETER :: ALKRXN           =  97                                                                      
      INTEGER, PARAMETER :: MAPAN            =  98                                                                      
      INTEGER, PARAMETER :: NTRCN            =  99                                                                      
      INTEGER, PARAMETER :: NTRCNOH          = 100                                                                      
      INTEGER, PARAMETER :: NTRPX            = 101                                                                      
      INTEGER, PARAMETER :: NOHO2            = 102                                                                      
      INTEGER, PARAMETER :: NCNO2            = 103                                                                      
      INTEGER, PARAMETER :: NCNOHO2          = 104                                                                      
      INTEGER, PARAMETER :: NTRMO2           = 105                                                                      
      INTEGER, PARAMETER :: NTRIO2           = 106                                                                      
      INTEGER, PARAMETER :: H2NO3PIJ         = 107                                                                      
      INTEGER, PARAMETER :: H2NO3PK          = 108                                                                      
      INTEGER, PARAMETER :: FORM_PRIMARY     = 109                                                                      
      INTEGER, PARAMETER :: ALD2_PRIMARY     = 110                                                                      
      INTEGER, PARAMETER :: BUTADIENE13      = 111                                                                      
      INTEGER, PARAMETER :: ACROLEIN         = 112                                                                      
      INTEGER, PARAMETER :: ACRO_PRIMARY     = 113                                                                      
      INTEGER, PARAMETER :: TOLU             = 114                                                                      
      INTEGER, PARAMETER :: MXYL             = 115                                                                      
      INTEGER, PARAMETER :: OXYL             = 116                                                                      
      INTEGER, PARAMETER :: PXYL             = 117                                                                      
      INTEGER, PARAMETER :: APIN             = 118                                                                      
      INTEGER, PARAMETER :: BPIN             = 119                                                                      
      INTEGER, PARAMETER :: HG               = 120                                                                      
      INTEGER, PARAMETER :: HGIIAER          = 121                                                                      
      INTEGER, PARAMETER :: HGIIGAS          = 122                                                                      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF( FIRSTIME ) THEN                                                                                               

         IF( MECHNAME .NE. 'CB05E51_AE6_AQ                  ' ) THEN                                                    
            MSG =  'This Jacobian was built for CB05E51_AE6_AQ                   '                                      
     &            // MECHNAME // ' is in use. Exiting.'                                                                 
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )                                                                     
         ENDIF                                                                                                          

         IF ( N_GC_SPC .NE. NJACSPC ) THEN                                                                              
            MSG =  'ERROR: Number of species in Jacobian '                                                              
     &          // 'does not match N_GC_SPC.'                                                                           
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )                                                                     
         ELSE IF ( NRXNS .NE. NJACRXN ) THEN                                                                            
            MSG =  'ERROR: Number of reactions in Jacobian '                                                            
     &          // 'does not match NRXNS.'                                                                              
            CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )                                                                     
         ENDIF                                                                                                          

         FIRSTIME = .FALSE.                                                                                             

      ENDIF                                                                                                             

      PD = 0.0                                                                                                          

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   1: NO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO2         ,NO2          ) = -           RK( 001 )                                                           
     &                                  -           RK( 004 ) * C( O            )                                       
     &                                  -           RK( 005 ) * C( O            )                                       
     &                                  -           RK( 007 ) * C( O3           )                                       
     &                                  -           RK( 018 ) * C( NO3          )                                       
     &                                  -           RK( 023 ) * C( NO           )                                       
     &                                  -           RK( 028 ) * C( OH           )                                       
     &                                  -           RK( 032 ) * C( HO2          )                                       
     &                                  -           RK( 092 ) * C( C2O3         )                                       
     &                                  -           RK( 107 ) * C( CXO3         )                                       
     &                                  -           RK( 119 ) * C( ROR          )                                       
     &                                  -           RK( 137 ) * C( OPEN         )                                       
     &                                  -           RK( 141 ) * C( CRON         )                                       
     &                                  -           RK( 156 ) * C( CRPX         )                                       
     &                                  -           RK( 184 ) * C( PAHRO2       )                                       
     &                                  -           RK( 209 ) * C( CL2          )                                       
     &                                  -           RK( 225 ) * C( NTRM         )                                       
     &                                  -           RK( 274 )                                                           
      PD( NO2         ,NO           ) =             RK( 003 ) * C( O3           )                                       
     &                                  +           RK( 006 ) * C( O            )                                       
     &                                  + 2.00000 * RK( 016 ) * C( NO3          )                                       
     &                                  + 4.00000 * RK( 022 ) * C( NO           )                                       
     &                                  -           RK( 023 ) * C( NO2          )                                       
     &                                  +           RK( 030 ) * C( HO2          )                                       
     &                                  +           RK( 055 ) * C( XO2          )                                       
     &                                  +           RK( 067 ) * C( IEPXO2       )                                       
     &                                  +           RK( 071 ) * C( MEO2         )                                       
     &                                  +           RK( 084 ) * C( HCO3         )                                       
     &                                  +           RK( 091 ) * C( C2O3         )                                       
     &                                  +           RK( 106 ) * C( CXO3         )                                       
     &                                  + 0.86000 * RK( 133 ) * C( CRES         )                                       
     &                                  +           RK( 143 ) * C( CRNO         )                                       
     &                                  + 0.86000 * RK( 153 ) * C( OPO3         )                                       
     &                                  +           RK( 155 ) * C( CRPX         )                                       
     &                                  + 0.90000 * RK( 164 ) * C( ISOPO2       )                                       
     &                                  +           RK( 189 ) * C( HOCL         )                                       
     &                                  +           RK( 224 ) * C( NTRM         )                                       
     &                                  + 1.30000 * RK( 235 ) * C( MAPAN        )                                       
     &                                  + 1.22000 * RK( 238 ) * C( NOHO2        )                                       
     &                                  + 1.53000 * RK( 241 ) * C( NCNO2        )                                       
     &                                  + 1.24000 * RK( 244 ) * C( NCNOHO2      )                                       
     &                                  + 0.87000 * RK( 248 ) * C( NTRMO2       )                                       
     &                                  + 1.40000 * RK( 251 ) * C( NTRIO2       )                                       
      PD( NO2         ,O            ) = -           RK( 004 ) * C( NO2          )                                       
     &                                  -           RK( 005 ) * C( NO2          )                                       
     &                                  +           RK( 006 ) * C( NO           )                                       
     &                                  +           RK( 047 ) * C( NO3          )                                       
      PD( NO2         ,O3           ) =             RK( 003 ) * C( NO           )                                       
     &                                  -           RK( 007 ) * C( NO2          )                                       
     &                                  +           RK( 050 ) * C( NO3          )                                       
      PD( NO2         ,NO3          ) =             RK( 014 )                                                           
     &                                  + 2.00000 * RK( 016 ) * C( NO           )                                       
     &                                  -           RK( 018 ) * C( NO2          )                                       
     &                                  +           RK( 047 ) * C( O            )                                       
     &                                  +           RK( 048 ) * C( OH           )                                       
     &                                  +           RK( 050 ) * C( O3           )                                       
     &                                  + 4.00000 * RK( 051 ) * C( NO3          )                                       
     &                                  +           RK( 123 ) * C( OLE          )                                       
     &                                  +           RK( 127 ) * C( NALKO2       )                                       
     &                                  +           RK( 131 ) * C( IOLE         )                                       
     &                                  + 0.20000 * RK( 168 ) * C( PAHRO2       )                                       
     &                                  + 0.47000 * RK( 180 ) * C( NTRI         )                                       
      PD( NO2         ,OH           ) =             RK( 026 ) * C( HONO         )                                       
     &                                  -           RK( 028 ) * C( NO2          )                                       
     &                                  +           RK( 034 ) * C( PNA          )                                       
     &                                  +           RK( 048 ) * C( NO3          )                                       
     &                                  +           RK( 110 ) * C( PANX         )                                       
     &                                  +           RK( 228 ) * C( ALKRXN       )                                       
      PD( NO2         ,HO2          ) =             RK( 030 ) * C( NO           )                                       
     &                                  -           RK( 032 ) * C( NO2          )                                       
      PD( NO2         ,N2O5         ) =             RK( 021 )                                                           
     &                                  +           RK( 054 )                                                           
      PD( NO2         ,HNO3         ) =             RK( 053 )                                                           
      PD( NO2         ,HONO         ) =             RK( 026 ) * C( OH           )                                       
     &                                  + 4.00000 * RK( 027 ) * C( HONO         )                                       
      PD( NO2         ,PNA          ) =             RK( 033 )                                                           
     &                                  +           RK( 034 ) * C( OH           )                                       
     &                                  + 0.61000 * RK( 052 )                                                           
      PD( NO2         ,XO2          ) =             RK( 055 ) * C( NO           )                                       
      PD( NO2         ,NTROH        ) =             RK( 256 )                                                           
      PD( NO2         ,NTRALK       ) =             RK( 255 )                                                           
      PD( NO2         ,ISOPO2       ) =   0.90000 * RK( 164 ) * C( NO           )                                       
      PD( NO2         ,IOLE         ) =             RK( 131 ) * C( NO3          )                                       
      PD( NO2         ,IEPXO2       ) =             RK( 067 ) * C( NO           )                                       
      PD( NO2         ,C2O3         ) =             RK( 091 ) * C( NO           )                                       
     &                                  -           RK( 092 ) * C( NO2          )                                       
      PD( NO2         ,MEO2         ) =             RK( 071 ) * C( NO           )                                       
      PD( NO2         ,HCO3         ) =             RK( 084 ) * C( NO           )                                       
      PD( NO2         ,PAN          ) =             RK( 093 )                                                           
     &                                  + 0.60000 * RK( 094 )                                                           
      PD( NO2         ,CXO3         ) =             RK( 106 ) * C( NO           )                                       
     &                                  -           RK( 107 ) * C( NO2          )                                       
      PD( NO2         ,PANX         ) =             RK( 108 )                                                           
     &                                  + 0.60000 * RK( 109 )                                                           
     &                                  +           RK( 110 ) * C( OH           )                                       
      PD( NO2         ,ROR          ) = -           RK( 119 ) * C( NO2          )                                       
      PD( NO2         ,OLE          ) =             RK( 123 ) * C( NO3          )                                       
      PD( NO2         ,NALKO2       ) =             RK( 127 ) * C( NO3          )                                       
      PD( NO2         ,CRES         ) =   0.86000 * RK( 133 ) * C( NO           )                                       
      PD( NO2         ,OPEN         ) = -           RK( 137 ) * C( NO2          )                                       
      PD( NO2         ,CRON         ) = -           RK( 141 ) * C( NO2          )                                       
      PD( NO2         ,CRNO         ) =             RK( 143 ) * C( NO           )                                       
      PD( NO2         ,CRPX         ) =             RK( 155 ) * C( NO           )                                       
     &                                  -           RK( 156 ) * C( NO2          )                                       
      PD( NO2         ,OPO3         ) =   0.86000 * RK( 153 ) * C( NO           )                                       
      PD( NO2         ,CAO2         ) =             RK( 157 )                                                           
      PD( NO2         ,PAHRO2       ) =   0.20000 * RK( 168 ) * C( NO3          )                                       
     &                                  -           RK( 184 ) * C( NO2          )                                       
      PD( NO2         ,ISOPRXN      ) =             RK( 253 )                                                           
      PD( NO2         ,NTRM         ) =             RK( 224 ) * C( NO           )                                       
     &                                  -           RK( 225 ) * C( NO2          )                                       
      PD( NO2         ,MACO3        ) =             RK( 254 )                                                           
      PD( NO2         ,NTRI         ) =   0.47000 * RK( 180 ) * C( NO3          )                                       
      PD( NO2         ,CL2          ) = -           RK( 209 ) * C( NO2          )                                       
      PD( NO2         ,HOCL         ) =             RK( 189 ) * C( NO           )                                       
      PD( NO2         ,HCL          ) =             RK( 210 )                                                           
      PD( NO2         ,ALKRXN       ) =             RK( 226 )                                                           
     &                                  + 0.60000 * RK( 227 )                                                           
     &                                  +           RK( 228 ) * C( OH           )                                       
      PD( NO2         ,MAPAN        ) =   1.30000 * RK( 235 ) * C( NO           )                                       
      PD( NO2         ,NTRCN        ) =             RK( 257 )                                                           
      PD( NO2         ,NTRCNOH      ) =             RK( 258 )                                                           
      PD( NO2         ,NTRPX        ) =             RK( 259 )                                                           
      PD( NO2         ,NOHO2        ) =   1.22000 * RK( 238 ) * C( NO           )                                       
      PD( NO2         ,NCNO2        ) =   1.53000 * RK( 241 ) * C( NO           )                                       
      PD( NO2         ,NCNOHO2      ) =   1.24000 * RK( 244 ) * C( NO           )                                       
      PD( NO2         ,NTRMO2       ) =   0.87000 * RK( 248 ) * C( NO           )                                       
      PD( NO2         ,NTRIO2       ) =   1.40000 * RK( 251 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   2: NO                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO          ,NO2          ) =             RK( 001 )                                                           
     &                                  +           RK( 004 ) * C( O            )                                       
     &                                  +           RK( 017 ) * C( NO3          )                                       
     &                                  -           RK( 023 ) * C( NO           )                                       
     &                                  + 0.20000 * RK( 184 ) * C( PAHRO2       )                                       
      PD( NO          ,NO           ) = -           RK( 003 ) * C( O3           )                                       
     &                                  -           RK( 006 ) * C( O            )                                       
     &                                  -           RK( 016 ) * C( NO3          )                                       
     &                                  - 4.00000 * RK( 022 ) * C( NO           )                                       
     &                                  -           RK( 023 ) * C( NO2          )                                       
     &                                  -           RK( 024 ) * C( OH           )                                       
     &                                  -           RK( 030 ) * C( HO2          )                                       
     &                                  -           RK( 031 ) * C( HO2          )                                       
     &                                  -           RK( 055 ) * C( XO2          )                                       
     &                                  -           RK( 056 ) * C( XO2N         )                                       
     &                                  -           RK( 067 ) * C( IEPXO2       )                                       
     &                                  -           RK( 071 ) * C( MEO2         )                                       
     &                                  -           RK( 084 ) * C( HCO3         )                                       
     &                                  -           RK( 091 ) * C( C2O3         )                                       
     &                                  -           RK( 106 ) * C( CXO3         )                                       
     &                                  -           RK( 133 ) * C( CRES         )                                       
     &                                  -           RK( 143 ) * C( CRNO         )                                       
     &                                  -           RK( 153 ) * C( OPO3         )                                       
     &                                  -           RK( 155 ) * C( CRPX         )                                       
     &                                  -           RK( 164 ) * C( ISOPO2       )                                       
     &                                  -           RK( 175 ) * C( TRPRXN       )                                       
     &                                  -           RK( 189 ) * C( HOCL         )                                       
     &                                  -           RK( 224 ) * C( NTRM         )                                       
     &                                  -           RK( 235 ) * C( MAPAN        )                                       
     &                                  -           RK( 238 ) * C( NOHO2        )                                       
     &                                  -           RK( 241 ) * C( NCNO2        )                                       
     &                                  -           RK( 244 ) * C( NCNOHO2      )                                       
     &                                  -           RK( 248 ) * C( NTRMO2       )                                       
     &                                  -           RK( 251 ) * C( NTRIO2       )                                       
      PD( NO          ,O            ) =             RK( 004 ) * C( NO2          )                                       
     &                                  -           RK( 006 ) * C( NO           )                                       
      PD( NO          ,O3           ) = -           RK( 003 ) * C( NO           )                                       
      PD( NO          ,NO3          ) =             RK( 015 )                                                           
     &                                  -           RK( 016 ) * C( NO           )                                       
     &                                  +           RK( 017 ) * C( NO2          )                                       
      PD( NO          ,OH           ) = -           RK( 024 ) * C( NO           )                                       
      PD( NO          ,HO2          ) = -           RK( 030 ) * C( NO           )                                       
     &                                  -           RK( 031 ) * C( NO           )                                       
      PD( NO          ,HONO         ) =             RK( 025 )                                                           
     &                                  + 4.00000 * RK( 027 ) * C( HONO         )                                       
      PD( NO          ,XO2          ) = -           RK( 055 ) * C( NO           )                                       
      PD( NO          ,XO2N         ) = -           RK( 056 ) * C( NO           )                                       
      PD( NO          ,ISOPO2       ) = -           RK( 164 ) * C( NO           )                                       
      PD( NO          ,IEPXO2       ) = -           RK( 067 ) * C( NO           )                                       
      PD( NO          ,C2O3         ) = -           RK( 091 ) * C( NO           )                                       
      PD( NO          ,MEO2         ) = -           RK( 071 ) * C( NO           )                                       
      PD( NO          ,HCO3         ) = -           RK( 084 ) * C( NO           )                                       
      PD( NO          ,CXO3         ) = -           RK( 106 ) * C( NO           )                                       
      PD( NO          ,CRES         ) = -           RK( 133 ) * C( NO           )                                       
      PD( NO          ,CRNO         ) = -           RK( 143 ) * C( NO           )                                       
      PD( NO          ,CRPX         ) = -           RK( 155 ) * C( NO           )                                       
      PD( NO          ,OPO3         ) = -           RK( 153 ) * C( NO           )                                       
      PD( NO          ,PAHRO2       ) =   0.20000 * RK( 184 ) * C( NO2          )                                       
      PD( NO          ,NTRM         ) = -           RK( 224 ) * C( NO           )                                       
      PD( NO          ,TRPRXN       ) = -           RK( 175 ) * C( NO           )                                       
      PD( NO          ,HOCL         ) = -           RK( 189 ) * C( NO           )                                       
      PD( NO          ,MAPAN        ) = -           RK( 235 ) * C( NO           )                                       
      PD( NO          ,NOHO2        ) = -           RK( 238 ) * C( NO           )                                       
      PD( NO          ,NCNO2        ) = -           RK( 241 ) * C( NO           )                                       
      PD( NO          ,NCNOHO2      ) = -           RK( 244 ) * C( NO           )                                       
      PD( NO          ,NTRMO2       ) = -           RK( 248 ) * C( NO           )                                       
      PD( NO          ,NTRIO2       ) = -           RK( 251 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   3: O                                                                          
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O           ,NO2          ) =             RK( 001 )                                                           
     &                                  -           RK( 004 ) * C( O            )                                       
     &                                  -           RK( 005 ) * C( O            )                                       
      PD( O           ,NO           ) = -           RK( 006 ) * C( O            )                                       
      PD( O           ,O            ) = -           RK( 002 )                                                           
     &                                  -           RK( 004 ) * C( NO2          )                                       
     &                                  -           RK( 005 ) * C( NO2          )                                       
     &                                  -           RK( 006 ) * C( NO           )                                       
     &                                  -           RK( 041 ) * C( OH           )                                       
     &                                  -           RK( 045 ) * C( HO2          )                                       
     &                                  -           RK( 046 ) * C( H2O2         )                                       
     &                                  -           RK( 047 ) * C( NO3          )                                       
     &                                  -           RK( 080 ) * C( FORM         )                                       
     &                                  -           RK( 087 ) * C( ALD2         )                                       
     &                                  -           RK( 102 ) * C( ALDX         )                                       
     &                                  -           RK( 120 ) * C( OLE          )                                       
     &                                  -           RK( 124 ) * C( NALKO2       )                                       
     &                                  -           RK( 128 ) * C( IOLE         )                                       
     &                                  -           RK( 162 ) * C( PAHRO2       )                                       
     &                                  -           RK( 173 ) * C( NTRI         )                                       
      PD( O           ,O3           ) =             RK( 008 )                                                           
     &                                  + 0.50000 * RK( 130 ) * C( IOLE         )                                       
      PD( O           ,NO3          ) =             RK( 014 )                                                           
     &                                  -           RK( 047 ) * C( O            )                                       
      PD( O           ,O1D          ) =             RK( 010 )                                                           
      PD( O           ,OH           ) = -           RK( 041 ) * C( O            )                                       
     &                                  + 4.00000 * RK( 042 ) * C( OH           )                                       
      PD( O           ,HO2          ) = -           RK( 045 ) * C( O            )                                       
      PD( O           ,H2O2         ) = -           RK( 046 ) * C( O            )                                       
      PD( O           ,ALD2         ) = -           RK( 087 ) * C( O            )                                       
      PD( O           ,ALDX         ) = -           RK( 102 ) * C( O            )                                       
      PD( O           ,IOLE         ) = -           RK( 128 ) * C( O            )                                       
     &                                  + 0.50000 * RK( 130 ) * C( O3           )                                       
      PD( O           ,FORM         ) = -           RK( 080 ) * C( O            )                                       
      PD( O           ,OLE          ) = -           RK( 120 ) * C( O            )                                       
      PD( O           ,NALKO2       ) = -           RK( 124 ) * C( O            )                                       
      PD( O           ,PAHRO2       ) = -           RK( 162 ) * C( O            )                                       
      PD( O           ,NTRI         ) = -           RK( 173 ) * C( O            )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   4: O3                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O3          ,NO2          ) = -           RK( 007 ) * C( O3           )                                       
      PD( O3          ,NO           ) = -           RK( 003 ) * C( O3           )                                       
      PD( O3          ,O            ) =             RK( 002 )                                                           
      PD( O3          ,O3           ) = -           RK( 003 ) * C( NO           )                                       
     &                                  -           RK( 007 ) * C( NO2          )                                       
     &                                  -           RK( 008 )                                                           
     &                                  -           RK( 009 )                                                           
     &                                  -           RK( 012 ) * C( OH           )                                       
     &                                  -           RK( 013 ) * C( HO2          )                                       
     &                                  -           RK( 050 ) * C( NO3          )                                       
     &                                  -           RK( 122 ) * C( OLE          )                                       
     &                                  -           RK( 126 ) * C( NALKO2       )                                       
     &                                  -           RK( 130 ) * C( IOLE         )                                       
     &                                  -           RK( 142 ) * C( CRON         )                                       
     &                                  -           RK( 149 ) * C( TOLRO2       )                                       
     &                                  -           RK( 167 ) * C( PAHRO2       )                                       
     &                                  -           RK( 170 ) * C( ISOP         )                                       
     &                                  -           RK( 179 ) * C( NTRI         )                                       
     &                                  -           RK( 187 ) * C( CL2          )                                       
     &                                  -           RK( 275 )                                                           
      PD( O3          ,NO3          ) = -           RK( 050 ) * C( O3           )                                       
      PD( O3          ,OH           ) = -           RK( 012 ) * C( O3           )                                       
      PD( O3          ,HO2          ) = -           RK( 013 ) * C( O3           )                                       
     &                                  + 0.15000 * RK( 095 ) * C( C2O3         )                                       
     &                                  + 0.15000 * RK( 111 ) * C( CXO3         )                                       
     &                                  + 0.15000 * RK( 229 ) * C( NTRM         )                                       
      PD( O3          ,IOLE         ) = -           RK( 130 ) * C( O3           )                                       
      PD( O3          ,C2O3         ) =   0.15000 * RK( 095 ) * C( HO2          )                                       
      PD( O3          ,CXO3         ) =   0.15000 * RK( 111 ) * C( HO2          )                                       
      PD( O3          ,OLE          ) = -           RK( 122 ) * C( O3           )                                       
      PD( O3          ,NALKO2       ) = -           RK( 126 ) * C( O3           )                                       
      PD( O3          ,TOLRO2       ) = -           RK( 149 ) * C( O3           )                                       
      PD( O3          ,CRON         ) = -           RK( 142 ) * C( O3           )                                       
      PD( O3          ,PAHRO2       ) = -           RK( 167 ) * C( O3           )                                       
      PD( O3          ,ISOP         ) = -           RK( 170 ) * C( O3           )                                       
      PD( O3          ,NTRM         ) =   0.15000 * RK( 229 ) * C( HO2          )                                       
      PD( O3          ,NTRI         ) = -           RK( 179 ) * C( O3           )                                       
      PD( O3          ,CL2          ) = -           RK( 187 ) * C( O3           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   5: NO3                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO3         ,NO2          ) =             RK( 005 ) * C( O            )                                       
     &                                  +           RK( 007 ) * C( O3           )                                       
     &                                  -           RK( 017 ) * C( NO3          )                                       
     &                                  -           RK( 018 ) * C( NO3          )                                       
      PD( NO3         ,NO           ) = -           RK( 016 ) * C( NO3          )                                       
      PD( NO3         ,O            ) =             RK( 005 ) * C( NO2          )                                       
     &                                  -           RK( 047 ) * C( NO3          )                                       
      PD( NO3         ,O3           ) =             RK( 007 ) * C( NO2          )                                       
     &                                  -           RK( 050 ) * C( NO3          )                                       
      PD( NO3         ,NO3          ) = -           RK( 014 )                                                           
     &                                  -           RK( 015 )                                                           
     &                                  -           RK( 016 ) * C( NO           )                                       
     &                                  -           RK( 017 ) * C( NO2          )                                       
     &                                  -           RK( 018 ) * C( NO2          )                                       
     &                                  -           RK( 047 ) * C( O            )                                       
     &                                  -           RK( 048 ) * C( OH           )                                       
     &                                  -           RK( 049 ) * C( HO2          )                                       
     &                                  -           RK( 050 ) * C( O3           )                                       
     &                                  - 4.00000 * RK( 051 ) * C( NO3          )                                       
     &                                  -           RK( 081 ) * C( FORM         )                                       
     &                                  -           RK( 089 ) * C( ALD2         )                                       
     &                                  -           RK( 104 ) * C( ALDX         )                                       
     &                                  -           RK( 123 ) * C( OLE          )                                       
     &                                  -           RK( 127 ) * C( NALKO2       )                                       
     &                                  -           RK( 131 ) * C( IOLE         )                                       
     &                                  -           RK( 136 ) * C( TOL          )                                       
     &                                  -           RK( 140 ) * C( CAT1         )                                       
     &                                  -           RK( 150 ) * C( TOLRO2       )                                       
     &                                  -           RK( 152 ) * C( CRO          )                                       
     &                                  -           RK( 168 ) * C( PAHRO2       )                                       
     &                                  -           RK( 171 ) * C( ISOP         )                                       
     &                                  -           RK( 180 ) * C( NTRI         )                                       
      PD( NO3         ,OH           ) =             RK( 029 ) * C( HNO3         )                                       
     &                                  -           RK( 048 ) * C( NO3          )                                       
      PD( NO3         ,HO2          ) = -           RK( 049 ) * C( NO3          )                                       
      PD( NO3         ,N2O5         ) =             RK( 021 )                                                           
     &                                  +           RK( 054 )                                                           
      PD( NO3         ,HNO3         ) =             RK( 029 ) * C( OH           )                                       
      PD( NO3         ,PNA          ) =   0.39000 * RK( 052 )                                                           
      PD( NO3         ,ALD2         ) = -           RK( 089 ) * C( NO3          )                                       
      PD( NO3         ,ALDX         ) = -           RK( 104 ) * C( NO3          )                                       
      PD( NO3         ,IOLE         ) = -           RK( 131 ) * C( NO3          )                                       
      PD( NO3         ,FORM         ) = -           RK( 081 ) * C( NO3          )                                       
      PD( NO3         ,PAN          ) =   0.40000 * RK( 094 )                                                           
      PD( NO3         ,PANX         ) =   0.40000 * RK( 109 )                                                           
      PD( NO3         ,OLE          ) = -           RK( 123 ) * C( NO3          )                                       
      PD( NO3         ,NALKO2       ) = -           RK( 127 ) * C( NO3          )                                       
      PD( NO3         ,TOL          ) = -           RK( 136 ) * C( NO3          )                                       
      PD( NO3         ,TOLRO2       ) = -           RK( 150 ) * C( NO3          )                                       
      PD( NO3         ,CRO          ) = -           RK( 152 ) * C( NO3          )                                       
      PD( NO3         ,CAT1         ) = -           RK( 140 ) * C( NO3          )                                       
      PD( NO3         ,PAHRO2       ) = -           RK( 168 ) * C( NO3          )                                       
      PD( NO3         ,ISOP         ) = -           RK( 171 ) * C( NO3          )                                       
      PD( NO3         ,NTRI         ) = -           RK( 180 ) * C( NO3          )                                       
      PD( NO3         ,ALKRXN       ) =   0.40000 * RK( 227 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   6: O1D                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O1D         ,O3           ) =             RK( 009 )                                                           
      PD( O1D         ,O1D          ) = -           RK( 010 )                                                           
     &                                  -           RK( 011 )                                                           
     &                                  -           RK( 039 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   7: OH                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OH          ,NO2          ) = -           RK( 028 ) * C( OH           )                                       
      PD( OH          ,NO           ) = -           RK( 024 ) * C( OH           )                                       
     &                                  +           RK( 030 ) * C( HO2          )                                       
     &                                  + 0.12500 * RK( 067 ) * C( IEPXO2       )                                       
      PD( OH          ,O            ) = -           RK( 041 ) * C( OH           )                                       
     &                                  +           RK( 045 ) * C( HO2          )                                       
     &                                  +           RK( 046 ) * C( H2O2         )                                       
     &                                  +           RK( 080 ) * C( FORM         )                                       
     &                                  +           RK( 087 ) * C( ALD2         )                                       
     &                                  +           RK( 102 ) * C( ALDX         )                                       
     &                                  + 0.10000 * RK( 120 ) * C( OLE          )                                       
     &                                  + 0.30000 * RK( 124 ) * C( NALKO2       )                                       
      PD( OH          ,O3           ) = -           RK( 012 ) * C( OH           )                                       
     &                                  +           RK( 013 ) * C( HO2          )                                       
     &                                  + 0.10000 * RK( 122 ) * C( OLE          )                                       
     &                                  + 0.13000 * RK( 126 ) * C( NALKO2       )                                       
     &                                  + 0.50000 * RK( 130 ) * C( IOLE         )                                       
     &                                  + 0.08000 * RK( 149 ) * C( TOLRO2       )                                       
     &                                  + 0.26600 * RK( 167 ) * C( PAHRO2       )                                       
     &                                  + 0.26800 * RK( 170 ) * C( ISOP         )                                       
     &                                  + 0.57000 * RK( 179 ) * C( NTRI         )                                       
      PD( OH          ,NO3          ) = -           RK( 048 ) * C( OH           )                                       
      PD( OH          ,O1D          ) =   2.00000 * RK( 011 )                                                           
     &                                  +           RK( 039 )                                                           
      PD( OH          ,OH           ) = -           RK( 012 ) * C( O3           )                                       
     &                                  -           RK( 024 ) * C( NO           )                                       
     &                                  -           RK( 026 ) * C( HONO         )                                       
     &                                  -           RK( 028 ) * C( NO2          )                                       
     &                                  -           RK( 029 ) * C( HNO3         )                                       
     &                                  -           RK( 034 ) * C( PNA          )                                       
     &                                  -           RK( 038 ) * C( H2O2         )                                       
     &                                  -           RK( 040 )                                                           
     &                                  -           RK( 041 ) * C( O            )                                       
     &                                  - 4.00000 * RK( 042 ) * C( OH           )                                       
     &                                  - 4.00000 * RK( 043 ) * C( OH           )                                       
     &                                  -           RK( 044 ) * C( HO2          )                                       
     &                                  -           RK( 048 ) * C( NO3          )                                       
     &                                  -           RK( 062 ) * C( ROOH         )                                       
     &                                  - 0.06700 * RK( 064 ) * C( ISOPX        )                                       
     &                                  -           RK( 065 ) * C( IEPOX        )                                       
     &                                  -           RK( 069 ) * C( CO           )                                       
     &                                  -           RK( 070 )                                                           
     &                                  -           RK( 074 ) * C( MEPX         )                                       
     &                                  -           RK( 076 ) * C( MEOH         )                                       
     &                                  -           RK( 077 ) * C( FORM         )                                       
     &                                  -           RK( 086 ) * C( FACD         )                                       
     &                                  -           RK( 088 ) * C( ALD2         )                                       
     &                                  -           RK( 099 ) * C( PACD         )                                       
     &                                  -           RK( 101 ) * C( AACD         )                                       
     &                                  -           RK( 103 ) * C( ALDX         )                                       
     &                                  -           RK( 110 ) * C( PANX         )                                       
     &                                  -           RK( 116 ) * C( PAR          )                                       
     &                                  -           RK( 121 ) * C( OLE          )                                       
     &                                  -           RK( 125 ) * C( NALKO2       )                                       
     &                                  -           RK( 129 ) * C( IOLE         )                                       
     &                                  - 0.92800 * RK( 132 ) * C( ETH          )                                       
     &                                  -           RK( 135 ) * C( TOL          )                                       
     &                                  -           RK( 139 ) * C( CAT1         )                                       
     &                                  -           RK( 146 ) * C( CRN2         )                                       
     &                                  -           RK( 148 ) * C( TOLRO2       )                                       
     &                                  -           RK( 151 ) * C( CRO          )                                       
     &                                  -           RK( 158 ) * C( OPAN         )                                       
     &                                  -           RK( 159 ) * C( XYLRO2       )                                       
     &                                  -           RK( 160 ) * C( MGLY         )                                       
     &                                  -           RK( 163 ) * C( PAHRO2       )                                       
     &                                  -           RK( 169 ) * C( ISOP         )                                       
     &                                  -           RK( 174 ) * C( NTRI         )                                       
     &                                  -           RK( 181 ) * C( XO2T         )                                       
     &                                  -           RK( 182 ) * C( SULRXN       )                                       
     &                                  -           RK( 183 ) * C( ETOH         )                                       
     &                                  -           RK( 191 ) * C( CLO          )                                       
     &                                  -           RK( 205 ) * C( FMCL         )                                       
     &                                  -           RK( 228 ) * C( ALKRXN       )                                       
     &                                  -           RK( 234 ) * C( NTRALK       )                                       
     &                                  -           RK( 237 ) * C( NTROH        )                                       
     &                                  -           RK( 240 ) * C( NTRCN        )                                       
     &                                  -           RK( 243 ) * C( NTRCNOH      )                                       
     &                                  -           RK( 247 ) * C( ISOPRXN      )                                       
     &                                  -           RK( 250 ) * C( MACO3        )                                       
      PD( OH          ,HO2          ) =             RK( 013 ) * C( O3           )                                       
     &                                  +           RK( 030 ) * C( NO           )                                       
     &                                  -           RK( 044 ) * C( OH           )                                       
     &                                  +           RK( 045 ) * C( O            )                                       
     &                                  + 1.12500 * RK( 066 ) * C( IEPXO2       )                                       
     &                                  + 0.44000 * RK( 095 ) * C( C2O3         )                                       
     &                                  + 0.44000 * RK( 111 ) * C( CXO3         )                                       
     &                                  + 0.44000 * RK( 229 ) * C( NTRM         )                                       
      PD( OH          ,HNO3         ) = -           RK( 029 ) * C( OH           )                                       
     &                                  +           RK( 053 )                                                           
      PD( OH          ,HONO         ) =             RK( 025 )                                                           
     &                                  -           RK( 026 ) * C( OH           )                                       
      PD( OH          ,PNA          ) = -           RK( 034 ) * C( OH           )                                       
     &                                  + 0.39000 * RK( 052 )                                                           
      PD( OH          ,H2O2         ) =   2.00000 * RK( 037 )                                                           
     &                                  -           RK( 038 ) * C( OH           )                                       
     &                                  +           RK( 046 ) * C( O            )                                       
      PD( OH          ,NTROH        ) = -           RK( 237 ) * C( OH           )                                       
      PD( OH          ,NTRALK       ) = -           RK( 234 ) * C( OH           )                                       
      PD( OH          ,ROOH         ) = -           RK( 062 ) * C( OH           )                                       
     &                                  +           RK( 063 )                                                           
      PD( OH          ,ALD2         ) =             RK( 087 ) * C( O            )                                       
     &                                  -           RK( 088 ) * C( OH           )                                       
      PD( OH          ,ALDX         ) =             RK( 102 ) * C( O            )                                       
     &                                  -           RK( 103 ) * C( OH           )                                       
      PD( OH          ,ISOPX        ) = - 0.06700 * RK( 064 ) * C( OH           )                                       
      PD( OH          ,IEPOX        ) = -           RK( 065 ) * C( OH           )                                       
      PD( OH          ,IOLE         ) = -           RK( 129 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 130 ) * C( O3           )                                       
      PD( OH          ,IEPXO2       ) =   1.12500 * RK( 066 ) * C( HO2          )                                       
     &                                  + 0.12500 * RK( 067 ) * C( NO           )                                       
     &                                  + 0.10000 * RK( 068 ) * C( C2O3         )                                       
      PD( OH          ,MGLY         ) = -           RK( 160 ) * C( OH           )                                       
      PD( OH          ,FORM         ) = -           RK( 077 ) * C( OH           )                                       
     &                                  +           RK( 080 ) * C( O            )                                       
      PD( OH          ,FACD         ) = -           RK( 086 ) * C( OH           )                                       
      PD( OH          ,CO           ) = -           RK( 069 ) * C( OH           )                                       
      PD( OH          ,PAR          ) = -           RK( 116 ) * C( OH           )                                       
      PD( OH          ,C2O3         ) =   0.10000 * RK( 068 ) * C( IEPXO2       )                                       
     &                                  + 0.44000 * RK( 095 ) * C( HO2          )                                       
      PD( OH          ,AACD         ) = -           RK( 101 ) * C( OH           )                                       
      PD( OH          ,MEPX         ) = -           RK( 074 ) * C( OH           )                                       
     &                                  +           RK( 075 )                                                           
      PD( OH          ,MEOH         ) = -           RK( 076 ) * C( OH           )                                       
      PD( OH          ,PACD         ) = -           RK( 099 ) * C( OH           )                                       
     &                                  +           RK( 100 )                                                           
      PD( OH          ,CXO3         ) =   0.44000 * RK( 111 ) * C( HO2          )                                       
      PD( OH          ,PANX         ) = -           RK( 110 ) * C( OH           )                                       
      PD( OH          ,OLE          ) =   0.10000 * RK( 120 ) * C( O            )                                       
     &                                  -           RK( 121 ) * C( OH           )                                       
     &                                  + 0.10000 * RK( 122 ) * C( O3           )                                       
      PD( OH          ,NALKO2       ) =   0.30000 * RK( 124 ) * C( O            )                                       
     &                                  -           RK( 125 ) * C( OH           )                                       
     &                                  + 0.13000 * RK( 126 ) * C( O3           )                                       
      PD( OH          ,ETH          ) = - 0.92800 * RK( 132 ) * C( OH           )                                       
      PD( OH          ,TOL          ) = -           RK( 135 ) * C( OH           )                                       
      PD( OH          ,TOLRO2       ) = -           RK( 148 ) * C( OH           )                                       
     &                                  + 0.08000 * RK( 149 ) * C( O3           )                                       
      PD( OH          ,CRO          ) = -           RK( 151 ) * C( OH           )                                       
      PD( OH          ,CAT1         ) = -           RK( 139 ) * C( OH           )                                       
      PD( OH          ,CRN2         ) =             RK( 145 )                                                           
     &                                  -           RK( 146 ) * C( OH           )                                       
      PD( OH          ,OPAN         ) = -           RK( 158 ) * C( OH           )                                       
      PD( OH          ,XYLRO2       ) = -           RK( 159 ) * C( OH           )                                       
      PD( OH          ,PAHRO2       ) = -           RK( 163 ) * C( OH           )                                       
     &                                  + 0.26600 * RK( 167 ) * C( O3           )                                       
      PD( OH          ,ISOP         ) = -           RK( 169 ) * C( OH           )                                       
     &                                  + 0.26800 * RK( 170 ) * C( O3           )                                       
      PD( OH          ,ISOPRXN      ) = -           RK( 247 ) * C( OH           )                                       
      PD( OH          ,NTRM         ) =   0.44000 * RK( 229 ) * C( HO2          )                                       
      PD( OH          ,MACO3        ) = -           RK( 250 ) * C( OH           )                                       
      PD( OH          ,NTRI         ) = -           RK( 174 ) * C( OH           )                                       
     &                                  + 0.57000 * RK( 179 ) * C( O3           )                                       
      PD( OH          ,XO2T         ) = -           RK( 181 ) * C( OH           )                                       
      PD( OH          ,SULRXN       ) = -           RK( 182 ) * C( OH           )                                       
      PD( OH          ,ETOH         ) = -           RK( 183 ) * C( OH           )                                       
      PD( OH          ,CL           ) =             RK( 186 )                                                           
      PD( OH          ,CLO          ) = -           RK( 191 ) * C( OH           )                                       
      PD( OH          ,FMCL         ) = -           RK( 205 ) * C( OH           )                                       
      PD( OH          ,ALKRXN       ) = -           RK( 228 ) * C( OH           )                                       
      PD( OH          ,NTRCN        ) = -           RK( 240 ) * C( OH           )                                       
      PD( OH          ,NTRCNOH      ) = -           RK( 243 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   8: HO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HO2         ,NO2          ) = -           RK( 032 ) * C( HO2          )                                       
     &                                  + 0.80000 * RK( 184 ) * C( PAHRO2       )                                       
      PD( HO2         ,NO           ) = -           RK( 030 ) * C( HO2          )                                       
     &                                  -           RK( 031 ) * C( HO2          )                                       
     &                                  + 0.82500 * RK( 067 ) * C( IEPXO2       )                                       
     &                                  +           RK( 071 ) * C( MEO2         )                                       
     &                                  +           RK( 084 ) * C( HCO3         )                                       
     &                                  +           RK( 106 ) * C( CXO3         )                                       
     &                                  + 1.20000 * RK( 133 ) * C( CRES         )                                       
     &                                  + 1.20000 * RK( 153 ) * C( OPO3         )                                       
     &                                  +           RK( 155 ) * C( CRPX         )                                       
     &                                  + 0.90000 * RK( 164 ) * C( ISOPO2       )                                       
     &                                  + 0.68000 * RK( 248 ) * C( NTRMO2       )                                       
      PD( HO2         ,O            ) =             RK( 041 ) * C( OH           )                                       
     &                                  -           RK( 045 ) * C( HO2          )                                       
     &                                  +           RK( 046 ) * C( H2O2         )                                       
     &                                  +           RK( 080 ) * C( FORM         )                                       
     &                                  + 0.30000 * RK( 120 ) * C( OLE          )                                       
     &                                  + 1.70000 * RK( 124 ) * C( NALKO2       )                                       
     &                                  + 0.10000 * RK( 128 ) * C( IOLE         )                                       
     &                                  + 0.25000 * RK( 162 ) * C( PAHRO2       )                                       
      PD( HO2         ,O3           ) =             RK( 012 ) * C( OH           )                                       
     &                                  -           RK( 013 ) * C( HO2          )                                       
     &                                  + 0.44000 * RK( 122 ) * C( OLE          )                                       
     &                                  + 0.13000 * RK( 126 ) * C( NALKO2       )                                       
     &                                  + 0.50000 * RK( 130 ) * C( IOLE         )                                       
     &                                  + 0.76000 * RK( 149 ) * C( TOLRO2       )                                       
     &                                  + 0.06600 * RK( 167 ) * C( PAHRO2       )                                       
     &                                  + 0.15400 * RK( 170 ) * C( ISOP         )                                       
     &                                  + 0.07000 * RK( 179 ) * C( NTRI         )                                       
      PD( HO2         ,NO3          ) =             RK( 048 ) * C( OH           )                                       
     &                                  -           RK( 049 ) * C( HO2          )                                       
     &                                  +           RK( 081 ) * C( FORM         )                                       
     &                                  +           RK( 131 ) * C( IOLE         )                                       
     &                                  + 0.36000 * RK( 136 ) * C( TOL          )                                       
     &                                  + 0.80000 * RK( 168 ) * C( PAHRO2       )                                       
     &                                  + 0.92500 * RK( 171 ) * C( ISOP         )                                       
     &                                  + 0.28000 * RK( 180 ) * C( NTRI         )                                       
      PD( HO2         ,O1D          ) =             RK( 039 )                                                           
      PD( HO2         ,OH           ) =             RK( 012 ) * C( O3           )                                       
     &                                  +           RK( 038 ) * C( H2O2         )                                       
     &                                  +           RK( 040 )                                                           
     &                                  +           RK( 041 ) * C( O            )                                       
     &                                  -           RK( 044 ) * C( HO2          )                                       
     &                                  +           RK( 048 ) * C( NO3          )                                       
     &                                  +           RK( 069 ) * C( CO           )                                       
     &                                  + 0.30000 * RK( 074 ) * C( MEPX         )                                       
     &                                  +           RK( 076 ) * C( MEOH         )                                       
     &                                  +           RK( 077 ) * C( FORM         )                                       
     &                                  +           RK( 086 ) * C( FACD         )                                       
     &                                  + 0.11000 * RK( 116 ) * C( PAR          )                                       
     &                                  + 0.95000 * RK( 121 ) * C( OLE          )                                       
     &                                  +           RK( 125 ) * C( NALKO2       )                                       
     &                                  +           RK( 129 ) * C( IOLE         )                                       
     &                                  + 0.28000 * RK( 132 ) * C( ETH          )                                       
     &                                  + 1.12000 * RK( 135 ) * C( TOL          )                                       
     &                                  + 0.70000 * RK( 158 ) * C( OPAN         )                                       
     &                                  + 0.70000 * RK( 159 ) * C( XYLRO2       )                                       
     &                                  + 0.50300 * RK( 169 ) * C( ISOP         )                                       
     &                                  + 0.75000 * RK( 174 ) * C( NTRI         )                                       
     &                                  +           RK( 181 ) * C( XO2T         )                                       
     &                                  +           RK( 182 ) * C( SULRXN       )                                       
     &                                  +           RK( 183 ) * C( ETOH         )                                       
      PD( HO2         ,HO2          ) = -           RK( 013 ) * C( O3           )                                       
     &                                  -           RK( 030 ) * C( NO           )                                       
     &                                  -           RK( 031 ) * C( NO           )                                       
     &                                  -           RK( 032 ) * C( NO2          )                                       
     &                                  - 4.00000 * RK( 035 ) * C( HO2          )                                       
     &                                  - 4.00000 * RK( 036 ) * C( HO2          )                                       
     &                                  -           RK( 044 ) * C( OH           )                                       
     &                                  -           RK( 045 ) * C( O            )                                       
     &                                  -           RK( 049 ) * C( NO3          )                                       
     &                                  -           RK( 057 ) * C( XO2          )                                       
     &                                  -           RK( 058 ) * C( XO2N         )                                       
     &                                  - 0.17500 * RK( 066 ) * C( IEPXO2       )                                       
     &                                  -           RK( 072 ) * C( MEO2         )                                       
     &                                  -           RK( 082 ) * C( FORM         )                                       
     &                                  -           RK( 085 ) * C( HCO3         )                                       
     &                                  -           RK( 095 ) * C( C2O3         )                                       
     &                                  -           RK( 111 ) * C( CXO3         )                                       
     &                                  -           RK( 134 ) * C( CRES         )                                       
     &                                  -           RK( 138 ) * C( OPEN         )                                       
     &                                  -           RK( 144 ) * C( CRNO         )                                       
     &                                  -           RK( 154 ) * C( OPO3         )                                       
     &                                  -           RK( 166 ) * C( ISOPO2       )                                       
     &                                  -           RK( 176 ) * C( TRPRXN       )                                       
     &                                  -           RK( 190 ) * C( HOCL         )                                       
     &                                  -           RK( 229 ) * C( NTRM         )                                       
     &                                  -           RK( 236 ) * C( MAPAN        )                                       
     &                                  -           RK( 239 ) * C( NOHO2        )                                       
     &                                  -           RK( 242 ) * C( NCNO2        )                                       
     &                                  -           RK( 245 ) * C( NCNOHO2      )                                       
     &                                  -           RK( 249 ) * C( NTRMO2       )                                       
     &                                  -           RK( 252 ) * C( NTRIO2       )                                       
      PD( HO2         ,PNA          ) =             RK( 033 )                                                           
     &                                  + 0.61000 * RK( 052 )                                                           
      PD( HO2         ,H2O2         ) =             RK( 038 ) * C( OH           )                                       
     &                                  +           RK( 046 ) * C( O            )                                       
      PD( HO2         ,XO2          ) = -           RK( 057 ) * C( HO2          )                                       
      PD( HO2         ,XO2N         ) = -           RK( 058 ) * C( HO2          )                                       
      PD( HO2         ,NTROH        ) =             RK( 256 )                                                           
      PD( HO2         ,NTRALK       ) =             RK( 255 )                                                           
      PD( HO2         ,ROOH         ) =             RK( 063 )                                                           
      PD( HO2         ,ALD2         ) =             RK( 090 )                                                           
      PD( HO2         ,ALDX         ) =             RK( 105 )                                                           
      PD( HO2         ,ISOPO2       ) =   0.90000 * RK( 164 ) * C( NO           )                                       
     &                                  + 0.80000 * RK( 165 ) * C( C2O3         )                                       
     &                                  -           RK( 166 ) * C( HO2          )                                       
      PD( HO2         ,IOLE         ) =   0.10000 * RK( 128 ) * C( O            )                                       
     &                                  +           RK( 129 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 130 ) * C( O3           )                                       
     &                                  +           RK( 131 ) * C( NO3          )                                       
     &                                  +           RK( 198 ) * C( CL2          )                                       
      PD( HO2         ,IEPXO2       ) = - 0.17500 * RK( 066 ) * C( HO2          )                                       
     &                                  + 0.82500 * RK( 067 ) * C( NO           )                                       
     &                                  + 0.66000 * RK( 068 ) * C( C2O3         )                                       
      PD( HO2         ,MGLY         ) =             RK( 161 )                                                           
      PD( HO2         ,FORM         ) =             RK( 077 ) * C( OH           )                                       
     &                                  + 2.00000 * RK( 078 )                                                           
     &                                  +           RK( 080 ) * C( O            )                                       
     &                                  +           RK( 081 ) * C( NO3          )                                       
     &                                  -           RK( 082 ) * C( HO2          )                                       
     &                                  +           RK( 200 ) * C( CL2          )                                       
      PD( HO2         ,FACD         ) =             RK( 086 ) * C( OH           )                                       
      PD( HO2         ,CO           ) =             RK( 069 ) * C( OH           )                                       
      PD( HO2         ,PAR          ) =   0.11000 * RK( 116 ) * C( OH           )                                       
     &                                  + 0.11000 * RK( 194 ) * C( CL2          )                                       
      PD( HO2         ,C2O3         ) =   0.66000 * RK( 068 ) * C( IEPXO2       )                                       
     &                                  -           RK( 095 ) * C( HO2          )                                       
     &                                  + 0.90000 * RK( 096 ) * C( MEO2         )                                       
     &                                  +           RK( 115 ) * C( CXO3         )                                       
     &                                  + 0.80000 * RK( 165 ) * C( ISOPO2       )                                       
      PD( HO2         ,MEO2         ) =             RK( 071 ) * C( NO           )                                       
     &                                  -           RK( 072 ) * C( HO2          )                                       
     &                                  + 4.00000 * RK( 073 ) * C( MEO2         )                                       
     &                                  + 0.90000 * RK( 096 ) * C( C2O3         )                                       
     &                                  +           RK( 112 ) * C( CXO3         )                                       
     &                                  + 0.90000 * RK( 230 ) * C( NTRM         )                                       
      PD( HO2         ,MEPX         ) =   0.30000 * RK( 074 ) * C( OH           )                                       
     &                                  +           RK( 075 )                                                           
      PD( HO2         ,MEOH         ) =             RK( 076 ) * C( OH           )                                       
     &                                  +           RK( 203 ) * C( CL2          )                                       
      PD( HO2         ,HCO3         ) =             RK( 083 )                                                           
     &                                  +           RK( 084 ) * C( NO           )                                       
     &                                  -           RK( 085 ) * C( HO2          )                                       
      PD( HO2         ,CXO3         ) =             RK( 106 ) * C( NO           )                                       
     &                                  -           RK( 111 ) * C( HO2          )                                       
     &                                  +           RK( 112 ) * C( MEO2         )                                       
     &                                  + 4.00000 * RK( 114 ) * C( CXO3         )                                       
     &                                  +           RK( 115 ) * C( C2O3         )                                       
     &                                  +           RK( 232 ) * C( NTRM         )                                       
      PD( HO2         ,PANX         ) =   0.40000 * RK( 109 )                                                           
      PD( HO2         ,ROR          ) =   0.94000 * RK( 117 )                                                           
     &                                  +           RK( 118 )                                                           
      PD( HO2         ,OLE          ) =   0.30000 * RK( 120 ) * C( O            )                                       
     &                                  + 0.95000 * RK( 121 ) * C( OH           )                                       
     &                                  + 0.44000 * RK( 122 ) * C( O3           )                                       
     &                                  +           RK( 197 ) * C( CL2          )                                       
      PD( HO2         ,NALKO2       ) =   1.70000 * RK( 124 ) * C( O            )                                       
     &                                  +           RK( 125 ) * C( OH           )                                       
     &                                  + 0.13000 * RK( 126 ) * C( O3           )                                       
     &                                  +           RK( 196 ) * C( CL2          )                                       
      PD( HO2         ,ETH          ) =   0.28000 * RK( 132 ) * C( OH           )                                       
     &                                  + 0.88000 * RK( 206 ) * C( CL2          )                                       
      PD( HO2         ,TOL          ) =   1.12000 * RK( 135 ) * C( OH           )                                       
     &                                  + 0.36000 * RK( 136 ) * C( NO3          )                                       
      PD( HO2         ,CRES         ) =   1.20000 * RK( 133 ) * C( NO           )                                       
     &                                  -           RK( 134 ) * C( HO2          )                                       
      PD( HO2         ,TOLRO2       ) =             RK( 147 )                                                           
     &                                  + 0.76000 * RK( 149 ) * C( O3           )                                       
      PD( HO2         ,OPEN         ) = -           RK( 138 ) * C( HO2          )                                       
      PD( HO2         ,CRNO         ) = -           RK( 144 ) * C( HO2          )                                       
      PD( HO2         ,CRPX         ) =             RK( 155 ) * C( NO           )                                       
      PD( HO2         ,OPO3         ) =   1.20000 * RK( 153 ) * C( NO           )                                       
     &                                  -           RK( 154 ) * C( HO2          )                                       
      PD( HO2         ,OPAN         ) =   0.70000 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.84000 * RK( 207 ) * C( CL2          )                                       
      PD( HO2         ,XYLRO2       ) =   0.70000 * RK( 159 ) * C( OH           )                                       
     &                                  + 0.84000 * RK( 208 ) * C( CL2          )                                       
      PD( HO2         ,PAHRO2       ) =   0.25000 * RK( 162 ) * C( O            )                                       
     &                                  + 0.06600 * RK( 167 ) * C( O3           )                                       
     &                                  + 0.80000 * RK( 168 ) * C( NO3          )                                       
     &                                  + 0.80000 * RK( 184 ) * C( NO2          )                                       
     &                                  +           RK( 199 ) * C( CL2          )                                       
      PD( HO2         ,ISOP         ) =   0.50300 * RK( 169 ) * C( OH           )                                       
     &                                  + 0.15400 * RK( 170 ) * C( O3           )                                       
     &                                  + 0.92500 * RK( 171 ) * C( NO3          )                                       
     &                                  + 1.03300 * RK( 172 )                                                           
      PD( HO2         ,ISOPRXN      ) =             RK( 253 )                                                           
      PD( HO2         ,NTRM         ) = -           RK( 229 ) * C( HO2          )                                       
     &                                  + 0.90000 * RK( 230 ) * C( MEO2         )                                       
     &                                  +           RK( 232 ) * C( CXO3         )                                       
      PD( HO2         ,MACO3        ) =             RK( 254 )                                                           
      PD( HO2         ,NTRI         ) =   0.75000 * RK( 174 ) * C( OH           )                                       
     &                                  + 0.07000 * RK( 179 ) * C( O3           )                                       
     &                                  + 0.28000 * RK( 180 ) * C( NO3          )                                       
      PD( HO2         ,TRPRXN       ) = -           RK( 176 ) * C( HO2          )                                       
      PD( HO2         ,XO2T         ) =             RK( 181 ) * C( OH           )                                       
      PD( HO2         ,SULRXN       ) =             RK( 182 ) * C( OH           )                                       
     &                                  +           RK( 204 ) * C( CL2          )                                       
      PD( HO2         ,ETOH         ) =             RK( 183 ) * C( OH           )                                       
     &                                  +           RK( 195 ) * C( CL2          )                                       
      PD( HO2         ,CL2          ) =   0.11000 * RK( 194 ) * C( PAR          )                                       
     &                                  +           RK( 195 ) * C( ETOH         )                                       
     &                                  +           RK( 196 ) * C( NALKO2       )                                       
     &                                  +           RK( 197 ) * C( OLE          )                                       
     &                                  +           RK( 198 ) * C( IOLE         )                                       
     &                                  +           RK( 199 ) * C( PAHRO2       )                                       
     &                                  +           RK( 200 ) * C( FORM         )                                       
     &                                  +           RK( 203 ) * C( MEOH         )                                       
     &                                  +           RK( 204 ) * C( SULRXN       )                                       
     &                                  + 0.88000 * RK( 206 ) * C( ETH          )                                       
     &                                  + 0.84000 * RK( 207 ) * C( OPAN         )                                       
     &                                  + 0.84000 * RK( 208 ) * C( XYLRO2       )                                       
      PD( HO2         ,HOCL         ) = -           RK( 190 ) * C( HO2          )                                       
      PD( HO2         ,CLO          ) =             RK( 192 )                                                           
      PD( HO2         ,MAPAN        ) = -           RK( 236 ) * C( HO2          )                                       
      PD( HO2         ,NTRCN        ) =             RK( 257 )                                                           
      PD( HO2         ,NTRCNOH      ) =             RK( 258 )                                                           
      PD( HO2         ,NTRPX        ) =             RK( 259 )                                                           
      PD( HO2         ,NOHO2        ) = -           RK( 239 ) * C( HO2          )                                       
      PD( HO2         ,NCNO2        ) = -           RK( 242 ) * C( HO2          )                                       
      PD( HO2         ,NCNOHO2      ) = -           RK( 245 ) * C( HO2          )                                       
      PD( HO2         ,NTRMO2       ) =   0.68000 * RK( 248 ) * C( NO           )                                       
     &                                  -           RK( 249 ) * C( HO2          )                                       
      PD( HO2         ,NTRIO2       ) = -           RK( 252 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   9: N2O5                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( N2O5        ,NO2          ) =             RK( 018 ) * C( NO3          )                                       
      PD( N2O5        ,NO3          ) =             RK( 018 ) * C( NO2          )                                       
      PD( N2O5        ,N2O5         ) = -           RK( 019 )                                                           
     &                                  -           RK( 020 )                                                           
     &                                  -           RK( 021 )                                                           
     &                                  -           RK( 054 )                                                           
     &                                  -           RK( 267 )                                                           
     &                                  -           RK( 268 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  10: HNO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HNO3        ,NO2          ) =             RK( 028 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 274 )                                                           
      PD( HNO3        ,NO           ) =             RK( 031 ) * C( HO2          )                                       
      PD( HNO3        ,NO3          ) =             RK( 049 ) * C( HO2          )                                       
     &                                  +           RK( 081 ) * C( FORM         )                                       
     &                                  +           RK( 089 ) * C( ALD2         )                                       
     &                                  +           RK( 104 ) * C( ALDX         )                                       
     &                                  +           RK( 136 ) * C( TOL          )                                       
     &                                  +           RK( 140 ) * C( CAT1         )                                       
     &                                  +           RK( 150 ) * C( TOLRO2       )                                       
     &                                  +           RK( 152 ) * C( CRO          )                                       
     &                                  + 0.15000 * RK( 171 ) * C( ISOP         )                                       
      PD( HNO3        ,OH           ) =             RK( 028 ) * C( NO2          )                                       
     &                                  -           RK( 029 ) * C( HNO3         )                                       
      PD( HNO3        ,HO2          ) =             RK( 031 ) * C( NO           )                                       
     &                                  +           RK( 049 ) * C( NO3          )                                       
      PD( HNO3        ,N2O5         ) =   2.00000 * RK( 019 )                                                           
     &                                  + 2.00000 * RK( 020 )                                                           
     &                                  +           RK( 267 )                                                           
     &                                  +           RK( 268 )                                                           
      PD( HNO3        ,HNO3         ) = -           RK( 029 ) * C( OH           )                                       
     &                                  -           RK( 053 )                                                           
      PD( HNO3        ,NTROH        ) =             RK( 261 )                                                           
      PD( HNO3        ,NTRALK       ) =             RK( 260 )                                                           
      PD( HNO3        ,ALD2         ) =             RK( 089 ) * C( NO3          )                                       
      PD( HNO3        ,ALDX         ) =             RK( 104 ) * C( NO3          )                                       
      PD( HNO3        ,FORM         ) =             RK( 081 ) * C( NO3          )                                       
      PD( HNO3        ,TOL          ) =             RK( 136 ) * C( NO3          )                                       
      PD( HNO3        ,TOLRO2       ) =             RK( 150 ) * C( NO3          )                                       
      PD( HNO3        ,CRO          ) =             RK( 152 ) * C( NO3          )                                       
      PD( HNO3        ,CAT1         ) =             RK( 140 ) * C( NO3          )                                       
      PD( HNO3        ,ISOP         ) =   0.15000 * RK( 171 ) * C( NO3          )                                       
      PD( HNO3        ,ISOPRXN      ) =             RK( 265 )                                                           
      PD( HNO3        ,MACO3        ) =             RK( 266 )                                                           
      PD( HNO3        ,NTRCN        ) =             RK( 262 )                                                           
      PD( HNO3        ,NTRCNOH      ) =             RK( 263 )                                                           
      PD( HNO3        ,NTRPX        ) =             RK( 264 )                                                           
      PD( HNO3        ,H2NO3PIJ     ) =             RK( 269 )                                                           
      PD( HNO3        ,H2NO3PK      ) =             RK( 270 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  11: HONO                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HONO        ,NO2          ) =   2.00000 * RK( 023 ) * C( NO           )                                       
     &                                  + 0.50000 * RK( 274 )                                                           
      PD( HONO        ,NO           ) =   2.00000 * RK( 023 ) * C( NO2          )                                       
     &                                  +           RK( 024 ) * C( OH           )                                       
      PD( HONO        ,OH           ) =             RK( 024 ) * C( NO           )                                       
     &                                  -           RK( 026 ) * C( HONO         )                                       
      PD( HONO        ,HONO         ) = -           RK( 025 )                                                           
     &                                  -           RK( 026 ) * C( OH           )                                       
     &                                  - 4.00000 * RK( 027 ) * C( HONO         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  12: PNA                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PNA         ,NO2          ) =             RK( 032 ) * C( HO2          )                                       
      PD( PNA         ,OH           ) = -           RK( 034 ) * C( PNA          )                                       
      PD( PNA         ,HO2          ) =             RK( 032 ) * C( NO2          )                                       
      PD( PNA         ,PNA          ) = -           RK( 033 )                                                           
     &                                  -           RK( 034 ) * C( OH           )                                       
     &                                  -           RK( 052 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  13: H2O2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( H2O2        ,O            ) = -           RK( 046 ) * C( H2O2         )                                       
      PD( H2O2        ,OH           ) = -           RK( 038 ) * C( H2O2         )                                       
     &                                  + 4.00000 * RK( 043 ) * C( OH           )                                       
      PD( H2O2        ,HO2          ) =   4.00000 * RK( 035 ) * C( HO2          )                                       
     &                                  + 4.00000 * RK( 036 ) * C( HO2          )                                       
      PD( H2O2        ,H2O2         ) = -           RK( 037 )                                                           
     &                                  -           RK( 038 ) * C( OH           )                                       
     &                                  -           RK( 046 ) * C( O            )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  14: XO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XO2         ,NO2          ) =             RK( 184 ) * C( PAHRO2       )                                       
      PD( XO2         ,NO           ) = -           RK( 055 ) * C( XO2          )                                       
     &                                  +           RK( 106 ) * C( CXO3         )                                       
     &                                  +           RK( 155 ) * C( CRPX         )                                       
      PD( XO2         ,O            ) =   0.20000 * RK( 120 ) * C( OLE          )                                       
     &                                  + 0.70000 * RK( 124 ) * C( NALKO2       )                                       
     &                                  + 0.10000 * RK( 128 ) * C( IOLE         )                                       
     &                                  + 0.25000 * RK( 162 ) * C( PAHRO2       )                                       
      PD( XO2         ,O3           ) =   0.22000 * RK( 122 ) * C( OLE          )                                       
     &                                  + 0.03000 * RK( 149 ) * C( TOLRO2       )                                       
     &                                  + 0.20000 * RK( 167 ) * C( PAHRO2       )                                       
     &                                  + 0.06400 * RK( 170 ) * C( ISOP         )                                       
     &                                  + 0.76000 * RK( 179 ) * C( NTRI         )                                       
      PD( XO2         ,NO3          ) =   0.91000 * RK( 123 ) * C( OLE          )                                       
     &                                  +           RK( 127 ) * C( NALKO2       )                                       
     &                                  + 0.60000 * RK( 136 ) * C( TOL          )                                       
     &                                  +           RK( 168 ) * C( PAHRO2       )                                       
     &                                  + 0.07500 * RK( 171 ) * C( ISOP         )                                       
     &                                  + 1.03000 * RK( 180 ) * C( NTRI         )                                       
      PD( XO2         ,OH           ) =             RK( 062 ) * C( ROOH         )                                       
     &                                  + 0.30000 * RK( 074 ) * C( MEPX         )                                       
     &                                  + 0.87000 * RK( 116 ) * C( PAR          )                                       
     &                                  + 0.80000 * RK( 121 ) * C( OLE          )                                       
     &                                  +           RK( 125 ) * C( NALKO2       )                                       
     &                                  +           RK( 129 ) * C( IOLE         )                                       
     &                                  + 0.10000 * RK( 132 ) * C( ETH          )                                       
     &                                  + 0.12000 * RK( 135 ) * C( TOL          )                                       
     &                                  + 0.50000 * RK( 158 ) * C( OPAN         )                                       
     &                                  + 0.50000 * RK( 159 ) * C( XYLRO2       )                                       
     &                                  +           RK( 160 ) * C( MGLY         )                                       
     &                                  + 0.71300 * RK( 169 ) * C( ISOP         )                                       
     &                                  + 1.25000 * RK( 174 ) * C( NTRI         )                                       
     &                                  + 0.05000 * RK( 182 ) * C( SULRXN       )                                       
     &                                  + 0.99100 * RK( 183 ) * C( ETOH         )                                       
      PD( XO2         ,HO2          ) = -           RK( 057 ) * C( XO2          )                                       
     &                                  + 0.44000 * RK( 111 ) * C( CXO3         )                                       
      PD( XO2         ,XO2          ) = -           RK( 055 ) * C( NO           )                                       
     &                                  -           RK( 057 ) * C( HO2          )                                       
     &                                  - 4.00000 * RK( 059 ) * C( XO2          )                                       
     &                                  -           RK( 061 ) * C( XO2N         )                                       
     &                                  -           RK( 097 ) * C( C2O3         )                                       
     &                                  -           RK( 113 ) * C( CXO3         )                                       
     &                                  -           RK( 178 ) * C( TRPRXN       )                                       
     &                                  -           RK( 231 ) * C( NTRM         )                                       
      PD( XO2         ,XO2N         ) = -           RK( 061 ) * C( XO2          )                                       
      PD( XO2         ,ROOH         ) =             RK( 062 ) * C( OH           )                                       
      PD( XO2         ,IOLE         ) =   0.10000 * RK( 128 ) * C( O            )                                       
     &                                  +           RK( 129 ) * C( OH           )                                       
     &                                  + 1.70000 * RK( 198 ) * C( CL2          )                                       
      PD( XO2         ,MGLY         ) =             RK( 160 ) * C( OH           )                                       
      PD( XO2         ,PAR          ) =   0.87000 * RK( 116 ) * C( OH           )                                       
     &                                  + 0.87000 * RK( 194 ) * C( CL2          )                                       
      PD( XO2         ,C2O3         ) = -           RK( 097 ) * C( XO2          )                                       
     &                                  +           RK( 115 ) * C( CXO3         )                                       
      PD( XO2         ,MEO2         ) =   0.90000 * RK( 112 ) * C( CXO3         )                                       
      PD( XO2         ,MEPX         ) =   0.30000 * RK( 074 ) * C( OH           )                                       
      PD( XO2         ,CXO3         ) =             RK( 106 ) * C( NO           )                                       
     &                                  + 0.44000 * RK( 111 ) * C( HO2          )                                       
     &                                  + 0.90000 * RK( 112 ) * C( MEO2         )                                       
     &                                  -           RK( 113 ) * C( XO2          )                                       
     &                                  + 4.00000 * RK( 114 ) * C( CXO3         )                                       
     &                                  +           RK( 115 ) * C( C2O3         )                                       
     &                                  +           RK( 232 ) * C( NTRM         )                                       
      PD( XO2         ,PANX         ) =   0.40000 * RK( 109 )                                                           
      PD( XO2         ,ROR          ) =   0.96000 * RK( 117 )                                                           
      PD( XO2         ,OLE          ) =   0.20000 * RK( 120 ) * C( O            )                                       
     &                                  + 0.80000 * RK( 121 ) * C( OH           )                                       
     &                                  + 0.22000 * RK( 122 ) * C( O3           )                                       
     &                                  + 0.91000 * RK( 123 ) * C( NO3          )                                       
     &                                  + 2.00000 * RK( 197 ) * C( CL2          )                                       
      PD( XO2         ,NALKO2       ) =   0.70000 * RK( 124 ) * C( O            )                                       
     &                                  +           RK( 125 ) * C( OH           )                                       
     &                                  +           RK( 127 ) * C( NO3          )                                       
     &                                  + 2.00000 * RK( 196 ) * C( CL2          )                                       
      PD( XO2         ,ETH          ) =   0.10000 * RK( 132 ) * C( OH           )                                       
     &                                  + 0.88000 * RK( 206 ) * C( CL2          )                                       
      PD( XO2         ,TOL          ) =   0.12000 * RK( 135 ) * C( OH           )                                       
     &                                  + 0.60000 * RK( 136 ) * C( NO3          )                                       
      PD( XO2         ,TOLRO2       ) =   0.03000 * RK( 149 ) * C( O3           )                                       
      PD( XO2         ,CRPX         ) =             RK( 155 ) * C( NO           )                                       
      PD( XO2         ,OPAN         ) =   0.50000 * RK( 158 ) * C( OH           )                                       
     &                                  + 0.84000 * RK( 207 ) * C( CL2          )                                       
      PD( XO2         ,XYLRO2       ) =   0.50000 * RK( 159 ) * C( OH           )                                       
     &                                  + 0.84000 * RK( 208 ) * C( CL2          )                                       
      PD( XO2         ,PAHRO2       ) =   0.25000 * RK( 162 ) * C( O            )                                       
     &                                  + 0.20000 * RK( 167 ) * C( O3           )                                       
     &                                  +           RK( 168 ) * C( NO3          )                                       
     &                                  +           RK( 184 ) * C( NO2          )                                       
     &                                  +           RK( 199 ) * C( CL2          )                                       
      PD( XO2         ,ISOP         ) =   0.71300 * RK( 169 ) * C( OH           )                                       
     &                                  + 0.06400 * RK( 170 ) * C( O3           )                                       
     &                                  + 0.07500 * RK( 171 ) * C( NO3          )                                       
     &                                  + 0.70000 * RK( 172 )                                                           
      PD( XO2         ,NTRM         ) = -           RK( 231 ) * C( XO2          )                                       
     &                                  +           RK( 232 ) * C( CXO3         )                                       
      PD( XO2         ,NTRI         ) =   1.25000 * RK( 174 ) * C( OH           )                                       
     &                                  + 0.76000 * RK( 179 ) * C( O3           )                                       
     &                                  + 1.03000 * RK( 180 ) * C( NO3          )                                       
      PD( XO2         ,TRPRXN       ) = -           RK( 178 ) * C( XO2          )                                       
      PD( XO2         ,SULRXN       ) =   0.05000 * RK( 182 ) * C( OH           )                                       
      PD( XO2         ,ETOH         ) =   0.99100 * RK( 183 ) * C( OH           )                                       
     &                                  + 0.99100 * RK( 195 ) * C( CL2          )                                       
      PD( XO2         ,CL2          ) =   0.87000 * RK( 194 ) * C( PAR          )                                       
     &                                  + 0.99100 * RK( 195 ) * C( ETOH         )                                       
     &                                  + 2.00000 * RK( 196 ) * C( NALKO2       )                                       
     &                                  + 2.00000 * RK( 197 ) * C( OLE          )                                       
     &                                  + 1.70000 * RK( 198 ) * C( IOLE         )                                       
     &                                  +           RK( 199 ) * C( PAHRO2       )                                       
     &                                  + 0.88000 * RK( 206 ) * C( ETH          )                                       
     &                                  + 0.84000 * RK( 207 ) * C( OPAN         )                                       
     &                                  + 0.84000 * RK( 208 ) * C( XYLRO2       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  15: XO2N                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XO2N        ,NO           ) = -           RK( 056 ) * C( XO2N         )                                       
      PD( XO2N        ,O            ) =   0.01000 * RK( 120 ) * C( OLE          )                                       
      PD( XO2N        ,NO3          ) =   0.09000 * RK( 123 ) * C( OLE          )                                       
     &                                  + 0.10000 * RK( 136 ) * C( TOL          )                                       
      PD( XO2N        ,OH           ) =   0.13000 * RK( 116 ) * C( PAR          )                                       
     &                                  + 0.06000 * RK( 135 ) * C( TOL          )                                       
     &                                  + 0.00900 * RK( 183 ) * C( ETOH         )                                       
      PD( XO2N        ,HO2          ) = -           RK( 058 ) * C( XO2N         )                                       
      PD( XO2N        ,XO2          ) = -           RK( 061 ) * C( XO2N         )                                       
      PD( XO2N        ,XO2N         ) = -           RK( 056 ) * C( NO           )                                       
     &                                  -           RK( 058 ) * C( HO2          )                                       
     &                                  - 4.00000 * RK( 060 ) * C( XO2N         )                                       
     &                                  -           RK( 061 ) * C( XO2          )                                       
      PD( XO2N        ,PAR          ) =   0.13000 * RK( 116 ) * C( OH           )                                       
     &                                  + 0.13000 * RK( 194 ) * C( CL2          )                                       
      PD( XO2N        ,ROR          ) =   0.04000 * RK( 117 )                                                           
      PD( XO2N        ,OLE          ) =   0.01000 * RK( 120 ) * C( O            )                                       
     &                                  + 0.09000 * RK( 123 ) * C( NO3          )                                       
      PD( XO2N        ,ETH          ) =   0.12000 * RK( 206 ) * C( CL2          )                                       
      PD( XO2N        ,TOL          ) =   0.06000 * RK( 135 ) * C( OH           )                                       
     &                                  + 0.10000 * RK( 136 ) * C( NO3          )                                       
      PD( XO2N        ,OPAN         ) =   0.16000 * RK( 207 ) * C( CL2          )                                       
      PD( XO2N        ,XYLRO2       ) =   0.16000 * RK( 208 ) * C( CL2          )                                       
      PD( XO2N        ,ETOH         ) =   0.00900 * RK( 183 ) * C( OH           )                                       
     &                                  + 0.00900 * RK( 195 ) * C( CL2          )                                       
      PD( XO2N        ,CL2          ) =   0.13000 * RK( 194 ) * C( PAR          )                                       
     &                                  + 0.00900 * RK( 195 ) * C( ETOH         )                                       
     &                                  + 0.12000 * RK( 206 ) * C( ETH          )                                       
     &                                  + 0.16000 * RK( 207 ) * C( OPAN         )                                       
     &                                  + 0.16000 * RK( 208 ) * C( XYLRO2       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  16: NTROH                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTROH       ,NO2          ) =   2.00000 * RK( 141 ) * C( CRON         )                                       
     &                                  + 0.80000 * RK( 184 ) * C( PAHRO2       )                                       
      PD( NTROH       ,NO           ) =   0.50000 * RK( 056 ) * C( XO2N         )                                       
     &                                  + 0.14000 * RK( 133 ) * C( CRES         )                                       
     &                                  + 0.14000 * RK( 153 ) * C( OPO3         )                                       
      PD( NTROH       ,OH           ) = -           RK( 237 ) * C( NTROH        )                                       
      PD( NTROH       ,XO2N         ) =   0.50000 * RK( 056 ) * C( NO           )                                       
      PD( NTROH       ,NTROH        ) = -           RK( 237 ) * C( OH           )                                       
     &                                  -           RK( 256 )                                                           
     &                                  -           RK( 261 )                                                           
      PD( NTROH       ,CRES         ) =   0.14000 * RK( 133 ) * C( NO           )                                       
      PD( NTROH       ,CRON         ) =   2.00000 * RK( 141 ) * C( NO2          )                                       
      PD( NTROH       ,OPO3         ) =   0.14000 * RK( 153 ) * C( NO           )                                       
      PD( NTROH       ,PAHRO2       ) =   0.80000 * RK( 184 ) * C( NO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  17: NTRALK                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTRALK      ,NO2          ) =             RK( 119 ) * C( ROR          )                                       
      PD( NTRALK      ,NO           ) =   0.50000 * RK( 056 ) * C( XO2N         )                                       
      PD( NTRALK      ,OH           ) = -           RK( 234 ) * C( NTRALK       )                                       
      PD( NTRALK      ,XO2N         ) =   0.50000 * RK( 056 ) * C( NO           )                                       
      PD( NTRALK      ,NTRALK       ) = -           RK( 234 ) * C( OH           )                                       
     &                                  -           RK( 255 )                                                           
     &                                  -           RK( 260 )                                                           
      PD( NTRALK      ,ROR          ) =             RK( 119 ) * C( NO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  18: ROOH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ROOH        ,OH           ) = -           RK( 062 ) * C( ROOH         )                                       
      PD( ROOH        ,HO2          ) =             RK( 057 ) * C( XO2          )                                       
     &                                  +           RK( 058 ) * C( XO2N         )                                       
     &                                  +           RK( 176 ) * C( TRPRXN       )                                       
      PD( ROOH        ,XO2          ) =             RK( 057 ) * C( HO2          )                                       
      PD( ROOH        ,XO2N         ) =             RK( 058 ) * C( HO2          )                                       
      PD( ROOH        ,ROOH         ) = -           RK( 062 ) * C( OH           )                                       
     &                                  -           RK( 063 )                                                           
      PD( ROOH        ,TRPRXN       ) =             RK( 176 ) * C( HO2          )                                       
      PD( ROOH        ,NTRPX        ) =             RK( 259 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  19: ALD2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALD2        ,NO           ) =   0.27500 * RK( 067 ) * C( IEPXO2       )                                       
     &                                  +           RK( 106 ) * C( CXO3         )                                       
     &                                  + 0.15000 * RK( 248 ) * C( NTRMO2       )                                       
      PD( ALD2        ,O            ) = -           RK( 087 ) * C( ALD2         )                                       
     &                                  + 0.20000 * RK( 120 ) * C( OLE          )                                       
     &                                  + 1.24000 * RK( 128 ) * C( IOLE         )                                       
      PD( ALD2        ,O3           ) =   0.18000 * RK( 122 ) * C( OLE          )                                       
     &                                  + 0.65000 * RK( 130 ) * C( IOLE         )                                       
     &                                  + 0.02000 * RK( 170 ) * C( ISOP         )                                       
      PD( ALD2        ,NO3          ) = -           RK( 089 ) * C( ALD2         )                                       
     &                                  + 0.35000 * RK( 123 ) * C( OLE          )                                       
     &                                  + 1.18000 * RK( 131 ) * C( IOLE         )                                       
      PD( ALD2        ,OH           ) =   0.50000 * RK( 062 ) * C( ROOH         )                                       
     &                                  -           RK( 088 ) * C( ALD2         )                                       
     &                                  +           RK( 110 ) * C( PANX         )                                       
     &                                  + 0.06000 * RK( 116 ) * C( PAR          )                                       
     &                                  + 0.33000 * RK( 121 ) * C( OLE          )                                       
     &                                  + 1.30000 * RK( 129 ) * C( IOLE         )                                       
     &                                  + 0.25200 * RK( 169 ) * C( ISOP         )                                       
     &                                  + 0.95000 * RK( 182 ) * C( SULRXN       )                                       
     &                                  + 0.99100 * RK( 183 ) * C( ETOH         )                                       
     &                                  +           RK( 228 ) * C( ALKRXN       )                                       
      PD( ALD2        ,HO2          ) =   0.27500 * RK( 066 ) * C( IEPXO2       )                                       
      PD( ALD2        ,XO2          ) =   0.90000 * RK( 113 ) * C( CXO3         )                                       
     &                                  +           RK( 231 ) * C( NTRM         )                                       
      PD( ALD2        ,NTROH        ) =   0.33000 * RK( 256 )                                                           
      PD( ALD2        ,NTRALK       ) =   0.33000 * RK( 255 )                                                           
      PD( ALD2        ,ROOH         ) =   0.50000 * RK( 062 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 063 )                                                           
      PD( ALD2        ,ALD2         ) = -           RK( 087 ) * C( O            )                                       
     &                                  -           RK( 088 ) * C( OH           )                                       
     &                                  -           RK( 089 ) * C( NO3          )                                       
     &                                  -           RK( 090 )                                                           
     &                                  -           RK( 201 ) * C( CL2          )                                       
      PD( ALD2        ,IOLE         ) =   1.24000 * RK( 128 ) * C( O            )                                       
     &                                  + 1.30000 * RK( 129 ) * C( OH           )                                       
     &                                  + 0.65000 * RK( 130 ) * C( O3           )                                       
     &                                  + 1.18000 * RK( 131 ) * C( NO3          )                                       
     &                                  + 0.45000 * RK( 198 ) * C( CL2          )                                       
      PD( ALD2        ,IEPXO2       ) =   0.27500 * RK( 066 ) * C( HO2          )                                       
     &                                  + 0.27500 * RK( 067 ) * C( NO           )                                       
     &                                  + 0.22000 * RK( 068 ) * C( C2O3         )                                       
      PD( ALD2        ,PAR          ) =   0.06000 * RK( 116 ) * C( OH           )                                       
     &                                  + 0.06000 * RK( 194 ) * C( CL2          )                                       
      PD( ALD2        ,C2O3         ) =   0.22000 * RK( 068 ) * C( IEPXO2       )                                       
     &                                  +           RK( 115 ) * C( CXO3         )                                       
      PD( ALD2        ,MEO2         ) =   0.90000 * RK( 112 ) * C( CXO3         )                                       
      PD( ALD2        ,CXO3         ) =             RK( 106 ) * C( NO           )                                       
     &                                  + 0.90000 * RK( 112 ) * C( MEO2         )                                       
     &                                  + 0.90000 * RK( 113 ) * C( XO2          )                                       
     &                                  + 4.00000 * RK( 114 ) * C( CXO3         )                                       
     &                                  +           RK( 115 ) * C( C2O3         )                                       
     &                                  +           RK( 232 ) * C( NTRM         )                                       
      PD( ALD2        ,PANX         ) =   0.40000 * RK( 109 )                                                           
     &                                  +           RK( 110 ) * C( OH           )                                       
      PD( ALD2        ,ROR          ) =   0.60000 * RK( 117 )                                                           
      PD( ALD2        ,OLE          ) =   0.20000 * RK( 120 ) * C( O            )                                       
     &                                  + 0.33000 * RK( 121 ) * C( OH           )                                       
     &                                  + 0.18000 * RK( 122 ) * C( O3           )                                       
     &                                  + 0.35000 * RK( 123 ) * C( NO3          )                                       
     &                                  + 0.33000 * RK( 197 ) * C( CL2          )                                       
      PD( ALD2        ,ISOP         ) =   0.25200 * RK( 169 ) * C( OH           )                                       
     &                                  + 0.02000 * RK( 170 ) * C( O3           )                                       
     &                                  + 0.06700 * RK( 172 )                                                           
      PD( ALD2        ,ISOPRXN      ) =             RK( 253 )                                                           
      PD( ALD2        ,NTRM         ) =             RK( 231 ) * C( XO2          )                                       
     &                                  +           RK( 232 ) * C( CXO3         )                                       
      PD( ALD2        ,MACO3        ) =   0.33000 * RK( 254 )                                                           
      PD( ALD2        ,SULRXN       ) =   0.95000 * RK( 182 ) * C( OH           )                                       
     &                                  +           RK( 204 ) * C( CL2          )                                       
      PD( ALD2        ,ETOH         ) =   0.99100 * RK( 183 ) * C( OH           )                                       
     &                                  + 0.99100 * RK( 195 ) * C( CL2          )                                       
      PD( ALD2        ,CL2          ) =   0.06000 * RK( 194 ) * C( PAR          )                                       
     &                                  + 0.99100 * RK( 195 ) * C( ETOH         )                                       
     &                                  + 0.33000 * RK( 197 ) * C( OLE          )                                       
     &                                  + 0.45000 * RK( 198 ) * C( IOLE         )                                       
     &                                  -           RK( 201 ) * C( ALD2         )                                       
     &                                  +           RK( 204 ) * C( SULRXN       )                                       
      PD( ALD2        ,ALKRXN       ) =             RK( 228 ) * C( OH           )                                       
      PD( ALD2        ,NTRCN        ) =   0.33000 * RK( 257 )                                                           
      PD( ALD2        ,NTRCNOH      ) =   0.33000 * RK( 258 )                                                           
      PD( ALD2        ,NTRMO2       ) =   0.15000 * RK( 248 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  20: ALDX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALDX        ,NO2          ) =   0.80000 * RK( 184 ) * C( PAHRO2       )                                       
      PD( ALDX        ,NO           ) =             RK( 155 ) * C( CRPX         )                                       
      PD( ALDX        ,O            ) = -           RK( 102 ) * C( ALDX         )                                       
     &                                  + 0.30000 * RK( 120 ) * C( OLE          )                                       
     &                                  + 0.66000 * RK( 128 ) * C( IOLE         )                                       
     &                                  + 0.15000 * RK( 173 ) * C( NTRI         )                                       
      PD( ALDX        ,O3           ) =   0.32000 * RK( 122 ) * C( OLE          )                                       
     &                                  + 0.35000 * RK( 130 ) * C( IOLE         )                                       
     &                                  + 0.03000 * RK( 149 ) * C( TOLRO2       )                                       
     &                                  + 0.15000 * RK( 167 ) * C( PAHRO2       )                                       
     &                                  + 0.21000 * RK( 179 ) * C( NTRI         )                                       
      PD( ALDX        ,NO3          ) = -           RK( 104 ) * C( ALDX         )                                       
     &                                  + 0.56000 * RK( 123 ) * C( OLE          )                                       
     &                                  + 0.64000 * RK( 131 ) * C( IOLE         )                                       
     &                                  + 0.48000 * RK( 136 ) * C( TOL          )                                       
     &                                  + 0.80000 * RK( 168 ) * C( PAHRO2       )                                       
     &                                  + 0.35700 * RK( 171 ) * C( ISOP         )                                       
     &                                  + 0.47000 * RK( 180 ) * C( NTRI         )                                       
      PD( ALDX        ,OH           ) =   0.50000 * RK( 062 ) * C( ROOH         )                                       
     &                                  + 0.02900 * RK( 064 ) * C( ISOPX        )                                       
     &                                  -           RK( 103 ) * C( ALDX         )                                       
     &                                  + 0.05000 * RK( 116 ) * C( PAR          )                                       
     &                                  + 0.62000 * RK( 121 ) * C( OLE          )                                       
     &                                  + 0.22000 * RK( 125 ) * C( NALKO2       )                                       
     &                                  + 0.70000 * RK( 129 ) * C( IOLE         )                                       
     &                                  + 0.12000 * RK( 169 ) * C( ISOP         )                                       
     &                                  + 0.47000 * RK( 174 ) * C( NTRI         )                                       
     &                                  + 0.01000 * RK( 182 ) * C( SULRXN       )                                       
      PD( ALDX        ,NTROH        ) =   0.33000 * RK( 256 )                                                           
      PD( ALDX        ,NTRALK       ) =   0.33000 * RK( 255 )                                                           
      PD( ALDX        ,ROOH         ) =   0.50000 * RK( 062 ) * C( OH           )                                       
     &                                  + 0.50000 * RK( 063 )                                                           
      PD( ALDX        ,ALDX         ) = -           RK( 102 ) * C( O            )                                       
     &                                  -           RK( 103 ) * C( OH           )                                       
     &                                  -           RK( 104 ) * C( NO3          )                                       
     &                                  -           RK( 105 )                                                           
     &                                  -           RK( 202 ) * C( CL2          )                                       
      PD( ALDX        ,ISOPX        ) =   0.02900 * RK( 064 ) * C( OH           )                                       
      PD( ALDX        ,IOLE         ) =   0.66000 * RK( 128 ) * C( O            )                                       
     &                                  + 0.70000 * RK( 129 ) * C( OH           )                                       
     &                                  + 0.35000 * RK( 130 ) * C( O3           )                                       
     &                                  + 0.64000 * RK( 131 ) * C( NO3          )                                       
     &                                  + 0.55000 * RK( 198 ) * C( CL2          )                                       
      PD( ALDX        ,PAR          ) =   0.05000 * RK( 116 ) * C( OH           )                                       
     &                                  + 0.05000 * RK( 194 ) * C( CL2          )                                       
      PD( ALDX        ,ROR          ) =   0.50000 * RK( 117 )                                                           
      PD( ALDX        ,OLE          ) =   0.30000 * RK( 120 ) * C( O            )                                       
     &                                  + 0.62000 * RK( 121 ) * C( OH           )                                       
     &                                  + 0.32000 * RK( 122 ) * C( O3           )                                       
     &                                  + 0.56000 * RK( 123 ) * C( NO3          )                                       
     &                                  + 0.67000 * RK( 197 ) * C( CL2          )                                       
      PD( ALDX        ,NALKO2       ) =   0.22000 * RK( 125 ) * C( OH           )                                       
      PD( ALDX        ,TOL          ) =   0.48000 * RK( 136 ) * C( NO3          )                                       
      PD( ALDX        ,TOLRO2       ) =   0.03000 * RK( 149 ) * C( O3           )                                       
      PD( ALDX        ,CRPX         ) =             RK( 155 ) * C( NO           )                                       
      PD( ALDX        ,PAHRO2       ) =   0.15000 * RK( 167 ) * C( O3           )                                       
     &                                  + 0.80000 * RK( 168 ) * C( NO3          )                                       
     &                                  + 0.80000 * RK( 184 ) * C( NO2          )                                       
      PD( ALDX        ,ISOP         ) =   0.12000 * RK( 169 ) * C( OH           )                                       
     &                                  + 0.35700 * RK( 171 ) * C( NO3          )                                       
      PD( ALDX        ,MACO3        ) =   0.33000 * RK( 254 )                                                           
      PD( ALDX        ,NTRI         ) =   0.15000 * RK( 173 ) * C( O            )                                       
     &                                  + 0.47000 * RK( 174 ) * C( OH           )                                       
     &                                  + 0.21000 * RK( 179 ) * C( O3           )                                       
     &                                  + 0.47000 * RK( 180 ) * C( NO3          )                                       
      PD( ALDX        ,SULRXN       ) =   0.01000 * RK( 182 ) * C( OH           )                                       
      PD( ALDX        ,CL2          ) =   0.05000 * RK( 194 ) * C( PAR          )                                       
     &                                  + 0.67000 * RK( 197 ) * C( OLE          )                                       
     &                                  + 0.55000 * RK( 198 ) * C( IOLE         )                                       
     &                                  -           RK( 202 ) * C( ALDX         )                                       
      PD( ALDX        ,NTRCN        ) =   0.33000 * RK( 257 )                                                           
      PD( ALDX        ,NTRCNOH      ) =   0.33000 * RK( 258 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  21: ISOPX                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISOPX       ,OH           ) = -           RK( 064 ) * C( ISOPX        )                                       
      PD( ISOPX       ,HO2          ) =             RK( 166 ) * C( ISOPO2       )                                       
      PD( ISOPX       ,ISOPX        ) = -           RK( 064 ) * C( OH           )                                       
      PD( ISOPX       ,ISOPO2       ) =             RK( 166 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  22: IEPOX                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( IEPOX       ,OH           ) =   0.90400 * RK( 064 ) * C( ISOPX        )                                       
     &                                  -           RK( 065 ) * C( IEPOX        )                                       
      PD( IEPOX       ,ISOPX        ) =   0.90400 * RK( 064 ) * C( OH           )                                       
      PD( IEPOX       ,IEPOX        ) = -           RK( 065 ) * C( OH           )                                       
     &                                  -           RK( 276 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  23: ISOPO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISOPO2      ,NO           ) = -           RK( 164 ) * C( ISOPO2       )                                       
      PD( ISOPO2      ,OH           ) =   0.06700 * RK( 064 ) * C( ISOPX        )                                       
     &                                  +           RK( 163 ) * C( PAHRO2       )                                       
      PD( ISOPO2      ,HO2          ) = -           RK( 166 ) * C( ISOPO2       )                                       
      PD( ISOPO2      ,ISOPX        ) =   0.06700 * RK( 064 ) * C( OH           )                                       
      PD( ISOPO2      ,ISOPO2       ) = -           RK( 164 ) * C( NO           )                                       
     &                                  -           RK( 165 ) * C( C2O3         )                                       
     &                                  -           RK( 166 ) * C( HO2          )                                       
      PD( ISOPO2      ,C2O3         ) = -           RK( 165 ) * C( ISOPO2       )                                       
      PD( ISOPO2      ,PAHRO2       ) =             RK( 163 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  24: IOLE                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( IOLE        ,O            ) = -           RK( 128 ) * C( IOLE         )                                       
      PD( IOLE        ,O3           ) = -           RK( 130 ) * C( IOLE         )                                       
      PD( IOLE        ,NO3          ) = -           RK( 131 ) * C( IOLE         )                                       
      PD( IOLE        ,OH           ) =   0.02900 * RK( 064 ) * C( ISOPX        )                                       
     &                                  -           RK( 129 ) * C( IOLE         )                                       
      PD( IOLE        ,ISOPX        ) =   0.02900 * RK( 064 ) * C( OH           )                                       
      PD( IOLE        ,IOLE         ) = -           RK( 128 ) * C( O            )                                       
     &                                  -           RK( 129 ) * C( OH           )                                       
     &                                  -           RK( 130 ) * C( O3           )                                       
     &                                  -           RK( 131 ) * C( NO3          )                                       
     &                                  -           RK( 198 ) * C( CL2          )                                       
      PD( IOLE        ,ISOPRXN      ) =             RK( 253 )                                                           
      PD( IOLE        ,CL2          ) = -           RK( 198 ) * C( IOLE         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  25: IEPXO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( IEPXO2      ,NO           ) = -           RK( 067 ) * C( IEPXO2       )                                       
      PD( IEPXO2      ,OH           ) =             RK( 065 ) * C( IEPOX        )                                       
      PD( IEPXO2      ,HO2          ) = -           RK( 066 ) * C( IEPXO2       )                                       
      PD( IEPXO2      ,IEPOX        ) =             RK( 065 ) * C( OH           )                                       
      PD( IEPXO2      ,IEPXO2       ) = -           RK( 066 ) * C( HO2          )                                       
     &                                  -           RK( 067 ) * C( NO           )                                       
     &                                  -           RK( 068 ) * C( C2O3         )                                       
      PD( IEPXO2      ,C2O3         ) = -           RK( 068 ) * C( IEPXO2       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  26: MGLY                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MGLY        ,NO           ) =   0.27500 * RK( 067 ) * C( IEPXO2       )                                       
     &                                  + 0.52000 * RK( 133 ) * C( CRES         )                                       
      PD( MGLY        ,O3           ) =   0.20000 * RK( 149 ) * C( TOLRO2       )                                       
     &                                  + 0.85000 * RK( 170 ) * C( ISOP         )                                       
      PD( MGLY        ,NO3          ) =   0.24000 * RK( 136 ) * C( TOL          )                                       
      PD( MGLY        ,OH           ) =   0.80000 * RK( 158 ) * C( OPAN         )                                       
     &                                  + 0.80000 * RK( 159 ) * C( XYLRO2       )                                       
     &                                  -           RK( 160 ) * C( MGLY         )                                       
     &                                  + 0.16800 * RK( 169 ) * C( ISOP         )                                       
      PD( MGLY        ,HO2          ) =   0.27500 * RK( 066 ) * C( IEPXO2       )                                       
      PD( MGLY        ,IEPXO2       ) =   0.27500 * RK( 066 ) * C( HO2          )                                       
     &                                  + 0.27500 * RK( 067 ) * C( NO           )                                       
     &                                  + 0.22000 * RK( 068 ) * C( C2O3         )                                       
      PD( MGLY        ,MGLY         ) = -           RK( 160 ) * C( OH           )                                       
     &                                  -           RK( 161 )                                                           
      PD( MGLY        ,C2O3         ) =   0.22000 * RK( 068 ) * C( IEPXO2       )                                       
      PD( MGLY        ,TOL          ) =   0.24000 * RK( 136 ) * C( NO3          )                                       
      PD( MGLY        ,CRES         ) =   0.52000 * RK( 133 ) * C( NO           )                                       
      PD( MGLY        ,TOLRO2       ) =   0.20000 * RK( 149 ) * C( O3           )                                       
      PD( MGLY        ,OPAN         ) =   0.80000 * RK( 158 ) * C( OH           )                                       
      PD( MGLY        ,XYLRO2       ) =   0.80000 * RK( 159 ) * C( OH           )                                       
      PD( MGLY        ,ISOP         ) =   0.16800 * RK( 169 ) * C( OH           )                                       
     &                                  + 0.85000 * RK( 170 ) * C( O3           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  27: FORM                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FORM        ,NO           ) =   0.65000 * RK( 067 ) * C( IEPXO2       )                                       
     &                                  +           RK( 071 ) * C( MEO2         )                                       
     &                                  + 0.33600 * RK( 133 ) * C( CRES         )                                       
     &                                  + 0.34400 * RK( 153 ) * C( OPO3         )                                       
     &                                  + 0.67300 * RK( 164 ) * C( ISOPO2       )                                       
     &                                  +           RK( 224 ) * C( NTRM         )                                       
     &                                  + 0.55000 * RK( 248 ) * C( NTRMO2       )                                       
      PD( FORM        ,O            ) = -           RK( 080 ) * C( FORM         )                                       
     &                                  + 0.20000 * RK( 120 ) * C( OLE          )                                       
     &                                  +           RK( 124 ) * C( NALKO2       )                                       
     &                                  + 0.50000 * RK( 162 ) * C( PAHRO2       )                                       
      PD( FORM        ,O3           ) =   0.74000 * RK( 122 ) * C( OLE          )                                       
     &                                  +           RK( 126 ) * C( NALKO2       )                                       
     &                                  + 0.25000 * RK( 130 ) * C( IOLE         )                                       
     &                                  + 0.70000 * RK( 149 ) * C( TOLRO2       )                                       
     &                                  + 0.60000 * RK( 167 ) * C( PAHRO2       )                                       
     &                                  + 0.15000 * RK( 170 ) * C( ISOP         )                                       
     &                                  + 0.24000 * RK( 179 ) * C( NTRI         )                                       
      PD( FORM        ,NO3          ) = -           RK( 081 ) * C( FORM         )                                       
     &                                  +           RK( 123 ) * C( OLE          )                                       
     &                                  + 2.00000 * RK( 127 ) * C( NALKO2       )                                       
     &                                  + 0.24000 * RK( 136 ) * C( TOL          )                                       
     &                                  + 0.28200 * RK( 171 ) * C( ISOP         )                                       
      PD( FORM        ,OH           ) =             RK( 076 ) * C( MEOH         )                                       
     &                                  -           RK( 077 ) * C( FORM         )                                       
     &                                  + 0.80000 * RK( 121 ) * C( OLE          )                                       
     &                                  + 1.56000 * RK( 125 ) * C( NALKO2       )                                       
     &                                  + 0.06000 * RK( 135 ) * C( TOL          )                                       
     &                                  + 0.16700 * RK( 169 ) * C( ISOP         )                                       
     &                                  + 0.28000 * RK( 174 ) * C( NTRI         )                                       
     &                                  + 0.08000 * RK( 182 ) * C( SULRXN       )                                       
      PD( FORM        ,HO2          ) =   0.65000 * RK( 066 ) * C( IEPXO2       )                                       
     &                                  -           RK( 082 ) * C( FORM         )                                       
     &                                  + 0.44000 * RK( 229 ) * C( NTRM         )                                       
      PD( FORM        ,XO2          ) =   0.90000 * RK( 231 ) * C( NTRM         )                                       
      PD( FORM        ,NTROH        ) =   0.33000 * RK( 256 )                                                           
      PD( FORM        ,NTRALK       ) =   0.33000 * RK( 255 )                                                           
      PD( FORM        ,ISOPO2       ) =   0.67300 * RK( 164 ) * C( NO           )                                       
     &                                  + 0.59800 * RK( 165 ) * C( C2O3         )                                       
      PD( FORM        ,IOLE         ) =   0.25000 * RK( 130 ) * C( O3           )                                       
      PD( FORM        ,IEPXO2       ) =   0.65000 * RK( 066 ) * C( HO2          )                                       
     &                                  + 0.65000 * RK( 067 ) * C( NO           )                                       
     &                                  + 0.52000 * RK( 068 ) * C( C2O3         )                                       
      PD( FORM        ,FORM         ) = -           RK( 077 ) * C( OH           )                                       
     &                                  -           RK( 078 )                                                           
     &                                  -           RK( 079 )                                                           
     &                                  -           RK( 080 ) * C( O            )                                       
     &                                  -           RK( 081 ) * C( NO3          )                                       
     &                                  -           RK( 082 ) * C( HO2          )                                       
     &                                  -           RK( 200 ) * C( CL2          )                                       
      PD( FORM        ,C2O3         ) =   0.52000 * RK( 068 ) * C( IEPXO2       )                                       
     &                                  +           RK( 096 ) * C( MEO2         )                                       
     &                                  + 0.59800 * RK( 165 ) * C( ISOPO2       )                                       
     &                                  +           RK( 233 ) * C( NTRM         )                                       
      PD( FORM        ,MEO2         ) =             RK( 071 ) * C( NO           )                                       
     &                                  + 4.00000 * RK( 073 ) * C( MEO2         )                                       
     &                                  +           RK( 096 ) * C( C2O3         )                                       
     &                                  + 0.10000 * RK( 112 ) * C( CXO3         )                                       
     &                                  + 1.90000 * RK( 230 ) * C( NTRM         )                                       
      PD( FORM        ,MEPX         ) =             RK( 075 )                                                           
      PD( FORM        ,MEOH         ) =             RK( 076 ) * C( OH           )                                       
     &                                  +           RK( 203 ) * C( CL2          )                                       
      PD( FORM        ,HCO3         ) =             RK( 083 )                                                           
      PD( FORM        ,CXO3         ) =   0.10000 * RK( 112 ) * C( MEO2         )                                       
     &                                  +           RK( 232 ) * C( NTRM         )                                       
      PD( FORM        ,OLE          ) =   0.20000 * RK( 120 ) * C( O            )                                       
     &                                  + 0.80000 * RK( 121 ) * C( OH           )                                       
     &                                  + 0.74000 * RK( 122 ) * C( O3           )                                       
     &                                  +           RK( 123 ) * C( NO3          )                                       
      PD( FORM        ,NALKO2       ) =             RK( 124 ) * C( O            )                                       
     &                                  + 1.56000 * RK( 125 ) * C( OH           )                                       
     &                                  +           RK( 126 ) * C( O3           )                                       
     &                                  + 2.00000 * RK( 127 ) * C( NO3          )                                       
     &                                  +           RK( 196 ) * C( CL2          )                                       
      PD( FORM        ,TOL          ) =   0.06000 * RK( 135 ) * C( OH           )                                       
     &                                  + 0.24000 * RK( 136 ) * C( NO3          )                                       
      PD( FORM        ,CRES         ) =   0.33600 * RK( 133 ) * C( NO           )                                       
      PD( FORM        ,TOLRO2       ) =   0.70000 * RK( 149 ) * C( O3           )                                       
      PD( FORM        ,OPO3         ) =   0.34400 * RK( 153 ) * C( NO           )                                       
      PD( FORM        ,PAHRO2       ) =   0.50000 * RK( 162 ) * C( O            )                                       
     &                                  + 0.60000 * RK( 167 ) * C( O3           )                                       
      PD( FORM        ,ISOP         ) =   0.16700 * RK( 169 ) * C( OH           )                                       
     &                                  + 0.15000 * RK( 170 ) * C( O3           )                                       
     &                                  + 0.28200 * RK( 171 ) * C( NO3          )                                       
     &                                  + 0.90000 * RK( 172 )                                                           
      PD( FORM        ,NTRM         ) =             RK( 224 ) * C( NO           )                                       
     &                                  + 0.44000 * RK( 229 ) * C( HO2          )                                       
     &                                  + 1.90000 * RK( 230 ) * C( MEO2         )                                       
     &                                  + 0.90000 * RK( 231 ) * C( XO2          )                                       
     &                                  +           RK( 232 ) * C( CXO3         )                                       
     &                                  +           RK( 233 ) * C( C2O3         )                                       
      PD( FORM        ,MACO3        ) =   0.33000 * RK( 254 )                                                           
      PD( FORM        ,NTRI         ) =   0.28000 * RK( 174 ) * C( OH           )                                       
     &                                  + 0.24000 * RK( 179 ) * C( O3           )                                       
      PD( FORM        ,SULRXN       ) =   0.08000 * RK( 182 ) * C( OH           )                                       
      PD( FORM        ,CL2          ) =             RK( 196 ) * C( NALKO2       )                                       
     &                                  -           RK( 200 ) * C( FORM         )                                       
     &                                  +           RK( 203 ) * C( MEOH         )                                       
      PD( FORM        ,ALKRXN       ) =   0.40000 * RK( 227 )                                                           
      PD( FORM        ,NTRCN        ) =   0.33000 * RK( 257 )                                                           
      PD( FORM        ,NTRCNOH      ) =   0.33000 * RK( 258 )                                                           
      PD( FORM        ,NTRMO2       ) =   0.55000 * RK( 248 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  28: FACD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FACD        ,NO           ) =             RK( 084 ) * C( HCO3         )                                       
      PD( FACD        ,O3           ) =   0.37000 * RK( 126 ) * C( NALKO2       )                                       
      PD( FACD        ,OH           ) = -           RK( 086 ) * C( FACD         )                                       
      PD( FACD        ,HO2          ) =   0.07400 * RK( 066 ) * C( IEPXO2       )                                       
      PD( FACD        ,IEPXO2       ) =   0.07400 * RK( 066 ) * C( HO2          )                                       
      PD( FACD        ,FACD         ) = -           RK( 086 ) * C( OH           )                                       
      PD( FACD        ,HCO3         ) =             RK( 084 ) * C( NO           )                                       
      PD( FACD        ,NALKO2       ) =   0.37000 * RK( 126 ) * C( O3           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  29: CO                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CO          ,NO           ) =   0.25100 * RK( 067 ) * C( IEPXO2       )                                       
     &                                  + 0.33600 * RK( 133 ) * C( CRES         )                                       
     &                                  + 0.34400 * RK( 153 ) * C( OPO3         )                                       
      PD( CO          ,O            ) =             RK( 080 ) * C( FORM         )                                       
     &                                  + 0.20000 * RK( 120 ) * C( OLE          )                                       
     &                                  +           RK( 124 ) * C( NALKO2       )                                       
     &                                  + 0.10000 * RK( 128 ) * C( IOLE         )                                       
      PD( CO          ,O3           ) =   0.33000 * RK( 122 ) * C( OLE          )                                       
     &                                  + 0.63000 * RK( 126 ) * C( NALKO2       )                                       
     &                                  + 0.25000 * RK( 130 ) * C( IOLE         )                                       
     &                                  + 0.69000 * RK( 149 ) * C( TOLRO2       )                                       
     &                                  + 0.06600 * RK( 167 ) * C( PAHRO2       )                                       
     &                                  + 0.22500 * RK( 170 ) * C( ISOP         )                                       
     &                                  + 0.00100 * RK( 179 ) * C( NTRI         )                                       
      PD( CO          ,NO3          ) =             RK( 081 ) * C( FORM         )                                       
     &                                  + 0.24000 * RK( 136 ) * C( TOL          )                                       
     &                                  + 0.64300 * RK( 171 ) * C( ISOP         )                                       
      PD( CO          ,OH           ) = -           RK( 069 ) * C( CO           )                                       
     &                                  +           RK( 077 ) * C( FORM         )                                       
     &                                  + 0.06000 * RK( 135 ) * C( TOL          )                                       
     &                                  + 0.33400 * RK( 169 ) * C( ISOP         )                                       
     &                                  +           RK( 191 ) * C( CLO          )                                       
     &                                  +           RK( 228 ) * C( ALKRXN       )                                       
      PD( CO          ,HO2          ) =   0.25100 * RK( 066 ) * C( IEPXO2       )                                       
     &                                  + 0.29000 * RK( 229 ) * C( NTRM         )                                       
      PD( CO          ,XO2          ) =   0.60000 * RK( 231 ) * C( NTRM         )                                       
      PD( CO          ,ALD2         ) =             RK( 090 )                                                           
      PD( CO          ,ALDX         ) =             RK( 105 )                                                           
      PD( CO          ,IOLE         ) =   0.10000 * RK( 128 ) * C( O            )                                       
     &                                  + 0.25000 * RK( 130 ) * C( O3           )                                       
      PD( CO          ,IEPXO2       ) =   0.25100 * RK( 066 ) * C( HO2          )                                       
     &                                  + 0.25100 * RK( 067 ) * C( NO           )                                       
     &                                  + 0.20000 * RK( 068 ) * C( C2O3         )                                       
      PD( CO          ,MGLY         ) =             RK( 161 )                                                           
      PD( CO          ,FORM         ) =             RK( 077 ) * C( OH           )                                       
     &                                  +           RK( 078 )                                                           
     &                                  +           RK( 079 )                                                           
     &                                  +           RK( 080 ) * C( O            )                                       
     &                                  +           RK( 081 ) * C( NO3          )                                       
     &                                  +           RK( 200 ) * C( CL2          )                                       
      PD( CO          ,CO           ) = -           RK( 069 ) * C( OH           )                                       
      PD( CO          ,C2O3         ) =   0.20000 * RK( 068 ) * C( IEPXO2       )                                       
     &                                  + 0.65000 * RK( 233 ) * C( NTRM         )                                       
      PD( CO          ,CXO3         ) =   0.65000 * RK( 232 ) * C( NTRM         )                                       
      PD( CO          ,OLE          ) =   0.20000 * RK( 120 ) * C( O            )                                       
     &                                  + 0.33000 * RK( 122 ) * C( O3           )                                       
      PD( CO          ,NALKO2       ) =             RK( 124 ) * C( O            )                                       
     &                                  + 0.63000 * RK( 126 ) * C( O3           )                                       
      PD( CO          ,TOL          ) =   0.06000 * RK( 135 ) * C( OH           )                                       
     &                                  + 0.24000 * RK( 136 ) * C( NO3          )                                       
      PD( CO          ,CRES         ) =   0.33600 * RK( 133 ) * C( NO           )                                       
      PD( CO          ,TOLRO2       ) =             RK( 147 )                                                           
     &                                  + 0.69000 * RK( 149 ) * C( O3           )                                       
      PD( CO          ,OPO3         ) =   0.34400 * RK( 153 ) * C( NO           )                                       
      PD( CO          ,PAHRO2       ) =   0.06600 * RK( 167 ) * C( O3           )                                       
      PD( CO          ,ISOP         ) =   0.33400 * RK( 169 ) * C( OH           )                                       
     &                                  + 0.22500 * RK( 170 ) * C( O3           )                                       
     &                                  + 0.64300 * RK( 171 ) * C( NO3          )                                       
     &                                  + 0.33300 * RK( 172 )                                                           
      PD( CO          ,NTRM         ) =   0.29000 * RK( 229 ) * C( HO2          )                                       
     &                                  + 0.60000 * RK( 231 ) * C( XO2          )                                       
     &                                  + 0.65000 * RK( 232 ) * C( CXO3         )                                       
     &                                  + 0.65000 * RK( 233 ) * C( C2O3         )                                       
      PD( CO          ,NTRI         ) =   0.00100 * RK( 179 ) * C( O3           )                                       
      PD( CO          ,CL2          ) =             RK( 200 ) * C( FORM         )                                       
      PD( CO          ,CLO          ) =             RK( 191 ) * C( OH           )                                       
     &                                  +           RK( 192 )                                                           
      PD( CO          ,ALKRXN       ) =             RK( 228 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  30: PAR                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAR         ,NO2          ) =   2.40000 * RK( 184 ) * C( PAHRO2       )                                       
      PD( PAR         ,NO           ) =   2.45000 * RK( 067 ) * C( IEPXO2       )                                       
     &                                  + 0.43000 * RK( 248 ) * C( NTRMO2       )                                       
      PD( PAR         ,O            ) =   0.20000 * RK( 120 ) * C( OLE          )                                       
     &                                  + 0.10000 * RK( 128 ) * C( IOLE         )                                       
     &                                  + 0.25000 * RK( 162 ) * C( PAHRO2       )                                       
     &                                  + 5.12000 * RK( 173 ) * C( NTRI         )                                       
      PD( PAR         ,O3           ) = -           RK( 122 ) * C( OLE          )                                       
     &                                  + 0.35000 * RK( 167 ) * C( PAHRO2       )                                       
     &                                  + 0.36000 * RK( 170 ) * C( ISOP         )                                       
     &                                  + 7.00000 * RK( 179 ) * C( NTRI         )                                       
      PD( PAR         ,NO3          ) = -           RK( 123 ) * C( OLE          )                                       
     &                                  + 2.40000 * RK( 168 ) * C( PAHRO2       )                                       
     &                                  + 1.28200 * RK( 171 ) * C( ISOP         )                                       
      PD( PAR         ,OH           ) = - 1.11000 * RK( 116 ) * C( PAR          )                                       
     &                                  - 0.70000 * RK( 121 ) * C( OLE          )                                       
     &                                  + 1.10000 * RK( 158 ) * C( OPAN         )                                       
     &                                  + 1.10000 * RK( 159 ) * C( XYLRO2       )                                       
     &                                  + 1.56500 * RK( 169 ) * C( ISOP         )                                       
     &                                  + 1.66000 * RK( 174 ) * C( NTRI         )                                       
      PD( PAR         ,HO2          ) =   2.45000 * RK( 066 ) * C( IEPXO2       )                                       
      PD( PAR         ,NTROH        ) = - 0.66000 * RK( 256 )                                                           
      PD( PAR         ,NTRALK       ) = - 0.66000 * RK( 255 )                                                           
      PD( PAR         ,IOLE         ) =   0.10000 * RK( 128 ) * C( O            )                                       
     &                                  + 0.30000 * RK( 198 ) * C( CL2          )                                       
      PD( PAR         ,IEPXO2       ) =   2.45000 * RK( 066 ) * C( HO2          )                                       
     &                                  + 2.45000 * RK( 067 ) * C( NO           )                                       
     &                                  + 1.96000 * RK( 068 ) * C( C2O3         )                                       
      PD( PAR         ,PAR          ) = - 1.11000 * RK( 116 ) * C( OH           )                                       
     &                                  - 1.11000 * RK( 194 ) * C( CL2          )                                       
      PD( PAR         ,C2O3         ) =   1.96000 * RK( 068 ) * C( IEPXO2       )                                       
      PD( PAR         ,ROR          ) = - 2.10000 * RK( 117 )                                                           
      PD( PAR         ,OLE          ) =   0.20000 * RK( 120 ) * C( O            )                                       
     &                                  - 0.70000 * RK( 121 ) * C( OH           )                                       
     &                                  -           RK( 122 ) * C( O3           )                                       
     &                                  -           RK( 123 ) * C( NO3          )                                       
     &                                  -           RK( 197 ) * C( CL2          )                                       
      PD( PAR         ,OPAN         ) =   1.10000 * RK( 158 ) * C( OH           )                                       
      PD( PAR         ,XYLRO2       ) =   1.10000 * RK( 159 ) * C( OH           )                                       
      PD( PAR         ,PAHRO2       ) =   0.25000 * RK( 162 ) * C( O            )                                       
     &                                  + 0.35000 * RK( 167 ) * C( O3           )                                       
     &                                  + 2.40000 * RK( 168 ) * C( NO3          )                                       
     &                                  + 2.40000 * RK( 184 ) * C( NO2          )                                       
      PD( PAR         ,ISOP         ) =   1.56500 * RK( 169 ) * C( OH           )                                       
     &                                  + 0.36000 * RK( 170 ) * C( O3           )                                       
     &                                  + 1.28200 * RK( 171 ) * C( NO3          )                                       
     &                                  + 0.83200 * RK( 172 )                                                           
      PD( PAR         ,MACO3        ) = - 0.66000 * RK( 254 )                                                           
      PD( PAR         ,NTRI         ) =   5.12000 * RK( 173 ) * C( O            )                                       
     &                                  + 1.66000 * RK( 174 ) * C( OH           )                                       
     &                                  + 7.00000 * RK( 179 ) * C( O3           )                                       
      PD( PAR         ,CL2          ) = - 1.11000 * RK( 194 ) * C( PAR          )                                       
     &                                  -           RK( 197 ) * C( OLE          )                                       
     &                                  + 0.30000 * RK( 198 ) * C( IOLE         )                                       
      PD( PAR         ,NTRCN        ) = - 0.66000 * RK( 257 )                                                           
      PD( PAR         ,NTRCNOH      ) = - 0.66000 * RK( 258 )                                                           
      PD( PAR         ,NTRMO2       ) =   0.43000 * RK( 248 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  31: C2O3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( C2O3        ,NO2          ) = -           RK( 092 ) * C( C2O3         )                                       
      PD( C2O3        ,NO           ) = -           RK( 091 ) * C( C2O3         )                                       
      PD( C2O3        ,O            ) =             RK( 087 ) * C( ALD2         )                                       
      PD( C2O3        ,O3           ) =   0.11400 * RK( 170 ) * C( ISOP         )                                       
      PD( C2O3        ,NO3          ) =             RK( 089 ) * C( ALD2         )                                       
      PD( C2O3        ,OH           ) =             RK( 088 ) * C( ALD2         )                                       
     &                                  +           RK( 099 ) * C( PACD         )                                       
     &                                  +           RK( 160 ) * C( MGLY         )                                       
     &                                  + 0.33000 * RK( 169 ) * C( ISOP         )                                       
      PD( C2O3        ,HO2          ) = -           RK( 095 ) * C( C2O3         )                                       
     &                                  + 0.15000 * RK( 229 ) * C( NTRM         )                                       
      PD( C2O3        ,XO2          ) = -           RK( 097 ) * C( C2O3         )                                       
     &                                  + 0.30000 * RK( 231 ) * C( NTRM         )                                       
      PD( C2O3        ,ALD2         ) =             RK( 087 ) * C( O            )                                       
     &                                  +           RK( 088 ) * C( OH           )                                       
     &                                  +           RK( 089 ) * C( NO3          )                                       
     &                                  +           RK( 201 ) * C( CL2          )                                       
      PD( C2O3        ,ISOPO2       ) = -           RK( 165 ) * C( C2O3         )                                       
      PD( C2O3        ,IEPXO2       ) = -           RK( 068 ) * C( C2O3         )                                       
      PD( C2O3        ,MGLY         ) =             RK( 160 ) * C( OH           )                                       
     &                                  +           RK( 161 )                                                           
      PD( C2O3        ,C2O3         ) = -           RK( 068 ) * C( IEPXO2       )                                       
     &                                  -           RK( 091 ) * C( NO           )                                       
     &                                  -           RK( 092 ) * C( NO2          )                                       
     &                                  -           RK( 095 ) * C( HO2          )                                       
     &                                  -           RK( 096 ) * C( MEO2         )                                       
     &                                  -           RK( 097 ) * C( XO2          )                                       
     &                                  - 4.00000 * RK( 098 ) * C( C2O3         )                                       
     &                                  -           RK( 115 ) * C( CXO3         )                                       
     &                                  -           RK( 165 ) * C( ISOPO2       )                                       
     &                                  - 0.65000 * RK( 233 ) * C( NTRM         )                                       
      PD( C2O3        ,MEO2         ) = -           RK( 096 ) * C( C2O3         )                                       
     &                                  + 0.30000 * RK( 230 ) * C( NTRM         )                                       
      PD( C2O3        ,PAN          ) =             RK( 093 )                                                           
     &                                  + 0.60000 * RK( 094 )                                                           
      PD( C2O3        ,PACD         ) =             RK( 099 ) * C( OH           )                                       
      PD( C2O3        ,CXO3         ) = -           RK( 115 ) * C( C2O3         )                                       
     &                                  + 0.35000 * RK( 232 ) * C( NTRM         )                                       
      PD( C2O3        ,ISOP         ) =   0.33000 * RK( 169 ) * C( OH           )                                       
     &                                  + 0.11400 * RK( 170 ) * C( O3           )                                       
     &                                  + 0.26700 * RK( 172 )                                                           
      PD( C2O3        ,NTRM         ) =   0.15000 * RK( 229 ) * C( HO2          )                                       
     &                                  + 0.30000 * RK( 230 ) * C( MEO2         )                                       
     &                                  + 0.30000 * RK( 231 ) * C( XO2          )                                       
     &                                  + 0.35000 * RK( 232 ) * C( CXO3         )                                       
     &                                  - 0.65000 * RK( 233 ) * C( C2O3         )                                       
      PD( C2O3        ,CL2          ) =             RK( 201 ) * C( ALD2         )                                       
      PD( C2O3        ,ALKRXN       ) =   0.40000 * RK( 227 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  32: MEO2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEO2        ,NO           ) = -           RK( 071 ) * C( MEO2         )                                       
     &                                  +           RK( 091 ) * C( C2O3         )                                       
     &                                  +           RK( 224 ) * C( NTRM         )                                       
      PD( MEO2        ,OH           ) =             RK( 070 )                                                           
     &                                  + 0.70000 * RK( 074 ) * C( MEPX         )                                       
     &                                  +           RK( 101 ) * C( AACD         )                                       
      PD( MEO2        ,HO2          ) = -           RK( 072 ) * C( MEO2         )                                       
     &                                  + 0.44000 * RK( 095 ) * C( C2O3         )                                       
     &                                  + 0.29000 * RK( 229 ) * C( NTRM         )                                       
      PD( MEO2        ,XO2          ) =   0.90000 * RK( 097 ) * C( C2O3         )                                       
     &                                  + 0.60000 * RK( 231 ) * C( NTRM         )                                       
      PD( MEO2        ,ALD2         ) =             RK( 090 )                                                           
      PD( MEO2        ,ALDX         ) =             RK( 105 )                                                           
      PD( MEO2        ,ISOPO2       ) =   0.80000 * RK( 165 ) * C( C2O3         )                                       
      PD( MEO2        ,IEPXO2       ) =   0.80000 * RK( 068 ) * C( C2O3         )                                       
      PD( MEO2        ,C2O3         ) =   0.80000 * RK( 068 ) * C( IEPXO2       )                                       
     &                                  +           RK( 091 ) * C( NO           )                                       
     &                                  + 0.44000 * RK( 095 ) * C( HO2          )                                       
     &                                  - 0.10000 * RK( 096 ) * C( MEO2         )                                       
     &                                  + 0.90000 * RK( 097 ) * C( XO2          )                                       
     &                                  + 4.00000 * RK( 098 ) * C( C2O3         )                                       
     &                                  +           RK( 115 ) * C( CXO3         )                                       
     &                                  + 0.80000 * RK( 165 ) * C( ISOPO2       )                                       
     &                                  + 1.65000 * RK( 233 ) * C( NTRM         )                                       
      PD( MEO2        ,MEO2         ) = -           RK( 071 ) * C( NO           )                                       
     &                                  -           RK( 072 ) * C( HO2          )                                       
     &                                  - 4.00000 * RK( 073 ) * C( MEO2         )                                       
     &                                  - 0.10000 * RK( 096 ) * C( C2O3         )                                       
     &                                  -           RK( 112 ) * C( CXO3         )                                       
     &                                  - 0.40000 * RK( 230 ) * C( NTRM         )                                       
      PD( MEO2        ,AACD         ) =             RK( 101 ) * C( OH           )                                       
      PD( MEO2        ,MEPX         ) =   0.70000 * RK( 074 ) * C( OH           )                                       
      PD( MEO2        ,PAN          ) =   0.40000 * RK( 094 )                                                           
      PD( MEO2        ,PACD         ) =             RK( 100 )                                                           
      PD( MEO2        ,CXO3         ) = -           RK( 112 ) * C( MEO2         )                                       
     &                                  +           RK( 115 ) * C( C2O3         )                                       
      PD( MEO2        ,NTRM         ) =             RK( 224 ) * C( NO           )                                       
     &                                  + 0.29000 * RK( 229 ) * C( HO2          )                                       
     &                                  - 0.40000 * RK( 230 ) * C( MEO2         )                                       
     &                                  + 0.60000 * RK( 231 ) * C( XO2          )                                       
     &                                  + 1.65000 * RK( 233 ) * C( C2O3         )                                       
      PD( MEO2        ,CL2          ) =             RK( 193 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  33: AACD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AACD        ,OH           ) = -           RK( 101 ) * C( AACD         )                                       
      PD( AACD        ,HO2          ) =   0.15000 * RK( 095 ) * C( C2O3         )                                       
     &                                  + 0.15000 * RK( 111 ) * C( CXO3         )                                       
     &                                  + 0.15000 * RK( 229 ) * C( NTRM         )                                       
      PD( AACD        ,XO2          ) =   0.10000 * RK( 097 ) * C( C2O3         )                                       
     &                                  + 0.10000 * RK( 113 ) * C( CXO3         )                                       
     &                                  + 0.10000 * RK( 231 ) * C( NTRM         )                                       
      PD( AACD        ,ISOPO2       ) =   0.20000 * RK( 165 ) * C( C2O3         )                                       
      PD( AACD        ,IEPXO2       ) =   0.20000 * RK( 068 ) * C( C2O3         )                                       
      PD( AACD        ,C2O3         ) =   0.20000 * RK( 068 ) * C( IEPXO2       )                                       
     &                                  + 0.15000 * RK( 095 ) * C( HO2          )                                       
     &                                  + 0.10000 * RK( 096 ) * C( MEO2         )                                       
     &                                  + 0.10000 * RK( 097 ) * C( XO2          )                                       
     &                                  + 0.20000 * RK( 165 ) * C( ISOPO2       )                                       
      PD( AACD        ,MEO2         ) =   0.10000 * RK( 096 ) * C( C2O3         )                                       
     &                                  + 0.10000 * RK( 112 ) * C( CXO3         )                                       
     &                                  + 0.10000 * RK( 230 ) * C( NTRM         )                                       
      PD( AACD        ,AACD         ) = -           RK( 101 ) * C( OH           )                                       
      PD( AACD        ,CXO3         ) =   0.15000 * RK( 111 ) * C( HO2          )                                       
     &                                  + 0.10000 * RK( 112 ) * C( MEO2         )                                       
     &                                  + 0.10000 * RK( 113 ) * C( XO2          )                                       
      PD( AACD        ,NTRM         ) =   0.15000 * RK( 229 ) * C( HO2          )                                       
     &                                  + 0.10000 * RK( 230 ) * C( MEO2         )                                       
     &                                  + 0.10000 * RK( 231 ) * C( XO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  34: MEPX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEPX        ,OH           ) = -           RK( 074 ) * C( MEPX         )                                       
      PD( MEPX        ,HO2          ) =             RK( 072 ) * C( MEO2         )                                       
     &                                  +           RK( 085 ) * C( HCO3         )                                       
      PD( MEPX        ,MEO2         ) =             RK( 072 ) * C( HO2          )                                       
      PD( MEPX        ,MEPX         ) = -           RK( 074 ) * C( OH           )                                       
     &                                  -           RK( 075 )                                                           
      PD( MEPX        ,HCO3         ) =             RK( 085 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  35: MEOH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEOH        ,OH           ) = -           RK( 076 ) * C( MEOH         )                                       
      PD( MEOH        ,MEO2         ) =   4.00000 * RK( 073 ) * C( MEO2         )                                       
      PD( MEOH        ,MEOH         ) = -           RK( 076 ) * C( OH           )                                       
     &                                  -           RK( 203 ) * C( CL2          )                                       
      PD( MEOH        ,CL2          ) = -           RK( 203 ) * C( MEOH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  36: HCO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HCO3        ,NO           ) = -           RK( 084 ) * C( HCO3         )                                       
      PD( HCO3        ,HO2          ) =             RK( 082 ) * C( FORM         )                                       
     &                                  -           RK( 085 ) * C( HCO3         )                                       
      PD( HCO3        ,FORM         ) =             RK( 082 ) * C( HO2          )                                       
      PD( HCO3        ,HCO3         ) = -           RK( 083 )                                                           
     &                                  -           RK( 084 ) * C( NO           )                                       
     &                                  -           RK( 085 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  37: PAN                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAN         ,NO2          ) =             RK( 092 ) * C( C2O3         )                                       
      PD( PAN         ,C2O3         ) =             RK( 092 ) * C( NO2          )                                       
      PD( PAN         ,PAN          ) = -           RK( 093 )                                                           
     &                                  -           RK( 094 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  38: PACD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PACD        ,OH           ) = -           RK( 099 ) * C( PACD         )                                       
      PD( PACD        ,HO2          ) =   0.41000 * RK( 095 ) * C( C2O3         )                                       
     &                                  + 0.41000 * RK( 111 ) * C( CXO3         )                                       
     &                                  + 0.41000 * RK( 229 ) * C( NTRM         )                                       
      PD( PACD        ,C2O3         ) =   0.41000 * RK( 095 ) * C( HO2          )                                       
      PD( PACD        ,PACD         ) = -           RK( 099 ) * C( OH           )                                       
     &                                  -           RK( 100 )                                                           
      PD( PACD        ,CXO3         ) =   0.41000 * RK( 111 ) * C( HO2          )                                       
      PD( PACD        ,NTRM         ) =   0.41000 * RK( 229 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  39: CXO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CXO3        ,NO2          ) = -           RK( 107 ) * C( CXO3         )                                       
      PD( CXO3        ,NO           ) = -           RK( 106 ) * C( CXO3         )                                       
      PD( CXO3        ,O            ) =             RK( 102 ) * C( ALDX         )                                       
     &                                  + 0.25000 * RK( 162 ) * C( PAHRO2       )                                       
      PD( CXO3        ,O3           ) =   0.39000 * RK( 179 ) * C( NTRI         )                                       
      PD( CXO3        ,NO3          ) =             RK( 104 ) * C( ALDX         )                                       
      PD( CXO3        ,OH           ) =             RK( 103 ) * C( ALDX         )                                       
      PD( CXO3        ,HO2          ) = -           RK( 111 ) * C( CXO3         )                                       
      PD( CXO3        ,XO2          ) = -           RK( 113 ) * C( CXO3         )                                       
      PD( CXO3        ,ALDX         ) =             RK( 102 ) * C( O            )                                       
     &                                  +           RK( 103 ) * C( OH           )                                       
     &                                  +           RK( 104 ) * C( NO3          )                                       
     &                                  +           RK( 202 ) * C( CL2          )                                       
      PD( CXO3        ,C2O3         ) = -           RK( 115 ) * C( CXO3         )                                       
      PD( CXO3        ,MEO2         ) = -           RK( 112 ) * C( CXO3         )                                       
      PD( CXO3        ,CXO3         ) = -           RK( 106 ) * C( NO           )                                       
     &                                  -           RK( 107 ) * C( NO2          )                                       
     &                                  -           RK( 111 ) * C( HO2          )                                       
     &                                  -           RK( 112 ) * C( MEO2         )                                       
     &                                  -           RK( 113 ) * C( XO2          )                                       
     &                                  - 4.00000 * RK( 114 ) * C( CXO3         )                                       
     &                                  -           RK( 115 ) * C( C2O3         )                                       
     &                                  -           RK( 232 ) * C( NTRM         )                                       
      PD( CXO3        ,PANX         ) =             RK( 108 )                                                           
     &                                  + 0.60000 * RK( 109 )                                                           
      PD( CXO3        ,PAHRO2       ) =   0.25000 * RK( 162 ) * C( O            )                                       
      PD( CXO3        ,NTRM         ) = -           RK( 232 ) * C( CXO3         )                                       
      PD( CXO3        ,NTRI         ) =   0.39000 * RK( 179 ) * C( O3           )                                       
      PD( CXO3        ,CL2          ) =             RK( 202 ) * C( ALDX         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  40: PANX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PANX        ,NO2          ) =             RK( 107 ) * C( CXO3         )                                       
      PD( PANX        ,OH           ) = -           RK( 110 ) * C( PANX         )                                       
      PD( PANX        ,CXO3         ) =             RK( 107 ) * C( NO2          )                                       
      PD( PANX        ,PANX         ) = -           RK( 108 )                                                           
     &                                  -           RK( 109 )                                                           
     &                                  -           RK( 110 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  41: ROR                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ROR         ,NO2          ) = -           RK( 119 ) * C( ROR          )                                       
      PD( ROR         ,OH           ) =   0.76000 * RK( 116 ) * C( PAR          )                                       
      PD( ROR         ,PAR          ) =   0.76000 * RK( 116 ) * C( OH           )                                       
     &                                  + 0.76000 * RK( 194 ) * C( CL2          )                                       
      PD( ROR         ,ROR          ) = - 0.98000 * RK( 117 )                                                           
     &                                  -           RK( 118 )                                                           
     &                                  -           RK( 119 ) * C( NO2          )                                       
      PD( ROR         ,CL2          ) =   0.76000 * RK( 194 ) * C( PAR          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  42: OLE                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OLE         ,O            ) = -           RK( 120 ) * C( OLE          )                                       
      PD( OLE         ,O3           ) = -           RK( 122 ) * C( OLE          )                                       
      PD( OLE         ,NO3          ) = -           RK( 123 ) * C( OLE          )                                       
      PD( OLE         ,OH           ) = -           RK( 121 ) * C( OLE          )                                       
      PD( OLE         ,IOLE         ) =   0.30000 * RK( 198 ) * C( CL2          )                                       
      PD( OLE         ,OLE          ) = -           RK( 120 ) * C( O            )                                       
     &                                  -           RK( 121 ) * C( OH           )                                       
     &                                  -           RK( 122 ) * C( O3           )                                       
     &                                  -           RK( 123 ) * C( NO3          )                                       
     &                                  -           RK( 197 ) * C( CL2          )                                       
      PD( OLE         ,CL2          ) = -           RK( 197 ) * C( OLE          )                                       
     &                                  + 0.30000 * RK( 198 ) * C( IOLE         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  43: NALKO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NALKO2      ,O            ) = -           RK( 124 ) * C( NALKO2       )                                       
      PD( NALKO2      ,O3           ) = -           RK( 126 ) * C( NALKO2       )                                       
      PD( NALKO2      ,NO3          ) = -           RK( 127 ) * C( NALKO2       )                                       
      PD( NALKO2      ,OH           ) = -           RK( 125 ) * C( NALKO2       )                                       
      PD( NALKO2      ,NALKO2       ) = -           RK( 124 ) * C( O            )                                       
     &                                  -           RK( 125 ) * C( OH           )                                       
     &                                  -           RK( 126 ) * C( O3           )                                       
     &                                  -           RK( 127 ) * C( NO3          )                                       
     &                                  -           RK( 196 ) * C( CL2          )                                       
      PD( NALKO2      ,CL2          ) = -           RK( 196 ) * C( NALKO2       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  44: ETH                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETH         ,OH           ) = -           RK( 132 ) * C( ETH          )                                       
      PD( ETH         ,ETH          ) = -           RK( 132 ) * C( OH           )                                       
     &                                  -           RK( 206 ) * C( CL2          )                                       
      PD( ETH         ,CL2          ) = -           RK( 206 ) * C( ETH          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  45: TOL                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOL         ,NO3          ) = -           RK( 136 ) * C( TOL          )                                       
      PD( TOL         ,OH           ) =   0.18000 * RK( 132 ) * C( ETH          )                                       
     &                                  -           RK( 135 ) * C( TOL          )                                       
     &                                  + 0.20000 * RK( 158 ) * C( OPAN         )                                       
     &                                  + 0.20000 * RK( 159 ) * C( XYLRO2       )                                       
      PD( TOL         ,HO2          ) =             RK( 138 ) * C( OPEN         )                                       
      PD( TOL         ,ETH          ) =   0.18000 * RK( 132 ) * C( OH           )                                       
      PD( TOL         ,TOL          ) = -           RK( 135 ) * C( OH           )                                       
     &                                  -           RK( 136 ) * C( NO3          )                                       
      PD( TOL         ,OPEN         ) =             RK( 138 ) * C( HO2          )                                       
      PD( TOL         ,OPAN         ) =   0.20000 * RK( 158 ) * C( OH           )                                       
      PD( TOL         ,XYLRO2       ) =   0.20000 * RK( 159 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  46: CRES                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRES        ,NO           ) = -           RK( 133 ) * C( CRES         )                                       
      PD( CRES        ,OH           ) =   0.65000 * RK( 132 ) * C( ETH          )                                       
     &                                  + 0.30000 * RK( 158 ) * C( OPAN         )                                       
     &                                  + 0.30000 * RK( 159 ) * C( XYLRO2       )                                       
      PD( CRES        ,HO2          ) = -           RK( 134 ) * C( CRES         )                                       
      PD( CRES        ,ETH          ) =   0.65000 * RK( 132 ) * C( OH           )                                       
      PD( CRES        ,CRES         ) = -           RK( 133 ) * C( NO           )                                       
     &                                  -           RK( 134 ) * C( HO2          )                                       
      PD( CRES        ,OPAN         ) =   0.30000 * RK( 158 ) * C( OH           )                                       
      PD( CRES        ,XYLRO2       ) =   0.30000 * RK( 159 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  47: TO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TO2         ,NO           ) = -           RK( 211 ) * C( TO2          )                                       
      PD( TO2         ,OH           ) =             RK( 132 ) * C( ETH          )                                       
      PD( TO2         ,HO2          ) = -           RK( 212 ) * C( TO2          )                                       
      PD( TO2         ,ETH          ) =             RK( 132 ) * C( OH           )                                       
      PD( TO2         ,TO2          ) = -           RK( 211 ) * C( NO           )                                       
     &                                  -           RK( 212 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  48: TOLRO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLRO2      ,NO           ) =   0.86000 * RK( 133 ) * C( CRES         )                                       
      PD( TOLRO2      ,O3           ) = -           RK( 149 ) * C( TOLRO2       )                                       
      PD( TOLRO2      ,NO3          ) =   0.12000 * RK( 136 ) * C( TOL          )                                       
     &                                  -           RK( 150 ) * C( TOLRO2       )                                       
      PD( TOLRO2      ,OH           ) =   0.13000 * RK( 135 ) * C( TOL          )                                       
     &                                  -           RK( 148 ) * C( TOLRO2       )                                       
      PD( TOLRO2      ,TOL          ) =   0.13000 * RK( 135 ) * C( OH           )                                       
     &                                  + 0.12000 * RK( 136 ) * C( NO3          )                                       
      PD( TOLRO2      ,CRES         ) =   0.86000 * RK( 133 ) * C( NO           )                                       
      PD( TOLRO2      ,TOLRO2       ) = -           RK( 147 )                                                           
     &                                  -           RK( 148 ) * C( OH           )                                       
     &                                  -           RK( 149 ) * C( O3           )                                       
     &                                  -           RK( 150 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  49: OPEN                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OPEN        ,NO2          ) = -           RK( 137 ) * C( OPEN         )                                       
      PD( OPEN        ,NO3          ) =   0.30000 * RK( 136 ) * C( TOL          )                                       
     &                                  +           RK( 152 ) * C( CRO          )                                       
      PD( OPEN        ,OH           ) =   0.06000 * RK( 135 ) * C( TOL          )                                       
      PD( OPEN        ,HO2          ) = -           RK( 138 ) * C( OPEN         )                                       
      PD( OPEN        ,TOL          ) =   0.06000 * RK( 135 ) * C( OH           )                                       
     &                                  + 0.30000 * RK( 136 ) * C( NO3          )                                       
      PD( OPEN        ,OPEN         ) = -           RK( 137 ) * C( NO2          )                                       
     &                                  -           RK( 138 ) * C( HO2          )                                       
      PD( OPEN        ,CRO          ) =             RK( 152 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  50: CRO                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRO         ,NO3          ) = -           RK( 152 ) * C( CRO          )                                       
      PD( CRO         ,OH           ) =   0.73200 * RK( 135 ) * C( TOL          )                                       
     &                                  -           RK( 151 ) * C( CRO          )                                       
      PD( CRO         ,TOL          ) =   0.73200 * RK( 135 ) * C( OH           )                                       
      PD( CRO         ,CRO          ) = -           RK( 151 ) * C( OH           )                                       
     &                                  -           RK( 152 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  51: CAT1                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CAT1        ,NO2          ) =             RK( 137 ) * C( OPEN         )                                       
      PD( CAT1        ,NO3          ) = -           RK( 140 ) * C( CAT1         )                                       
      PD( CAT1        ,OH           ) = -           RK( 139 ) * C( CAT1         )                                       
      PD( CAT1        ,OPEN         ) =             RK( 137 ) * C( NO2          )                                       
      PD( CAT1        ,CAT1         ) = -           RK( 139 ) * C( OH           )                                       
     &                                  -           RK( 140 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  52: CRON                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRON        ,NO2          ) = -           RK( 141 ) * C( CRON         )                                       
      PD( CRON        ,NO           ) =             RK( 143 ) * C( CRNO         )                                       
      PD( CRON        ,O3           ) = -           RK( 142 ) * C( CRON         )                                       
      PD( CRON        ,NO3          ) =             RK( 140 ) * C( CAT1         )                                       
      PD( CRON        ,OH           ) =             RK( 139 ) * C( CAT1         )                                       
      PD( CRON        ,CAT1         ) =             RK( 139 ) * C( OH           )                                       
     &                                  +           RK( 140 ) * C( NO3          )                                       
      PD( CRON        ,CRON         ) = -           RK( 141 ) * C( NO2          )                                       
     &                                  -           RK( 142 ) * C( O3           )                                       
      PD( CRON        ,CRNO         ) =             RK( 143 ) * C( NO           )                                       
      PD( CRON        ,CRN2         ) =             RK( 145 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  53: CRNO                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRNO        ,NO           ) = -           RK( 143 ) * C( CRNO         )                                       
      PD( CRNO        ,O3           ) =             RK( 142 ) * C( CRON         )                                       
      PD( CRNO        ,OH           ) =             RK( 146 ) * C( CRN2         )                                       
      PD( CRNO        ,HO2          ) = -           RK( 144 ) * C( CRNO         )                                       
      PD( CRNO        ,CRON         ) =             RK( 142 ) * C( O3           )                                       
      PD( CRNO        ,CRNO         ) = -           RK( 143 ) * C( NO           )                                       
     &                                  -           RK( 144 ) * C( HO2          )                                       
      PD( CRNO        ,CRN2         ) =             RK( 146 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  54: CRN2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRN2        ,OH           ) = -           RK( 146 ) * C( CRN2         )                                       
      PD( CRN2        ,HO2          ) =             RK( 144 ) * C( CRNO         )                                       
      PD( CRN2        ,CRNO         ) =             RK( 144 ) * C( HO2          )                                       
      PD( CRN2        ,CRN2         ) = -           RK( 145 )                                                           
     &                                  -           RK( 146 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  55: CRPX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRPX        ,NO2          ) = -           RK( 156 ) * C( CRPX         )                                       
      PD( CRPX        ,NO           ) = -           RK( 155 ) * C( CRPX         )                                       
      PD( CRPX        ,O3           ) =   0.62000 * RK( 149 ) * C( TOLRO2       )                                       
      PD( CRPX        ,NO3          ) =             RK( 150 ) * C( TOLRO2       )                                       
      PD( CRPX        ,OH           ) =   0.60000 * RK( 148 ) * C( TOLRO2       )                                       
      PD( CRPX        ,TOLRO2       ) =             RK( 147 )                                                           
     &                                  + 0.60000 * RK( 148 ) * C( OH           )                                       
     &                                  + 0.62000 * RK( 149 ) * C( O3           )                                       
     &                                  +           RK( 150 ) * C( NO3          )                                       
      PD( CRPX        ,CRPX         ) = -           RK( 155 ) * C( NO           )                                       
     &                                  -           RK( 156 ) * C( NO2          )                                       
      PD( CRPX        ,CAO2         ) =             RK( 157 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  56: OPO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OPO3        ,NO           ) = -           RK( 153 ) * C( OPO3         )                                       
      PD( OPO3        ,OH           ) =   0.40000 * RK( 148 ) * C( TOLRO2       )                                       
     &                                  +           RK( 151 ) * C( CRO          )                                       
      PD( OPO3        ,HO2          ) = -           RK( 154 ) * C( OPO3         )                                       
      PD( OPO3        ,TOLRO2       ) =   0.40000 * RK( 148 ) * C( OH           )                                       
      PD( OPO3        ,CRO          ) =             RK( 151 ) * C( OH           )                                       
      PD( OPO3        ,OPO3         ) = -           RK( 153 ) * C( NO           )                                       
     &                                  -           RK( 154 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  57: CAO2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CAO2        ,NO2          ) =             RK( 156 ) * C( CRPX         )                                       
      PD( CAO2        ,CRPX         ) =             RK( 156 ) * C( NO2          )                                       
      PD( CAO2        ,CAO2         ) = -           RK( 157 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  58: OPAN                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OPAN        ,OH           ) = -           RK( 158 ) * C( OPAN         )                                       
      PD( OPAN        ,OPAN         ) = -           RK( 158 ) * C( OH           )                                       
     &                                  -           RK( 207 ) * C( CL2          )                                       
      PD( OPAN        ,CL2          ) = -           RK( 207 ) * C( OPAN         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  59: XYLMN                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLMN       ,NO           ) = -           RK( 213 ) * C( XYLMN        )                                       
      PD( XYLMN       ,OH           ) =             RK( 158 ) * C( OPAN         )                                       
      PD( XYLMN       ,HO2          ) = -           RK( 214 ) * C( XYLMN        )                                       
      PD( XYLMN       ,OPAN         ) =             RK( 158 ) * C( OH           )                                       
      PD( XYLMN       ,XYLMN        ) = -           RK( 213 ) * C( NO           )                                       
     &                                  -           RK( 214 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  60: XYLRO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLRO2      ,OH           ) = -           RK( 159 ) * C( XYLRO2       )                                       
      PD( XYLRO2      ,XYLRO2       ) = -           RK( 159 ) * C( OH           )                                       
     &                                  -           RK( 208 ) * C( CL2          )                                       
      PD( XYLRO2      ,CL2          ) = -           RK( 208 ) * C( XYLRO2       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  61: NAPH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NAPH        ,NO           ) = -           RK( 221 ) * C( NAPH         )                                       
      PD( NAPH        ,OH           ) =             RK( 159 ) * C( XYLRO2       )                                       
      PD( NAPH        ,HO2          ) = -           RK( 222 ) * C( NAPH         )                                       
      PD( NAPH        ,XYLRO2       ) =             RK( 159 ) * C( OH           )                                       
      PD( NAPH        ,NAPH         ) = -           RK( 221 ) * C( NO           )                                       
     &                                  -           RK( 222 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  62: PAHRO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAHRO2      ,NO2          ) = -           RK( 184 ) * C( PAHRO2       )                                       
      PD( PAHRO2      ,O            ) = -           RK( 162 ) * C( PAHRO2       )                                       
      PD( PAHRO2      ,O3           ) = -           RK( 167 ) * C( PAHRO2       )                                       
      PD( PAHRO2      ,NO3          ) = -           RK( 168 ) * C( PAHRO2       )                                       
      PD( PAHRO2      ,OH           ) = -           RK( 163 ) * C( PAHRO2       )                                       
      PD( PAHRO2      ,PAHRO2       ) = -           RK( 162 ) * C( O            )                                       
     &                                  -           RK( 163 ) * C( OH           )                                       
     &                                  -           RK( 167 ) * C( O3           )                                       
     &                                  -           RK( 168 ) * C( NO3          )                                       
     &                                  -           RK( 184 ) * C( NO2          )                                       
     &                                  -           RK( 199 ) * C( CL2          )                                       
      PD( PAHRO2      ,CL2          ) = -           RK( 199 ) * C( PAHRO2       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  63: ISOP                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISOP        ,NO2          ) =   0.20000 * RK( 184 ) * C( PAHRO2       )                                       
      PD( ISOP        ,NO           ) =   0.90000 * RK( 164 ) * C( ISOPO2       )                                       
      PD( ISOP        ,O            ) =   0.75000 * RK( 162 ) * C( PAHRO2       )                                       
      PD( ISOP        ,O3           ) =   0.65000 * RK( 167 ) * C( PAHRO2       )                                       
     &                                  -           RK( 170 ) * C( ISOP         )                                       
      PD( ISOP        ,NO3          ) =   0.20000 * RK( 168 ) * C( PAHRO2       )                                       
     &                                  -           RK( 171 ) * C( ISOP         )                                       
      PD( ISOP        ,OH           ) = -           RK( 169 ) * C( ISOP         )                                       
      PD( ISOP        ,ISOPO2       ) =   0.90000 * RK( 164 ) * C( NO           )                                       
     &                                  +           RK( 165 ) * C( C2O3         )                                       
      PD( ISOP        ,C2O3         ) =             RK( 165 ) * C( ISOPO2       )                                       
      PD( ISOP        ,PAHRO2       ) =   0.75000 * RK( 162 ) * C( O            )                                       
     &                                  + 0.65000 * RK( 167 ) * C( O3           )                                       
     &                                  + 0.20000 * RK( 168 ) * C( NO3          )                                       
     &                                  + 0.20000 * RK( 184 ) * C( NO2          )                                       
     &                                  +           RK( 199 ) * C( CL2          )                                       
      PD( ISOP        ,ISOP         ) = -           RK( 169 ) * C( OH           )                                       
     &                                  -           RK( 170 ) * C( O3           )                                       
     &                                  -           RK( 171 ) * C( NO3          )                                       
     &                                  -           RK( 172 )                                                           
      PD( ISOP        ,CL2          ) =             RK( 199 ) * C( PAHRO2       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  64: ISPD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISPD        ,NO3          ) =             RK( 168 ) * C( PAHRO2       )                                       
      PD( ISPD        ,OH           ) =             RK( 163 ) * C( PAHRO2       )                                       
      PD( ISPD        ,PAHRO2       ) =             RK( 163 ) * C( OH           )                                       
     &                                  +           RK( 168 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  65: ISOPRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISOPRXN     ,NO           ) =   0.10000 * RK( 164 ) * C( ISOPO2       )                                       
     &                                  +           RK( 175 ) * C( TRPRXN       )                                       
      PD( ISOPRXN     ,NO3          ) =   0.80000 * RK( 168 ) * C( PAHRO2       )                                       
     &                                  + 0.53000 * RK( 180 ) * C( NTRI         )                                       
      PD( ISOPRXN     ,OH           ) = -           RK( 247 ) * C( ISOPRXN      )                                       
      PD( ISOPRXN     ,ISOPO2       ) =   0.10000 * RK( 164 ) * C( NO           )                                       
      PD( ISOPRXN     ,PAHRO2       ) =   0.80000 * RK( 168 ) * C( NO3          )                                       
      PD( ISOPRXN     ,ISOPRXN      ) = -           RK( 247 ) * C( OH           )                                       
     &                                  -           RK( 253 )                                                           
     &                                  -           RK( 265 )                                                           
      PD( ISOPRXN     ,NTRI         ) =   0.53000 * RK( 180 ) * C( NO3          )                                       
      PD( ISOPRXN     ,TRPRXN       ) =             RK( 175 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  66: NTRM                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTRM        ,NO2          ) = -           RK( 225 ) * C( NTRM         )                                       
      PD( NTRM        ,NO           ) = -           RK( 224 ) * C( NTRM         )                                       
      PD( NTRM        ,O3           ) =   0.20000 * RK( 167 ) * C( PAHRO2       )                                       
      PD( NTRM        ,NO3          ) =   0.07500 * RK( 171 ) * C( ISOP         )                                       
      PD( NTRM        ,OH           ) =   0.13000 * RK( 169 ) * C( ISOP         )                                       
      PD( NTRM        ,HO2          ) = -           RK( 229 ) * C( NTRM         )                                       
      PD( NTRM        ,XO2          ) = -           RK( 231 ) * C( NTRM         )                                       
      PD( NTRM        ,C2O3         ) = -           RK( 233 ) * C( NTRM         )                                       
      PD( NTRM        ,MEO2         ) = -           RK( 230 ) * C( NTRM         )                                       
      PD( NTRM        ,CXO3         ) = -           RK( 232 ) * C( NTRM         )                                       
      PD( NTRM        ,PAHRO2       ) =   0.20000 * RK( 167 ) * C( O3           )                                       
      PD( NTRM        ,ISOP         ) =   0.13000 * RK( 169 ) * C( OH           )                                       
     &                                  + 0.07500 * RK( 171 ) * C( NO3          )                                       
     &                                  + 0.70000 * RK( 172 )                                                           
      PD( NTRM        ,NTRM         ) = -           RK( 224 ) * C( NO           )                                       
     &                                  -           RK( 225 ) * C( NO2          )                                       
     &                                  -           RK( 229 ) * C( HO2          )                                       
     &                                  -           RK( 230 ) * C( MEO2         )                                       
     &                                  -           RK( 231 ) * C( XO2          )                                       
     &                                  -           RK( 232 ) * C( CXO3         )                                       
     &                                  -           RK( 233 ) * C( C2O3         )                                       
      PD( NTRM        ,ALKRXN       ) =             RK( 226 )                                                           
     &                                  + 0.60000 * RK( 227 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  67: MACO3                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MACO3       ,NO           ) =   1.15000 * RK( 248 ) * C( NTRMO2       )                                       
     &                                  + 0.60000 * RK( 251 ) * C( NTRIO2       )                                       
      PD( MACO3       ,NO3          ) =   0.85000 * RK( 171 ) * C( ISOP         )                                       
      PD( MACO3       ,OH           ) = -           RK( 250 ) * C( MACO3        )                                       
      PD( MACO3       ,HO2          ) =             RK( 249 ) * C( NTRMO2       )                                       
     &                                  +           RK( 252 ) * C( NTRIO2       )                                       
      PD( MACO3       ,ISOP         ) =   0.85000 * RK( 171 ) * C( NO3          )                                       
      PD( MACO3       ,MACO3        ) = -           RK( 250 ) * C( OH           )                                       
     &                                  -           RK( 254 )                                                           
     &                                  -           RK( 266 )                                                           
      PD( MACO3       ,NTRMO2       ) =   1.15000 * RK( 248 ) * C( NO           )                                       
     &                                  +           RK( 249 ) * C( HO2          )                                       
      PD( MACO3       ,NTRIO2       ) =   0.60000 * RK( 251 ) * C( NO           )                                       
     &                                  +           RK( 252 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  68: NTRI                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTRI        ,O            ) = -           RK( 173 ) * C( NTRI         )                                       
      PD( NTRI        ,O3           ) = -           RK( 179 ) * C( NTRI         )                                       
      PD( NTRI        ,NO3          ) = -           RK( 180 ) * C( NTRI         )                                       
      PD( NTRI        ,OH           ) = -           RK( 174 ) * C( NTRI         )                                       
      PD( NTRI        ,NTRI         ) = -           RK( 173 ) * C( O            )                                       
     &                                  -           RK( 174 ) * C( OH           )                                       
     &                                  -           RK( 179 ) * C( O3           )                                       
     &                                  -           RK( 180 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  69: TERP                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TERP        ,O            ) =             RK( 173 ) * C( NTRI         )                                       
      PD( TERP        ,O3           ) =             RK( 179 ) * C( NTRI         )                                       
      PD( TERP        ,NO3          ) =             RK( 180 ) * C( NTRI         )                                       
      PD( TERP        ,OH           ) =             RK( 174 ) * C( NTRI         )                                       
      PD( TERP        ,NTRI         ) =             RK( 173 ) * C( O            )                                       
     &                                  +           RK( 174 ) * C( OH           )                                       
     &                                  +           RK( 179 ) * C( O3           )                                       
     &                                  +           RK( 180 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  70: TRPRXN                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TRPRXN      ,NO           ) = -           RK( 175 ) * C( TRPRXN       )                                       
      PD( TRPRXN      ,O3           ) =   0.18000 * RK( 179 ) * C( NTRI         )                                       
      PD( TRPRXN      ,NO3          ) =   0.25000 * RK( 180 ) * C( NTRI         )                                       
      PD( TRPRXN      ,OH           ) =   0.25000 * RK( 174 ) * C( NTRI         )                                       
      PD( TRPRXN      ,HO2          ) = -           RK( 176 ) * C( TRPRXN       )                                       
      PD( TRPRXN      ,XO2          ) = -           RK( 178 ) * C( TRPRXN       )                                       
      PD( TRPRXN      ,NTRI         ) =   0.25000 * RK( 174 ) * C( OH           )                                       
     &                                  + 0.18000 * RK( 179 ) * C( O3           )                                       
     &                                  + 0.25000 * RK( 180 ) * C( NO3          )                                       
      PD( TRPRXN      ,TRPRXN       ) = -           RK( 175 ) * C( NO           )                                       
     &                                  -           RK( 176 ) * C( HO2          )                                       
     &                                  - 4.00000 * RK( 177 ) * C( TRPRXN       )                                       
     &                                  -           RK( 178 ) * C( XO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  71: XO2T                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XO2T        ,OH           ) = -           RK( 181 ) * C( XO2T         )                                       
      PD( XO2T        ,XO2T         ) = -           RK( 181 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  72: SO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SO2         ,OH           ) =             RK( 181 ) * C( XO2T         )                                       
      PD( SO2         ,XO2T         ) =             RK( 181 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  73: SULF                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SULF        ,OH           ) =             RK( 181 ) * C( XO2T         )                                       
      PD( SULF        ,XO2T         ) =             RK( 181 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  74: SULRXN                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SULRXN      ,OH           ) = -           RK( 182 ) * C( SULRXN       )                                       
      PD( SULRXN      ,SULRXN       ) = -           RK( 182 ) * C( OH           )                                       
     &                                  -           RK( 204 ) * C( CL2          )                                       
      PD( SULRXN      ,CL2          ) = -           RK( 204 ) * C( SULRXN       )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  75: ETOH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETOH        ,OH           ) = -           RK( 183 ) * C( ETOH         )                                       
      PD( ETOH        ,ETOH         ) = -           RK( 183 ) * C( OH           )                                       
     &                                  -           RK( 195 ) * C( CL2          )                                       
      PD( ETOH        ,CL2          ) = -           RK( 195 ) * C( ETOH         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  76: ETHA                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETHA        ,ETHA         ) = -           RK( 185 )                                                           
      PD( ETHA        ,HOCL         ) =   4.00000 * RK( 188 ) * C( HOCL         )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  77: CL2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CL2         ,NO2          ) = -           RK( 209 ) * C( CL2          )                                       
      PD( CL2         ,NO           ) =             RK( 189 ) * C( HOCL         )                                       
      PD( CL2         ,O3           ) = -           RK( 187 ) * C( CL2          )                                       
      PD( CL2         ,OH           ) =             RK( 191 ) * C( CLO          )                                       
     &                                  +           RK( 205 ) * C( FMCL         )                                       
      PD( CL2         ,ALD2         ) = -           RK( 201 ) * C( CL2          )                                       
      PD( CL2         ,ALDX         ) = -           RK( 202 ) * C( CL2          )                                       
      PD( CL2         ,IOLE         ) = -           RK( 198 ) * C( CL2          )                                       
      PD( CL2         ,FORM         ) = -           RK( 200 ) * C( CL2          )                                       
      PD( CL2         ,PAR          ) = -           RK( 194 ) * C( CL2          )                                       
      PD( CL2         ,MEOH         ) = -           RK( 203 ) * C( CL2          )                                       
      PD( CL2         ,OLE          ) = -           RK( 197 ) * C( CL2          )                                       
      PD( CL2         ,NALKO2       ) = -           RK( 196 ) * C( CL2          )                                       
      PD( CL2         ,ETH          ) = -           RK( 206 ) * C( CL2          )                                       
      PD( CL2         ,OPAN         ) = -           RK( 207 ) * C( CL2          )                                       
      PD( CL2         ,XYLRO2       ) = -           RK( 208 ) * C( CL2          )                                       
      PD( CL2         ,PAHRO2       ) = -           RK( 199 ) * C( CL2          )                                       
      PD( CL2         ,SULRXN       ) = -           RK( 204 ) * C( CL2          )                                       
      PD( CL2         ,ETOH         ) = -           RK( 195 ) * C( CL2          )                                       
      PD( CL2         ,ETHA         ) =   2.00000 * RK( 185 )                                                           
      PD( CL2         ,CL2          ) = -           RK( 187 ) * C( O3           )                                       
     &                                  -           RK( 193 )                                                           
     &                                  -           RK( 194 ) * C( PAR          )                                       
     &                                  -           RK( 195 ) * C( ETOH         )                                       
     &                                  -           RK( 196 ) * C( NALKO2       )                                       
     &                                  -           RK( 197 ) * C( OLE          )                                       
     &                                  -           RK( 198 ) * C( IOLE         )                                       
     &                                  -           RK( 199 ) * C( PAHRO2       )                                       
     &                                  -           RK( 200 ) * C( FORM         )                                       
     &                                  -           RK( 201 ) * C( ALD2         )                                       
     &                                  -           RK( 202 ) * C( ALDX         )                                       
     &                                  -           RK( 203 ) * C( MEOH         )                                       
     &                                  -           RK( 204 ) * C( SULRXN       )                                       
     &                                  -           RK( 206 ) * C( ETH          )                                       
     &                                  -           RK( 207 ) * C( OPAN         )                                       
     &                                  -           RK( 208 ) * C( XYLRO2       )                                       
     &                                  -           RK( 209 ) * C( NO2          )                                       
      PD( CL2         ,CL           ) =             RK( 186 )                                                           
      PD( CL2         ,HOCL         ) =   4.00000 * RK( 188 ) * C( HOCL         )                                       
     &                                  +           RK( 189 ) * C( NO           )                                       
      PD( CL2         ,CLO          ) =             RK( 191 ) * C( OH           )                                       
     &                                  +           RK( 192 )                                                           
      PD( CL2         ,FMCL         ) =             RK( 205 ) * C( OH           )                                       
      PD( CL2         ,HCL          ) =             RK( 210 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  78: CL                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CL          ,HO2          ) =             RK( 190 ) * C( HOCL         )                                       
      PD( CL          ,CL           ) = -           RK( 186 )                                                           
      PD( CL          ,HOCL         ) =             RK( 190 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  79: HOCL                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HOCL        ,NO           ) = -           RK( 189 ) * C( HOCL         )                                       
      PD( HOCL        ,O3           ) =             RK( 187 ) * C( CL2          )                                       
      PD( HOCL        ,HO2          ) = -           RK( 190 ) * C( HOCL         )                                       
      PD( HOCL        ,CL2          ) =             RK( 187 ) * C( O3           )                                       
      PD( HOCL        ,HOCL         ) = - 4.00000 * RK( 188 ) * C( HOCL         )                                       
     &                                  -           RK( 189 ) * C( NO           )                                       
     &                                  -           RK( 190 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  80: CLO                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLO         ,OH           ) = -           RK( 191 ) * C( CLO          )                                       
      PD( CLO         ,IOLE         ) =   0.70000 * RK( 198 ) * C( CL2          )                                       
      PD( CLO         ,OLE          ) =             RK( 197 ) * C( CL2          )                                       
      PD( CLO         ,NALKO2       ) =             RK( 196 ) * C( CL2          )                                       
      PD( CLO         ,PAHRO2       ) =   0.85000 * RK( 199 ) * C( CL2          )                                       
      PD( CLO         ,CL2          ) =             RK( 196 ) * C( NALKO2       )                                       
     &                                  +           RK( 197 ) * C( OLE          )                                       
     &                                  + 0.70000 * RK( 198 ) * C( IOLE         )                                       
     &                                  + 0.85000 * RK( 199 ) * C( PAHRO2       )                                       
      PD( CLO         ,CLO          ) = -           RK( 191 ) * C( OH           )                                       
     &                                  -           RK( 192 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  81: FMCL                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FMCL        ,OH           ) = -           RK( 205 ) * C( FMCL         )                                       
      PD( FMCL        ,ALD2         ) =             RK( 201 ) * C( CL2          )                                       
      PD( FMCL        ,ALDX         ) =             RK( 202 ) * C( CL2          )                                       
      PD( FMCL        ,IOLE         ) =   0.30000 * RK( 198 ) * C( CL2          )                                       
      PD( FMCL        ,FORM         ) =             RK( 200 ) * C( CL2          )                                       
      PD( FMCL        ,PAR          ) =             RK( 194 ) * C( CL2          )                                       
      PD( FMCL        ,MEOH         ) =             RK( 203 ) * C( CL2          )                                       
      PD( FMCL        ,ETH          ) =             RK( 206 ) * C( CL2          )                                       
      PD( FMCL        ,OPAN         ) =             RK( 207 ) * C( CL2          )                                       
      PD( FMCL        ,XYLRO2       ) =             RK( 208 ) * C( CL2          )                                       
      PD( FMCL        ,PAHRO2       ) =   0.15000 * RK( 199 ) * C( CL2          )                                       
      PD( FMCL        ,SULRXN       ) =             RK( 204 ) * C( CL2          )                                       
      PD( FMCL        ,ETOH         ) =             RK( 195 ) * C( CL2          )                                       
      PD( FMCL        ,CL2          ) =             RK( 193 )                                                           
     &                                  +           RK( 194 ) * C( PAR          )                                       
     &                                  +           RK( 195 ) * C( ETOH         )                                       
     &                                  + 0.30000 * RK( 198 ) * C( IOLE         )                                       
     &                                  + 0.15000 * RK( 199 ) * C( PAHRO2       )                                       
     &                                  +           RK( 200 ) * C( FORM         )                                       
     &                                  +           RK( 201 ) * C( ALD2         )                                       
     &                                  +           RK( 202 ) * C( ALDX         )                                       
     &                                  +           RK( 203 ) * C( MEOH         )                                       
     &                                  +           RK( 204 ) * C( SULRXN       )                                       
     &                                  +           RK( 206 ) * C( ETH          )                                       
     &                                  +           RK( 207 ) * C( OPAN         )                                       
     &                                  +           RK( 208 ) * C( XYLRO2       )                                       
      PD( FMCL        ,FMCL         ) = -           RK( 205 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  82: HCL                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HCL         ,NO2          ) =             RK( 209 ) * C( CL2          )                                       
      PD( HCL         ,CL2          ) =             RK( 209 ) * C( NO2          )                                       
      PD( HCL         ,HCL          ) = -           RK( 210 )                                                           
      PD( HCL         ,H2NO3PIJ     ) =             RK( 271 ) * C( FORM_PRIMARY )                                       
     &                                  +           RK( 272 ) * C( ALD2_PRIMARY )                                       
      PD( HCL         ,H2NO3PK      ) =             RK( 273 ) * C( BUTADIENE13  )                                       
      PD( HCL         ,FORM_PRIMARY ) =             RK( 271 ) * C( H2NO3PIJ     )                                       
      PD( HCL         ,ALD2_PRIMARY ) =             RK( 272 ) * C( H2NO3PIJ     )                                       
      PD( HCL         ,BUTADIENE13  ) =             RK( 273 ) * C( H2NO3PK      )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  83: CLNO2                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLNO2       ,NO           ) =             RK( 211 ) * C( TO2          )                                       
      PD( CLNO2       ,TO2          ) =             RK( 211 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  84: TOLNRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLNRXN     ,HO2          ) =             RK( 212 ) * C( TO2          )                                       
      PD( TOLNRXN     ,TO2          ) =             RK( 212 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  85: TOLHRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLHRXN     ,NO           ) =             RK( 213 ) * C( XYLMN        )                                       
      PD( TOLHRXN     ,XYLMN        ) =             RK( 213 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  86: XYLNRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLNRXN     ,HO2          ) =             RK( 214 ) * C( XYLMN        )                                       
      PD( XYLNRXN     ,XYLMN        ) =             RK( 214 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  87: XYLHRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLHRXN     ,OH           ) = -           RK( 215 ) * C( XYLHRXN      )                                       
      PD( XYLHRXN     ,XYLHRXN      ) = -           RK( 215 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  88: BENZENE                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BENZENE     ,NO           ) = -           RK( 216 ) * C( BENZENE      )                                       
      PD( BENZENE     ,OH           ) =             RK( 215 ) * C( XYLHRXN      )                                       
      PD( BENZENE     ,HO2          ) = -           RK( 217 ) * C( BENZENE      )                                       
      PD( BENZENE     ,XYLHRXN      ) =             RK( 215 ) * C( OH           )                                       
      PD( BENZENE     ,BENZENE      ) = -           RK( 216 ) * C( NO           )                                       
     &                                  -           RK( 217 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  89: BENZRO2                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BENZRO2     ,NO           ) =             RK( 216 ) * C( BENZENE      )                                       
      PD( BENZRO2     ,BENZENE      ) =             RK( 216 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  90: BNZNRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BNZNRXN     ,HO2          ) =             RK( 217 ) * C( BENZENE      )                                       
      PD( BNZNRXN     ,BENZENE      ) =             RK( 217 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  91: BNZHRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BNZHRXN     ,O3           ) = -           RK( 218 ) * C( BNZHRXN      )                                       
      PD( BNZHRXN     ,NO3          ) = -           RK( 220 ) * C( BNZHRXN      )                                       
      PD( BNZHRXN     ,OH           ) = -           RK( 219 ) * C( BNZHRXN      )                                       
      PD( BNZHRXN     ,BNZHRXN      ) = -           RK( 218 ) * C( O3           )                                       
     &                                  -           RK( 219 ) * C( OH           )                                       
     &                                  -           RK( 220 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  92: SESQ                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SESQ        ,O3           ) =             RK( 218 ) * C( BNZHRXN      )                                       
      PD( SESQ        ,NO3          ) =             RK( 220 ) * C( BNZHRXN      )                                       
      PD( SESQ        ,OH           ) =             RK( 219 ) * C( BNZHRXN      )                                       
      PD( SESQ        ,BNZHRXN      ) =             RK( 218 ) * C( O3           )                                       
     &                                  +           RK( 219 ) * C( OH           )                                       
     &                                  +           RK( 220 ) * C( NO3          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  93: SESQRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SESQRXN     ,NO           ) =             RK( 221 ) * C( NAPH         )                                       
      PD( SESQRXN     ,NAPH         ) =             RK( 221 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  94: PAHNRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAHNRXN     ,HO2          ) =             RK( 222 ) * C( NAPH         )                                       
      PD( PAHNRXN     ,NAPH         ) =             RK( 222 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  95: PAHHRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAHHRXN     ,OH           ) = -           RK( 223 ) * C( PAHHRXN      )                                       
      PD( PAHHRXN     ,PAHHRXN      ) = -           RK( 223 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  96: SOAALK                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SOAALK      ,OH           ) =   0.47000 * RK( 223 ) * C( PAHHRXN      )                                       
      PD( SOAALK      ,PAHHRXN      ) =   0.47000 * RK( 223 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  97: ALKRXN                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALKRXN      ,NO2          ) =             RK( 225 ) * C( NTRM         )                                       
      PD( ALKRXN      ,OH           ) = -           RK( 228 ) * C( ALKRXN       )                                       
      PD( ALKRXN      ,NTRM         ) =             RK( 225 ) * C( NO2          )                                       
      PD( ALKRXN      ,ALKRXN       ) = -           RK( 226 )                                                           
     &                                  -           RK( 227 )                                                           
     &                                  -           RK( 228 ) * C( OH           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  98: MAPAN                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MAPAN       ,NO           ) = -           RK( 235 ) * C( MAPAN        )                                       
      PD( MAPAN       ,OH           ) =             RK( 234 ) * C( NTRALK       )                                       
      PD( MAPAN       ,HO2          ) = -           RK( 236 ) * C( MAPAN        )                                       
      PD( MAPAN       ,NTRALK       ) =             RK( 234 ) * C( OH           )                                       
      PD( MAPAN       ,MAPAN        ) = -           RK( 235 ) * C( NO           )                                       
     &                                  -           RK( 236 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  99: NTRCN                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTRCN       ,NO           ) =   0.15000 * RK( 235 ) * C( MAPAN        )                                       
     &                                  + 0.53000 * RK( 238 ) * C( NOHO2        )                                       
     &                                  + 0.21000 * RK( 241 ) * C( NCNO2        )                                       
     &                                  + 0.17000 * RK( 244 ) * C( NCNOHO2      )                                       
      PD( NTRCN       ,OH           ) = -           RK( 240 ) * C( NTRCN        )                                       
     &                                  +           RK( 246 ) * C( NTRPX        )                                       
      PD( NTRCN       ,MAPAN        ) =   0.15000 * RK( 235 ) * C( NO           )                                       
      PD( NTRCN       ,NTRCN        ) = -           RK( 240 ) * C( OH           )                                       
     &                                  -           RK( 257 )                                                           
     &                                  -           RK( 262 )                                                           
      PD( NTRCN       ,NTRPX        ) =             RK( 246 ) * C( OH           )                                       
      PD( NTRCN       ,NOHO2        ) =   0.53000 * RK( 238 ) * C( NO           )                                       
      PD( NTRCN       ,NCNO2        ) =   0.21000 * RK( 241 ) * C( NO           )                                       
      PD( NTRCN       ,NCNOHO2      ) =   0.17000 * RK( 244 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 100: NTRCNOH                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTRCNOH     ,NO           ) =   0.55000 * RK( 235 ) * C( MAPAN        )                                       
     &                                  + 0.25000 * RK( 238 ) * C( NOHO2        )                                       
     &                                  + 0.26000 * RK( 241 ) * C( NCNO2        )                                       
     &                                  + 0.59000 * RK( 244 ) * C( NCNOHO2      )                                       
      PD( NTRCNOH     ,OH           ) = -           RK( 243 ) * C( NTRCNOH      )                                       
      PD( NTRCNOH     ,MAPAN        ) =   0.55000 * RK( 235 ) * C( NO           )                                       
      PD( NTRCNOH     ,NTRCNOH      ) = -           RK( 243 ) * C( OH           )                                       
     &                                  -           RK( 258 )                                                           
     &                                  -           RK( 263 )                                                           
      PD( NTRCNOH     ,NOHO2        ) =   0.25000 * RK( 238 ) * C( NO           )                                       
      PD( NTRCNOH     ,NCNO2        ) =   0.26000 * RK( 241 ) * C( NO           )                                       
      PD( NTRCNOH     ,NCNOHO2      ) =   0.59000 * RK( 244 ) * C( NO           )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 101: NTRPX                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTRPX       ,OH           ) = -           RK( 246 ) * C( NTRPX        )                                       
      PD( NTRPX       ,HO2          ) =             RK( 236 ) * C( MAPAN        )                                       
     &                                  +           RK( 239 ) * C( NOHO2        )                                       
     &                                  +           RK( 242 ) * C( NCNO2        )                                       
     &                                  +           RK( 245 ) * C( NCNOHO2      )                                       
      PD( NTRPX       ,MAPAN        ) =             RK( 236 ) * C( HO2          )                                       
      PD( NTRPX       ,NTRPX        ) = -           RK( 246 ) * C( OH           )                                       
     &                                  -           RK( 259 )                                                           
     &                                  -           RK( 264 )                                                           
      PD( NTRPX       ,NOHO2        ) =             RK( 239 ) * C( HO2          )                                       
      PD( NTRPX       ,NCNO2        ) =             RK( 242 ) * C( HO2          )                                       
      PD( NTRPX       ,NCNOHO2      ) =             RK( 245 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 102: NOHO2                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NOHO2       ,NO           ) = -           RK( 238 ) * C( NOHO2        )                                       
      PD( NOHO2       ,OH           ) =             RK( 237 ) * C( NTROH        )                                       
      PD( NOHO2       ,HO2          ) = -           RK( 239 ) * C( NOHO2        )                                       
      PD( NOHO2       ,NTROH        ) =             RK( 237 ) * C( OH           )                                       
      PD( NOHO2       ,NOHO2        ) = -           RK( 238 ) * C( NO           )                                       
     &                                  -           RK( 239 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 103: NCNO2                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NCNO2       ,NO           ) = -           RK( 241 ) * C( NCNO2        )                                       
      PD( NCNO2       ,OH           ) =             RK( 240 ) * C( NTRCN        )                                       
      PD( NCNO2       ,HO2          ) = -           RK( 242 ) * C( NCNO2        )                                       
      PD( NCNO2       ,NTRCN        ) =             RK( 240 ) * C( OH           )                                       
      PD( NCNO2       ,NCNO2        ) = -           RK( 241 ) * C( NO           )                                       
     &                                  -           RK( 242 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 104: NCNOHO2                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NCNOHO2     ,NO           ) = -           RK( 244 ) * C( NCNOHO2      )                                       
      PD( NCNOHO2     ,OH           ) =             RK( 243 ) * C( NTRCNOH      )                                       
      PD( NCNOHO2     ,HO2          ) = -           RK( 245 ) * C( NCNOHO2      )                                       
      PD( NCNOHO2     ,NTRCNOH      ) =             RK( 243 ) * C( OH           )                                       
      PD( NCNOHO2     ,NCNOHO2      ) = -           RK( 244 ) * C( NO           )                                       
     &                                  -           RK( 245 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 105: NTRMO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTRMO2      ,NO           ) = -           RK( 248 ) * C( NTRMO2       )                                       
      PD( NTRMO2      ,OH           ) =             RK( 247 ) * C( ISOPRXN      )                                       
      PD( NTRMO2      ,HO2          ) = -           RK( 249 ) * C( NTRMO2       )                                       
      PD( NTRMO2      ,ISOPRXN      ) =             RK( 247 ) * C( OH           )                                       
      PD( NTRMO2      ,NTRMO2       ) = -           RK( 248 ) * C( NO           )                                       
     &                                  -           RK( 249 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 106: NTRIO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTRIO2      ,NO           ) = -           RK( 251 ) * C( NTRIO2       )                                       
      PD( NTRIO2      ,OH           ) =             RK( 250 ) * C( MACO3        )                                       
      PD( NTRIO2      ,HO2          ) = -           RK( 252 ) * C( NTRIO2       )                                       
      PD( NTRIO2      ,MACO3        ) =             RK( 250 ) * C( OH           )                                       
      PD( NTRIO2      ,NTRIO2       ) = -           RK( 251 ) * C( NO           )                                       
     &                                  -           RK( 252 ) * C( HO2          )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 107: H2NO3PIJ                                                                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( H2NO3PIJ    ,N2O5         ) =             RK( 267 )                                                           
      PD( H2NO3PIJ    ,H2NO3PIJ     ) = -           RK( 269 )                                                           
     &                                  -           RK( 271 ) * C( FORM_PRIMARY )                                       
     &                                  -           RK( 272 ) * C( ALD2_PRIMARY )                                       
      PD( H2NO3PIJ    ,FORM_PRIMARY ) = -           RK( 271 ) * C( H2NO3PIJ     )                                       
      PD( H2NO3PIJ    ,ALD2_PRIMARY ) = -           RK( 272 ) * C( H2NO3PIJ     )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 108: H2NO3PK                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( H2NO3PK     ,N2O5         ) =             RK( 268 )                                                           
      PD( H2NO3PK     ,H2NO3PK      ) = -           RK( 270 )                                                           
     &                                  -           RK( 273 ) * C( BUTADIENE13  )                                       
      PD( H2NO3PK     ,BUTADIENE13  ) = -           RK( 273 ) * C( H2NO3PK      )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 109: FORM_PRIMARY                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FORM_PRIMARY,H2NO3PIJ     ) = -           RK( 271 ) * C( FORM_PRIMARY )                                       
      PD( FORM_PRIMARY,FORM_PRIMARY ) = -           RK( 271 ) * C( H2NO3PIJ     )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 110: ALD2_PRIMARY                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALD2_PRIMARY,H2NO3PIJ     ) = -           RK( 272 ) * C( ALD2_PRIMARY )                                       
      PD( ALD2_PRIMARY,ALD2_PRIMARY ) = -           RK( 272 ) * C( H2NO3PIJ     )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 111: BUTADIENE13                                                                
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BUTADIENE13 ,H2NO3PK      ) = -           RK( 273 ) * C( BUTADIENE13  )                                       
      PD( BUTADIENE13 ,BUTADIENE13  ) = -           RK( 273 ) * C( H2NO3PK      )                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 112: ACROLEIN                                                                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACROLEIN    ,IEPOX        ) =             RK( 276 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 113: ACRO_PRIMARY                                                               
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ACRO_PRIMARY,ACRO_PRIMARY ) = -           RK( 277 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 114: TOLU                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLU        ,ACRO_PRIMARY ) =   1.14280 * RK( 277 )                                                           
      PD( TOLU        ,MXYL         ) =   1.14280 * RK( 278 )                                                           
      PD( TOLU        ,OXYL         ) =             RK( 279 )                                                           
      PD( TOLU        ,PXYL         ) =             RK( 280 )                                                           
      PD( TOLU        ,APIN         ) =   0.85714 * RK( 281 )                                                           
      PD( TOLU        ,BPIN         ) =   0.85714 * RK( 282 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 115: MXYL                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MXYL        ,MXYL         ) = -           RK( 278 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 116: OXYL                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OXYL        ,OXYL         ) = -           RK( 279 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 117: PXYL                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PXYL        ,PXYL         ) = -           RK( 280 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 118: APIN                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( APIN        ,APIN         ) = -           RK( 281 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 119: BPIN                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BPIN        ,BPIN         ) = -           RK( 282 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 120: HG                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HG          ,HG           ) = -           RK( 283 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 121: HGIIAER                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HGIIAER     ,HG           ) =             RK( 283 )                                                           
      PD( HGIIAER     ,HGIIGAS      ) =             RK( 284 )                                                           

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species 122: HGIIGAS                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HGIIGAS     ,HGIIGAS      ) = -           RK( 284 )                                                           


      RETURN                                                                                                            
      END                                                                                                               
