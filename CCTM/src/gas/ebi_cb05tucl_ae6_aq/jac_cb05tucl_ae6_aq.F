      SUBROUTINE JAC( C, PD, RK )                                                                                       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C                                                                                                                       
C Automatically generated Jacobian for DDM-3D                                                                           
C                                                                                                                       
C =-=-= MECHANISM: CB05TUCL_AE6_AQ                  =-=-=                                                               
C                                                                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      Use RXNS_DATA, Only: NRXNS, MECHNAME
      Use CGRID_SPCS, Only: N_GC_SPC
      Use UTILIO_DEFN                   ! IOAPI parameters and functions declarations

      IMPLICIT NONE                                                                                                     

      INTEGER, PARAMETER :: NJACSPC =  83                                                                               
      INTEGER, PARAMETER :: NJACRXN = 235                                                                               

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
      INTEGER, PARAMETER :: NTR              =  16                                                                      
      INTEGER, PARAMETER :: ROOH             =  17                                                                      
      INTEGER, PARAMETER :: FORM             =  18                                                                      
      INTEGER, PARAMETER :: ALD2             =  19                                                                      
      INTEGER, PARAMETER :: ALDX             =  20                                                                      
      INTEGER, PARAMETER :: PAR              =  21                                                                      
      INTEGER, PARAMETER :: CO               =  22                                                                      
      INTEGER, PARAMETER :: MEO2             =  23                                                                      
      INTEGER, PARAMETER :: MEPX             =  24                                                                      
      INTEGER, PARAMETER :: MEOH             =  25                                                                      
      INTEGER, PARAMETER :: HCO3             =  26                                                                      
      INTEGER, PARAMETER :: FACD             =  27                                                                      
      INTEGER, PARAMETER :: C2O3             =  28                                                                      
      INTEGER, PARAMETER :: PAN              =  29                                                                      
      INTEGER, PARAMETER :: PACD             =  30                                                                      
      INTEGER, PARAMETER :: AACD             =  31                                                                      
      INTEGER, PARAMETER :: CXO3             =  32                                                                      
      INTEGER, PARAMETER :: PANX             =  33                                                                      
      INTEGER, PARAMETER :: ROR              =  34                                                                      
      INTEGER, PARAMETER :: OLE              =  35                                                                      
      INTEGER, PARAMETER :: ETH              =  36                                                                      
      INTEGER, PARAMETER :: IOLE             =  37                                                                      
      INTEGER, PARAMETER :: TOL              =  38                                                                      
      INTEGER, PARAMETER :: CRES             =  39                                                                      
      INTEGER, PARAMETER :: TO2              =  40                                                                      
      INTEGER, PARAMETER :: TOLRO2           =  41                                                                      
      INTEGER, PARAMETER :: OPEN             =  42                                                                      
      INTEGER, PARAMETER :: MGLY             =  43                                                                      
      INTEGER, PARAMETER :: CRO              =  44                                                                      
      INTEGER, PARAMETER :: CAT1             =  45                                                                      
      INTEGER, PARAMETER :: CRON             =  46                                                                      
      INTEGER, PARAMETER :: CRNO             =  47                                                                      
      INTEGER, PARAMETER :: CRN2             =  48                                                                      
      INTEGER, PARAMETER :: CRPX             =  49                                                                      
      INTEGER, PARAMETER :: OPO3             =  50                                                                      
      INTEGER, PARAMETER :: CAO2             =  51                                                                      
      INTEGER, PARAMETER :: OPAN             =  52                                                                      
      INTEGER, PARAMETER :: XYL              =  53                                                                      
      INTEGER, PARAMETER :: XYLRO2           =  54                                                                      
      INTEGER, PARAMETER :: ISOP             =  55                                                                      
      INTEGER, PARAMETER :: ISPD             =  56                                                                      
      INTEGER, PARAMETER :: ISOPRXN          =  57                                                                      
      INTEGER, PARAMETER :: TERP             =  58                                                                      
      INTEGER, PARAMETER :: TRPRXN           =  59                                                                      
      INTEGER, PARAMETER :: SO2              =  60                                                                      
      INTEGER, PARAMETER :: SULF             =  61                                                                      
      INTEGER, PARAMETER :: SULRXN           =  62                                                                      
      INTEGER, PARAMETER :: ETOH             =  63                                                                      
      INTEGER, PARAMETER :: ETHA             =  64                                                                      
      INTEGER, PARAMETER :: CL2              =  65                                                                      
      INTEGER, PARAMETER :: CL               =  66                                                                      
      INTEGER, PARAMETER :: HOCL             =  67                                                                      
      INTEGER, PARAMETER :: CLO              =  68                                                                      
      INTEGER, PARAMETER :: FMCL             =  69                                                                      
      INTEGER, PARAMETER :: HCL              =  70                                                                      
      INTEGER, PARAMETER :: CLNO2            =  71                                                                      
      INTEGER, PARAMETER :: TOLNRXN          =  72                                                                      
      INTEGER, PARAMETER :: TOLHRXN          =  73                                                                      
      INTEGER, PARAMETER :: XYLNRXN          =  74                                                                      
      INTEGER, PARAMETER :: XYLHRXN          =  75                                                                      
      INTEGER, PARAMETER :: BENZENE          =  76                                                                      
      INTEGER, PARAMETER :: BENZRO2          =  77                                                                      
      INTEGER, PARAMETER :: BNZNRXN          =  78                                                                      
      INTEGER, PARAMETER :: BNZHRXN          =  79                                                                      
      INTEGER, PARAMETER :: SESQ             =  80                                                                      
      INTEGER, PARAMETER :: SESQRXN          =  81                                                                      
      INTEGER, PARAMETER :: H2NO3PIJ         =  82                                                                      
      INTEGER, PARAMETER :: H2NO3PK          =  83                                                                      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF( FIRSTIME ) THEN                                                                                               

         IF( MECHNAME .NE. 'CB05TUCL_AE6_AQ                 ' ) THEN                                                    
            MSG =  'This Jacobian was built for CB05TUCL_AE6_AQ                  '                                      
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
      PD( NO2     ,NO2      ) = -           RK( 001 )                                                                   
     &                          -           RK( 004 ) * C( O        )                                                   
     &                          -           RK( 005 ) * C( O        )                                                   
     &                          -           RK( 007 ) * C( O3       )                                                   
     &                          -           RK( 018 ) * C( NO3      )                                                   
     &                          -           RK( 023 ) * C( NO       )                                                   
     &                          -           RK( 028 ) * C( OH       )                                                   
     &                          -           RK( 031 ) * C( HO2      )                                                   
     &                          -           RK( 088 ) * C( C2O3     )                                                   
     &                          -           RK( 103 ) * C( CXO3     )                                                   
     &                          -           RK( 115 ) * C( ROR      )                                                   
     &                          -           RK( 133 ) * C( CRO      )                                                   
     &                          -           RK( 137 ) * C( CRNO     )                                                   
     &                          -           RK( 152 ) * C( OPO3     )                                                   
     &                          -           RK( 172 ) * C( ISOP     )                                                   
     &                          -           RK( 196 ) * C( CL       )                                                   
     &                          -           RK( 215 )                                                                   
      PD( NO2     ,NO       ) =             RK( 003 ) * C( O3       )                                                   
     &                          +           RK( 006 ) * C( O        )                                                   
     &                          + 2.00000 * RK( 016 ) * C( NO3      )                                                   
     &                          + 4.00000 * RK( 022 ) * C( NO       )                                                   
     &                          -           RK( 023 ) * C( NO2      )                                                   
     &                          +           RK( 030 ) * C( HO2      )                                                   
     &                          +           RK( 054 ) * C( XO2      )                                                   
     &                          +           RK( 067 ) * C( MEO2     )                                                   
     &                          +           RK( 080 ) * C( HCO3     )                                                   
     &                          +           RK( 087 ) * C( C2O3     )                                                   
     &                          +           RK( 102 ) * C( CXO3     )                                                   
     &                          + 0.86000 * RK( 129 ) * C( TO2      )                                                   
     &                          +           RK( 139 ) * C( CRN2     )                                                   
     &                          + 0.86000 * RK( 149 ) * C( CAO2     )                                                   
     &                          +           RK( 151 ) * C( OPO3     )                                                   
     &                          +           RK( 177 ) * C( CLO      )                                                   
      PD( NO2     ,O        ) = -           RK( 004 ) * C( NO2      )                                                   
     &                          -           RK( 005 ) * C( NO2      )                                                   
     &                          +           RK( 006 ) * C( NO       )                                                   
     &                          +           RK( 046 ) * C( NO3      )                                                   
      PD( NO2     ,O3       ) =             RK( 003 ) * C( NO       )                                                   
     &                          -           RK( 007 ) * C( NO2      )                                                   
     &                          +           RK( 049 ) * C( NO3      )                                                   
      PD( NO2     ,NO3      ) =             RK( 014 )                                                                   
     &                          + 2.00000 * RK( 016 ) * C( NO       )                                                   
     &                          -           RK( 018 ) * C( NO2      )                                                   
     &                          +           RK( 046 ) * C( O        )                                                   
     &                          +           RK( 047 ) * C( OH       )                                                   
     &                          +           RK( 049 ) * C( O3       )                                                   
     &                          + 4.00000 * RK( 050 ) * C( NO3      )                                                   
     &                          +           RK( 119 ) * C( OLE      )                                                   
     &                          +           RK( 123 ) * C( ETH      )                                                   
     &                          +           RK( 127 ) * C( IOLE     )                                                   
     &                          + 0.20000 * RK( 160 ) * C( ISOP     )                                                   
     &                          + 0.47000 * RK( 168 ) * C( TERP     )                                                   
      PD( NO2     ,OH       ) =             RK( 026 ) * C( HONO     )                                                   
     &                          -           RK( 028 ) * C( NO2      )                                                   
     &                          +           RK( 033 ) * C( PNA      )                                                   
     &                          +           RK( 047 ) * C( NO3      )                                                   
     &                          +           RK( 106 ) * C( PANX     )                                                   
      PD( NO2     ,HO2      ) =             RK( 030 ) * C( NO       )                                                   
     &                          -           RK( 031 ) * C( NO2      )                                                   
      PD( NO2     ,N2O5     ) =             RK( 021 )                                                                   
     &                          +           RK( 053 )                                                                   
      PD( NO2     ,HNO3     ) =             RK( 052 )                                                                   
      PD( NO2     ,HONO     ) =             RK( 026 ) * C( OH       )                                                   
     &                          + 4.00000 * RK( 027 ) * C( HONO     )                                                   
      PD( NO2     ,PNA      ) =             RK( 032 )                                                                   
     &                          +           RK( 033 ) * C( OH       )                                                   
     &                          + 0.61000 * RK( 051 )                                                                   
      PD( NO2     ,XO2      ) =             RK( 054 ) * C( NO       )                                                   
      PD( NO2     ,NTR      ) =             RK( 062 )                                                                   
      PD( NO2     ,MEO2     ) =             RK( 067 ) * C( NO       )                                                   
      PD( NO2     ,HCO3     ) =             RK( 080 ) * C( NO       )                                                   
      PD( NO2     ,C2O3     ) =             RK( 087 ) * C( NO       )                                                   
     &                          -           RK( 088 ) * C( NO2      )                                                   
      PD( NO2     ,PAN      ) =             RK( 089 )                                                                   
     &                          +           RK( 090 )                                                                   
      PD( NO2     ,CXO3     ) =             RK( 102 ) * C( NO       )                                                   
     &                          -           RK( 103 ) * C( NO2      )                                                   
      PD( NO2     ,PANX     ) =             RK( 104 )                                                                   
     &                          +           RK( 105 )                                                                   
     &                          +           RK( 106 ) * C( OH       )                                                   
      PD( NO2     ,ROR      ) = -           RK( 115 ) * C( NO2      )                                                   
      PD( NO2     ,OLE      ) =             RK( 119 ) * C( NO3      )                                                   
      PD( NO2     ,ETH      ) =             RK( 123 ) * C( NO3      )                                                   
      PD( NO2     ,IOLE     ) =             RK( 127 ) * C( NO3      )                                                   
      PD( NO2     ,TO2      ) =   0.86000 * RK( 129 ) * C( NO       )                                                   
      PD( NO2     ,CRO      ) = -           RK( 133 ) * C( NO2      )                                                   
      PD( NO2     ,CRNO     ) = -           RK( 137 ) * C( NO2      )                                                   
      PD( NO2     ,CRN2     ) =             RK( 139 ) * C( NO       )                                                   
      PD( NO2     ,OPO3     ) =             RK( 151 ) * C( NO       )                                                   
     &                          -           RK( 152 ) * C( NO2      )                                                   
      PD( NO2     ,CAO2     ) =   0.86000 * RK( 149 ) * C( NO       )                                                   
      PD( NO2     ,OPAN     ) =             RK( 153 )                                                                   
      PD( NO2     ,ISOP     ) =   0.20000 * RK( 160 ) * C( NO3      )                                                   
     &                          -           RK( 172 ) * C( NO2      )                                                   
      PD( NO2     ,TERP     ) =   0.47000 * RK( 168 ) * C( NO3      )                                                   
      PD( NO2     ,CL       ) = -           RK( 196 ) * C( NO2      )                                                   
      PD( NO2     ,CLO      ) =             RK( 177 ) * C( NO       )                                                   
      PD( NO2     ,CLNO2    ) =             RK( 197 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   2: NO                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO      ,NO2      ) =             RK( 001 )                                                                   
     &                          +           RK( 004 ) * C( O        )                                                   
     &                          +           RK( 017 ) * C( NO3      )                                                   
     &                          -           RK( 023 ) * C( NO       )                                                   
     &                          + 0.20000 * RK( 172 ) * C( ISOP     )                                                   
      PD( NO      ,NO       ) = -           RK( 003 ) * C( O3       )                                                   
     &                          -           RK( 006 ) * C( O        )                                                   
     &                          -           RK( 016 ) * C( NO3      )                                                   
     &                          - 4.00000 * RK( 022 ) * C( NO       )                                                   
     &                          -           RK( 023 ) * C( NO2      )                                                   
     &                          -           RK( 024 ) * C( OH       )                                                   
     &                          -           RK( 030 ) * C( HO2      )                                                   
     &                          -           RK( 054 ) * C( XO2      )                                                   
     &                          -           RK( 055 ) * C( XO2N     )                                                   
     &                          -           RK( 067 ) * C( MEO2     )                                                   
     &                          -           RK( 080 ) * C( HCO3     )                                                   
     &                          -           RK( 087 ) * C( C2O3     )                                                   
     &                          -           RK( 102 ) * C( CXO3     )                                                   
     &                          -           RK( 129 ) * C( TO2      )                                                   
     &                          -           RK( 139 ) * C( CRN2     )                                                   
     &                          -           RK( 149 ) * C( CAO2     )                                                   
     &                          -           RK( 151 ) * C( OPO3     )                                                   
     &                          -           RK( 177 ) * C( CLO      )                                                   
      PD( NO      ,O        ) =             RK( 004 ) * C( NO2      )                                                   
     &                          -           RK( 006 ) * C( NO       )                                                   
      PD( NO      ,O3       ) = -           RK( 003 ) * C( NO       )                                                   
      PD( NO      ,NO3      ) =             RK( 015 )                                                                   
     &                          -           RK( 016 ) * C( NO       )                                                   
     &                          +           RK( 017 ) * C( NO2      )                                                   
      PD( NO      ,OH       ) = -           RK( 024 ) * C( NO       )                                                   
      PD( NO      ,HO2      ) = -           RK( 030 ) * C( NO       )                                                   
      PD( NO      ,HONO     ) =             RK( 025 )                                                                   
     &                          + 4.00000 * RK( 027 ) * C( HONO     )                                                   
      PD( NO      ,XO2      ) = -           RK( 054 ) * C( NO       )                                                   
      PD( NO      ,XO2N     ) = -           RK( 055 ) * C( NO       )                                                   
      PD( NO      ,MEO2     ) = -           RK( 067 ) * C( NO       )                                                   
      PD( NO      ,HCO3     ) = -           RK( 080 ) * C( NO       )                                                   
      PD( NO      ,C2O3     ) = -           RK( 087 ) * C( NO       )                                                   
      PD( NO      ,CXO3     ) = -           RK( 102 ) * C( NO       )                                                   
      PD( NO      ,TO2      ) = -           RK( 129 ) * C( NO       )                                                   
      PD( NO      ,CRN2     ) = -           RK( 139 ) * C( NO       )                                                   
      PD( NO      ,OPO3     ) = -           RK( 151 ) * C( NO       )                                                   
      PD( NO      ,CAO2     ) = -           RK( 149 ) * C( NO       )                                                   
      PD( NO      ,ISOP     ) =   0.20000 * RK( 172 ) * C( NO2      )                                                   
      PD( NO      ,CLO      ) = -           RK( 177 ) * C( NO       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   3: O                                                                          
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O       ,NO2      ) =             RK( 001 )                                                                   
     &                          -           RK( 004 ) * C( O        )                                                   
     &                          -           RK( 005 ) * C( O        )                                                   
      PD( O       ,NO       ) = -           RK( 006 ) * C( O        )                                                   
      PD( O       ,O        ) = -           RK( 002 )                                                                   
     &                          -           RK( 004 ) * C( NO2      )                                                   
     &                          -           RK( 005 ) * C( NO2      )                                                   
     &                          -           RK( 006 ) * C( NO       )                                                   
     &                          -           RK( 040 ) * C( OH       )                                                   
     &                          -           RK( 044 ) * C( HO2      )                                                   
     &                          -           RK( 045 ) * C( H2O2     )                                                   
     &                          -           RK( 046 ) * C( NO3      )                                                   
     &                          -           RK( 076 ) * C( FORM     )                                                   
     &                          -           RK( 083 ) * C( ALD2     )                                                   
     &                          -           RK( 098 ) * C( ALDX     )                                                   
     &                          -           RK( 116 ) * C( OLE      )                                                   
     &                          -           RK( 120 ) * C( ETH      )                                                   
     &                          -           RK( 124 ) * C( IOLE     )                                                   
     &                          -           RK( 157 ) * C( ISOP     )                                                   
     &                          -           RK( 165 ) * C( TERP     )                                                   
      PD( O       ,O3       ) =             RK( 008 )                                                                   
     &                          + 0.50000 * RK( 126 ) * C( IOLE     )                                                   
      PD( O       ,NO3      ) =             RK( 014 )                                                                   
     &                          -           RK( 046 ) * C( O        )                                                   
      PD( O       ,O1D      ) =             RK( 010 )                                                                   
      PD( O       ,OH       ) = -           RK( 040 ) * C( O        )                                                   
     &                          + 4.00000 * RK( 041 ) * C( OH       )                                                   
      PD( O       ,HO2      ) = -           RK( 044 ) * C( O        )                                                   
      PD( O       ,H2O2     ) = -           RK( 045 ) * C( O        )                                                   
      PD( O       ,FORM     ) = -           RK( 076 ) * C( O        )                                                   
      PD( O       ,ALD2     ) = -           RK( 083 ) * C( O        )                                                   
      PD( O       ,ALDX     ) = -           RK( 098 ) * C( O        )                                                   
      PD( O       ,OLE      ) = -           RK( 116 ) * C( O        )                                                   
      PD( O       ,ETH      ) = -           RK( 120 ) * C( O        )                                                   
      PD( O       ,IOLE     ) = -           RK( 124 ) * C( O        )                                                   
     &                          + 0.50000 * RK( 126 ) * C( O3       )                                                   
      PD( O       ,ISOP     ) = -           RK( 157 ) * C( O        )                                                   
      PD( O       ,TERP     ) = -           RK( 165 ) * C( O        )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   4: O3                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O3      ,NO2      ) = -           RK( 007 ) * C( O3       )                                                   
      PD( O3      ,NO       ) = -           RK( 003 ) * C( O3       )                                                   
      PD( O3      ,O        ) =             RK( 002 )                                                                   
      PD( O3      ,O3       ) = -           RK( 003 ) * C( NO       )                                                   
     &                          -           RK( 007 ) * C( NO2      )                                                   
     &                          -           RK( 008 )                                                                   
     &                          -           RK( 009 )                                                                   
     &                          -           RK( 012 ) * C( OH       )                                                   
     &                          -           RK( 013 ) * C( HO2      )                                                   
     &                          -           RK( 049 ) * C( NO3      )                                                   
     &                          -           RK( 118 ) * C( OLE      )                                                   
     &                          -           RK( 122 ) * C( ETH      )                                                   
     &                          -           RK( 126 ) * C( IOLE     )                                                   
     &                          -           RK( 138 ) * C( CRNO     )                                                   
     &                          -           RK( 145 ) * C( OPEN     )                                                   
     &                          -           RK( 159 ) * C( ISOP     )                                                   
     &                          -           RK( 162 ) * C( ISPD     )                                                   
     &                          -           RK( 167 ) * C( TERP     )                                                   
     &                          -           RK( 175 ) * C( CL       )                                                   
     &                          -           RK( 216 )                                                                   
      PD( O3      ,NO3      ) = -           RK( 049 ) * C( O3       )                                                   
      PD( O3      ,OH       ) = -           RK( 012 ) * C( O3       )                                                   
      PD( O3      ,HO2      ) = -           RK( 013 ) * C( O3       )                                                   
     &                          + 0.20000 * RK( 091 ) * C( C2O3     )                                                   
     &                          + 0.20000 * RK( 107 ) * C( CXO3     )                                                   
      PD( O3      ,C2O3     ) =   0.20000 * RK( 091 ) * C( HO2      )                                                   
      PD( O3      ,CXO3     ) =   0.20000 * RK( 107 ) * C( HO2      )                                                   
      PD( O3      ,OLE      ) = -           RK( 118 ) * C( O3       )                                                   
      PD( O3      ,ETH      ) = -           RK( 122 ) * C( O3       )                                                   
      PD( O3      ,IOLE     ) = -           RK( 126 ) * C( O3       )                                                   
      PD( O3      ,OPEN     ) = -           RK( 145 ) * C( O3       )                                                   
      PD( O3      ,CRNO     ) = -           RK( 138 ) * C( O3       )                                                   
      PD( O3      ,ISOP     ) = -           RK( 159 ) * C( O3       )                                                   
      PD( O3      ,ISPD     ) = -           RK( 162 ) * C( O3       )                                                   
      PD( O3      ,TERP     ) = -           RK( 167 ) * C( O3       )                                                   
      PD( O3      ,CL       ) = -           RK( 175 ) * C( O3       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   5: NO3                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NO3     ,NO2      ) =             RK( 005 ) * C( O        )                                                   
     &                          +           RK( 007 ) * C( O3       )                                                   
     &                          -           RK( 017 ) * C( NO3      )                                                   
     &                          -           RK( 018 ) * C( NO3      )                                                   
      PD( NO3     ,NO       ) = -           RK( 016 ) * C( NO3      )                                                   
      PD( NO3     ,O        ) =             RK( 005 ) * C( NO2      )                                                   
     &                          -           RK( 046 ) * C( NO3      )                                                   
      PD( NO3     ,O3       ) =             RK( 007 ) * C( NO2      )                                                   
     &                          -           RK( 049 ) * C( NO3      )                                                   
      PD( NO3     ,NO3      ) = -           RK( 014 )                                                                   
     &                          -           RK( 015 )                                                                   
     &                          -           RK( 016 ) * C( NO       )                                                   
     &                          -           RK( 017 ) * C( NO2      )                                                   
     &                          -           RK( 018 ) * C( NO2      )                                                   
     &                          -           RK( 046 ) * C( O        )                                                   
     &                          -           RK( 047 ) * C( OH       )                                                   
     &                          -           RK( 048 ) * C( HO2      )                                                   
     &                          -           RK( 049 ) * C( O3       )                                                   
     &                          - 4.00000 * RK( 050 ) * C( NO3      )                                                   
     &                          -           RK( 077 ) * C( FORM     )                                                   
     &                          -           RK( 085 ) * C( ALD2     )                                                   
     &                          -           RK( 100 ) * C( ALDX     )                                                   
     &                          -           RK( 119 ) * C( OLE      )                                                   
     &                          -           RK( 123 ) * C( ETH      )                                                   
     &                          -           RK( 127 ) * C( IOLE     )                                                   
     &                          -           RK( 132 ) * C( CRES     )                                                   
     &                          -           RK( 136 ) * C( CRON     )                                                   
     &                          -           RK( 146 ) * C( OPEN     )                                                   
     &                          -           RK( 148 ) * C( CAT1     )                                                   
     &                          -           RK( 160 ) * C( ISOP     )                                                   
     &                          -           RK( 163 ) * C( ISPD     )                                                   
     &                          -           RK( 168 ) * C( TERP     )                                                   
      PD( NO3     ,OH       ) =             RK( 029 ) * C( HNO3     )                                                   
     &                          -           RK( 047 ) * C( NO3      )                                                   
      PD( NO3     ,HO2      ) = -           RK( 048 ) * C( NO3      )                                                   
      PD( NO3     ,N2O5     ) =             RK( 021 )                                                                   
     &                          +           RK( 053 )                                                                   
      PD( NO3     ,HNO3     ) =             RK( 029 ) * C( OH       )                                                   
      PD( NO3     ,PNA      ) =   0.39000 * RK( 051 )                                                                   
      PD( NO3     ,FORM     ) = -           RK( 077 ) * C( NO3      )                                                   
      PD( NO3     ,ALD2     ) = -           RK( 085 ) * C( NO3      )                                                   
      PD( NO3     ,ALDX     ) = -           RK( 100 ) * C( NO3      )                                                   
      PD( NO3     ,OLE      ) = -           RK( 119 ) * C( NO3      )                                                   
      PD( NO3     ,ETH      ) = -           RK( 123 ) * C( NO3      )                                                   
      PD( NO3     ,IOLE     ) = -           RK( 127 ) * C( NO3      )                                                   
      PD( NO3     ,CRES     ) = -           RK( 132 ) * C( NO3      )                                                   
      PD( NO3     ,OPEN     ) = -           RK( 146 ) * C( NO3      )                                                   
      PD( NO3     ,CAT1     ) = -           RK( 148 ) * C( NO3      )                                                   
      PD( NO3     ,CRON     ) = -           RK( 136 ) * C( NO3      )                                                   
      PD( NO3     ,ISOP     ) = -           RK( 160 ) * C( NO3      )                                                   
      PD( NO3     ,ISPD     ) = -           RK( 163 ) * C( NO3      )                                                   
      PD( NO3     ,TERP     ) = -           RK( 168 ) * C( NO3      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   6: O1D                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( O1D     ,O3       ) =             RK( 009 )                                                                   
      PD( O1D     ,O1D      ) = -           RK( 010 )                                                                   
     &                          -           RK( 011 )                                                                   
     &                          -           RK( 038 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   7: OH                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OH      ,NO2      ) = -           RK( 028 ) * C( OH       )                                                   
      PD( OH      ,NO       ) = -           RK( 024 ) * C( OH       )                                                   
     &                          +           RK( 030 ) * C( HO2      )                                                   
      PD( OH      ,O        ) = -           RK( 040 ) * C( OH       )                                                   
     &                          +           RK( 044 ) * C( HO2      )                                                   
     &                          +           RK( 045 ) * C( H2O2     )                                                   
     &                          +           RK( 076 ) * C( FORM     )                                                   
     &                          +           RK( 083 ) * C( ALD2     )                                                   
     &                          +           RK( 098 ) * C( ALDX     )                                                   
     &                          + 0.10000 * RK( 116 ) * C( OLE      )                                                   
     &                          + 0.30000 * RK( 120 ) * C( ETH      )                                                   
      PD( OH      ,O3       ) = -           RK( 012 ) * C( OH       )                                                   
     &                          +           RK( 013 ) * C( HO2      )                                                   
     &                          + 0.10000 * RK( 118 ) * C( OLE      )                                                   
     &                          + 0.13000 * RK( 122 ) * C( ETH      )                                                   
     &                          + 0.50000 * RK( 126 ) * C( IOLE     )                                                   
     &                          + 0.08000 * RK( 145 ) * C( OPEN     )                                                   
     &                          + 0.26600 * RK( 159 ) * C( ISOP     )                                                   
     &                          + 0.26800 * RK( 162 ) * C( ISPD     )                                                   
     &                          + 0.57000 * RK( 167 ) * C( TERP     )                                                   
      PD( OH      ,NO3      ) = -           RK( 047 ) * C( OH       )                                                   
      PD( OH      ,O1D      ) =   2.00000 * RK( 011 )                                                                   
     &                          +           RK( 038 )                                                                   
      PD( OH      ,OH       ) = -           RK( 012 ) * C( O3       )                                                   
     &                          -           RK( 024 ) * C( NO       )                                                   
     &                          -           RK( 026 ) * C( HONO     )                                                   
     &                          -           RK( 028 ) * C( NO2      )                                                   
     &                          -           RK( 029 ) * C( HNO3     )                                                   
     &                          -           RK( 033 ) * C( PNA      )                                                   
     &                          -           RK( 037 ) * C( H2O2     )                                                   
     &                          -           RK( 039 )                                                                   
     &                          -           RK( 040 ) * C( O        )                                                   
     &                          - 4.00000 * RK( 041 ) * C( OH       )                                                   
     &                          - 4.00000 * RK( 042 ) * C( OH       )                                                   
     &                          -           RK( 043 ) * C( HO2      )                                                   
     &                          -           RK( 047 ) * C( NO3      )                                                   
     &                          -           RK( 061 ) * C( NTR      )                                                   
     &                          -           RK( 063 ) * C( ROOH     )                                                   
     &                          -           RK( 065 ) * C( CO       )                                                   
     &                          -           RK( 066 )                                                                   
     &                          -           RK( 070 ) * C( MEPX     )                                                   
     &                          -           RK( 072 ) * C( MEOH     )                                                   
     &                          -           RK( 073 ) * C( FORM     )                                                   
     &                          -           RK( 082 ) * C( FACD     )                                                   
     &                          -           RK( 084 ) * C( ALD2     )                                                   
     &                          -           RK( 095 ) * C( PACD     )                                                   
     &                          -           RK( 097 ) * C( AACD     )                                                   
     &                          -           RK( 099 ) * C( ALDX     )                                                   
     &                          -           RK( 106 ) * C( PANX     )                                                   
     &                          -           RK( 112 ) * C( PAR      )                                                   
     &                          -           RK( 117 ) * C( OLE      )                                                   
     &                          -           RK( 121 ) * C( ETH      )                                                   
     &                          -           RK( 125 ) * C( IOLE     )                                                   
     &                          - 0.92800 * RK( 128 ) * C( TOL      )                                                   
     &                          -           RK( 131 ) * C( CRES     )                                                   
     &                          -           RK( 135 ) * C( CRON     )                                                   
     &                          -           RK( 142 ) * C( CRPX     )                                                   
     &                          -           RK( 144 ) * C( OPEN     )                                                   
     &                          -           RK( 147 ) * C( CAT1     )                                                   
     &                          -           RK( 154 ) * C( XYL      )                                                   
     &                          -           RK( 155 ) * C( MGLY     )                                                   
     &                          -           RK( 158 ) * C( ISOP     )                                                   
     &                          -           RK( 161 ) * C( ISPD     )                                                   
     &                          -           RK( 166 ) * C( TERP     )                                                   
     &                          -           RK( 169 ) * C( SO2      )                                                   
     &                          -           RK( 170 ) * C( ETOH     )                                                   
     &                          -           RK( 171 ) * C( ETHA     )                                                   
     &                          -           RK( 179 ) * C( FMCL     )                                                   
     &                          -           RK( 193 ) * C( HCL      )                                                   
      PD( OH      ,HO2      ) =             RK( 013 ) * C( O3       )                                                   
     &                          +           RK( 030 ) * C( NO       )                                                   
     &                          -           RK( 043 ) * C( OH       )                                                   
     &                          +           RK( 044 ) * C( O        )                                                   
      PD( OH      ,HNO3     ) = -           RK( 029 ) * C( OH       )                                                   
     &                          +           RK( 052 )                                                                   
      PD( OH      ,HONO     ) =             RK( 025 )                                                                   
     &                          -           RK( 026 ) * C( OH       )                                                   
      PD( OH      ,PNA      ) = -           RK( 033 ) * C( OH       )                                                   
     &                          + 0.39000 * RK( 051 )                                                                   
      PD( OH      ,H2O2     ) =   2.00000 * RK( 036 )                                                                   
     &                          -           RK( 037 ) * C( OH       )                                                   
     &                          +           RK( 045 ) * C( O        )                                                   
      PD( OH      ,NTR      ) = -           RK( 061 ) * C( OH       )                                                   
      PD( OH      ,ROOH     ) = -           RK( 063 ) * C( OH       )                                                   
     &                          +           RK( 064 )                                                                   
      PD( OH      ,FORM     ) = -           RK( 073 ) * C( OH       )                                                   
     &                          +           RK( 076 ) * C( O        )                                                   
      PD( OH      ,ALD2     ) =             RK( 083 ) * C( O        )                                                   
     &                          -           RK( 084 ) * C( OH       )                                                   
      PD( OH      ,ALDX     ) =             RK( 098 ) * C( O        )                                                   
     &                          -           RK( 099 ) * C( OH       )                                                   
      PD( OH      ,PAR      ) = -           RK( 112 ) * C( OH       )                                                   
      PD( OH      ,CO       ) = -           RK( 065 ) * C( OH       )                                                   
      PD( OH      ,MEPX     ) = -           RK( 070 ) * C( OH       )                                                   
     &                          +           RK( 071 )                                                                   
      PD( OH      ,MEOH     ) = -           RK( 072 ) * C( OH       )                                                   
      PD( OH      ,FACD     ) = -           RK( 082 ) * C( OH       )                                                   
      PD( OH      ,PACD     ) = -           RK( 095 ) * C( OH       )                                                   
     &                          +           RK( 096 )                                                                   
      PD( OH      ,AACD     ) = -           RK( 097 ) * C( OH       )                                                   
      PD( OH      ,PANX     ) = -           RK( 106 ) * C( OH       )                                                   
      PD( OH      ,OLE      ) =   0.10000 * RK( 116 ) * C( O        )                                                   
     &                          -           RK( 117 ) * C( OH       )                                                   
     &                          + 0.10000 * RK( 118 ) * C( O3       )                                                   
      PD( OH      ,ETH      ) =   0.30000 * RK( 120 ) * C( O        )                                                   
     &                          -           RK( 121 ) * C( OH       )                                                   
     &                          + 0.13000 * RK( 122 ) * C( O3       )                                                   
      PD( OH      ,IOLE     ) = -           RK( 125 ) * C( OH       )                                                   
     &                          + 0.50000 * RK( 126 ) * C( O3       )                                                   
      PD( OH      ,TOL      ) = - 0.92800 * RK( 128 ) * C( OH       )                                                   
      PD( OH      ,CRES     ) = -           RK( 131 ) * C( OH       )                                                   
      PD( OH      ,OPEN     ) = -           RK( 144 ) * C( OH       )                                                   
     &                          + 0.08000 * RK( 145 ) * C( O3       )                                                   
      PD( OH      ,MGLY     ) = -           RK( 155 ) * C( OH       )                                                   
      PD( OH      ,CAT1     ) = -           RK( 147 ) * C( OH       )                                                   
      PD( OH      ,CRON     ) = -           RK( 135 ) * C( OH       )                                                   
      PD( OH      ,CRPX     ) =             RK( 141 )                                                                   
     &                          -           RK( 142 ) * C( OH       )                                                   
      PD( OH      ,XYL      ) = -           RK( 154 ) * C( OH       )                                                   
      PD( OH      ,ISOP     ) = -           RK( 158 ) * C( OH       )                                                   
     &                          + 0.26600 * RK( 159 ) * C( O3       )                                                   
      PD( OH      ,ISPD     ) = -           RK( 161 ) * C( OH       )                                                   
     &                          + 0.26800 * RK( 162 ) * C( O3       )                                                   
      PD( OH      ,TERP     ) = -           RK( 166 ) * C( OH       )                                                   
     &                          + 0.57000 * RK( 167 ) * C( O3       )                                                   
      PD( OH      ,SO2      ) = -           RK( 169 ) * C( OH       )                                                   
      PD( OH      ,ETOH     ) = -           RK( 170 ) * C( OH       )                                                   
      PD( OH      ,ETHA     ) = -           RK( 171 ) * C( OH       )                                                   
      PD( OH      ,HOCL     ) =             RK( 174 )                                                                   
      PD( OH      ,FMCL     ) = -           RK( 179 ) * C( OH       )                                                   
      PD( OH      ,HCL      ) = -           RK( 193 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   8: HO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HO2     ,NO2      ) = -           RK( 031 ) * C( HO2      )                                                   
     &                          + 0.80000 * RK( 172 ) * C( ISOP     )                                                   
      PD( HO2     ,NO       ) = -           RK( 030 ) * C( HO2      )                                                   
     &                          +           RK( 067 ) * C( MEO2     )                                                   
     &                          +           RK( 080 ) * C( HCO3     )                                                   
     &                          +           RK( 102 ) * C( CXO3     )                                                   
     &                          + 1.20000 * RK( 129 ) * C( TO2      )                                                   
     &                          + 1.20000 * RK( 149 ) * C( CAO2     )                                                   
     &                          +           RK( 151 ) * C( OPO3     )                                                   
      PD( HO2     ,O        ) =             RK( 040 ) * C( OH       )                                                   
     &                          -           RK( 044 ) * C( HO2      )                                                   
     &                          +           RK( 045 ) * C( H2O2     )                                                   
     &                          +           RK( 076 ) * C( FORM     )                                                   
     &                          + 0.30000 * RK( 116 ) * C( OLE      )                                                   
     &                          + 1.70000 * RK( 120 ) * C( ETH      )                                                   
     &                          + 0.10000 * RK( 124 ) * C( IOLE     )                                                   
     &                          + 0.25000 * RK( 157 ) * C( ISOP     )                                                   
      PD( HO2     ,O3       ) =             RK( 012 ) * C( OH       )                                                   
     &                          -           RK( 013 ) * C( HO2      )                                                   
     &                          + 0.44000 * RK( 118 ) * C( OLE      )                                                   
     &                          + 0.13000 * RK( 122 ) * C( ETH      )                                                   
     &                          + 0.50000 * RK( 126 ) * C( IOLE     )                                                   
     &                          + 0.76000 * RK( 145 ) * C( OPEN     )                                                   
     &                          + 0.06600 * RK( 159 ) * C( ISOP     )                                                   
     &                          + 0.15400 * RK( 162 ) * C( ISPD     )                                                   
     &                          + 0.07000 * RK( 167 ) * C( TERP     )                                                   
      PD( HO2     ,NO3      ) =             RK( 047 ) * C( OH       )                                                   
     &                          -           RK( 048 ) * C( HO2      )                                                   
     &                          +           RK( 077 ) * C( FORM     )                                                   
     &                          +           RK( 127 ) * C( IOLE     )                                                   
     &                          + 0.36000 * RK( 132 ) * C( CRES     )                                                   
     &                          + 0.80000 * RK( 160 ) * C( ISOP     )                                                   
     &                          + 0.92500 * RK( 163 ) * C( ISPD     )                                                   
     &                          + 0.28000 * RK( 168 ) * C( TERP     )                                                   
      PD( HO2     ,O1D      ) =             RK( 038 )                                                                   
      PD( HO2     ,OH       ) =             RK( 012 ) * C( O3       )                                                   
     &                          +           RK( 037 ) * C( H2O2     )                                                   
     &                          +           RK( 039 )                                                                   
     &                          +           RK( 040 ) * C( O        )                                                   
     &                          -           RK( 043 ) * C( HO2      )                                                   
     &                          +           RK( 047 ) * C( NO3      )                                                   
     &                          +           RK( 061 ) * C( NTR      )                                                   
     &                          +           RK( 065 ) * C( CO       )                                                   
     &                          + 0.30000 * RK( 070 ) * C( MEPX     )                                                   
     &                          +           RK( 072 ) * C( MEOH     )                                                   
     &                          +           RK( 073 ) * C( FORM     )                                                   
     &                          +           RK( 082 ) * C( FACD     )                                                   
     &                          + 0.11000 * RK( 112 ) * C( PAR      )                                                   
     &                          + 0.95000 * RK( 117 ) * C( OLE      )                                                   
     &                          +           RK( 121 ) * C( ETH      )                                                   
     &                          +           RK( 125 ) * C( IOLE     )                                                   
     &                          + 0.28000 * RK( 128 ) * C( TOL      )                                                   
     &                          + 1.12000 * RK( 131 ) * C( CRES     )                                                   
     &                          + 0.70000 * RK( 154 ) * C( XYL      )                                                   
     &                          + 0.91200 * RK( 158 ) * C( ISOP     )                                                   
     &                          + 0.50300 * RK( 161 ) * C( ISPD     )                                                   
     &                          + 0.75000 * RK( 166 ) * C( TERP     )                                                   
     &                          +           RK( 169 ) * C( SO2      )                                                   
     &                          +           RK( 170 ) * C( ETOH     )                                                   
     &                          +           RK( 171 ) * C( ETHA     )                                                   
      PD( HO2     ,HO2      ) = -           RK( 013 ) * C( O3       )                                                   
     &                          -           RK( 030 ) * C( NO       )                                                   
     &                          -           RK( 031 ) * C( NO2      )                                                   
     &                          - 4.00000 * RK( 034 ) * C( HO2      )                                                   
     &                          - 4.00000 * RK( 035 ) * C( HO2      )                                                   
     &                          -           RK( 043 ) * C( OH       )                                                   
     &                          -           RK( 044 ) * C( O        )                                                   
     &                          -           RK( 048 ) * C( NO3      )                                                   
     &                          -           RK( 056 ) * C( XO2      )                                                   
     &                          -           RK( 057 ) * C( XO2N     )                                                   
     &                          -           RK( 068 ) * C( MEO2     )                                                   
     &                          -           RK( 078 ) * C( FORM     )                                                   
     &                          -           RK( 081 ) * C( HCO3     )                                                   
     &                          -           RK( 091 ) * C( C2O3     )                                                   
     &                          -           RK( 107 ) * C( CXO3     )                                                   
     &                          -           RK( 130 ) * C( TO2      )                                                   
     &                          -           RK( 134 ) * C( CRO      )                                                   
     &                          -           RK( 140 ) * C( CRN2     )                                                   
     &                          -           RK( 150 ) * C( CAO2     )                                                   
     &                          -           RK( 178 ) * C( CLO      )                                                   
      PD( HO2     ,PNA      ) =             RK( 032 )                                                                   
     &                          + 0.61000 * RK( 051 )                                                                   
      PD( HO2     ,H2O2     ) =             RK( 037 ) * C( OH       )                                                   
     &                          +           RK( 045 ) * C( O        )                                                   
      PD( HO2     ,XO2      ) = -           RK( 056 ) * C( HO2      )                                                   
      PD( HO2     ,XO2N     ) = -           RK( 057 ) * C( HO2      )                                                   
      PD( HO2     ,NTR      ) =             RK( 061 ) * C( OH       )                                                   
     &                          +           RK( 062 )                                                                   
      PD( HO2     ,ROOH     ) =             RK( 064 )                                                                   
      PD( HO2     ,FORM     ) =             RK( 073 ) * C( OH       )                                                   
     &                          + 2.00000 * RK( 074 )                                                                   
     &                          +           RK( 076 ) * C( O        )                                                   
     &                          +           RK( 077 ) * C( NO3      )                                                   
     &                          -           RK( 078 ) * C( HO2      )                                                   
     &                          +           RK( 188 ) * C( CL       )                                                   
      PD( HO2     ,ALD2     ) =             RK( 086 )                                                                   
      PD( HO2     ,ALDX     ) =             RK( 101 )                                                                   
      PD( HO2     ,PAR      ) =   0.11000 * RK( 112 ) * C( OH       )                                                   
     &                          + 0.11000 * RK( 182 ) * C( CL       )                                                   
      PD( HO2     ,CO       ) =             RK( 065 ) * C( OH       )                                                   
      PD( HO2     ,MEO2     ) =             RK( 067 ) * C( NO       )                                                   
     &                          -           RK( 068 ) * C( HO2      )                                                   
     &                          + 4.00000 * RK( 069 ) * C( MEO2     )                                                   
     &                          + 0.90000 * RK( 092 ) * C( C2O3     )                                                   
     &                          +           RK( 108 ) * C( CXO3     )                                                   
      PD( HO2     ,MEPX     ) =   0.30000 * RK( 070 ) * C( OH       )                                                   
     &                          +           RK( 071 )                                                                   
      PD( HO2     ,MEOH     ) =             RK( 072 ) * C( OH       )                                                   
     &                          +           RK( 191 ) * C( CL       )                                                   
      PD( HO2     ,HCO3     ) =             RK( 079 )                                                                   
     &                          +           RK( 080 ) * C( NO       )                                                   
     &                          -           RK( 081 ) * C( HO2      )                                                   
      PD( HO2     ,FACD     ) =             RK( 082 ) * C( OH       )                                                   
      PD( HO2     ,C2O3     ) = -           RK( 091 ) * C( HO2      )                                                   
     &                          + 0.90000 * RK( 092 ) * C( MEO2     )                                                   
     &                          +           RK( 111 ) * C( CXO3     )                                                   
      PD( HO2     ,CXO3     ) =             RK( 102 ) * C( NO       )                                                   
     &                          -           RK( 107 ) * C( HO2      )                                                   
     &                          +           RK( 108 ) * C( MEO2     )                                                   
     &                          + 4.00000 * RK( 110 ) * C( CXO3     )                                                   
     &                          +           RK( 111 ) * C( C2O3     )                                                   
      PD( HO2     ,ROR      ) =   0.94000 * RK( 113 )                                                                   
     &                          +           RK( 114 )                                                                   
      PD( HO2     ,OLE      ) =   0.30000 * RK( 116 ) * C( O        )                                                   
     &                          + 0.95000 * RK( 117 ) * C( OH       )                                                   
     &                          + 0.44000 * RK( 118 ) * C( O3       )                                                   
     &                          +           RK( 185 ) * C( CL       )                                                   
      PD( HO2     ,ETH      ) =   1.70000 * RK( 120 ) * C( O        )                                                   
     &                          +           RK( 121 ) * C( OH       )                                                   
     &                          + 0.13000 * RK( 122 ) * C( O3       )                                                   
     &                          +           RK( 184 ) * C( CL       )                                                   
      PD( HO2     ,IOLE     ) =   0.10000 * RK( 124 ) * C( O        )                                                   
     &                          +           RK( 125 ) * C( OH       )                                                   
     &                          + 0.50000 * RK( 126 ) * C( O3       )                                                   
     &                          +           RK( 127 ) * C( NO3      )                                                   
     &                          +           RK( 186 ) * C( CL       )                                                   
      PD( HO2     ,TOL      ) =   0.28000 * RK( 128 ) * C( OH       )                                                   
     &                          + 0.88000 * RK( 194 ) * C( CL       )                                                   
      PD( HO2     ,CRES     ) =   1.12000 * RK( 131 ) * C( OH       )                                                   
     &                          + 0.36000 * RK( 132 ) * C( NO3      )                                                   
      PD( HO2     ,TO2      ) =   1.20000 * RK( 129 ) * C( NO       )                                                   
     &                          -           RK( 130 ) * C( HO2      )                                                   
      PD( HO2     ,OPEN     ) =             RK( 143 )                                                                   
     &                          + 0.76000 * RK( 145 ) * C( O3       )                                                   
      PD( HO2     ,MGLY     ) =             RK( 156 )                                                                   
      PD( HO2     ,CRO      ) = -           RK( 134 ) * C( HO2      )                                                   
      PD( HO2     ,CRN2     ) = -           RK( 140 ) * C( HO2      )                                                   
      PD( HO2     ,OPO3     ) =             RK( 151 ) * C( NO       )                                                   
      PD( HO2     ,CAO2     ) =   1.20000 * RK( 149 ) * C( NO       )                                                   
     &                          -           RK( 150 ) * C( HO2      )                                                   
      PD( HO2     ,XYL      ) =   0.70000 * RK( 154 ) * C( OH       )                                                   
     &                          + 0.84000 * RK( 195 ) * C( CL       )                                                   
      PD( HO2     ,ISOP     ) =   0.25000 * RK( 157 ) * C( O        )                                                   
     &                          + 0.91200 * RK( 158 ) * C( OH       )                                                   
     &                          + 0.06600 * RK( 159 ) * C( O3       )                                                   
     &                          + 0.80000 * RK( 160 ) * C( NO3      )                                                   
     &                          + 0.80000 * RK( 172 ) * C( NO2      )                                                   
     &                          +           RK( 187 ) * C( CL       )                                                   
      PD( HO2     ,ISPD     ) =   0.50300 * RK( 161 ) * C( OH       )                                                   
     &                          + 0.15400 * RK( 162 ) * C( O3       )                                                   
     &                          + 0.92500 * RK( 163 ) * C( NO3      )                                                   
     &                          + 1.03300 * RK( 164 )                                                                   
      PD( HO2     ,TERP     ) =   0.75000 * RK( 166 ) * C( OH       )                                                   
     &                          + 0.07000 * RK( 167 ) * C( O3       )                                                   
     &                          + 0.28000 * RK( 168 ) * C( NO3      )                                                   
      PD( HO2     ,SO2      ) =             RK( 169 ) * C( OH       )                                                   
      PD( HO2     ,ETOH     ) =             RK( 170 ) * C( OH       )                                                   
     &                          +           RK( 192 ) * C( CL       )                                                   
      PD( HO2     ,ETHA     ) =             RK( 171 ) * C( OH       )                                                   
     &                          +           RK( 183 ) * C( CL       )                                                   
      PD( HO2     ,CL       ) =   0.11000 * RK( 182 ) * C( PAR      )                                                   
     &                          +           RK( 183 ) * C( ETHA     )                                                   
     &                          +           RK( 184 ) * C( ETH      )                                                   
     &                          +           RK( 185 ) * C( OLE      )                                                   
     &                          +           RK( 186 ) * C( IOLE     )                                                   
     &                          +           RK( 187 ) * C( ISOP     )                                                   
     &                          +           RK( 188 ) * C( FORM     )                                                   
     &                          +           RK( 191 ) * C( MEOH     )                                                   
     &                          +           RK( 192 ) * C( ETOH     )                                                   
     &                          + 0.88000 * RK( 194 ) * C( TOL      )                                                   
     &                          + 0.84000 * RK( 195 ) * C( XYL      )                                                   
      PD( HO2     ,CLO      ) = -           RK( 178 ) * C( HO2      )                                                   
      PD( HO2     ,FMCL     ) =             RK( 180 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species   9: N2O5                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( N2O5    ,NO2      ) =             RK( 018 ) * C( NO3      )                                                   
      PD( N2O5    ,NO3      ) =             RK( 018 ) * C( NO2      )                                                   
      PD( N2O5    ,N2O5     ) = -           RK( 019 )                                                                   
     &                          -           RK( 020 )                                                                   
     &                          -           RK( 021 )                                                                   
     &                          -           RK( 053 )                                                                   
     &                          -           RK( 208 )                                                                   
     &                          -           RK( 209 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  10: HNO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HNO3    ,NO2      ) =             RK( 028 ) * C( OH       )                                                   
     &                          + 0.50000 * RK( 215 )                                                                   
      PD( HNO3    ,NO3      ) =             RK( 048 ) * C( HO2      )                                                   
     &                          +           RK( 077 ) * C( FORM     )                                                   
     &                          +           RK( 085 ) * C( ALD2     )                                                   
     &                          +           RK( 100 ) * C( ALDX     )                                                   
     &                          +           RK( 132 ) * C( CRES     )                                                   
     &                          +           RK( 136 ) * C( CRON     )                                                   
     &                          +           RK( 146 ) * C( OPEN     )                                                   
     &                          +           RK( 148 ) * C( CAT1     )                                                   
     &                          + 0.15000 * RK( 163 ) * C( ISPD     )                                                   
      PD( HNO3    ,OH       ) =             RK( 028 ) * C( NO2      )                                                   
     &                          -           RK( 029 ) * C( HNO3     )                                                   
     &                          +           RK( 061 ) * C( NTR      )                                                   
      PD( HNO3    ,HO2      ) =             RK( 048 ) * C( NO3      )                                                   
      PD( HNO3    ,N2O5     ) =   2.00000 * RK( 019 )                                                                   
     &                          + 2.00000 * RK( 020 )                                                                   
     &                          +           RK( 208 )                                                                   
     &                          +           RK( 209 )                                                                   
      PD( HNO3    ,HNO3     ) = -           RK( 029 ) * C( OH       )                                                   
     &                          -           RK( 052 )                                                                   
      PD( HNO3    ,NTR      ) =             RK( 061 ) * C( OH       )                                                   
      PD( HNO3    ,FORM     ) =             RK( 077 ) * C( NO3      )                                                   
      PD( HNO3    ,ALD2     ) =             RK( 085 ) * C( NO3      )                                                   
      PD( HNO3    ,ALDX     ) =             RK( 100 ) * C( NO3      )                                                   
      PD( HNO3    ,CRES     ) =             RK( 132 ) * C( NO3      )                                                   
      PD( HNO3    ,OPEN     ) =             RK( 146 ) * C( NO3      )                                                   
      PD( HNO3    ,CAT1     ) =             RK( 148 ) * C( NO3      )                                                   
      PD( HNO3    ,CRON     ) =             RK( 136 ) * C( NO3      )                                                   
      PD( HNO3    ,ISPD     ) =   0.15000 * RK( 163 ) * C( NO3      )                                                   
      PD( HNO3    ,H2NO3PIJ ) =             RK( 210 )                                                                   
      PD( HNO3    ,H2NO3PK  ) =             RK( 211 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  11: HONO                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HONO    ,NO2      ) =   2.00000 * RK( 023 ) * C( NO       )                                                   
     &                          + 0.50000 * RK( 215 )                                                                   
      PD( HONO    ,NO       ) =   2.00000 * RK( 023 ) * C( NO2      )                                                   
     &                          +           RK( 024 ) * C( OH       )                                                   
      PD( HONO    ,OH       ) =             RK( 024 ) * C( NO       )                                                   
     &                          -           RK( 026 ) * C( HONO     )                                                   
      PD( HONO    ,HONO     ) = -           RK( 025 )                                                                   
     &                          -           RK( 026 ) * C( OH       )                                                   
     &                          - 4.00000 * RK( 027 ) * C( HONO     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  12: PNA                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PNA     ,NO2      ) =             RK( 031 ) * C( HO2      )                                                   
      PD( PNA     ,OH       ) = -           RK( 033 ) * C( PNA      )                                                   
      PD( PNA     ,HO2      ) =             RK( 031 ) * C( NO2      )                                                   
      PD( PNA     ,PNA      ) = -           RK( 032 )                                                                   
     &                          -           RK( 033 ) * C( OH       )                                                   
     &                          -           RK( 051 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  13: H2O2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( H2O2    ,O        ) = -           RK( 045 ) * C( H2O2     )                                                   
      PD( H2O2    ,OH       ) = -           RK( 037 ) * C( H2O2     )                                                   
     &                          + 4.00000 * RK( 042 ) * C( OH       )                                                   
      PD( H2O2    ,HO2      ) =   4.00000 * RK( 034 ) * C( HO2      )                                                   
     &                          + 4.00000 * RK( 035 ) * C( HO2      )                                                   
      PD( H2O2    ,H2O2     ) = -           RK( 036 )                                                                   
     &                          -           RK( 037 ) * C( OH       )                                                   
     &                          -           RK( 045 ) * C( O        )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  14: XO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XO2     ,NO2      ) =             RK( 172 ) * C( ISOP     )                                                   
      PD( XO2     ,NO       ) = -           RK( 054 ) * C( XO2      )                                                   
     &                          +           RK( 102 ) * C( CXO3     )                                                   
     &                          +           RK( 151 ) * C( OPO3     )                                                   
      PD( XO2     ,O        ) =   0.20000 * RK( 116 ) * C( OLE      )                                                   
     &                          + 0.70000 * RK( 120 ) * C( ETH      )                                                   
     &                          + 0.10000 * RK( 124 ) * C( IOLE     )                                                   
     &                          + 0.25000 * RK( 157 ) * C( ISOP     )                                                   
      PD( XO2     ,O3       ) =   0.22000 * RK( 118 ) * C( OLE      )                                                   
     &                          + 0.03000 * RK( 145 ) * C( OPEN     )                                                   
     &                          + 0.20000 * RK( 159 ) * C( ISOP     )                                                   
     &                          + 0.06400 * RK( 162 ) * C( ISPD     )                                                   
     &                          + 0.76000 * RK( 167 ) * C( TERP     )                                                   
      PD( XO2     ,NO3      ) =   0.91000 * RK( 119 ) * C( OLE      )                                                   
     &                          +           RK( 123 ) * C( ETH      )                                                   
     &                          + 0.60000 * RK( 132 ) * C( CRES     )                                                   
     &                          +           RK( 160 ) * C( ISOP     )                                                   
     &                          + 0.07500 * RK( 163 ) * C( ISPD     )                                                   
     &                          + 1.03000 * RK( 168 ) * C( TERP     )                                                   
      PD( XO2     ,OH       ) =             RK( 063 ) * C( ROOH     )                                                   
     &                          + 0.30000 * RK( 070 ) * C( MEPX     )                                                   
     &                          + 0.87000 * RK( 112 ) * C( PAR      )                                                   
     &                          + 0.80000 * RK( 117 ) * C( OLE      )                                                   
     &                          +           RK( 121 ) * C( ETH      )                                                   
     &                          +           RK( 125 ) * C( IOLE     )                                                   
     &                          + 0.10000 * RK( 128 ) * C( TOL      )                                                   
     &                          + 0.12000 * RK( 131 ) * C( CRES     )                                                   
     &                          + 0.50000 * RK( 154 ) * C( XYL      )                                                   
     &                          +           RK( 155 ) * C( MGLY     )                                                   
     &                          + 0.99100 * RK( 158 ) * C( ISOP     )                                                   
     &                          + 0.71300 * RK( 161 ) * C( ISPD     )                                                   
     &                          + 1.25000 * RK( 166 ) * C( TERP     )                                                   
     &                          + 0.10000 * RK( 170 ) * C( ETOH     )                                                   
     &                          + 0.99100 * RK( 171 ) * C( ETHA     )                                                   
      PD( XO2     ,HO2      ) = -           RK( 056 ) * C( XO2      )                                                   
      PD( XO2     ,XO2      ) = -           RK( 054 ) * C( NO       )                                                   
     &                          -           RK( 056 ) * C( HO2      )                                                   
     &                          - 4.00000 * RK( 058 ) * C( XO2      )                                                   
     &                          -           RK( 060 ) * C( XO2N     )                                                   
     &                          -           RK( 093 ) * C( C2O3     )                                                   
     &                          -           RK( 109 ) * C( CXO3     )                                                   
      PD( XO2     ,XO2N     ) = -           RK( 060 ) * C( XO2      )                                                   
      PD( XO2     ,ROOH     ) =             RK( 063 ) * C( OH       )                                                   
      PD( XO2     ,PAR      ) =   0.87000 * RK( 112 ) * C( OH       )                                                   
     &                          + 0.87000 * RK( 182 ) * C( CL       )                                                   
      PD( XO2     ,MEO2     ) =   0.90000 * RK( 108 ) * C( CXO3     )                                                   
      PD( XO2     ,MEPX     ) =   0.30000 * RK( 070 ) * C( OH       )                                                   
      PD( XO2     ,C2O3     ) = -           RK( 093 ) * C( XO2      )                                                   
     &                          +           RK( 111 ) * C( CXO3     )                                                   
      PD( XO2     ,CXO3     ) =             RK( 102 ) * C( NO       )                                                   
     &                          + 0.90000 * RK( 108 ) * C( MEO2     )                                                   
     &                          -           RK( 109 ) * C( XO2      )                                                   
     &                          + 4.00000 * RK( 110 ) * C( CXO3     )                                                   
     &                          +           RK( 111 ) * C( C2O3     )                                                   
      PD( XO2     ,ROR      ) =   0.96000 * RK( 113 )                                                                   
      PD( XO2     ,OLE      ) =   0.20000 * RK( 116 ) * C( O        )                                                   
     &                          + 0.80000 * RK( 117 ) * C( OH       )                                                   
     &                          + 0.22000 * RK( 118 ) * C( O3       )                                                   
     &                          + 0.91000 * RK( 119 ) * C( NO3      )                                                   
     &                          + 2.00000 * RK( 185 ) * C( CL       )                                                   
      PD( XO2     ,ETH      ) =   0.70000 * RK( 120 ) * C( O        )                                                   
     &                          +           RK( 121 ) * C( OH       )                                                   
     &                          +           RK( 123 ) * C( NO3      )                                                   
     &                          + 2.00000 * RK( 184 ) * C( CL       )                                                   
      PD( XO2     ,IOLE     ) =   0.10000 * RK( 124 ) * C( O        )                                                   
     &                          +           RK( 125 ) * C( OH       )                                                   
     &                          + 1.70000 * RK( 186 ) * C( CL       )                                                   
      PD( XO2     ,TOL      ) =   0.10000 * RK( 128 ) * C( OH       )                                                   
     &                          + 0.88000 * RK( 194 ) * C( CL       )                                                   
      PD( XO2     ,CRES     ) =   0.12000 * RK( 131 ) * C( OH       )                                                   
     &                          + 0.60000 * RK( 132 ) * C( NO3      )                                                   
      PD( XO2     ,OPEN     ) =   0.03000 * RK( 145 ) * C( O3       )                                                   
      PD( XO2     ,MGLY     ) =             RK( 155 ) * C( OH       )                                                   
      PD( XO2     ,OPO3     ) =             RK( 151 ) * C( NO       )                                                   
      PD( XO2     ,XYL      ) =   0.50000 * RK( 154 ) * C( OH       )                                                   
     &                          + 0.84000 * RK( 195 ) * C( CL       )                                                   
      PD( XO2     ,ISOP     ) =   0.25000 * RK( 157 ) * C( O        )                                                   
     &                          + 0.99100 * RK( 158 ) * C( OH       )                                                   
     &                          + 0.20000 * RK( 159 ) * C( O3       )                                                   
     &                          +           RK( 160 ) * C( NO3      )                                                   
     &                          +           RK( 172 ) * C( NO2      )                                                   
     &                          +           RK( 187 ) * C( CL       )                                                   
      PD( XO2     ,ISPD     ) =   0.71300 * RK( 161 ) * C( OH       )                                                   
     &                          + 0.06400 * RK( 162 ) * C( O3       )                                                   
     &                          + 0.07500 * RK( 163 ) * C( NO3      )                                                   
     &                          + 0.70000 * RK( 164 )                                                                   
      PD( XO2     ,TERP     ) =   1.25000 * RK( 166 ) * C( OH       )                                                   
     &                          + 0.76000 * RK( 167 ) * C( O3       )                                                   
     &                          + 1.03000 * RK( 168 ) * C( NO3      )                                                   
      PD( XO2     ,ETOH     ) =   0.10000 * RK( 170 ) * C( OH       )                                                   
      PD( XO2     ,ETHA     ) =   0.99100 * RK( 171 ) * C( OH       )                                                   
     &                          + 0.99100 * RK( 183 ) * C( CL       )                                                   
      PD( XO2     ,CL       ) =   0.87000 * RK( 182 ) * C( PAR      )                                                   
     &                          + 0.99100 * RK( 183 ) * C( ETHA     )                                                   
     &                          + 2.00000 * RK( 184 ) * C( ETH      )                                                   
     &                          + 2.00000 * RK( 185 ) * C( OLE      )                                                   
     &                          + 1.70000 * RK( 186 ) * C( IOLE     )                                                   
     &                          +           RK( 187 ) * C( ISOP     )                                                   
     &                          + 0.88000 * RK( 194 ) * C( TOL      )                                                   
     &                          + 0.84000 * RK( 195 ) * C( XYL      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  15: XO2N                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XO2N    ,NO       ) = -           RK( 055 ) * C( XO2N     )                                                   
      PD( XO2N    ,O        ) =   0.01000 * RK( 116 ) * C( OLE      )                                                   
      PD( XO2N    ,O3       ) =   0.18000 * RK( 167 ) * C( TERP     )                                                   
      PD( XO2N    ,NO3      ) =   0.09000 * RK( 119 ) * C( OLE      )                                                   
     &                          + 0.10000 * RK( 132 ) * C( CRES     )                                                   
     &                          + 0.25000 * RK( 168 ) * C( TERP     )                                                   
      PD( XO2N    ,OH       ) =   0.13000 * RK( 112 ) * C( PAR      )                                                   
     &                          + 0.06000 * RK( 131 ) * C( CRES     )                                                   
     &                          + 0.08800 * RK( 158 ) * C( ISOP     )                                                   
     &                          + 0.25000 * RK( 166 ) * C( TERP     )                                                   
     &                          + 0.00900 * RK( 171 ) * C( ETHA     )                                                   
      PD( XO2N    ,HO2      ) = -           RK( 057 ) * C( XO2N     )                                                   
      PD( XO2N    ,XO2      ) = -           RK( 060 ) * C( XO2N     )                                                   
      PD( XO2N    ,XO2N     ) = -           RK( 055 ) * C( NO       )                                                   
     &                          -           RK( 057 ) * C( HO2      )                                                   
     &                          - 4.00000 * RK( 059 ) * C( XO2N     )                                                   
     &                          -           RK( 060 ) * C( XO2      )                                                   
      PD( XO2N    ,PAR      ) =   0.13000 * RK( 112 ) * C( OH       )                                                   
     &                          + 0.13000 * RK( 182 ) * C( CL       )                                                   
      PD( XO2N    ,ROR      ) =   0.04000 * RK( 113 )                                                                   
      PD( XO2N    ,OLE      ) =   0.01000 * RK( 116 ) * C( O        )                                                   
     &                          + 0.09000 * RK( 119 ) * C( NO3      )                                                   
      PD( XO2N    ,TOL      ) =   0.12000 * RK( 194 ) * C( CL       )                                                   
      PD( XO2N    ,CRES     ) =   0.06000 * RK( 131 ) * C( OH       )                                                   
     &                          + 0.10000 * RK( 132 ) * C( NO3      )                                                   
      PD( XO2N    ,XYL      ) =   0.16000 * RK( 195 ) * C( CL       )                                                   
      PD( XO2N    ,ISOP     ) =   0.08800 * RK( 158 ) * C( OH       )                                                   
      PD( XO2N    ,TERP     ) =   0.25000 * RK( 166 ) * C( OH       )                                                   
     &                          + 0.18000 * RK( 167 ) * C( O3       )                                                   
     &                          + 0.25000 * RK( 168 ) * C( NO3      )                                                   
      PD( XO2N    ,ETHA     ) =   0.00900 * RK( 171 ) * C( OH       )                                                   
     &                          + 0.00900 * RK( 183 ) * C( CL       )                                                   
      PD( XO2N    ,CL       ) =   0.13000 * RK( 182 ) * C( PAR      )                                                   
     &                          + 0.00900 * RK( 183 ) * C( ETHA     )                                                   
     &                          + 0.12000 * RK( 194 ) * C( TOL      )                                                   
     &                          + 0.16000 * RK( 195 ) * C( XYL      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  16: NTR                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( NTR     ,NO2      ) =             RK( 115 ) * C( ROR      )                                                   
     &                          + 2.00000 * RK( 137 ) * C( CRNO     )                                                   
     &                          + 0.80000 * RK( 172 ) * C( ISOP     )                                                   
      PD( NTR     ,NO       ) =             RK( 055 ) * C( XO2N     )                                                   
     &                          + 0.14000 * RK( 129 ) * C( TO2      )                                                   
     &                          + 0.14000 * RK( 149 ) * C( CAO2     )                                                   
      PD( NTR     ,NO3      ) =   0.80000 * RK( 160 ) * C( ISOP     )                                                   
     &                          + 0.85000 * RK( 163 ) * C( ISPD     )                                                   
     &                          + 0.53000 * RK( 168 ) * C( TERP     )                                                   
      PD( NTR     ,OH       ) = -           RK( 061 ) * C( NTR      )                                                   
      PD( NTR     ,XO2N     ) =             RK( 055 ) * C( NO       )                                                   
      PD( NTR     ,NTR      ) = -           RK( 061 ) * C( OH       )                                                   
     &                          -           RK( 062 )                                                                   
      PD( NTR     ,ROR      ) =             RK( 115 ) * C( NO2      )                                                   
      PD( NTR     ,TO2      ) =   0.14000 * RK( 129 ) * C( NO       )                                                   
      PD( NTR     ,CRNO     ) =   2.00000 * RK( 137 ) * C( NO2      )                                                   
      PD( NTR     ,CAO2     ) =   0.14000 * RK( 149 ) * C( NO       )                                                   
      PD( NTR     ,ISOP     ) =   0.80000 * RK( 160 ) * C( NO3      )                                                   
     &                          + 0.80000 * RK( 172 ) * C( NO2      )                                                   
      PD( NTR     ,ISPD     ) =   0.85000 * RK( 163 ) * C( NO3      )                                                   
      PD( NTR     ,TERP     ) =   0.53000 * RK( 168 ) * C( NO3      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  17: ROOH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ROOH    ,OH       ) = -           RK( 063 ) * C( ROOH     )                                                   
      PD( ROOH    ,HO2      ) =             RK( 056 ) * C( XO2      )                                                   
     &                          +           RK( 057 ) * C( XO2N     )                                                   
      PD( ROOH    ,XO2      ) =             RK( 056 ) * C( HO2      )                                                   
      PD( ROOH    ,XO2N     ) =             RK( 057 ) * C( HO2      )                                                   
      PD( ROOH    ,ROOH     ) = -           RK( 063 ) * C( OH       )                                                   
     &                          -           RK( 064 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  18: FORM                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FORM    ,NO       ) =             RK( 067 ) * C( MEO2     )                                                   
     &                          + 0.33600 * RK( 129 ) * C( TO2      )                                                   
     &                          + 0.34400 * RK( 149 ) * C( CAO2     )                                                   
      PD( FORM    ,O        ) = -           RK( 076 ) * C( FORM     )                                                   
     &                          + 0.20000 * RK( 116 ) * C( OLE      )                                                   
     &                          +           RK( 120 ) * C( ETH      )                                                   
     &                          + 0.50000 * RK( 157 ) * C( ISOP     )                                                   
      PD( FORM    ,O3       ) =   0.74000 * RK( 118 ) * C( OLE      )                                                   
     &                          +           RK( 122 ) * C( ETH      )                                                   
     &                          + 0.25000 * RK( 126 ) * C( IOLE     )                                                   
     &                          + 0.70000 * RK( 145 ) * C( OPEN     )                                                   
     &                          + 0.60000 * RK( 159 ) * C( ISOP     )                                                   
     &                          + 0.15000 * RK( 162 ) * C( ISPD     )                                                   
     &                          + 0.24000 * RK( 167 ) * C( TERP     )                                                   
      PD( FORM    ,NO3      ) = -           RK( 077 ) * C( FORM     )                                                   
     &                          +           RK( 119 ) * C( OLE      )                                                   
     &                          + 2.00000 * RK( 123 ) * C( ETH      )                                                   
     &                          + 0.24000 * RK( 132 ) * C( CRES     )                                                   
     &                          + 0.28200 * RK( 163 ) * C( ISPD     )                                                   
      PD( FORM    ,OH       ) =   0.33000 * RK( 061 ) * C( NTR      )                                                   
     &                          +           RK( 072 ) * C( MEOH     )                                                   
     &                          -           RK( 073 ) * C( FORM     )                                                   
     &                          + 0.80000 * RK( 117 ) * C( OLE      )                                                   
     &                          + 1.56000 * RK( 121 ) * C( ETH      )                                                   
     &                          + 0.06000 * RK( 131 ) * C( CRES     )                                                   
     &                          + 0.62900 * RK( 158 ) * C( ISOP     )                                                   
     &                          + 0.16700 * RK( 161 ) * C( ISPD     )                                                   
     &                          + 0.28000 * RK( 166 ) * C( TERP     )                                                   
     &                          + 0.10000 * RK( 170 ) * C( ETOH     )                                                   
      PD( FORM    ,HO2      ) = -           RK( 078 ) * C( FORM     )                                                   
      PD( FORM    ,NTR      ) =   0.33000 * RK( 061 ) * C( OH       )                                                   
     &                          + 0.33000 * RK( 062 )                                                                   
      PD( FORM    ,FORM     ) = -           RK( 073 ) * C( OH       )                                                   
     &                          -           RK( 074 )                                                                   
     &                          -           RK( 075 )                                                                   
     &                          -           RK( 076 ) * C( O        )                                                   
     &                          -           RK( 077 ) * C( NO3      )                                                   
     &                          -           RK( 078 ) * C( HO2      )                                                   
     &                          -           RK( 188 ) * C( CL       )                                                   
      PD( FORM    ,MEO2     ) =             RK( 067 ) * C( NO       )                                                   
     &                          + 4.00000 * RK( 069 ) * C( MEO2     )                                                   
     &                          +           RK( 092 ) * C( C2O3     )                                                   
     &                          + 0.10000 * RK( 108 ) * C( CXO3     )                                                   
      PD( FORM    ,MEPX     ) =             RK( 071 )                                                                   
      PD( FORM    ,MEOH     ) =             RK( 072 ) * C( OH       )                                                   
     &                          +           RK( 191 ) * C( CL       )                                                   
      PD( FORM    ,HCO3     ) =             RK( 079 )                                                                   
      PD( FORM    ,C2O3     ) =             RK( 092 ) * C( MEO2     )                                                   
      PD( FORM    ,CXO3     ) =   0.10000 * RK( 108 ) * C( MEO2     )                                                   
      PD( FORM    ,OLE      ) =   0.20000 * RK( 116 ) * C( O        )                                                   
     &                          + 0.80000 * RK( 117 ) * C( OH       )                                                   
     &                          + 0.74000 * RK( 118 ) * C( O3       )                                                   
     &                          +           RK( 119 ) * C( NO3      )                                                   
      PD( FORM    ,ETH      ) =             RK( 120 ) * C( O        )                                                   
     &                          + 1.56000 * RK( 121 ) * C( OH       )                                                   
     &                          +           RK( 122 ) * C( O3       )                                                   
     &                          + 2.00000 * RK( 123 ) * C( NO3      )                                                   
     &                          +           RK( 184 ) * C( CL       )                                                   
      PD( FORM    ,IOLE     ) =   0.25000 * RK( 126 ) * C( O3       )                                                   
      PD( FORM    ,CRES     ) =   0.06000 * RK( 131 ) * C( OH       )                                                   
     &                          + 0.24000 * RK( 132 ) * C( NO3      )                                                   
      PD( FORM    ,TO2      ) =   0.33600 * RK( 129 ) * C( NO       )                                                   
      PD( FORM    ,OPEN     ) =   0.70000 * RK( 145 ) * C( O3       )                                                   
      PD( FORM    ,CAO2     ) =   0.34400 * RK( 149 ) * C( NO       )                                                   
      PD( FORM    ,ISOP     ) =   0.50000 * RK( 157 ) * C( O        )                                                   
     &                          + 0.62900 * RK( 158 ) * C( OH       )                                                   
     &                          + 0.60000 * RK( 159 ) * C( O3       )                                                   
      PD( FORM    ,ISPD     ) =   0.16700 * RK( 161 ) * C( OH       )                                                   
     &                          + 0.15000 * RK( 162 ) * C( O3       )                                                   
     &                          + 0.28200 * RK( 163 ) * C( NO3      )                                                   
     &                          + 0.90000 * RK( 164 )                                                                   
      PD( FORM    ,TERP     ) =   0.28000 * RK( 166 ) * C( OH       )                                                   
     &                          + 0.24000 * RK( 167 ) * C( O3       )                                                   
      PD( FORM    ,ETOH     ) =   0.10000 * RK( 170 ) * C( OH       )                                                   
      PD( FORM    ,CL       ) =             RK( 184 ) * C( ETH      )                                                   
     &                          -           RK( 188 ) * C( FORM     )                                                   
     &                          +           RK( 191 ) * C( MEOH     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  19: ALD2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALD2    ,NO       ) =             RK( 102 ) * C( CXO3     )                                                   
      PD( ALD2    ,O        ) = -           RK( 083 ) * C( ALD2     )                                                   
     &                          + 0.20000 * RK( 116 ) * C( OLE      )                                                   
     &                          + 1.24000 * RK( 124 ) * C( IOLE     )                                                   
      PD( ALD2    ,O3       ) =   0.18000 * RK( 118 ) * C( OLE      )                                                   
     &                          + 0.65000 * RK( 126 ) * C( IOLE     )                                                   
     &                          + 0.02000 * RK( 162 ) * C( ISPD     )                                                   
      PD( ALD2    ,NO3      ) = -           RK( 085 ) * C( ALD2     )                                                   
     &                          + 0.35000 * RK( 119 ) * C( OLE      )                                                   
     &                          + 1.18000 * RK( 127 ) * C( IOLE     )                                                   
      PD( ALD2    ,OH       ) =   0.33000 * RK( 061 ) * C( NTR      )                                                   
     &                          + 0.50000 * RK( 063 ) * C( ROOH     )                                                   
     &                          -           RK( 084 ) * C( ALD2     )                                                   
     &                          +           RK( 106 ) * C( PANX     )                                                   
     &                          + 0.06000 * RK( 112 ) * C( PAR      )                                                   
     &                          + 0.33000 * RK( 117 ) * C( OLE      )                                                   
     &                          + 1.30000 * RK( 125 ) * C( IOLE     )                                                   
     &                          + 0.25200 * RK( 161 ) * C( ISPD     )                                                   
     &                          + 0.90000 * RK( 170 ) * C( ETOH     )                                                   
     &                          + 0.99100 * RK( 171 ) * C( ETHA     )                                                   
      PD( ALD2    ,XO2      ) =   0.90000 * RK( 109 ) * C( CXO3     )                                                   
      PD( ALD2    ,NTR      ) =   0.33000 * RK( 061 ) * C( OH       )                                                   
     &                          + 0.33000 * RK( 062 )                                                                   
      PD( ALD2    ,ROOH     ) =   0.50000 * RK( 063 ) * C( OH       )                                                   
     &                          + 0.50000 * RK( 064 )                                                                   
      PD( ALD2    ,ALD2     ) = -           RK( 083 ) * C( O        )                                                   
     &                          -           RK( 084 ) * C( OH       )                                                   
     &                          -           RK( 085 ) * C( NO3      )                                                   
     &                          -           RK( 086 )                                                                   
     &                          -           RK( 189 ) * C( CL       )                                                   
      PD( ALD2    ,PAR      ) =   0.06000 * RK( 112 ) * C( OH       )                                                   
     &                          + 0.06000 * RK( 182 ) * C( CL       )                                                   
      PD( ALD2    ,MEO2     ) =   0.90000 * RK( 108 ) * C( CXO3     )                                                   
      PD( ALD2    ,C2O3     ) =             RK( 111 ) * C( CXO3     )                                                   
      PD( ALD2    ,CXO3     ) =             RK( 102 ) * C( NO       )                                                   
     &                          + 0.90000 * RK( 108 ) * C( MEO2     )                                                   
     &                          + 0.90000 * RK( 109 ) * C( XO2      )                                                   
     &                          + 4.00000 * RK( 110 ) * C( CXO3     )                                                   
     &                          +           RK( 111 ) * C( C2O3     )                                                   
      PD( ALD2    ,PANX     ) =             RK( 106 ) * C( OH       )                                                   
      PD( ALD2    ,ROR      ) =   0.60000 * RK( 113 )                                                                   
      PD( ALD2    ,OLE      ) =   0.20000 * RK( 116 ) * C( O        )                                                   
     &                          + 0.33000 * RK( 117 ) * C( OH       )                                                   
     &                          + 0.18000 * RK( 118 ) * C( O3       )                                                   
     &                          + 0.35000 * RK( 119 ) * C( NO3      )                                                   
     &                          + 0.33000 * RK( 185 ) * C( CL       )                                                   
      PD( ALD2    ,IOLE     ) =   1.24000 * RK( 124 ) * C( O        )                                                   
     &                          + 1.30000 * RK( 125 ) * C( OH       )                                                   
     &                          + 0.65000 * RK( 126 ) * C( O3       )                                                   
     &                          + 1.18000 * RK( 127 ) * C( NO3      )                                                   
     &                          + 0.45000 * RK( 186 ) * C( CL       )                                                   
      PD( ALD2    ,ISPD     ) =   0.25200 * RK( 161 ) * C( OH       )                                                   
     &                          + 0.02000 * RK( 162 ) * C( O3       )                                                   
     &                          + 0.06700 * RK( 164 )                                                                   
      PD( ALD2    ,ETOH     ) =   0.90000 * RK( 170 ) * C( OH       )                                                   
     &                          +           RK( 192 ) * C( CL       )                                                   
      PD( ALD2    ,ETHA     ) =   0.99100 * RK( 171 ) * C( OH       )                                                   
     &                          + 0.99100 * RK( 183 ) * C( CL       )                                                   
      PD( ALD2    ,CL       ) =   0.06000 * RK( 182 ) * C( PAR      )                                                   
     &                          + 0.99100 * RK( 183 ) * C( ETHA     )                                                   
     &                          + 0.33000 * RK( 185 ) * C( OLE      )                                                   
     &                          + 0.45000 * RK( 186 ) * C( IOLE     )                                                   
     &                          -           RK( 189 ) * C( ALD2     )                                                   
     &                          +           RK( 192 ) * C( ETOH     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  20: ALDX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ALDX    ,NO2      ) =   0.80000 * RK( 172 ) * C( ISOP     )                                                   
      PD( ALDX    ,NO       ) =             RK( 151 ) * C( OPO3     )                                                   
      PD( ALDX    ,O        ) = -           RK( 098 ) * C( ALDX     )                                                   
     &                          + 0.30000 * RK( 116 ) * C( OLE      )                                                   
     &                          + 0.66000 * RK( 124 ) * C( IOLE     )                                                   
     &                          + 0.15000 * RK( 165 ) * C( TERP     )                                                   
      PD( ALDX    ,O3       ) =   0.32000 * RK( 118 ) * C( OLE      )                                                   
     &                          + 0.35000 * RK( 126 ) * C( IOLE     )                                                   
     &                          + 0.03000 * RK( 145 ) * C( OPEN     )                                                   
     &                          + 0.15000 * RK( 159 ) * C( ISOP     )                                                   
     &                          + 0.21000 * RK( 167 ) * C( TERP     )                                                   
      PD( ALDX    ,NO3      ) = -           RK( 100 ) * C( ALDX     )                                                   
     &                          + 0.56000 * RK( 119 ) * C( OLE      )                                                   
     &                          + 0.64000 * RK( 127 ) * C( IOLE     )                                                   
     &                          + 0.48000 * RK( 132 ) * C( CRES     )                                                   
     &                          + 0.80000 * RK( 160 ) * C( ISOP     )                                                   
     &                          + 0.35700 * RK( 163 ) * C( ISPD     )                                                   
     &                          + 0.47000 * RK( 168 ) * C( TERP     )                                                   
      PD( ALDX    ,OH       ) =   0.33000 * RK( 061 ) * C( NTR      )                                                   
     &                          + 0.50000 * RK( 063 ) * C( ROOH     )                                                   
     &                          -           RK( 099 ) * C( ALDX     )                                                   
     &                          + 0.05000 * RK( 112 ) * C( PAR      )                                                   
     &                          + 0.62000 * RK( 117 ) * C( OLE      )                                                   
     &                          + 0.22000 * RK( 121 ) * C( ETH      )                                                   
     &                          + 0.70000 * RK( 125 ) * C( IOLE     )                                                   
     &                          + 0.12000 * RK( 161 ) * C( ISPD     )                                                   
     &                          + 0.47000 * RK( 166 ) * C( TERP     )                                                   
     &                          + 0.05000 * RK( 170 ) * C( ETOH     )                                                   
      PD( ALDX    ,NTR      ) =   0.33000 * RK( 061 ) * C( OH       )                                                   
     &                          + 0.33000 * RK( 062 )                                                                   
      PD( ALDX    ,ROOH     ) =   0.50000 * RK( 063 ) * C( OH       )                                                   
     &                          + 0.50000 * RK( 064 )                                                                   
      PD( ALDX    ,ALDX     ) = -           RK( 098 ) * C( O        )                                                   
     &                          -           RK( 099 ) * C( OH       )                                                   
     &                          -           RK( 100 ) * C( NO3      )                                                   
     &                          -           RK( 101 )                                                                   
     &                          -           RK( 190 ) * C( CL       )                                                   
      PD( ALDX    ,PAR      ) =   0.05000 * RK( 112 ) * C( OH       )                                                   
     &                          + 0.05000 * RK( 182 ) * C( CL       )                                                   
      PD( ALDX    ,ROR      ) =   0.50000 * RK( 113 )                                                                   
      PD( ALDX    ,OLE      ) =   0.30000 * RK( 116 ) * C( O        )                                                   
     &                          + 0.62000 * RK( 117 ) * C( OH       )                                                   
     &                          + 0.32000 * RK( 118 ) * C( O3       )                                                   
     &                          + 0.56000 * RK( 119 ) * C( NO3      )                                                   
     &                          + 0.67000 * RK( 185 ) * C( CL       )                                                   
      PD( ALDX    ,ETH      ) =   0.22000 * RK( 121 ) * C( OH       )                                                   
      PD( ALDX    ,IOLE     ) =   0.66000 * RK( 124 ) * C( O        )                                                   
     &                          + 0.70000 * RK( 125 ) * C( OH       )                                                   
     &                          + 0.35000 * RK( 126 ) * C( O3       )                                                   
     &                          + 0.64000 * RK( 127 ) * C( NO3      )                                                   
     &                          + 0.55000 * RK( 186 ) * C( CL       )                                                   
      PD( ALDX    ,CRES     ) =   0.48000 * RK( 132 ) * C( NO3      )                                                   
      PD( ALDX    ,OPEN     ) =   0.03000 * RK( 145 ) * C( O3       )                                                   
      PD( ALDX    ,OPO3     ) =             RK( 151 ) * C( NO       )                                                   
      PD( ALDX    ,ISOP     ) =   0.15000 * RK( 159 ) * C( O3       )                                                   
     &                          + 0.80000 * RK( 160 ) * C( NO3      )                                                   
     &                          + 0.80000 * RK( 172 ) * C( NO2      )                                                   
      PD( ALDX    ,ISPD     ) =   0.12000 * RK( 161 ) * C( OH       )                                                   
     &                          + 0.35700 * RK( 163 ) * C( NO3      )                                                   
      PD( ALDX    ,TERP     ) =   0.15000 * RK( 165 ) * C( O        )                                                   
     &                          + 0.47000 * RK( 166 ) * C( OH       )                                                   
     &                          + 0.21000 * RK( 167 ) * C( O3       )                                                   
     &                          + 0.47000 * RK( 168 ) * C( NO3      )                                                   
      PD( ALDX    ,ETOH     ) =   0.05000 * RK( 170 ) * C( OH       )                                                   
      PD( ALDX    ,CL       ) =   0.05000 * RK( 182 ) * C( PAR      )                                                   
     &                          + 0.67000 * RK( 185 ) * C( OLE      )                                                   
     &                          + 0.55000 * RK( 186 ) * C( IOLE     )                                                   
     &                          -           RK( 190 ) * C( ALDX     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  21: PAR                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAR     ,NO2      ) =   2.40000 * RK( 172 ) * C( ISOP     )                                                   
      PD( PAR     ,O        ) =   0.20000 * RK( 116 ) * C( OLE      )                                                   
     &                          + 0.10000 * RK( 124 ) * C( IOLE     )                                                   
     &                          + 0.25000 * RK( 157 ) * C( ISOP     )                                                   
     &                          + 5.12000 * RK( 165 ) * C( TERP     )                                                   
      PD( PAR     ,O3       ) = -           RK( 118 ) * C( OLE      )                                                   
     &                          + 0.35000 * RK( 159 ) * C( ISOP     )                                                   
     &                          + 0.36000 * RK( 162 ) * C( ISPD     )                                                   
     &                          + 7.00000 * RK( 167 ) * C( TERP     )                                                   
      PD( PAR     ,NO3      ) = -           RK( 119 ) * C( OLE      )                                                   
     &                          + 2.40000 * RK( 160 ) * C( ISOP     )                                                   
     &                          + 1.28200 * RK( 163 ) * C( ISPD     )                                                   
      PD( PAR     ,OH       ) = - 0.66000 * RK( 061 ) * C( NTR      )                                                   
     &                          - 1.11000 * RK( 112 ) * C( PAR      )                                                   
     &                          - 0.70000 * RK( 117 ) * C( OLE      )                                                   
     &                          + 1.10000 * RK( 154 ) * C( XYL      )                                                   
     &                          + 1.56500 * RK( 161 ) * C( ISPD     )                                                   
     &                          + 1.66000 * RK( 166 ) * C( TERP     )                                                   
      PD( PAR     ,NTR      ) = - 0.66000 * RK( 061 ) * C( OH       )                                                   
     &                          - 0.66000 * RK( 062 )                                                                   
      PD( PAR     ,PAR      ) = - 1.11000 * RK( 112 ) * C( OH       )                                                   
     &                          - 1.11000 * RK( 182 ) * C( CL       )                                                   
      PD( PAR     ,ROR      ) = - 2.10000 * RK( 113 )                                                                   
      PD( PAR     ,OLE      ) =   0.20000 * RK( 116 ) * C( O        )                                                   
     &                          - 0.70000 * RK( 117 ) * C( OH       )                                                   
     &                          -           RK( 118 ) * C( O3       )                                                   
     &                          -           RK( 119 ) * C( NO3      )                                                   
     &                          -           RK( 185 ) * C( CL       )                                                   
      PD( PAR     ,IOLE     ) =   0.10000 * RK( 124 ) * C( O        )                                                   
     &                          + 0.30000 * RK( 186 ) * C( CL       )                                                   
      PD( PAR     ,XYL      ) =   1.10000 * RK( 154 ) * C( OH       )                                                   
      PD( PAR     ,ISOP     ) =   0.25000 * RK( 157 ) * C( O        )                                                   
     &                          + 0.35000 * RK( 159 ) * C( O3       )                                                   
     &                          + 2.40000 * RK( 160 ) * C( NO3      )                                                   
     &                          + 2.40000 * RK( 172 ) * C( NO2      )                                                   
      PD( PAR     ,ISPD     ) =   1.56500 * RK( 161 ) * C( OH       )                                                   
     &                          + 0.36000 * RK( 162 ) * C( O3       )                                                   
     &                          + 1.28200 * RK( 163 ) * C( NO3      )                                                   
     &                          + 0.83200 * RK( 164 )                                                                   
      PD( PAR     ,TERP     ) =   5.12000 * RK( 165 ) * C( O        )                                                   
     &                          + 1.66000 * RK( 166 ) * C( OH       )                                                   
     &                          + 7.00000 * RK( 167 ) * C( O3       )                                                   
      PD( PAR     ,CL       ) = - 1.11000 * RK( 182 ) * C( PAR      )                                                   
     &                          -           RK( 185 ) * C( OLE      )                                                   
     &                          + 0.30000 * RK( 186 ) * C( IOLE     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  22: CO                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CO      ,NO       ) =   0.33600 * RK( 129 ) * C( TO2      )                                                   
     &                          + 0.34400 * RK( 149 ) * C( CAO2     )                                                   
      PD( CO      ,O        ) =             RK( 076 ) * C( FORM     )                                                   
     &                          + 0.20000 * RK( 116 ) * C( OLE      )                                                   
     &                          +           RK( 120 ) * C( ETH      )                                                   
     &                          + 0.10000 * RK( 124 ) * C( IOLE     )                                                   
      PD( CO      ,O3       ) =   0.33000 * RK( 118 ) * C( OLE      )                                                   
     &                          + 0.63000 * RK( 122 ) * C( ETH      )                                                   
     &                          + 0.25000 * RK( 126 ) * C( IOLE     )                                                   
     &                          + 0.69000 * RK( 145 ) * C( OPEN     )                                                   
     &                          + 0.06600 * RK( 159 ) * C( ISOP     )                                                   
     &                          + 0.22500 * RK( 162 ) * C( ISPD     )                                                   
     &                          + 0.00100 * RK( 167 ) * C( TERP     )                                                   
      PD( CO      ,NO3      ) =             RK( 077 ) * C( FORM     )                                                   
     &                          + 0.24000 * RK( 132 ) * C( CRES     )                                                   
     &                          + 0.64300 * RK( 163 ) * C( ISPD     )                                                   
      PD( CO      ,OH       ) = -           RK( 065 ) * C( CO       )                                                   
     &                          +           RK( 073 ) * C( FORM     )                                                   
     &                          + 0.06000 * RK( 131 ) * C( CRES     )                                                   
     &                          + 0.33400 * RK( 161 ) * C( ISPD     )                                                   
     &                          +           RK( 179 ) * C( FMCL     )                                                   
      PD( CO      ,FORM     ) =             RK( 073 ) * C( OH       )                                                   
     &                          +           RK( 074 )                                                                   
     &                          +           RK( 075 )                                                                   
     &                          +           RK( 076 ) * C( O        )                                                   
     &                          +           RK( 077 ) * C( NO3      )                                                   
     &                          +           RK( 188 ) * C( CL       )                                                   
      PD( CO      ,ALD2     ) =             RK( 086 )                                                                   
      PD( CO      ,ALDX     ) =             RK( 101 )                                                                   
      PD( CO      ,CO       ) = -           RK( 065 ) * C( OH       )                                                   
      PD( CO      ,OLE      ) =   0.20000 * RK( 116 ) * C( O        )                                                   
     &                          + 0.33000 * RK( 118 ) * C( O3       )                                                   
      PD( CO      ,ETH      ) =             RK( 120 ) * C( O        )                                                   
     &                          + 0.63000 * RK( 122 ) * C( O3       )                                                   
      PD( CO      ,IOLE     ) =   0.10000 * RK( 124 ) * C( O        )                                                   
     &                          + 0.25000 * RK( 126 ) * C( O3       )                                                   
      PD( CO      ,CRES     ) =   0.06000 * RK( 131 ) * C( OH       )                                                   
     &                          + 0.24000 * RK( 132 ) * C( NO3      )                                                   
      PD( CO      ,TO2      ) =   0.33600 * RK( 129 ) * C( NO       )                                                   
      PD( CO      ,OPEN     ) =             RK( 143 )                                                                   
     &                          + 0.69000 * RK( 145 ) * C( O3       )                                                   
      PD( CO      ,MGLY     ) =             RK( 156 )                                                                   
      PD( CO      ,CAO2     ) =   0.34400 * RK( 149 ) * C( NO       )                                                   
      PD( CO      ,ISOP     ) =   0.06600 * RK( 159 ) * C( O3       )                                                   
      PD( CO      ,ISPD     ) =   0.33400 * RK( 161 ) * C( OH       )                                                   
     &                          + 0.22500 * RK( 162 ) * C( O3       )                                                   
     &                          + 0.64300 * RK( 163 ) * C( NO3      )                                                   
     &                          + 0.33300 * RK( 164 )                                                                   
      PD( CO      ,TERP     ) =   0.00100 * RK( 167 ) * C( O3       )                                                   
      PD( CO      ,CL       ) =             RK( 188 ) * C( FORM     )                                                   
      PD( CO      ,FMCL     ) =             RK( 179 ) * C( OH       )                                                   
     &                          +           RK( 180 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  23: MEO2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEO2    ,NO       ) = -           RK( 067 ) * C( MEO2     )                                                   
     &                          +           RK( 087 ) * C( C2O3     )                                                   
      PD( MEO2    ,OH       ) =             RK( 066 )                                                                   
     &                          + 0.70000 * RK( 070 ) * C( MEPX     )                                                   
     &                          +           RK( 097 ) * C( AACD     )                                                   
      PD( MEO2    ,HO2      ) = -           RK( 068 ) * C( MEO2     )                                                   
      PD( MEO2    ,XO2      ) =   0.90000 * RK( 093 ) * C( C2O3     )                                                   
      PD( MEO2    ,ALD2     ) =             RK( 086 )                                                                   
      PD( MEO2    ,ALDX     ) =             RK( 101 )                                                                   
      PD( MEO2    ,MEO2     ) = -           RK( 067 ) * C( NO       )                                                   
     &                          -           RK( 068 ) * C( HO2      )                                                   
     &                          - 4.00000 * RK( 069 ) * C( MEO2     )                                                   
     &                          - 0.10000 * RK( 092 ) * C( C2O3     )                                                   
     &                          -           RK( 108 ) * C( CXO3     )                                                   
      PD( MEO2    ,MEPX     ) =   0.70000 * RK( 070 ) * C( OH       )                                                   
      PD( MEO2    ,C2O3     ) =             RK( 087 ) * C( NO       )                                                   
     &                          - 0.10000 * RK( 092 ) * C( MEO2     )                                                   
     &                          + 0.90000 * RK( 093 ) * C( XO2      )                                                   
     &                          + 4.00000 * RK( 094 ) * C( C2O3     )                                                   
     &                          +           RK( 111 ) * C( CXO3     )                                                   
      PD( MEO2    ,PACD     ) =             RK( 096 )                                                                   
      PD( MEO2    ,AACD     ) =             RK( 097 ) * C( OH       )                                                   
      PD( MEO2    ,CXO3     ) = -           RK( 108 ) * C( MEO2     )                                                   
     &                          +           RK( 111 ) * C( C2O3     )                                                   
      PD( MEO2    ,CL       ) =             RK( 181 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  24: MEPX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEPX    ,OH       ) = -           RK( 070 ) * C( MEPX     )                                                   
      PD( MEPX    ,HO2      ) =             RK( 068 ) * C( MEO2     )                                                   
     &                          +           RK( 081 ) * C( HCO3     )                                                   
      PD( MEPX    ,MEO2     ) =             RK( 068 ) * C( HO2      )                                                   
      PD( MEPX    ,MEPX     ) = -           RK( 070 ) * C( OH       )                                                   
     &                          -           RK( 071 )                                                                   
      PD( MEPX    ,HCO3     ) =             RK( 081 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  25: MEOH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MEOH    ,OH       ) = -           RK( 072 ) * C( MEOH     )                                                   
      PD( MEOH    ,MEO2     ) =   4.00000 * RK( 069 ) * C( MEO2     )                                                   
      PD( MEOH    ,MEOH     ) = -           RK( 072 ) * C( OH       )                                                   
     &                          -           RK( 191 ) * C( CL       )                                                   
      PD( MEOH    ,CL       ) = -           RK( 191 ) * C( MEOH     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  26: HCO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HCO3    ,NO       ) = -           RK( 080 ) * C( HCO3     )                                                   
      PD( HCO3    ,HO2      ) =             RK( 078 ) * C( FORM     )                                                   
     &                          -           RK( 081 ) * C( HCO3     )                                                   
      PD( HCO3    ,FORM     ) =             RK( 078 ) * C( HO2      )                                                   
      PD( HCO3    ,HCO3     ) = -           RK( 079 )                                                                   
     &                          -           RK( 080 ) * C( NO       )                                                   
     &                          -           RK( 081 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  27: FACD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FACD    ,NO       ) =             RK( 080 ) * C( HCO3     )                                                   
      PD( FACD    ,O3       ) =   0.37000 * RK( 122 ) * C( ETH      )                                                   
      PD( FACD    ,OH       ) = -           RK( 082 ) * C( FACD     )                                                   
      PD( FACD    ,HCO3     ) =             RK( 080 ) * C( NO       )                                                   
      PD( FACD    ,FACD     ) = -           RK( 082 ) * C( OH       )                                                   
      PD( FACD    ,ETH      ) =   0.37000 * RK( 122 ) * C( O3       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  28: C2O3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( C2O3    ,NO2      ) = -           RK( 088 ) * C( C2O3     )                                                   
      PD( C2O3    ,NO       ) = -           RK( 087 ) * C( C2O3     )                                                   
      PD( C2O3    ,O        ) =             RK( 083 ) * C( ALD2     )                                                   
      PD( C2O3    ,O3       ) =   0.11400 * RK( 162 ) * C( ISPD     )                                                   
      PD( C2O3    ,NO3      ) =             RK( 085 ) * C( ALD2     )                                                   
      PD( C2O3    ,OH       ) =             RK( 084 ) * C( ALD2     )                                                   
     &                          +           RK( 095 ) * C( PACD     )                                                   
     &                          +           RK( 155 ) * C( MGLY     )                                                   
     &                          + 0.21000 * RK( 161 ) * C( ISPD     )                                                   
      PD( C2O3    ,HO2      ) = -           RK( 091 ) * C( C2O3     )                                                   
      PD( C2O3    ,XO2      ) = -           RK( 093 ) * C( C2O3     )                                                   
      PD( C2O3    ,ALD2     ) =             RK( 083 ) * C( O        )                                                   
     &                          +           RK( 084 ) * C( OH       )                                                   
     &                          +           RK( 085 ) * C( NO3      )                                                   
     &                          +           RK( 189 ) * C( CL       )                                                   
      PD( C2O3    ,MEO2     ) = -           RK( 092 ) * C( C2O3     )                                                   
      PD( C2O3    ,C2O3     ) = -           RK( 087 ) * C( NO       )                                                   
     &                          -           RK( 088 ) * C( NO2      )                                                   
     &                          -           RK( 091 ) * C( HO2      )                                                   
     &                          -           RK( 092 ) * C( MEO2     )                                                   
     &                          -           RK( 093 ) * C( XO2      )                                                   
     &                          - 4.00000 * RK( 094 ) * C( C2O3     )                                                   
     &                          -           RK( 111 ) * C( CXO3     )                                                   
      PD( C2O3    ,PAN      ) =             RK( 089 )                                                                   
     &                          +           RK( 090 )                                                                   
      PD( C2O3    ,PACD     ) =             RK( 095 ) * C( OH       )                                                   
      PD( C2O3    ,CXO3     ) = -           RK( 111 ) * C( C2O3     )                                                   
      PD( C2O3    ,MGLY     ) =             RK( 155 ) * C( OH       )                                                   
     &                          +           RK( 156 )                                                                   
      PD( C2O3    ,ISPD     ) =   0.21000 * RK( 161 ) * C( OH       )                                                   
     &                          + 0.11400 * RK( 162 ) * C( O3       )                                                   
     &                          + 0.96700 * RK( 164 )                                                                   
      PD( C2O3    ,CL       ) =             RK( 189 ) * C( ALD2     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  29: PAN                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PAN     ,NO2      ) =             RK( 088 ) * C( C2O3     )                                                   
      PD( PAN     ,C2O3     ) =             RK( 088 ) * C( NO2      )                                                   
      PD( PAN     ,PAN      ) = -           RK( 089 )                                                                   
     &                          -           RK( 090 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  30: PACD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PACD    ,OH       ) = -           RK( 095 ) * C( PACD     )                                                   
      PD( PACD    ,HO2      ) =   0.80000 * RK( 091 ) * C( C2O3     )                                                   
     &                          + 0.80000 * RK( 107 ) * C( CXO3     )                                                   
      PD( PACD    ,C2O3     ) =   0.80000 * RK( 091 ) * C( HO2      )                                                   
      PD( PACD    ,PACD     ) = -           RK( 095 ) * C( OH       )                                                   
     &                          -           RK( 096 )                                                                   
      PD( PACD    ,CXO3     ) =   0.80000 * RK( 107 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  31: AACD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( AACD    ,OH       ) = -           RK( 097 ) * C( AACD     )                                                   
      PD( AACD    ,HO2      ) =   0.20000 * RK( 091 ) * C( C2O3     )                                                   
     &                          + 0.20000 * RK( 107 ) * C( CXO3     )                                                   
      PD( AACD    ,XO2      ) =   0.10000 * RK( 093 ) * C( C2O3     )                                                   
     &                          + 0.10000 * RK( 109 ) * C( CXO3     )                                                   
      PD( AACD    ,MEO2     ) =   0.10000 * RK( 092 ) * C( C2O3     )                                                   
     &                          + 0.10000 * RK( 108 ) * C( CXO3     )                                                   
      PD( AACD    ,C2O3     ) =   0.20000 * RK( 091 ) * C( HO2      )                                                   
     &                          + 0.10000 * RK( 092 ) * C( MEO2     )                                                   
     &                          + 0.10000 * RK( 093 ) * C( XO2      )                                                   
      PD( AACD    ,AACD     ) = -           RK( 097 ) * C( OH       )                                                   
      PD( AACD    ,CXO3     ) =   0.20000 * RK( 107 ) * C( HO2      )                                                   
     &                          + 0.10000 * RK( 108 ) * C( MEO2     )                                                   
     &                          + 0.10000 * RK( 109 ) * C( XO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  32: CXO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CXO3    ,NO2      ) = -           RK( 103 ) * C( CXO3     )                                                   
      PD( CXO3    ,NO       ) = -           RK( 102 ) * C( CXO3     )                                                   
      PD( CXO3    ,O        ) =             RK( 098 ) * C( ALDX     )                                                   
     &                          + 0.25000 * RK( 157 ) * C( ISOP     )                                                   
      PD( CXO3    ,O3       ) =   0.20000 * RK( 159 ) * C( ISOP     )                                                   
     &                          + 0.39000 * RK( 167 ) * C( TERP     )                                                   
      PD( CXO3    ,NO3      ) =             RK( 100 ) * C( ALDX     )                                                   
     &                          + 0.07500 * RK( 163 ) * C( ISPD     )                                                   
      PD( CXO3    ,OH       ) =             RK( 099 ) * C( ALDX     )                                                   
     &                          + 0.25000 * RK( 161 ) * C( ISPD     )                                                   
      PD( CXO3    ,HO2      ) = -           RK( 107 ) * C( CXO3     )                                                   
      PD( CXO3    ,XO2      ) = -           RK( 109 ) * C( CXO3     )                                                   
      PD( CXO3    ,ALDX     ) =             RK( 098 ) * C( O        )                                                   
     &                          +           RK( 099 ) * C( OH       )                                                   
     &                          +           RK( 100 ) * C( NO3      )                                                   
     &                          +           RK( 190 ) * C( CL       )                                                   
      PD( CXO3    ,MEO2     ) = -           RK( 108 ) * C( CXO3     )                                                   
      PD( CXO3    ,C2O3     ) = -           RK( 111 ) * C( CXO3     )                                                   
      PD( CXO3    ,CXO3     ) = -           RK( 102 ) * C( NO       )                                                   
     &                          -           RK( 103 ) * C( NO2      )                                                   
     &                          -           RK( 107 ) * C( HO2      )                                                   
     &                          -           RK( 108 ) * C( MEO2     )                                                   
     &                          -           RK( 109 ) * C( XO2      )                                                   
     &                          - 4.00000 * RK( 110 ) * C( CXO3     )                                                   
     &                          -           RK( 111 ) * C( C2O3     )                                                   
      PD( CXO3    ,PANX     ) =             RK( 104 )                                                                   
     &                          +           RK( 105 )                                                                   
      PD( CXO3    ,ISOP     ) =   0.25000 * RK( 157 ) * C( O        )                                                   
     &                          + 0.20000 * RK( 159 ) * C( O3       )                                                   
      PD( CXO3    ,ISPD     ) =   0.25000 * RK( 161 ) * C( OH       )                                                   
     &                          + 0.07500 * RK( 163 ) * C( NO3      )                                                   
      PD( CXO3    ,TERP     ) =   0.39000 * RK( 167 ) * C( O3       )                                                   
      PD( CXO3    ,CL       ) =             RK( 190 ) * C( ALDX     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  33: PANX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( PANX    ,NO2      ) =             RK( 103 ) * C( CXO3     )                                                   
      PD( PANX    ,OH       ) = -           RK( 106 ) * C( PANX     )                                                   
      PD( PANX    ,CXO3     ) =             RK( 103 ) * C( NO2      )                                                   
      PD( PANX    ,PANX     ) = -           RK( 104 )                                                                   
     &                          -           RK( 105 )                                                                   
     &                          -           RK( 106 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  34: ROR                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ROR     ,NO2      ) = -           RK( 115 ) * C( ROR      )                                                   
      PD( ROR     ,OH       ) =   0.76000 * RK( 112 ) * C( PAR      )                                                   
      PD( ROR     ,PAR      ) =   0.76000 * RK( 112 ) * C( OH       )                                                   
     &                          + 0.76000 * RK( 182 ) * C( CL       )                                                   
      PD( ROR     ,ROR      ) = - 0.98000 * RK( 113 )                                                                   
     &                          -           RK( 114 )                                                                   
     &                          -           RK( 115 ) * C( NO2      )                                                   
      PD( ROR     ,CL       ) =   0.76000 * RK( 182 ) * C( PAR      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  35: OLE                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OLE     ,O        ) = -           RK( 116 ) * C( OLE      )                                                   
      PD( OLE     ,O3       ) = -           RK( 118 ) * C( OLE      )                                                   
      PD( OLE     ,NO3      ) = -           RK( 119 ) * C( OLE      )                                                   
      PD( OLE     ,OH       ) = -           RK( 117 ) * C( OLE      )                                                   
      PD( OLE     ,OLE      ) = -           RK( 116 ) * C( O        )                                                   
     &                          -           RK( 117 ) * C( OH       )                                                   
     &                          -           RK( 118 ) * C( O3       )                                                   
     &                          -           RK( 119 ) * C( NO3      )                                                   
     &                          -           RK( 185 ) * C( CL       )                                                   
      PD( OLE     ,IOLE     ) =   0.30000 * RK( 186 ) * C( CL       )                                                   
      PD( OLE     ,CL       ) = -           RK( 185 ) * C( OLE      )                                                   
     &                          + 0.30000 * RK( 186 ) * C( IOLE     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  36: ETH                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETH     ,O        ) = -           RK( 120 ) * C( ETH      )                                                   
      PD( ETH     ,O3       ) = -           RK( 122 ) * C( ETH      )                                                   
      PD( ETH     ,NO3      ) = -           RK( 123 ) * C( ETH      )                                                   
      PD( ETH     ,OH       ) = -           RK( 121 ) * C( ETH      )                                                   
      PD( ETH     ,ETH      ) = -           RK( 120 ) * C( O        )                                                   
     &                          -           RK( 121 ) * C( OH       )                                                   
     &                          -           RK( 122 ) * C( O3       )                                                   
     &                          -           RK( 123 ) * C( NO3      )                                                   
     &                          -           RK( 184 ) * C( CL       )                                                   
      PD( ETH     ,CL       ) = -           RK( 184 ) * C( ETH      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  37: IOLE                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( IOLE    ,O        ) = -           RK( 124 ) * C( IOLE     )                                                   
      PD( IOLE    ,O3       ) = -           RK( 126 ) * C( IOLE     )                                                   
      PD( IOLE    ,NO3      ) = -           RK( 127 ) * C( IOLE     )                                                   
      PD( IOLE    ,OH       ) = -           RK( 125 ) * C( IOLE     )                                                   
      PD( IOLE    ,IOLE     ) = -           RK( 124 ) * C( O        )                                                   
     &                          -           RK( 125 ) * C( OH       )                                                   
     &                          -           RK( 126 ) * C( O3       )                                                   
     &                          -           RK( 127 ) * C( NO3      )                                                   
     &                          -           RK( 186 ) * C( CL       )                                                   
      PD( IOLE    ,CL       ) = -           RK( 186 ) * C( IOLE     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  38: TOL                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOL     ,OH       ) = -           RK( 128 ) * C( TOL      )                                                   
      PD( TOL     ,TOL      ) = -           RK( 128 ) * C( OH       )                                                   
     &                          -           RK( 194 ) * C( CL       )                                                   
      PD( TOL     ,CL       ) = -           RK( 194 ) * C( TOL      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  39: CRES                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRES    ,NO3      ) = -           RK( 132 ) * C( CRES     )                                                   
      PD( CRES    ,OH       ) =   0.18000 * RK( 128 ) * C( TOL      )                                                   
     &                          -           RK( 131 ) * C( CRES     )                                                   
     &                          + 0.20000 * RK( 154 ) * C( XYL      )                                                   
      PD( CRES    ,HO2      ) =             RK( 134 ) * C( CRO      )                                                   
      PD( CRES    ,TOL      ) =   0.18000 * RK( 128 ) * C( OH       )                                                   
      PD( CRES    ,CRES     ) = -           RK( 131 ) * C( OH       )                                                   
     &                          -           RK( 132 ) * C( NO3      )                                                   
      PD( CRES    ,CRO      ) =             RK( 134 ) * C( HO2      )                                                   
      PD( CRES    ,XYL      ) =   0.20000 * RK( 154 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  40: TO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TO2     ,NO       ) = -           RK( 129 ) * C( TO2      )                                                   
      PD( TO2     ,OH       ) =   0.65000 * RK( 128 ) * C( TOL      )                                                   
     &                          + 0.30000 * RK( 154 ) * C( XYL      )                                                   
      PD( TO2     ,HO2      ) = -           RK( 130 ) * C( TO2      )                                                   
      PD( TO2     ,TOL      ) =   0.65000 * RK( 128 ) * C( OH       )                                                   
      PD( TO2     ,TO2      ) = -           RK( 129 ) * C( NO       )                                                   
     &                          -           RK( 130 ) * C( HO2      )                                                   
      PD( TO2     ,XYL      ) =   0.30000 * RK( 154 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  41: TOLRO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLRO2  ,NO       ) = -           RK( 198 ) * C( TOLRO2   )                                                   
      PD( TOLRO2  ,OH       ) =             RK( 128 ) * C( TOL      )                                                   
      PD( TOLRO2  ,HO2      ) = -           RK( 199 ) * C( TOLRO2   )                                                   
      PD( TOLRO2  ,TOL      ) =             RK( 128 ) * C( OH       )                                                   
      PD( TOLRO2  ,TOLRO2   ) = -           RK( 198 ) * C( NO       )                                                   
     &                          -           RK( 199 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  42: OPEN                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OPEN    ,NO       ) =   0.86000 * RK( 129 ) * C( TO2      )                                                   
      PD( OPEN    ,O3       ) = -           RK( 145 ) * C( OPEN     )                                                   
      PD( OPEN    ,NO3      ) =   0.12000 * RK( 132 ) * C( CRES     )                                                   
     &                          -           RK( 146 ) * C( OPEN     )                                                   
      PD( OPEN    ,OH       ) =   0.13000 * RK( 131 ) * C( CRES     )                                                   
     &                          -           RK( 144 ) * C( OPEN     )                                                   
      PD( OPEN    ,CRES     ) =   0.13000 * RK( 131 ) * C( OH       )                                                   
     &                          + 0.12000 * RK( 132 ) * C( NO3      )                                                   
      PD( OPEN    ,TO2      ) =   0.86000 * RK( 129 ) * C( NO       )                                                   
      PD( OPEN    ,OPEN     ) = -           RK( 143 )                                                                   
     &                          -           RK( 144 ) * C( OH       )                                                   
     &                          -           RK( 145 ) * C( O3       )                                                   
     &                          -           RK( 146 ) * C( NO3      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  43: MGLY                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( MGLY    ,NO       ) =   0.52000 * RK( 129 ) * C( TO2      )                                                   
      PD( MGLY    ,O3       ) =   0.20000 * RK( 145 ) * C( OPEN     )                                                   
     &                          + 0.85000 * RK( 162 ) * C( ISPD     )                                                   
      PD( MGLY    ,NO3      ) =   0.24000 * RK( 132 ) * C( CRES     )                                                   
      PD( MGLY    ,OH       ) =   0.80000 * RK( 154 ) * C( XYL      )                                                   
     &                          -           RK( 155 ) * C( MGLY     )                                                   
     &                          + 0.16800 * RK( 161 ) * C( ISPD     )                                                   
      PD( MGLY    ,CRES     ) =   0.24000 * RK( 132 ) * C( NO3      )                                                   
      PD( MGLY    ,TO2      ) =   0.52000 * RK( 129 ) * C( NO       )                                                   
      PD( MGLY    ,OPEN     ) =   0.20000 * RK( 145 ) * C( O3       )                                                   
      PD( MGLY    ,MGLY     ) = -           RK( 155 ) * C( OH       )                                                   
     &                          -           RK( 156 )                                                                   
      PD( MGLY    ,XYL      ) =   0.80000 * RK( 154 ) * C( OH       )                                                   
      PD( MGLY    ,ISPD     ) =   0.16800 * RK( 161 ) * C( OH       )                                                   
     &                          + 0.85000 * RK( 162 ) * C( O3       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  44: CRO                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRO     ,NO2      ) = -           RK( 133 ) * C( CRO      )                                                   
      PD( CRO     ,NO3      ) =   0.30000 * RK( 132 ) * C( CRES     )                                                   
     &                          +           RK( 148 ) * C( CAT1     )                                                   
      PD( CRO     ,OH       ) =   0.06000 * RK( 131 ) * C( CRES     )                                                   
      PD( CRO     ,HO2      ) = -           RK( 134 ) * C( CRO      )                                                   
      PD( CRO     ,CRES     ) =   0.06000 * RK( 131 ) * C( OH       )                                                   
     &                          + 0.30000 * RK( 132 ) * C( NO3      )                                                   
      PD( CRO     ,CRO      ) = -           RK( 133 ) * C( NO2      )                                                   
     &                          -           RK( 134 ) * C( HO2      )                                                   
      PD( CRO     ,CAT1     ) =             RK( 148 ) * C( NO3      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  45: CAT1                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CAT1    ,NO3      ) = -           RK( 148 ) * C( CAT1     )                                                   
      PD( CAT1    ,OH       ) =   0.73200 * RK( 131 ) * C( CRES     )                                                   
     &                          -           RK( 147 ) * C( CAT1     )                                                   
      PD( CAT1    ,CRES     ) =   0.73200 * RK( 131 ) * C( OH       )                                                   
      PD( CAT1    ,CAT1     ) = -           RK( 147 ) * C( OH       )                                                   
     &                          -           RK( 148 ) * C( NO3      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  46: CRON                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRON    ,NO2      ) =             RK( 133 ) * C( CRO      )                                                   
      PD( CRON    ,NO3      ) = -           RK( 136 ) * C( CRON     )                                                   
      PD( CRON    ,OH       ) = -           RK( 135 ) * C( CRON     )                                                   
      PD( CRON    ,CRO      ) =             RK( 133 ) * C( NO2      )                                                   
      PD( CRON    ,CRON     ) = -           RK( 135 ) * C( OH       )                                                   
     &                          -           RK( 136 ) * C( NO3      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  47: CRNO                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRNO    ,NO2      ) = -           RK( 137 ) * C( CRNO     )                                                   
      PD( CRNO    ,NO       ) =             RK( 139 ) * C( CRN2     )                                                   
      PD( CRNO    ,O3       ) = -           RK( 138 ) * C( CRNO     )                                                   
      PD( CRNO    ,NO3      ) =             RK( 136 ) * C( CRON     )                                                   
      PD( CRNO    ,OH       ) =             RK( 135 ) * C( CRON     )                                                   
      PD( CRNO    ,CRON     ) =             RK( 135 ) * C( OH       )                                                   
     &                          +           RK( 136 ) * C( NO3      )                                                   
      PD( CRNO    ,CRNO     ) = -           RK( 137 ) * C( NO2      )                                                   
     &                          -           RK( 138 ) * C( O3       )                                                   
      PD( CRNO    ,CRN2     ) =             RK( 139 ) * C( NO       )                                                   
      PD( CRNO    ,CRPX     ) =             RK( 141 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  48: CRN2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRN2    ,NO       ) = -           RK( 139 ) * C( CRN2     )                                                   
      PD( CRN2    ,O3       ) =             RK( 138 ) * C( CRNO     )                                                   
      PD( CRN2    ,OH       ) =             RK( 142 ) * C( CRPX     )                                                   
      PD( CRN2    ,HO2      ) = -           RK( 140 ) * C( CRN2     )                                                   
      PD( CRN2    ,CRNO     ) =             RK( 138 ) * C( O3       )                                                   
      PD( CRN2    ,CRN2     ) = -           RK( 139 ) * C( NO       )                                                   
     &                          -           RK( 140 ) * C( HO2      )                                                   
      PD( CRN2    ,CRPX     ) =             RK( 142 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  49: CRPX                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CRPX    ,OH       ) = -           RK( 142 ) * C( CRPX     )                                                   
      PD( CRPX    ,HO2      ) =             RK( 140 ) * C( CRN2     )                                                   
      PD( CRPX    ,CRN2     ) =             RK( 140 ) * C( HO2      )                                                   
      PD( CRPX    ,CRPX     ) = -           RK( 141 )                                                                   
     &                          -           RK( 142 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  50: OPO3                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OPO3    ,NO2      ) = -           RK( 152 ) * C( OPO3     )                                                   
      PD( OPO3    ,NO       ) = -           RK( 151 ) * C( OPO3     )                                                   
      PD( OPO3    ,O3       ) =   0.62000 * RK( 145 ) * C( OPEN     )                                                   
      PD( OPO3    ,NO3      ) =             RK( 146 ) * C( OPEN     )                                                   
      PD( OPO3    ,OH       ) =   0.60000 * RK( 144 ) * C( OPEN     )                                                   
      PD( OPO3    ,OPEN     ) =             RK( 143 )                                                                   
     &                          + 0.60000 * RK( 144 ) * C( OH       )                                                   
     &                          + 0.62000 * RK( 145 ) * C( O3       )                                                   
     &                          +           RK( 146 ) * C( NO3      )                                                   
      PD( OPO3    ,OPO3     ) = -           RK( 151 ) * C( NO       )                                                   
     &                          -           RK( 152 ) * C( NO2      )                                                   
      PD( OPO3    ,OPAN     ) =             RK( 153 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  51: CAO2                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CAO2    ,NO       ) = -           RK( 149 ) * C( CAO2     )                                                   
      PD( CAO2    ,OH       ) =   0.40000 * RK( 144 ) * C( OPEN     )                                                   
     &                          +           RK( 147 ) * C( CAT1     )                                                   
      PD( CAO2    ,HO2      ) = -           RK( 150 ) * C( CAO2     )                                                   
      PD( CAO2    ,OPEN     ) =   0.40000 * RK( 144 ) * C( OH       )                                                   
      PD( CAO2    ,CAT1     ) =             RK( 147 ) * C( OH       )                                                   
      PD( CAO2    ,CAO2     ) = -           RK( 149 ) * C( NO       )                                                   
     &                          -           RK( 150 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  52: OPAN                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( OPAN    ,NO2      ) =             RK( 152 ) * C( OPO3     )                                                   
      PD( OPAN    ,OPO3     ) =             RK( 152 ) * C( NO2      )                                                   
      PD( OPAN    ,OPAN     ) = -           RK( 153 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  53: XYL                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYL     ,OH       ) = -           RK( 154 ) * C( XYL      )                                                   
      PD( XYL     ,XYL      ) = -           RK( 154 ) * C( OH       )                                                   
     &                          -           RK( 195 ) * C( CL       )                                                   
      PD( XYL     ,CL       ) = -           RK( 195 ) * C( XYL      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  54: XYLRO2                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLRO2  ,NO       ) = -           RK( 200 ) * C( XYLRO2   )                                                   
      PD( XYLRO2  ,OH       ) =             RK( 154 ) * C( XYL      )                                                   
      PD( XYLRO2  ,HO2      ) = -           RK( 201 ) * C( XYLRO2   )                                                   
      PD( XYLRO2  ,XYL      ) =             RK( 154 ) * C( OH       )                                                   
      PD( XYLRO2  ,XYLRO2   ) = -           RK( 200 ) * C( NO       )                                                   
     &                          -           RK( 201 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  55: ISOP                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISOP    ,NO2      ) = -           RK( 172 ) * C( ISOP     )                                                   
      PD( ISOP    ,O        ) = -           RK( 157 ) * C( ISOP     )                                                   
      PD( ISOP    ,O3       ) = -           RK( 159 ) * C( ISOP     )                                                   
      PD( ISOP    ,NO3      ) = -           RK( 160 ) * C( ISOP     )                                                   
      PD( ISOP    ,OH       ) = -           RK( 158 ) * C( ISOP     )                                                   
      PD( ISOP    ,ISOP     ) = -           RK( 157 ) * C( O        )                                                   
     &                          -           RK( 158 ) * C( OH       )                                                   
     &                          -           RK( 159 ) * C( O3       )                                                   
     &                          -           RK( 160 ) * C( NO3      )                                                   
     &                          -           RK( 172 ) * C( NO2      )                                                   
     &                          -           RK( 187 ) * C( CL       )                                                   
      PD( ISOP    ,CL       ) = -           RK( 187 ) * C( ISOP     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  56: ISPD                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISPD    ,NO2      ) =   0.20000 * RK( 172 ) * C( ISOP     )                                                   
      PD( ISPD    ,O        ) =   0.75000 * RK( 157 ) * C( ISOP     )                                                   
      PD( ISPD    ,O3       ) =   0.65000 * RK( 159 ) * C( ISOP     )                                                   
     &                          -           RK( 162 ) * C( ISPD     )                                                   
      PD( ISPD    ,NO3      ) =   0.20000 * RK( 160 ) * C( ISOP     )                                                   
     &                          -           RK( 163 ) * C( ISPD     )                                                   
      PD( ISPD    ,OH       ) =   0.91200 * RK( 158 ) * C( ISOP     )                                                   
     &                          -           RK( 161 ) * C( ISPD     )                                                   
      PD( ISPD    ,ISOP     ) =   0.75000 * RK( 157 ) * C( O        )                                                   
     &                          + 0.91200 * RK( 158 ) * C( OH       )                                                   
     &                          + 0.65000 * RK( 159 ) * C( O3       )                                                   
     &                          + 0.20000 * RK( 160 ) * C( NO3      )                                                   
     &                          + 0.20000 * RK( 172 ) * C( NO2      )                                                   
     &                          +           RK( 187 ) * C( CL       )                                                   
      PD( ISPD    ,ISPD     ) = -           RK( 161 ) * C( OH       )                                                   
     &                          -           RK( 162 ) * C( O3       )                                                   
     &                          -           RK( 163 ) * C( NO3      )                                                   
     &                          -           RK( 164 )                                                                   
      PD( ISPD    ,CL       ) =             RK( 187 ) * C( ISOP     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  57: ISOPRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ISOPRXN ,OH       ) =             RK( 158 ) * C( ISOP     )                                                   
      PD( ISOPRXN ,ISOP     ) =             RK( 158 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  58: TERP                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TERP    ,O        ) = -           RK( 165 ) * C( TERP     )                                                   
      PD( TERP    ,O3       ) = -           RK( 167 ) * C( TERP     )                                                   
      PD( TERP    ,NO3      ) = -           RK( 168 ) * C( TERP     )                                                   
      PD( TERP    ,OH       ) = -           RK( 166 ) * C( TERP     )                                                   
      PD( TERP    ,TERP     ) = -           RK( 165 ) * C( O        )                                                   
     &                          -           RK( 166 ) * C( OH       )                                                   
     &                          -           RK( 167 ) * C( O3       )                                                   
     &                          -           RK( 168 ) * C( NO3      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  59: TRPRXN                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TRPRXN  ,O        ) =             RK( 165 ) * C( TERP     )                                                   
      PD( TRPRXN  ,O3       ) =             RK( 167 ) * C( TERP     )                                                   
      PD( TRPRXN  ,NO3      ) =             RK( 168 ) * C( TERP     )                                                   
      PD( TRPRXN  ,OH       ) =             RK( 166 ) * C( TERP     )                                                   
      PD( TRPRXN  ,TERP     ) =             RK( 165 ) * C( O        )                                                   
     &                          +           RK( 166 ) * C( OH       )                                                   
     &                          +           RK( 167 ) * C( O3       )                                                   
     &                          +           RK( 168 ) * C( NO3      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  60: SO2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SO2     ,OH       ) = -           RK( 169 ) * C( SO2      )                                                   
      PD( SO2     ,SO2      ) = -           RK( 169 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  61: SULF                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SULF    ,OH       ) =             RK( 169 ) * C( SO2      )                                                   
      PD( SULF    ,SO2      ) =             RK( 169 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  62: SULRXN                                                                     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SULRXN  ,OH       ) =             RK( 169 ) * C( SO2      )                                                   
      PD( SULRXN  ,SO2      ) =             RK( 169 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  63: ETOH                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETOH    ,OH       ) = -           RK( 170 ) * C( ETOH     )                                                   
      PD( ETOH    ,ETOH     ) = -           RK( 170 ) * C( OH       )                                                   
     &                          -           RK( 192 ) * C( CL       )                                                   
      PD( ETOH    ,CL       ) = -           RK( 192 ) * C( ETOH     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  64: ETHA                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( ETHA    ,OH       ) = -           RK( 171 ) * C( ETHA     )                                                   
      PD( ETHA    ,ETHA     ) = -           RK( 171 ) * C( OH       )                                                   
     &                          -           RK( 183 ) * C( CL       )                                                   
      PD( ETHA    ,CL       ) = -           RK( 183 ) * C( ETHA     )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  65: CL2                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CL2     ,CL2      ) = -           RK( 173 )                                                                   
      PD( CL2     ,CLO      ) =   4.00000 * RK( 176 ) * C( CLO      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  66: CL                                                                         
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CL      ,NO2      ) = -           RK( 196 ) * C( CL       )                                                   
      PD( CL      ,NO       ) =             RK( 177 ) * C( CLO      )                                                   
      PD( CL      ,O3       ) = -           RK( 175 ) * C( CL       )                                                   
      PD( CL      ,OH       ) =             RK( 179 ) * C( FMCL     )                                                   
     &                          +           RK( 193 ) * C( HCL      )                                                   
      PD( CL      ,FORM     ) = -           RK( 188 ) * C( CL       )                                                   
      PD( CL      ,ALD2     ) = -           RK( 189 ) * C( CL       )                                                   
      PD( CL      ,ALDX     ) = -           RK( 190 ) * C( CL       )                                                   
      PD( CL      ,PAR      ) = -           RK( 182 ) * C( CL       )                                                   
      PD( CL      ,MEOH     ) = -           RK( 191 ) * C( CL       )                                                   
      PD( CL      ,OLE      ) = -           RK( 185 ) * C( CL       )                                                   
      PD( CL      ,ETH      ) = -           RK( 184 ) * C( CL       )                                                   
      PD( CL      ,IOLE     ) = -           RK( 186 ) * C( CL       )                                                   
      PD( CL      ,TOL      ) = -           RK( 194 ) * C( CL       )                                                   
      PD( CL      ,XYL      ) = -           RK( 195 ) * C( CL       )                                                   
      PD( CL      ,ISOP     ) = -           RK( 187 ) * C( CL       )                                                   
      PD( CL      ,ETOH     ) = -           RK( 192 ) * C( CL       )                                                   
      PD( CL      ,ETHA     ) = -           RK( 183 ) * C( CL       )                                                   
      PD( CL      ,CL2      ) =   2.00000 * RK( 173 )                                                                   
      PD( CL      ,CL       ) = -           RK( 175 ) * C( O3       )                                                   
     &                          -           RK( 181 )                                                                   
     &                          -           RK( 182 ) * C( PAR      )                                                   
     &                          -           RK( 183 ) * C( ETHA     )                                                   
     &                          -           RK( 184 ) * C( ETH      )                                                   
     &                          -           RK( 185 ) * C( OLE      )                                                   
     &                          -           RK( 186 ) * C( IOLE     )                                                   
     &                          -           RK( 187 ) * C( ISOP     )                                                   
     &                          -           RK( 188 ) * C( FORM     )                                                   
     &                          -           RK( 189 ) * C( ALD2     )                                                   
     &                          -           RK( 190 ) * C( ALDX     )                                                   
     &                          -           RK( 191 ) * C( MEOH     )                                                   
     &                          -           RK( 192 ) * C( ETOH     )                                                   
     &                          -           RK( 194 ) * C( TOL      )                                                   
     &                          -           RK( 195 ) * C( XYL      )                                                   
     &                          -           RK( 196 ) * C( NO2      )                                                   
      PD( CL      ,HOCL     ) =             RK( 174 )                                                                   
      PD( CL      ,CLO      ) =   4.00000 * RK( 176 ) * C( CLO      )                                                   
     &                          +           RK( 177 ) * C( NO       )                                                   
      PD( CL      ,FMCL     ) =             RK( 179 ) * C( OH       )                                                   
     &                          +           RK( 180 )                                                                   
      PD( CL      ,HCL      ) =             RK( 193 ) * C( OH       )                                                   
      PD( CL      ,CLNO2    ) =             RK( 197 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  67: HOCL                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HOCL    ,HO2      ) =             RK( 178 ) * C( CLO      )                                                   
      PD( HOCL    ,HOCL     ) = -           RK( 174 )                                                                   
      PD( HOCL    ,CLO      ) =             RK( 178 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  68: CLO                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLO     ,NO       ) = -           RK( 177 ) * C( CLO      )                                                   
      PD( CLO     ,O3       ) =             RK( 175 ) * C( CL       )                                                   
      PD( CLO     ,HO2      ) = -           RK( 178 ) * C( CLO      )                                                   
      PD( CLO     ,CL       ) =             RK( 175 ) * C( O3       )                                                   
      PD( CLO     ,CLO      ) = - 4.00000 * RK( 176 ) * C( CLO      )                                                   
     &                          -           RK( 177 ) * C( NO       )                                                   
     &                          -           RK( 178 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  69: FMCL                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( FMCL    ,OH       ) = -           RK( 179 ) * C( FMCL     )                                                   
      PD( FMCL    ,OLE      ) =             RK( 185 ) * C( CL       )                                                   
      PD( FMCL    ,ETH      ) =             RK( 184 ) * C( CL       )                                                   
      PD( FMCL    ,IOLE     ) =   0.70000 * RK( 186 ) * C( CL       )                                                   
      PD( FMCL    ,ISOP     ) =   0.85000 * RK( 187 ) * C( CL       )                                                   
      PD( FMCL    ,CL       ) =             RK( 184 ) * C( ETH      )                                                   
     &                          +           RK( 185 ) * C( OLE      )                                                   
     &                          + 0.70000 * RK( 186 ) * C( IOLE     )                                                   
     &                          + 0.85000 * RK( 187 ) * C( ISOP     )                                                   
      PD( FMCL    ,FMCL     ) = -           RK( 179 ) * C( OH       )                                                   
     &                          -           RK( 180 )                                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  70: HCL                                                                        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( HCL     ,OH       ) = -           RK( 193 ) * C( HCL      )                                                   
      PD( HCL     ,FORM     ) =             RK( 188 ) * C( CL       )                                                   
      PD( HCL     ,ALD2     ) =             RK( 189 ) * C( CL       )                                                   
      PD( HCL     ,ALDX     ) =             RK( 190 ) * C( CL       )                                                   
      PD( HCL     ,PAR      ) =             RK( 182 ) * C( CL       )                                                   
      PD( HCL     ,MEOH     ) =             RK( 191 ) * C( CL       )                                                   
      PD( HCL     ,IOLE     ) =   0.30000 * RK( 186 ) * C( CL       )                                                   
      PD( HCL     ,TOL      ) =             RK( 194 ) * C( CL       )                                                   
      PD( HCL     ,XYL      ) =             RK( 195 ) * C( CL       )                                                   
      PD( HCL     ,ISOP     ) =   0.15000 * RK( 187 ) * C( CL       )                                                   
      PD( HCL     ,ETOH     ) =             RK( 192 ) * C( CL       )                                                   
      PD( HCL     ,ETHA     ) =             RK( 183 ) * C( CL       )                                                   
      PD( HCL     ,CL       ) =             RK( 181 )                                                                   
     &                          +           RK( 182 ) * C( PAR      )                                                   
     &                          +           RK( 183 ) * C( ETHA     )                                                   
     &                          + 0.30000 * RK( 186 ) * C( IOLE     )                                                   
     &                          + 0.15000 * RK( 187 ) * C( ISOP     )                                                   
     &                          +           RK( 188 ) * C( FORM     )                                                   
     &                          +           RK( 189 ) * C( ALD2     )                                                   
     &                          +           RK( 190 ) * C( ALDX     )                                                   
     &                          +           RK( 191 ) * C( MEOH     )                                                   
     &                          +           RK( 192 ) * C( ETOH     )                                                   
     &                          +           RK( 194 ) * C( TOL      )                                                   
     &                          +           RK( 195 ) * C( XYL      )                                                   
      PD( HCL     ,HCL      ) = -           RK( 193 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  71: CLNO2                                                                      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( CLNO2   ,NO2      ) =             RK( 196 ) * C( CL       )                                                   
      PD( CLNO2   ,CL       ) =             RK( 196 ) * C( NO2      )                                                   
      PD( CLNO2   ,CLNO2    ) = -           RK( 197 )                                                                   
      PD( CLNO2   ,H2NO3PIJ ) =             RK( 212 ) * C(          )                                                   
     &                          +           RK( 213 ) * C(          )                                                   
      PD( CLNO2   ,H2NO3PK  ) =             RK( 214 ) * C(          )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  72: TOLNRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLNRXN ,NO       ) =             RK( 198 ) * C( TOLRO2   )                                                   
      PD( TOLNRXN ,TOLRO2   ) =             RK( 198 ) * C( NO       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  73: TOLHRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( TOLHRXN ,HO2      ) =             RK( 199 ) * C( TOLRO2   )                                                   
      PD( TOLHRXN ,TOLRO2   ) =             RK( 199 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  74: XYLNRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLNRXN ,NO       ) =             RK( 200 ) * C( XYLRO2   )                                                   
      PD( XYLNRXN ,XYLRO2   ) =             RK( 200 ) * C( NO       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  75: XYLHRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( XYLHRXN ,HO2      ) =             RK( 201 ) * C( XYLRO2   )                                                   
      PD( XYLHRXN ,XYLRO2   ) =             RK( 201 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  76: BENZENE                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BENZENE ,OH       ) = -           RK( 202 ) * C( BENZENE  )                                                   
      PD( BENZENE ,BENZENE  ) = -           RK( 202 ) * C( OH       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  77: BENZRO2                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BENZRO2 ,NO       ) = -           RK( 203 ) * C( BENZRO2  )                                                   
      PD( BENZRO2 ,OH       ) =             RK( 202 ) * C( BENZENE  )                                                   
      PD( BENZRO2 ,HO2      ) = -           RK( 204 ) * C( BENZRO2  )                                                   
      PD( BENZRO2 ,BENZENE  ) =             RK( 202 ) * C( OH       )                                                   
      PD( BENZRO2 ,BENZRO2  ) = -           RK( 203 ) * C( NO       )                                                   
     &                          -           RK( 204 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  78: BNZNRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BNZNRXN ,NO       ) =             RK( 203 ) * C( BENZRO2  )                                                   
      PD( BNZNRXN ,BENZRO2  ) =             RK( 203 ) * C( NO       )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  79: BNZHRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( BNZHRXN ,HO2      ) =             RK( 204 ) * C( BENZRO2  )                                                   
      PD( BNZHRXN ,BENZRO2  ) =             RK( 204 ) * C( HO2      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  80: SESQ                                                                       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SESQ    ,O3       ) = -           RK( 205 ) * C( SESQ     )                                                   
      PD( SESQ    ,NO3      ) = -           RK( 207 ) * C( SESQ     )                                                   
      PD( SESQ    ,OH       ) = -           RK( 206 ) * C( SESQ     )                                                   
      PD( SESQ    ,SESQ     ) = -           RK( 205 ) * C( O3       )                                                   
     &                          -           RK( 206 ) * C( OH       )                                                   
     &                          -           RK( 207 ) * C( NO3      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  81: SESQRXN                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( SESQRXN ,O3       ) =             RK( 205 ) * C( SESQ     )                                                   
      PD( SESQRXN ,NO3      ) =             RK( 207 ) * C( SESQ     )                                                   
      PD( SESQRXN ,OH       ) =             RK( 206 ) * C( SESQ     )                                                   
      PD( SESQRXN ,SESQ     ) =             RK( 205 ) * C( O3       )                                                   
     &                          +           RK( 206 ) * C( OH       )                                                   
     &                          +           RK( 207 ) * C( NO3      )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  82: H2NO3PIJ                                                                   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( H2NO3PIJ,N2O5     ) =             RK( 208 )                                                                   
      PD( H2NO3PIJ,H2NO3PIJ ) = -           RK( 210 )                                                                   
     &                          -           RK( 212 ) * C(          )                                                   
     &                          -           RK( 213 ) * C(          )                                                   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  PDs of the rate of change of species  83: H2NO3PK                                                                    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PD( H2NO3PK ,N2O5     ) =             RK( 209 )                                                                   
      PD( H2NO3PK ,H2NO3PK  ) = -           RK( 211 )                                                                   
     &                          -           RK( 214 ) * C(          )                                                   


      RETURN                                                                                                            
      END                                                                                                               
