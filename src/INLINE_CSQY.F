      MODULE CSQY_DATA

      IMPLICIT NONE

C.....PARAMETERS and their descriptions:

      INTEGER, PARAMETER :: NPHOT_REF =  77 ! # ref phot reactions 

      INTEGER, PARAMETER :: NTEMP     =   3 ! # ref temperatures 

      INTEGER, PARAMETER :: NWL_REF   =   7 ! # ref wavelengths 

C...Names of the mapped photolysis reactions (available to chemical)
C... mechanisms) and their pointers to the reference photolysis rxn


      INTEGER, PARAMETER :: INO2-06           =   1 ! pointer to NO2-06          
      INTEGER, PARAMETER :: INO3NO-06         =   2 ! pointer to NO3NO-06        
      INTEGER, PARAMETER :: INO3NO2-6         =   3 ! pointer to NO3NO2-6        
      INTEGER, PARAMETER :: IO3O1D-06         =   4 ! pointer to O3O1D-06        
      INTEGER, PARAMETER :: IO3O3P-06         =   5 ! pointer to O3O3P-06        
      INTEGER, PARAMETER :: IHONO-06          =   6 ! pointer to HONO-06         
      INTEGER, PARAMETER :: IHNO3             =   7 ! pointer to HNO3            
      INTEGER, PARAMETER :: IHNO4-06          =   8 ! pointer to HNO4-06         
      INTEGER, PARAMETER :: IH2O2             =   9 ! pointer to H2O2            
      INTEGER, PARAMETER :: INO2EX            =  10 ! pointer to NO2EX           
      INTEGER, PARAMETER :: IPAN              =  11 ! pointer to PAN             
      INTEGER, PARAMETER :: IHCHOR-06         =  12 ! pointer to HCHOR-06        
      INTEGER, PARAMETER :: IHCHOM-06         =  13 ! pointer to HCHOM-06        
      INTEGER, PARAMETER :: ICCHO_R           =  14 ! pointer to CCHO_R          
      INTEGER, PARAMETER :: IC2CHO            =  15 ! pointer to C2CHO           
      INTEGER, PARAMETER :: IACET-06          =  16 ! pointer to ACET-06         
      INTEGER, PARAMETER :: IMEK-06           =  17 ! pointer to MEK-06          
      INTEGER, PARAMETER :: ICOOH             =  18 ! pointer to COOH            
      INTEGER, PARAMETER :: IGLY-07R          =  19 ! pointer to GLY-07R         
      INTEGER, PARAMETER :: IGLY-07M          =  20 ! pointer to GLY-07M         
      INTEGER, PARAMETER :: IMGLY-06          =  21 ! pointer to MGLY-06         
      INTEGER, PARAMETER :: IBACL-07          =  22 ! pointer to BACL-07         
      INTEGER, PARAMETER :: IBALD-06          =  23 ! pointer to BALD-06         
      INTEGER, PARAMETER :: IAFG1             =  24 ! pointer to AFG1            
      INTEGER, PARAMETER :: IMACR-06          =  25 ! pointer to MACR-06         
      INTEGER, PARAMETER :: IMVK-06           =  26 ! pointer to MVK-06          
      INTEGER, PARAMETER :: IIC3ONO2          =  27 ! pointer to IC3ONO2         
      INTEGER, PARAMETER :: IHOCCHO_IUPAC     =  28 ! pointer to HOCCHO_IUPAC    
      INTEGER, PARAMETER :: IACRO-09          =  29 ! pointer to ACRO-09         
      INTEGER, PARAMETER :: IPAA              =  30 ! pointer to PAA             
      INTEGER, PARAMETER :: ICL2              =  31 ! pointer to CL2             
      INTEGER, PARAMETER :: ICLNO-06          =  32 ! pointer to CLNO-06         
      INTEGER, PARAMETER :: ICLONO            =  33 ! pointer to CLONO           
      INTEGER, PARAMETER :: ICLNO2            =  34 ! pointer to CLNO2           
      INTEGER, PARAMETER :: ICLONO2-1         =  35 ! pointer to CLONO2-1        
      INTEGER, PARAMETER :: ICLONO2-2         =  36 ! pointer to CLONO2-2        
      INTEGER, PARAMETER :: IHOCL-06          =  37 ! pointer to HOCL-06         
      INTEGER, PARAMETER :: ICLCCHO           =  38 ! pointer to CLCCHO          
      INTEGER, PARAMETER :: ICLACET           =  39 ! pointer to CLACET          
      INTEGER, PARAMETER :: INO2_SAPRC99      =  40 ! pointer to NO2_SAPRC99     
      INTEGER, PARAMETER :: INO3NO_SAPRC99    =  41 ! pointer to NO3NO_SAPRC99   
      INTEGER, PARAMETER :: INO3NO2_SAPRC99   =  42 ! pointer to NO3NO2_SAPRC99  
      INTEGER, PARAMETER :: IO3O3P_SAPRC99    =  43 ! pointer to O3O3P_SAPRC99   
      INTEGER, PARAMETER :: IO3O1D_SAPRC99    =  44 ! pointer to O3O1D_SAPRC99   
      INTEGER, PARAMETER :: IHONO_NO_SAPRC99  =  45 ! pointer to HONO_NO_SAPRC99 
      INTEGER, PARAMETER :: IHONO_NO2_SAPRC99 =  46 ! pointer to HONO_NO2_SAPRC99
      INTEGER, PARAMETER :: IHNO3_SAPRC99     =  47 ! pointer to HNO3_SAPRC99    
      INTEGER, PARAMETER :: IHO2NO2_SAPRC99   =  48 ! pointer to HO2NO2_SAPRC99  
      INTEGER, PARAMETER :: IH2O2_SAPRC99     =  49 ! pointer to H2O2_SAPRC99    
      INTEGER, PARAMETER :: IHCHO_R_SAPRC99   =  50 ! pointer to HCHO_R_SAPRC99  
      INTEGER, PARAMETER :: IHCHO_M_SAPRC99   =  51 ! pointer to HCHO_M_SAPRC99  
      INTEGER, PARAMETER :: ICCHO_R_SAPRC99   =  52 ! pointer to CCHO_R_SAPRC99  
      INTEGER, PARAMETER :: IC2CHO_SAPRC99    =  53 ! pointer to C2CHO_SAPRC99   
      INTEGER, PARAMETER :: IACETONE_SAPRC99  =  54 ! pointer to ACETONE_SAPRC99 
      INTEGER, PARAMETER :: IKETONE_SAPRC99   =  55 ! pointer to KETONE_SAPRC99  
      INTEGER, PARAMETER :: ICOOH_SAPRC99     =  56 ! pointer to COOH_SAPRC99    
      INTEGER, PARAMETER :: IGLY_R_SAPRC99    =  57 ! pointer to GLY_R_SAPRC99   
      INTEGER, PARAMETER :: IGLY_ABS_SAPRC99  =  58 ! pointer to GLY_ABS_SAPRC99 
      INTEGER, PARAMETER :: IMGLY_ADJ_SAPRC99 =  59 ! pointer to MGLY_ADJ_SAPRC99
      INTEGER, PARAMETER :: IBACL_ADJ_SAPRC99 =  60 ! pointer to BACL_ADJ_SAPRC99
      INTEGER, PARAMETER :: IBZCHO_SAPRC99    =  61 ! pointer to BZCHO_SAPRC99   
      INTEGER, PARAMETER :: IACROLEIN_SAPRC99 =  62 ! pointer to ACROLEIN_SAPRC99
      INTEGER, PARAMETER :: IIC3ONO2_SAPRC99  =  63 ! pointer to IC3ONO2_SAPRC99 
      INTEGER, PARAMETER :: IMGLY_ABS_SAPRC99 =  64 ! pointer to MGLY_ABS_SAPRC99
      INTEGER, PARAMETER :: IO3_O3P_IUPAC04   =  65 ! pointer to O3_O3P_IUPAC04  
      INTEGER, PARAMETER :: IO3_O1D_IUPAC04   =  66 ! pointer to O3_O1D_IUPAC04  
      INTEGER, PARAMETER :: IHONO_IUPAC04     =  67 ! pointer to HONO_IUPAC04    
      INTEGER, PARAMETER :: IHO2NO2_IUPAC04   =  68 ! pointer to HO2NO2_IUPAC04  
      INTEGER, PARAMETER :: IHNO3_IUPAC04     =  69 ! pointer to HNO3_IUPAC04    
      INTEGER, PARAMETER :: IN2O5_IUPAC04     =  70 ! pointer to N2O5_IUPAC04    
      INTEGER, PARAMETER :: INTR_IUPAC04      =  71 ! pointer to NTR_IUPAC04     
      INTEGER, PARAMETER :: IPAN_IUPAC04      =  72 ! pointer to PAN_IUPAC04     
      INTEGER, PARAMETER :: IPACD_CB05        =  73 ! pointer to PACD_CB05       
      INTEGER, PARAMETER :: IMGLY_IUPAC04     =  74 ! pointer to MGLY_IUPAC04    
      INTEGER, PARAMETER :: ICL2_IUPAC04      =  75 ! pointer to CL2_IUPAC04     
      INTEGER, PARAMETER :: IHOCL_IUPAC04     =  76 ! pointer to HOCL_IUPAC04    
      INTEGER, PARAMETER :: IFMCL_IUPAC04     =  77 ! pointer to FMCL_IUPAC04    

      DATA PNAME_REF( INO2-06           ) / 'NO2-06          ' /
      DATA PNAME_REF( INO3NO-06         ) / 'NO3NO-06        ' /
      DATA PNAME_REF( INO3NO2-6         ) / 'NO3NO2-6        ' /
      DATA PNAME_REF( IO3O1D-06         ) / 'O3O1D-06        ' /
      DATA PNAME_REF( IO3O3P-06         ) / 'O3O3P-06        ' /
      DATA PNAME_REF( IHONO-06          ) / 'HONO-06         ' /
      DATA PNAME_REF( IHNO3             ) / 'HNO3            ' /
      DATA PNAME_REF( IHNO4-06          ) / 'HNO4-06         ' /
      DATA PNAME_REF( IH2O2             ) / 'H2O2            ' /
      DATA PNAME_REF( INO2EX            ) / 'NO2EX           ' /
      DATA PNAME_REF( IPAN              ) / 'PAN             ' /
      DATA PNAME_REF( IHCHOR-06         ) / 'HCHOR-06        ' /
      DATA PNAME_REF( IHCHOM-06         ) / 'HCHOM-06        ' /
      DATA PNAME_REF( ICCHO_R           ) / 'CCHO_R          ' /
      DATA PNAME_REF( IC2CHO            ) / 'C2CHO           ' /
      DATA PNAME_REF( IACET-06          ) / 'ACET-06         ' /
      DATA PNAME_REF( IMEK-06           ) / 'MEK-06          ' /
      DATA PNAME_REF( ICOOH             ) / 'COOH            ' /
      DATA PNAME_REF( IGLY-07R          ) / 'GLY-07R         ' /
      DATA PNAME_REF( IGLY-07M          ) / 'GLY-07M         ' /
      DATA PNAME_REF( IMGLY-06          ) / 'MGLY-06         ' /
      DATA PNAME_REF( IBACL-07          ) / 'BACL-07         ' /
      DATA PNAME_REF( IBALD-06          ) / 'BALD-06         ' /
      DATA PNAME_REF( IAFG1             ) / 'AFG1            ' /
      DATA PNAME_REF( IMACR-06          ) / 'MACR-06         ' /
      DATA PNAME_REF( IMVK-06           ) / 'MVK-06          ' /
      DATA PNAME_REF( IIC3ONO2          ) / 'IC3ONO2         ' /
      DATA PNAME_REF( IHOCCHO_IUPAC     ) / 'HOCCHO_IUPAC    ' /
      DATA PNAME_REF( IACRO-09          ) / 'ACRO-09         ' /
      DATA PNAME_REF( IPAA              ) / 'PAA             ' /
      DATA PNAME_REF( ICL2              ) / 'CL2             ' /
      DATA PNAME_REF( ICLNO-06          ) / 'CLNO-06         ' /
      DATA PNAME_REF( ICLONO            ) / 'CLONO           ' /
      DATA PNAME_REF( ICLNO2            ) / 'CLNO2           ' /
      DATA PNAME_REF( ICLONO2-1         ) / 'CLONO2-1        ' /
      DATA PNAME_REF( ICLONO2-2         ) / 'CLONO2-2        ' /
      DATA PNAME_REF( IHOCL-06          ) / 'HOCL-06         ' /
      DATA PNAME_REF( ICLCCHO           ) / 'CLCCHO          ' /
      DATA PNAME_REF( ICLACET           ) / 'CLACET          ' /
      DATA PNAME_REF( INO2_SAPRC99      ) / 'NO2_SAPRC99     ' /
      DATA PNAME_REF( INO3NO_SAPRC99    ) / 'NO3NO_SAPRC99   ' /
      DATA PNAME_REF( INO3NO2_SAPRC99   ) / 'NO3NO2_SAPRC99  ' /
      DATA PNAME_REF( IO3O3P_SAPRC99    ) / 'O3O3P_SAPRC99   ' /
      DATA PNAME_REF( IO3O1D_SAPRC99    ) / 'O3O1D_SAPRC99   ' /
      DATA PNAME_REF( IHONO_NO_SAPRC99  ) / 'HONO_NO_SAPRC99 ' /
      DATA PNAME_REF( IHONO_NO2_SAPRC99 ) / 'HONO_NO2_SAPRC99' /
      DATA PNAME_REF( IHNO3_SAPRC99     ) / 'HNO3_SAPRC99    ' /
      DATA PNAME_REF( IHO2NO2_SAPRC99   ) / 'HO2NO2_SAPRC99  ' /
      DATA PNAME_REF( IH2O2_SAPRC99     ) / 'H2O2_SAPRC99    ' /
      DATA PNAME_REF( IHCHO_R_SAPRC99   ) / 'HCHO_R_SAPRC99  ' /
      DATA PNAME_REF( IHCHO_M_SAPRC99   ) / 'HCHO_M_SAPRC99  ' /
      DATA PNAME_REF( ICCHO_R_SAPRC99   ) / 'CCHO_R_SAPRC99  ' /
      DATA PNAME_REF( IC2CHO_SAPRC99    ) / 'C2CHO_SAPRC99   ' /
      DATA PNAME_REF( IACETONE_SAPRC99  ) / 'ACETONE_SAPRC99 ' /
      DATA PNAME_REF( IKETONE_SAPRC99   ) / 'KETONE_SAPRC99  ' /
      DATA PNAME_REF( ICOOH_SAPRC99     ) / 'COOH_SAPRC99    ' /
      DATA PNAME_REF( IGLY_R_SAPRC99    ) / 'GLY_R_SAPRC99   ' /
      DATA PNAME_REF( IGLY_ABS_SAPRC99  ) / 'GLY_ABS_SAPRC99 ' /
      DATA PNAME_REF( IMGLY_ADJ_SAPRC99 ) / 'MGLY_ADJ_SAPRC99' /
      DATA PNAME_REF( IBACL_ADJ_SAPRC99 ) / 'BACL_ADJ_SAPRC99' /
      DATA PNAME_REF( IBZCHO_SAPRC99    ) / 'BZCHO_SAPRC99   ' /
      DATA PNAME_REF( IACROLEIN_SAPRC99 ) / 'ACROLEIN_SAPRC99' /
      DATA PNAME_REF( IIC3ONO2_SAPRC99  ) / 'IC3ONO2_SAPRC99 ' /
      DATA PNAME_REF( IMGLY_ABS_SAPRC99 ) / 'MGLY_ABS_SAPRC99' /
      DATA PNAME_REF( IO3_O3P_IUPAC04   ) / 'O3_O3P_IUPAC04  ' /
      DATA PNAME_REF( IO3_O1D_IUPAC04   ) / 'O3_O1D_IUPAC04  ' /
      DATA PNAME_REF( IHONO_IUPAC04     ) / 'HONO_IUPAC04    ' /
      DATA PNAME_REF( IHO2NO2_IUPAC04   ) / 'HO2NO2_IUPAC04  ' /
      DATA PNAME_REF( IHNO3_IUPAC04     ) / 'HNO3_IUPAC04    ' /
      DATA PNAME_REF( IN2O5_IUPAC04     ) / 'N2O5_IUPAC04    ' /
      DATA PNAME_REF( INTR_IUPAC04      ) / 'NTR_IUPAC04     ' /
      DATA PNAME_REF( IPAN_IUPAC04      ) / 'PAN_IUPAC04     ' /
      DATA PNAME_REF( IPACD_CB05        ) / 'PACD_CB05       ' /
      DATA PNAME_REF( IMGLY_IUPAC04     ) / 'MGLY_IUPAC04    ' /
      DATA PNAME_REF( ICL2_IUPAC04      ) / 'CL2_IUPAC04     ' /
      DATA PNAME_REF( IHOCL_IUPAC04     ) / 'HOCL_IUPAC04    ' /
      DATA PNAME_REF( IFMCL_IUPAC04     ) / 'FMCL_IUPAC04    ' /

C...Setup the Mapping from CMAQ chemical reactions to the reference data

      INTEGER, PARAMETER :: NPHOT_MAP =  77 ! #  phot mapped reactions 

      CHARACTER(16), SAVE :: PNAME_MAP( NPHOT_MAP )
      CHARACTER(16), SAVE :: PHOT_MAP( NPHOT_MAP )

      DATA PNAME_MAP(   1 ),  PHOT_MAP(   1 )  / 'NO2-06          ', INO2-06           / 
      DATA PNAME_MAP(   2 ),  PHOT_MAP(   2 )  / 'NO3NO-06        ', INO3NO-06         / 
      DATA PNAME_MAP(   3 ),  PHOT_MAP(   3 )  / 'NO3NO2-6        ', INO3NO2-6         / 
      DATA PNAME_MAP(   4 ),  PHOT_MAP(   4 )  / 'O3O1D-06        ', IO3O1D-06         / 
      DATA PNAME_MAP(   5 ),  PHOT_MAP(   5 )  / 'O3O3P-06        ', IO3O3P-06         / 
      DATA PNAME_MAP(   6 ),  PHOT_MAP(   6 )  / 'HONO-06         ', IHONO-06          / 
      DATA PNAME_MAP(   7 ),  PHOT_MAP(   7 )  / 'HNO3            ', IHNO3             / 
      DATA PNAME_MAP(   8 ),  PHOT_MAP(   8 )  / 'HNO4-06         ', IHNO4-06          / 
      DATA PNAME_MAP(   9 ),  PHOT_MAP(   9 )  / 'H2O2            ', IH2O2             / 
      DATA PNAME_MAP(  10 ),  PHOT_MAP(  10 )  / 'NO2EX           ', INO2EX            / 
      DATA PNAME_MAP(  11 ),  PHOT_MAP(  11 )  / 'PAN             ', IPAN              / 
      DATA PNAME_MAP(  12 ),  PHOT_MAP(  12 )  / 'HCHOR-06        ', IHCHOR-06         / 
      DATA PNAME_MAP(  13 ),  PHOT_MAP(  13 )  / 'HCHOM-06        ', IHCHOM-06         / 
      DATA PNAME_MAP(  14 ),  PHOT_MAP(  14 )  / 'CCHO_R          ', ICCHO_R           / 
      DATA PNAME_MAP(  15 ),  PHOT_MAP(  15 )  / 'C2CHO           ', IC2CHO            / 
      DATA PNAME_MAP(  16 ),  PHOT_MAP(  16 )  / 'ACET-06         ', IACET-06          / 
      DATA PNAME_MAP(  17 ),  PHOT_MAP(  17 )  / 'MEK-06          ', IMEK-06           / 
      DATA PNAME_MAP(  18 ),  PHOT_MAP(  18 )  / 'COOH            ', ICOOH             / 
      DATA PNAME_MAP(  19 ),  PHOT_MAP(  19 )  / 'GLY-07R         ', IGLY-07R          / 
      DATA PNAME_MAP(  20 ),  PHOT_MAP(  20 )  / 'GLY-07M         ', IGLY-07M          / 
      DATA PNAME_MAP(  21 ),  PHOT_MAP(  21 )  / 'MGLY-06         ', IMGLY-06          / 
      DATA PNAME_MAP(  22 ),  PHOT_MAP(  22 )  / 'BACL-07         ', IBACL-07          / 
      DATA PNAME_MAP(  23 ),  PHOT_MAP(  23 )  / 'BALD-06         ', IBALD-06          / 
      DATA PNAME_MAP(  24 ),  PHOT_MAP(  24 )  / 'AFG1            ', IAFG1             / 
      DATA PNAME_MAP(  25 ),  PHOT_MAP(  25 )  / 'MACR-06         ', IMACR-06          / 
      DATA PNAME_MAP(  26 ),  PHOT_MAP(  26 )  / 'MVK-06          ', IMVK-06           / 
      DATA PNAME_MAP(  27 ),  PHOT_MAP(  27 )  / 'IC3ONO2         ', IIC3ONO2          / 
      DATA PNAME_MAP(  28 ),  PHOT_MAP(  28 )  / 'HOCCHO_IUPAC    ', IHOCCHO_IUPAC     / 
      DATA PNAME_MAP(  29 ),  PHOT_MAP(  29 )  / 'ACRO-09         ', IACRO-09          / 
      DATA PNAME_MAP(  30 ),  PHOT_MAP(  30 )  / 'PAA             ', IPAA              / 
      DATA PNAME_MAP(  31 ),  PHOT_MAP(  31 )  / 'CL2             ', ICL2              / 
      DATA PNAME_MAP(  32 ),  PHOT_MAP(  32 )  / 'CLNO-06         ', ICLNO-06          / 
      DATA PNAME_MAP(  33 ),  PHOT_MAP(  33 )  / 'CLONO           ', ICLONO            / 
      DATA PNAME_MAP(  34 ),  PHOT_MAP(  34 )  / 'CLNO2           ', ICLNO2            / 
      DATA PNAME_MAP(  35 ),  PHOT_MAP(  35 )  / 'CLONO2-1        ', ICLONO2-1         / 
      DATA PNAME_MAP(  36 ),  PHOT_MAP(  36 )  / 'CLONO2-2        ', ICLONO2-2         / 
      DATA PNAME_MAP(  37 ),  PHOT_MAP(  37 )  / 'HOCL-06         ', IHOCL-06          / 
      DATA PNAME_MAP(  38 ),  PHOT_MAP(  38 )  / 'CLCCHO          ', ICLCCHO           / 
      DATA PNAME_MAP(  39 ),  PHOT_MAP(  39 )  / 'CLACET          ', ICLACET           / 
      DATA PNAME_MAP(  40 ),  PHOT_MAP(  40 )  / 'NO2_SAPRC99     ', INO2_SAPRC99      / 
      DATA PNAME_MAP(  41 ),  PHOT_MAP(  41 )  / 'NO3NO_SAPRC99   ', INO3NO_SAPRC99    / 
      DATA PNAME_MAP(  42 ),  PHOT_MAP(  42 )  / 'NO3NO2_SAPRC99  ', INO3NO2_SAPRC99   / 
      DATA PNAME_MAP(  43 ),  PHOT_MAP(  43 )  / 'O3O3P_SAPRC99   ', IO3O3P_SAPRC99    / 
      DATA PNAME_MAP(  44 ),  PHOT_MAP(  44 )  / 'O3O1D_SAPRC99   ', IO3O1D_SAPRC99    / 
      DATA PNAME_MAP(  45 ),  PHOT_MAP(  45 )  / 'HONO_NO_SAPRC99 ', IHONO_NO_SAPRC99  / 
      DATA PNAME_MAP(  46 ),  PHOT_MAP(  46 )  / 'HONO_NO2_SAPRC99', IHONO_NO2_SAPRC99 / 
      DATA PNAME_MAP(  47 ),  PHOT_MAP(  47 )  / 'HNO3_SAPRC99    ', IHNO3_SAPRC99     / 
      DATA PNAME_MAP(  48 ),  PHOT_MAP(  48 )  / 'HO2NO2_SAPRC99  ', IHO2NO2_SAPRC99   / 
      DATA PNAME_MAP(  49 ),  PHOT_MAP(  49 )  / 'H2O2_SAPRC99    ', IH2O2_SAPRC99     / 
      DATA PNAME_MAP(  50 ),  PHOT_MAP(  50 )  / 'HCHO_R_SAPRC99  ', IHCHO_R_SAPRC99   / 
      DATA PNAME_MAP(  51 ),  PHOT_MAP(  51 )  / 'HCHO_M_SAPRC99  ', IHCHO_M_SAPRC99   / 
      DATA PNAME_MAP(  52 ),  PHOT_MAP(  52 )  / 'CCHO_R_SAPRC99  ', ICCHO_R_SAPRC99   / 
      DATA PNAME_MAP(  53 ),  PHOT_MAP(  53 )  / 'C2CHO_SAPRC99   ', IC2CHO_SAPRC99    / 
      DATA PNAME_MAP(  54 ),  PHOT_MAP(  54 )  / 'ACETONE_SAPRC99 ', IACETONE_SAPRC99  / 
      DATA PNAME_MAP(  55 ),  PHOT_MAP(  55 )  / 'KETONE_SAPRC99  ', IKETONE_SAPRC99   / 
      DATA PNAME_MAP(  56 ),  PHOT_MAP(  56 )  / 'COOH_SAPRC99    ', ICOOH_SAPRC99     / 
      DATA PNAME_MAP(  57 ),  PHOT_MAP(  57 )  / 'GLY_R_SAPRC99   ', IGLY_R_SAPRC99    / 
      DATA PNAME_MAP(  58 ),  PHOT_MAP(  58 )  / 'GLY_ABS_SAPRC99 ', IGLY_ABS_SAPRC99  / 
      DATA PNAME_MAP(  59 ),  PHOT_MAP(  59 )  / 'MGLY_ADJ_SAPRC99', IMGLY_ADJ_SAPRC99 / 
      DATA PNAME_MAP(  60 ),  PHOT_MAP(  60 )  / 'BACL_ADJ_SAPRC99', IBACL_ADJ_SAPRC99 / 
      DATA PNAME_MAP(  61 ),  PHOT_MAP(  61 )  / 'BZCHO_SAPRC99   ', IBZCHO_SAPRC99    / 
      DATA PNAME_MAP(  62 ),  PHOT_MAP(  62 )  / 'ACROLEIN_SAPRC99', IACROLEIN_SAPRC99 / 
      DATA PNAME_MAP(  63 ),  PHOT_MAP(  63 )  / 'IC3ONO2_SAPRC99 ', IIC3ONO2_SAPRC99  / 
      DATA PNAME_MAP(  64 ),  PHOT_MAP(  64 )  / 'MGLY_ABS_SAPRC99', IMGLY_ABS_SAPRC99 / 
      DATA PNAME_MAP(  65 ),  PHOT_MAP(  65 )  / 'O3_O3P_IUPAC04  ', IO3_O3P_IUPAC04   / 
      DATA PNAME_MAP(  66 ),  PHOT_MAP(  66 )  / 'O3_O1D_IUPAC04  ', IO3_O1D_IUPAC04   / 
      DATA PNAME_MAP(  67 ),  PHOT_MAP(  67 )  / 'HONO_IUPAC04    ', IHONO_IUPAC04     / 
      DATA PNAME_MAP(  68 ),  PHOT_MAP(  68 )  / 'HO2NO2_IUPAC04  ', IHO2NO2_IUPAC04   / 
      DATA PNAME_MAP(  69 ),  PHOT_MAP(  69 )  / 'HNO3_IUPAC04    ', IHNO3_IUPAC04     / 
      DATA PNAME_MAP(  70 ),  PHOT_MAP(  70 )  / 'N2O5_IUPAC04    ', IN2O5_IUPAC04     / 
      DATA PNAME_MAP(  71 ),  PHOT_MAP(  71 )  / 'NTR_IUPAC04     ', INTR_IUPAC04      / 
      DATA PNAME_MAP(  72 ),  PHOT_MAP(  72 )  / 'PAN_IUPAC04     ', IPAN_IUPAC04      / 
      DATA PNAME_MAP(  73 ),  PHOT_MAP(  73 )  / 'PACD_CB05       ', IPACD_CB05        / 
      DATA PNAME_MAP(  74 ),  PHOT_MAP(  74 )  / 'MGLY_IUPAC04    ', IMGLY_IUPAC04     / 
      DATA PNAME_MAP(  75 ),  PHOT_MAP(  75 )  / 'CL2_IUPAC04     ', ICL2_IUPAC04      / 
      DATA PNAME_MAP(  76 ),  PHOT_MAP(  76 )  / 'HOCL_IUPAC04    ', IHOCL_IUPAC04     / 
      DATA PNAME_MAP(  77 ),  PHOT_MAP(  77 )  / 'FMCL_IUPAC04    ', IFMCL_IUPAC04     / 

      REAL, SAVE :: TEMP_REF( NTEMP_REF, NPHOT_REF )    ! reference temperatures

      REAL, SAVE :: CS_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! effective cross sections

      REAL, SAVE :: QY_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! effective quantum yields

      REAL, SAVE :: ECS_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! CS*QY averaged UCI Solar Flux

C...    effective quantum yields were computed by performing separate
C...    interval integrations for the cross sections and for the
C...    effective cross sections (cs*qy) (calculated on the finer
C...    wavelength grid.  The effective quantum yield values
C...    were then calculated for the 7 wavelength intervals by 
C...    dividing the effective cross sections by the interval average
C...    cross sections (eQY=eCS/CS).

      REAL, SAVE :: EQY_REF( NPHOT_REF, NTEMP_REF, NWL_REF ) ! eCS/CS averaged 77 bins in UCI Model


      INTEGER  :: IWLR  ! wavelength loop variable
      INTEGER  :: ITT   ! temperature loop variable

C...NO2-06
C..  NO2 + HV = NO + O
C..  From NASA (2006).
C..  Absorption cross sections are averages for wavelength intervals given.
C..  Data alligned to smallest wavelength intervals for abs. coefs and qy's.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, INO2-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.075247E-19, 1.484195E-19, 1.882053E-19, 2.274756E-19, 
     & 3.449308E-19, 5.754674E-19, 1.195357E-20 /
      DATA ( CS_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.075247E-19, 1.484195E-19, 1.882053E-19, 2.274756E-19, 
     & 3.449308E-19, 5.754674E-19, 1.195357E-20 /
      DATA ( CS_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.075247E-19, 1.484195E-19, 1.882053E-19, 2.274756E-19, 
     & 3.449308E-19, 5.754674E-19, 1.195357E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.174767E-01, 7.500325E-04 /
      DATA ( QY_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.174767E-01, 7.500325E-04 /
      DATA ( QY_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.174767E-01, 7.500325E-04 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.075247E-19, 1.484195E-19, 1.882053E-19, 2.274756E-19, 
     & 3.449308E-19, 4.644382E-19, 4.476087E-22 /
      DATA ( ECS_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.075247E-19, 1.484195E-19, 1.882053E-19, 2.274756E-19, 
     & 3.449308E-19, 4.644382E-19, 4.476087E-22 /
      DATA ( ECS_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.075247E-19, 1.484195E-19, 1.882053E-19, 2.274756E-19, 
     & 3.449308E-19, 4.644382E-19, 4.476087E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.070626E-01, 3.744562E-02 /
      DATA ( EQY_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.070626E-01, 3.744562E-02 /
      DATA ( EQY_REF( INO2-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.070626E-01, 3.744562E-02 /


C...NO3NO-06
C..  NO3 + HV = NO + O2
C..  From NASA (2006), for 298 K.
C..  Quantum yields for wavelengths above 640 nm estimated by linear extrapolation.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, INO3NO-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 6.269964E-19 /
      DATA ( CS_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 6.269964E-19 /
      DATA ( CS_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 6.269964E-19 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.298857E-02 /
      DATA ( QY_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.298857E-02 /
      DATA ( QY_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.298857E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 9.811739E-20 /
      DATA ( ECS_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 9.811739E-20 /
      DATA ( ECS_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 9.811739E-20 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.564880E-01 /
      DATA ( EQY_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.564880E-01 /
      DATA ( EQY_REF( INO3NO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.564880E-01 /


C...NO3NO2-6
C..  NO3 + HV = NO2 + O
C..  From NASA (2006), for 298 K.
C..  Quantum yields for wavelengths above 640 nm estimated by linear extrapolation.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, INO3NO2-6 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.027492E-21, 1.227490E-18 /
      DATA ( CS_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.027492E-21, 1.227490E-18 /
      DATA ( CS_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.027492E-21, 1.227490E-18 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.130039E-01, 4.268056E-01 /
      DATA ( QY_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.130039E-01, 4.268056E-01 /
      DATA ( QY_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.130039E-01, 4.268056E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.021786E-21, 7.834470E-19 /
      DATA ( ECS_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.021786E-21, 7.834470E-19 /
      DATA ( ECS_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.021786E-21, 7.834470E-19 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 9.985834E-01, 6.382512E-01 /
      DATA ( EQY_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 9.985834E-01, 6.382512E-01 /
      DATA ( EQY_REF( INO3NO2-6, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 9.985834E-01, 6.382512E-01 /


C...O3O1D-06
C..  O3 + HV = O1D + O2
C..  NASA (2006) abs. Coefs and IUPAC (2006) quantum yields
C..  Absorption cross sections from NASA (2006).
C..  Quantum yields are from IUPAC (2006) recommendation, interpolated
C..  to the NASA (2006) absorption cross section wavelengths.
C..  NASA (2006) does not give useable recommendations for the absorption
C..  cross sections, except at <305 and >329 nm, where they are consistent
C..  with the IUPAC recommendations.
C..  Formation of O1D assumed not to occur at the high wavelength band.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IO3O1D-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.538772E-19, 2.750724E-19, 1.063743E-19, 4.625945E-20, 
     & 6.599141E-21, 5.460147E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.538772E-19, 2.750724E-19, 1.063743E-19, 4.625945E-20, 
     & 6.599141E-21, 5.460147E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.538772E-19, 2.750724E-19, 1.063743E-19, 4.625945E-20, 
     & 6.599141E-21, 5.460147E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929039E-01, 5.359917E-01, 2.218193E-01, 
     & 8.688425E-02, 4.278574E-02, 0.000000E+00 /
      DATA ( QY_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929039E-01, 5.359917E-01, 2.218193E-01, 
     & 8.688425E-02, 4.278574E-02, 0.000000E+00 /
      DATA ( QY_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929039E-01, 5.359917E-01, 2.218193E-01, 
     & 8.688425E-02, 4.278574E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.684895E-19, 2.464158E-19, 6.078768E-20, 1.068708E-20, 
     & 6.625296E-22, 4.350859E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.684895E-19, 2.464158E-19, 6.078768E-20, 1.068708E-20, 
     & 6.625296E-22, 4.350859E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.684895E-19, 2.464158E-19, 6.078768E-20, 1.068708E-20, 
     & 6.625296E-22, 4.350859E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.958217E-01, 5.714506E-01, 2.310247E-01, 
     & 1.003963E-01, 7.968391E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.958217E-01, 5.714506E-01, 2.310247E-01, 
     & 1.003963E-01, 7.968391E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.958217E-01, 5.714506E-01, 2.310247E-01, 
     & 1.003963E-01, 7.968391E-02, 0.000000E+00 /


C...O3O3P-06
C..  O3 + HV = O3P + O2
C..  Absorption cross sections from NASA (2006).
C..  Quantum yields are derived from the O1D quantum yields in the low wavelength re
C..  Unit quantum yields assumed in high wavelength region.
C..  Absorption cross sections below 829 nm are extrapolated
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IO3O3P-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.408006E-19, 2.749081E-19, 1.063743E-19, 4.625945E-20, 
     & 6.803506E-21, 6.139258E-23, 1.666851E-21 /
      DATA ( CS_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.408006E-19, 2.749081E-19, 1.063743E-19, 4.625945E-20, 
     & 6.803506E-21, 6.139258E-23, 1.666851E-21 /
      DATA ( CS_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.408006E-19, 2.749081E-19, 1.063743E-19, 4.625945E-20, 
     & 6.803506E-21, 6.139258E-23, 1.666851E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070961E-01, 4.640082E-01, 7.781807E-01, 
     & 9.123901E-01, 9.572143E-01, 9.795594E-01 /
      DATA ( QY_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070961E-01, 4.640082E-01, 7.781807E-01, 
     & 9.123901E-01, 9.572143E-01, 9.795594E-01 /
      DATA ( QY_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070961E-01, 4.640082E-01, 7.781807E-01, 
     & 9.123901E-01, 9.572143E-01, 9.795594E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.408006E-20, 2.864014E-20, 4.558666E-20, 3.557237E-20, 
     & 6.109267E-21, 5.704172E-23, 1.666851E-21 /
      DATA ( ECS_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.408006E-20, 2.864014E-20, 4.558666E-20, 3.557237E-20, 
     & 6.109267E-21, 5.704172E-23, 1.666851E-21 /
      DATA ( ECS_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.408006E-20, 2.864014E-20, 4.558666E-20, 3.557237E-20, 
     & 6.109267E-21, 5.704172E-23, 1.666851E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.041808E-01, 4.285494E-01, 7.689753E-01, 
     & 8.979586E-01, 9.291306E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.041808E-01, 4.285494E-01, 7.689753E-01, 
     & 8.979586E-01, 9.291306E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.041808E-01, 4.285494E-01, 7.689753E-01, 
     & 8.979586E-01, 9.291306E-01, 1.000000E+00 /


C...HONO-06
C..  HONO + HV = HO. + NO
C..  NASA (2006) recommended absorption cross sections. No recommendation on quantum yields.
C..  IUPAC (2005) recommendation (Data Sheet PNOx1, July, 2001) recommends unit quantum yield
C..  for OH + NO.
C..  (IUPAC recommended absorption cross sections are low resolution, so NASA values are
C..   preferred)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHONO-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.487002E-21, 9.335758E-21, 1.881657E-20, 3.188727E-20, 
     & 9.007236E-20, 6.903963E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.487002E-21, 9.335758E-21, 1.881657E-20, 3.188727E-20, 
     & 9.007236E-20, 6.903963E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.487002E-21, 9.335758E-21, 1.881657E-20, 3.188727E-20, 
     & 9.007236E-20, 6.903963E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.771221E-01, 0.000000E+00 /
      DATA ( QY_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.771221E-01, 0.000000E+00 /
      DATA ( QY_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.771221E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.487002E-21, 9.335758E-21, 1.881657E-20, 3.188727E-20, 
     & 9.007236E-20, 6.903962E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.487002E-21, 9.335758E-21, 1.881657E-20, 3.188727E-20, 
     & 9.007236E-20, 6.903962E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.487002E-21, 9.335758E-21, 1.881657E-20, 3.188727E-20, 
     & 9.007236E-20, 6.903962E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /


C...HNO3
C..  HNO3 + HV = products
C..  IUPAC (1997) Recommendation

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHNO3 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875286E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875286E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875286E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875253E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875253E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875253E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999914E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999914E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999914E-01, 0.000000E+00 /


C...HNO4-06
C..  HO2NO2 + HV = PRODUCTS
C..  NASA (2006)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHNO4-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.685093E-20, 1.173546E-20, 5.713464E-21, 3.110148E-21, 
     & 8.269189E-22, 2.119348E-23, 0.000000E+00 /
      DATA ( CS_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.685093E-20, 1.173546E-20, 5.713464E-21, 3.110148E-21, 
     & 8.269189E-22, 2.119348E-23, 0.000000E+00 /
      DATA ( CS_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.685093E-20, 1.173546E-20, 5.713464E-21, 3.110148E-21, 
     & 8.269189E-22, 2.119348E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.684180E-01, 0.000000E+00 /
      DATA ( QY_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.684180E-01, 0.000000E+00 /
      DATA ( QY_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.684180E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.685093E-20, 1.173546E-20, 5.713464E-21, 3.110148E-21, 
     & 8.269189E-22, 2.119341E-23, 0.000000E+00 /
      DATA ( ECS_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.685093E-20, 1.173546E-20, 5.713464E-21, 3.110148E-21, 
     & 8.269189E-22, 2.119341E-23, 0.000000E+00 /
      DATA ( ECS_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.685093E-20, 1.173546E-20, 5.713464E-21, 3.110148E-21, 
     & 8.269189E-22, 2.119341E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999966E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999966E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO4-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999966E-01, 0.000000E+00 /


C...H2O2
C..  H2O2 + HV = 2 OH
C..  IUPAC (1997) Recommended.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IH2O2 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606317E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606317E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606317E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606284E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606284E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606284E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999907E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999907E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999907E-01, 0.000000E+00 /


C...NO2EX
C..  NO2 + HV = NO2*
C..  From NASA (2006), for 294 K.
C..  Absorption cross sections are averages for wavelength intervals given.
C..  Data alligned to smallest wavelength intervals for abs. coefs and qy's.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, INO2EX ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.075247E-19, 1.484195E-19, 1.882053E-19, 2.274756E-19, 
     & 3.449308E-19, 5.754674E-19, 1.067281E-19 /
      DATA ( CS_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.075247E-19, 1.484195E-19, 1.882053E-19, 2.274756E-19, 
     & 3.449308E-19, 5.754674E-19, 1.067281E-19 /
      DATA ( CS_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.075247E-19, 1.484195E-19, 1.882053E-19, 2.274756E-19, 
     & 3.449308E-19, 5.754674E-19, 1.067281E-19 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.825233E-01, 5.823866E-01 /
      DATA ( QY_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.825233E-01, 5.823866E-01 /
      DATA ( QY_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.825233E-01, 5.823866E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.110292E-19, 1.062805E-19 /
      DATA ( ECS_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.110292E-19, 1.062805E-19 /
      DATA ( ECS_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.110292E-19, 1.062805E-19 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.929374E-01, 9.958060E-01 /
      DATA ( EQY_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.929374E-01, 9.958060E-01 /
      DATA ( EQY_REF( INO2EX, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.929374E-01, 9.958060E-01 /


C...PAN
C..  PAN + HV = #.6 {MECO3 + NO2} + #.4 {MEO2 + CO2 + NO3}
C..  IUPAC Data Sheet P21.updated: 12/19/05
C..  Reaction reflects recommended quantum yields at 308 nm
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IPAN ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.597525E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.597525E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.597525E-24, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.009612E-01, 0.000000E+00 /
      DATA ( QY_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.009612E-01, 0.000000E+00 /
      DATA ( QY_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.009612E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.597507E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.597507E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.597507E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999891E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999891E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999891E-01, 0.000000E+00 /


C...HCHOR-06
C..  HCHO + HV = HCO. + H.
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P1
C..  This datasheet updated: 16th May 2002.
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/.
C..  IUPAC recommendations used for both absorption cross sections and quantum yield
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHCHOR-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.170091E-20, 3.298359E-20, 1.534647E-20, 3.259232E-20, 
     & 1.516326E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.170091E-20, 3.298359E-20, 1.534647E-20, 3.259232E-20, 
     & 1.516326E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.170091E-20, 3.298359E-20, 1.534647E-20, 3.259232E-20, 
     & 1.516326E-20, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.851841E-01, 7.182517E-01, 7.159068E-01, 6.839368E-01, 
     & 2.830291E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.851841E-01, 7.182517E-01, 7.159068E-01, 6.839368E-01, 
     & 2.830291E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.851841E-01, 7.182517E-01, 7.159068E-01, 6.839368E-01, 
     & 2.830291E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.165855E-20, 2.395182E-20, 1.104784E-20, 2.236374E-20, 
     & 5.554417E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.165855E-20, 2.395182E-20, 1.104784E-20, 2.236374E-20, 
     & 5.554417E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.165855E-20, 2.395182E-20, 1.104784E-20, 2.236374E-20, 
     & 5.554417E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.832153E-01, 7.261736E-01, 7.198943E-01, 6.861660E-01, 
     & 3.663075E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.832153E-01, 7.261736E-01, 7.198943E-01, 6.861660E-01, 
     & 3.663075E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.832153E-01, 7.261736E-01, 7.198943E-01, 6.861660E-01, 
     & 3.663075E-01, 0.000000E+00, 0.000000E+00 /


C...HCHOM-06
C..  HCHO + HV = H2 + CO
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P1
C..  This datasheet updated: 16th May 2002.
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/.
C..  IUPAC recommendations used for both absorption cross sections and quantum yield
C..  Quantum yields for wl < 0.360 interpolated.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHCHOM-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.170091E-20, 3.298359E-20, 1.534647E-20, 3.259232E-20, 
     & 1.806812E-20, 7.417168E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.170091E-20, 3.298359E-20, 1.534647E-20, 3.259232E-20, 
     & 1.806812E-20, 7.417168E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.170091E-20, 3.298359E-20, 1.534647E-20, 3.259232E-20, 
     & 1.806812E-20, 7.417168E-22, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.954920E-01, 2.804146E-01, 2.842617E-01, 3.165715E-01, 
     & 4.879647E-01, 3.037111E-02, 0.000000E+00 /
      DATA ( QY_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.954920E-01, 2.804146E-01, 2.842617E-01, 3.165715E-01, 
     & 4.879647E-01, 3.037111E-02, 0.000000E+00 /
      DATA ( QY_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.954920E-01, 2.804146E-01, 2.842617E-01, 3.165715E-01, 
     & 4.879647E-01, 3.037111E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.456174E-21, 8.997082E-21, 4.301534E-21, 1.024255E-20, 
     & 8.938719E-21, 1.182693E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.456174E-21, 8.997082E-21, 4.301534E-21, 1.024255E-20, 
     & 8.938719E-21, 1.182693E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.456174E-21, 8.997082E-21, 4.301534E-21, 1.024255E-20, 
     & 8.938719E-21, 1.182693E-22, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.982934E-01, 2.727745E-01, 2.802947E-01, 3.142626E-01, 
     & 4.947232E-01, 1.594535E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.982934E-01, 2.727745E-01, 2.802947E-01, 3.142626E-01, 
     & 4.947232E-01, 1.594535E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOM-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.982934E-01, 2.727745E-01, 2.802947E-01, 3.142626E-01, 
     & 4.947232E-01, 1.594535E-01, 0.000000E+00 /


C...CCHO_R
C..  CCHO + HV = CH3 + CHO
C..  IUPAC (1997)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICCHO_R ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.432585E-20, 3.717942E-20, 2.933100E-20, 2.104718E-20, 
     & 3.715620E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.432585E-20, 3.717942E-20, 2.933100E-20, 2.104718E-20, 
     & 3.715620E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.432585E-20, 3.717942E-20, 2.933100E-20, 2.104718E-20, 
     & 3.715620E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902035E-01, 2.845210E-01, 1.538490E-01, 
     & 1.409418E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902035E-01, 2.845210E-01, 1.538490E-01, 
     & 1.409418E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902035E-01, 2.845210E-01, 1.538490E-01, 
     & 1.409418E-02, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461039E-20, 8.426933E-21, 3.330087E-21, 
     & 1.731238E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461039E-20, 8.426933E-21, 3.330087E-21, 
     & 1.731238E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461039E-20, 8.426933E-21, 3.330087E-21, 
     & 1.731238E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.845356E-01, 3.929698E-01, 2.873047E-01, 1.582202E-01, 
     & 4.659352E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.845356E-01, 3.929698E-01, 2.873047E-01, 1.582202E-01, 
     & 4.659352E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.845356E-01, 3.929698E-01, 2.873047E-01, 1.582202E-01, 
     & 4.659352E-02, 0.000000E+00, 0.000000E+00 /


C...C2CHO
C..  C2CHO + HV = C2H5. + CHO.
C..  IUPAC (1997)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IC2CHO ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634685E-20, 3.579653E-20, 2.441747E-20, 
     & 5.808840E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634685E-20, 3.579653E-20, 2.441747E-20, 
     & 5.808840E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634685E-20, 3.579653E-20, 2.441747E-20, 
     & 5.808840E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.112440E-01, 7.954015E-01, 5.951666E-01, 4.312301E-01, 
     & 1.520067E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.112440E-01, 7.954015E-01, 5.951666E-01, 4.312301E-01, 
     & 1.520067E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.112440E-01, 7.954015E-01, 5.951666E-01, 4.312301E-01, 
     & 1.520067E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.730259E-20, 3.713793E-20, 2.133678E-20, 1.077363E-20, 
     & 1.383935E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.730259E-20, 3.713793E-20, 2.133678E-20, 1.077363E-20, 
     & 1.383935E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.730259E-20, 3.713793E-20, 2.133678E-20, 1.077363E-20, 
     & 1.383935E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.769110E-01, 8.013043E-01, 5.960574E-01, 4.412262E-01, 
     & 2.382463E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.769110E-01, 8.013043E-01, 5.960574E-01, 4.412262E-01, 
     & 2.382463E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.769110E-01, 8.013043E-01, 5.960574E-01, 4.412262E-01, 
     & 2.382463E-01, 0.000000E+00, 0.000000E+00 /


C...ACET-06
C..  CH3-CO-CH3 + HV = Radical products
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P7
C..  This datasheet updated: 19th December 2005.
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/.
C..  IUPAC recommendations used for both absorption cross sections and quantum yield
C..  Cross sections and quantum yields are for 298K only.
C..  Uncertain whether the cross sections are calculated correctly from the complex
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IACET-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.464056E-20, 2.276632E-20, 1.367790E-20, 7.373154E-21, 
     & 8.850663E-22, 8.179758E-25, 0.000000E+00 /
      DATA ( CS_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.464056E-20, 2.276632E-20, 1.367790E-20, 7.373154E-21, 
     & 8.850663E-22, 8.179758E-25, 0.000000E+00 /
      DATA ( CS_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.464056E-20, 2.276632E-20, 1.367790E-20, 7.373154E-21, 
     & 8.850663E-22, 8.179758E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.457705E-01, 2.094483E-01, 8.573302E-02, 3.882164E-02, 
     & 9.387449E-03, 1.342109E-04, 0.000000E+00 /
      DATA ( QY_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.457705E-01, 2.094483E-01, 8.573302E-02, 3.882164E-02, 
     & 9.387449E-03, 1.342109E-04, 0.000000E+00 /
      DATA ( QY_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.457705E-01, 2.094483E-01, 8.573302E-02, 3.882164E-02, 
     & 9.387449E-03, 1.342109E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.207276E-20, 4.950919E-21, 1.203542E-21, 3.067027E-22, 
     & 1.419293E-23, 2.303094E-27, 0.000000E+00 /
      DATA ( ECS_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.207276E-20, 4.950919E-21, 1.203542E-21, 3.067027E-22, 
     & 1.419293E-23, 2.303094E-27, 0.000000E+00 /
      DATA ( ECS_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.207276E-20, 4.950919E-21, 1.203542E-21, 3.067027E-22, 
     & 1.419293E-23, 2.303094E-27, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.485150E-01, 2.174668E-01, 8.799174E-02, 4.159722E-02, 
     & 1.603600E-02, 2.815602E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.485150E-01, 2.174668E-01, 8.799174E-02, 4.159722E-02, 
     & 1.603600E-02, 2.815602E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACET-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.485150E-01, 2.174668E-01, 8.799174E-02, 4.159722E-02, 
     & 1.603600E-02, 2.815602E-03, 0.000000E+00 /


C...MEK-06
C..  MEK absorption cross sections
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P8
C..  This datasheet updated: 5th December 2005.
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/.
C..  Note that recommended quantum yield is 0.34 and SAPRC07T sets
C..  value to 0.175 in mechanism definition file
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IMEK-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686873E-20, 1.573254E-20, 7.723394E-21, 
     & 8.067071E-22, 1.610022E-25, 0.000000E+00 /
      DATA ( CS_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686873E-20, 1.573254E-20, 7.723394E-21, 
     & 8.067071E-22, 1.610022E-25, 0.000000E+00 /
      DATA ( CS_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686873E-20, 1.573254E-20, 7.723394E-21, 
     & 8.067071E-22, 1.610022E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.161979E-02, 0.000000E+00 /
      DATA ( QY_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.161979E-02, 0.000000E+00 /
      DATA ( QY_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.161979E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686873E-20, 1.573254E-20, 7.723394E-21, 
     & 8.067071E-22, 1.609663E-25, 0.000000E+00 /
      DATA ( ECS_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686873E-20, 1.573254E-20, 7.723394E-21, 
     & 8.067071E-22, 1.609663E-25, 0.000000E+00 /
      DATA ( ECS_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686873E-20, 1.573254E-20, 7.723394E-21, 
     & 8.067071E-22, 1.609663E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.997768E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.997768E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMEK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.997768E-01, 0.000000E+00 /


C...COOH
C..  CH3OOH + HV = PRODUCTS
C..  IUPAC (1997).  Also recommend unit quantum yields.
C..  wl      abs        qy

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICOOH ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395106E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395106E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395106E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /
      DATA ( QY_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /
      DATA ( QY_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395094E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395094E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395094E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999977E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999977E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999977E-01, 0.000000E+00 /


C...GLY-07R
C..  HCOCHO + HV = HCO. + HCO.
C..  Absorption cross sections from Volkamer, R., P. Spietz, J. Burrows, and
C..  U. Platt (2005): "High-resolution absorption cross sections of glyoxal
C..  in the UV-vis and IR spectral ranges," J. Photochem. Photobiol. A, 172
C..  35-46.  Quantum yields at wavelengths below 350 nm based on the data of
C..  Zhu et al (1996) which are consistent with the data of Langford and
C..  Moore (1984).  Quantum yields at higher wavelengths assumed to decline
C..  expodentially with wavelength at a rate that gives a photolysis rate
C..  relative to NO2 consistent with the data of Klotz et al (2000).
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IGLY-07R ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.792119E-20, 3.572107E-20, 3.194427E-20, 2.574261E-20, 
     & 1.106790E-20, 2.757140E-20, 1.054196E-20 /
      DATA ( CS_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.792119E-20, 3.572107E-20, 3.194427E-20, 2.574261E-20, 
     & 1.106790E-20, 2.757140E-20, 1.054196E-20 /
      DATA ( CS_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.792119E-20, 3.572107E-20, 3.194427E-20, 2.574261E-20, 
     & 1.106790E-20, 2.757140E-20, 1.054196E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.321564E-01, 3.435688E-01, 3.690098E-01, 4.286591E-01, 
     & 5.846431E-01, 2.414760E-01, 1.331259E-03 /
      DATA ( QY_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.321564E-01, 3.435688E-01, 3.690098E-01, 4.286591E-01, 
     & 5.846431E-01, 2.414760E-01, 1.331259E-03 /
      DATA ( QY_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.321564E-01, 3.435688E-01, 3.690098E-01, 4.286591E-01, 
     & 5.846431E-01, 2.414760E-01, 1.331259E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.259928E-20, 1.226096E-20, 1.179648E-20, 1.092394E-20, 
     & 6.158664E-21, 3.111815E-21, 1.360344E-22 /
      DATA ( ECS_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.259928E-20, 1.226096E-20, 1.179648E-20, 1.092394E-20, 
     & 6.158664E-21, 3.111815E-21, 1.360344E-22 /
      DATA ( ECS_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.259928E-20, 1.226096E-20, 1.179648E-20, 1.092394E-20, 
     & 6.158664E-21, 3.111815E-21, 1.360344E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.322492E-01, 3.432416E-01, 3.692832E-01, 4.243522E-01, 
     & 5.564439E-01, 1.128639E-01, 1.290409E-02 /
      DATA ( EQY_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.322492E-01, 3.432416E-01, 3.692832E-01, 4.243522E-01, 
     & 5.564439E-01, 1.128639E-01, 1.290409E-02 /
      DATA ( EQY_REF( IGLY-07R, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.322492E-01, 3.432416E-01, 3.692832E-01, 4.243522E-01, 
     & 5.564439E-01, 1.128639E-01, 1.290409E-02 /


C...GLY-07M
C..  HCOCHO + HV = HCHO + H2
C..  Absorption cross sections from Volkamer, R., P. Spietz, J. Burrows,
C..  and U. Platt (2005): "High-resolution absorption cross sections of
C..  glyoxal in the UV-vis and IR spectral ranges," J. Photochem. Photobiol.
C..  A, 172 35-46.  Quantum yields at wavelengths below 350 nm derived from
C..  those used for the radical forming process assuming total quantum
C..  yields of 1.  Quantum yields at higher wavelengths assumed to decline
C..  expodentially at a rate that gives a 13% formaldehyde yield telative to
C..  photodecomposition under the conditions of Plum et al (1983).
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IGLY-07M ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.792119E-20, 3.572107E-20, 3.194427E-20, 2.574261E-20, 
     & 1.106790E-20, 2.757140E-20, 3.572529E-21 /
      DATA ( CS_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.792119E-20, 3.572107E-20, 3.194427E-20, 2.574261E-20, 
     & 1.106790E-20, 2.757140E-20, 3.572529E-21 /
      DATA ( CS_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.792119E-20, 3.572107E-20, 3.194427E-20, 2.574261E-20, 
     & 1.106790E-20, 2.757140E-20, 3.572529E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.678436E-01, 6.564312E-01, 6.310091E-01, 5.714539E-01, 
     & 4.154251E-01, 6.007640E-02, 5.018775E-05 /
      DATA ( QY_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.678436E-01, 6.564312E-01, 6.310091E-01, 5.714539E-01, 
     & 4.154251E-01, 6.007640E-02, 5.018775E-05 /
      DATA ( QY_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.678436E-01, 6.564312E-01, 6.310091E-01, 5.714539E-01, 
     & 4.154251E-01, 6.007640E-02, 5.018775E-05 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.532190E-20, 2.346011E-20, 2.014842E-20, 1.482247E-20, 
     & 4.909981E-21, 5.296760E-22, 4.592913E-24 /
      DATA ( ECS_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.532190E-20, 2.346011E-20, 2.014842E-20, 1.482247E-20, 
     & 4.909981E-21, 5.296760E-22, 4.592913E-24 /
      DATA ( ECS_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.532190E-20, 2.346011E-20, 2.014842E-20, 1.482247E-20, 
     & 4.909981E-21, 5.296760E-22, 4.592913E-24 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.677508E-01, 6.567584E-01, 6.307366E-01, 5.757951E-01, 
     & 4.436235E-01, 1.921107E-02, 1.285619E-03 /
      DATA ( EQY_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.677508E-01, 6.567584E-01, 6.307366E-01, 5.757951E-01, 
     & 4.436235E-01, 1.921107E-02, 1.285619E-03 /
      DATA ( EQY_REF( IGLY-07M, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.677508E-01, 6.567584E-01, 6.307366E-01, 5.757951E-01, 
     & 4.436235E-01, 1.921107E-02, 1.285619E-03 /


C...MGLY-06
C..  CH3COCHO + HV = CH3CO. + HCO.
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P6
C..  This datasheet updated: 16th January 2003.
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/.
C..  Quantum yield calculated from the expression given for 472 torr N2.
C..  This "pressure" adjustment was made so the calculated photlysis rates
C..  for solar conditions would agree with the data of Klotz et al (2000)
C..  for the Euphore outdoor chamber.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IMGLY-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.380390E-20, 3.467067E-20, 2.430921E-20, 1.788231E-20, 
     & 6.183693E-21, 3.700272E-20, 7.910760E-21 /
      DATA ( CS_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.380390E-20, 3.467067E-20, 2.430921E-20, 1.788231E-20, 
     & 6.183693E-21, 3.700272E-20, 7.910760E-21 /
      DATA ( CS_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.380390E-20, 3.467067E-20, 2.430921E-20, 1.788231E-20, 
     & 6.183693E-21, 3.700272E-20, 7.910760E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.565113E-01, 9.283987E-01, 8.962741E-01, 8.564201E-01, 
     & 7.065107E-01, 2.411670E-01, 3.970910E-03 /
      DATA ( QY_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.565113E-01, 9.283987E-01, 8.962741E-01, 8.564201E-01, 
     & 7.065107E-01, 2.411670E-01, 3.970910E-03 /
      DATA ( QY_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.565113E-01, 9.283987E-01, 8.962741E-01, 8.564201E-01, 
     & 7.065107E-01, 2.411670E-01, 3.970910E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.191152E-20, 3.221660E-20, 2.180538E-20, 1.534381E-20, 
     & 4.571315E-21, 5.327236E-21, 3.199067E-22 /
      DATA ( ECS_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.191152E-20, 3.221660E-20, 2.180538E-20, 1.534381E-20, 
     & 4.571315E-21, 5.327236E-21, 3.199067E-22 /
      DATA ( ECS_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.191152E-20, 3.221660E-20, 2.180538E-20, 1.534381E-20, 
     & 4.571315E-21, 5.327236E-21, 3.199067E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.567987E-01, 9.292175E-01, 8.970009E-01, 8.580443E-01, 
     & 7.392532E-01, 1.439688E-01, 4.043943E-02 /
      DATA ( EQY_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.567987E-01, 9.292175E-01, 8.970009E-01, 8.580443E-01, 
     & 7.392532E-01, 1.439688E-01, 4.043943E-02 /
      DATA ( EQY_REF( IMGLY-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.567987E-01, 9.292175E-01, 8.970009E-01, 8.580443E-01, 
     & 7.392532E-01, 1.439688E-01, 4.043943E-02 /


C...BACL-07
C..  CH3COCOCH3 + HV = 2 CH3CO.
C..  Absorption cross sections from Plum et al (1983) as used in SAPRC-99
C..  mechanism.  Quantum yields calculated using  the IUPAC (2005)-recommended
C..  expression for the pressure and wavelength-dependence quantum yields for
C..  methyl glyoxal, but with the effective presssure adjusted so the
C..  photolysis rate, relative to that for NO2, under ambient conditions is
C..  consistent with that measured by Klotz et al (2000) in the Euphore outdoor
C..  chamber.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IBACL-07 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.658382E-20, 1.613233E-20, 9.254848E-21, 6.071219E-21, 
     & 4.661718E-21, 3.201370E-20, 4.709774E-21 /
      DATA ( CS_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.658382E-20, 1.613233E-20, 9.254848E-21, 6.071219E-21, 
     & 4.661718E-21, 3.201370E-20, 4.709774E-21 /
      DATA ( CS_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.658382E-20, 1.613233E-20, 9.254848E-21, 6.071219E-21, 
     & 4.661718E-21, 3.201370E-20, 4.709774E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.894449E-01, 9.817067E-01, 9.728644E-01, 9.609106E-01, 
     & 9.065533E-01, 4.913755E-01, 9.660122E-03 /
      DATA ( QY_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.894449E-01, 9.817067E-01, 9.728644E-01, 9.609106E-01, 
     & 9.065533E-01, 4.913755E-01, 9.660122E-03 /
      DATA ( QY_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.894449E-01, 9.817067E-01, 9.728644E-01, 9.609106E-01, 
     & 9.065533E-01, 4.913755E-01, 9.660122E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.630799E-20, 1.584374E-20, 9.006381E-21, 5.836851E-21, 
     & 4.210304E-21, 1.187160E-20, 4.906408E-22 /
      DATA ( ECS_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.630799E-20, 1.584374E-20, 9.006381E-21, 5.836851E-21, 
     & 4.210304E-21, 1.187160E-20, 4.906408E-22 /
      DATA ( ECS_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.630799E-20, 1.584374E-20, 9.006381E-21, 5.836851E-21, 
     & 4.210304E-21, 1.187160E-20, 4.906408E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.896241E-01, 9.821110E-01, 9.731528E-01, 9.613968E-01, 
     & 9.031659E-01, 3.708288E-01, 1.041750E-01 /
      DATA ( EQY_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.896241E-01, 9.821110E-01, 9.731528E-01, 9.613968E-01, 
     & 9.031659E-01, 3.708288E-01, 1.041750E-01 /
      DATA ( EQY_REF( IBACL-07, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.896241E-01, 9.821110E-01, 9.731528E-01, 9.613968E-01, 
     & 9.031659E-01, 3.708288E-01, 1.041750E-01 /


C...BALD-06
C..  Benzaldehyde absorption cross sections recommended by Calvert et al (2002)
C..  From Lang (1961-1971) in hexane solution
C..
C..  Note that recommended quantum ranges from 0,14 to 0,4 and SAPRC07T sets
C..  value to 0.060 in mechanism definition file

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IBALD-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.452234E-19, 7.297681E-20, 7.090584E-20, 8.592445E-20, 
     & 9.197928E-20, 1.679406E-20, 0.000000E+00 /
      DATA ( CS_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.452234E-19, 7.297681E-20, 7.090584E-20, 8.592445E-20, 
     & 9.197928E-20, 1.679406E-20, 0.000000E+00 /
      DATA ( CS_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.452234E-19, 7.297681E-20, 7.090584E-20, 8.592445E-20, 
     & 9.197928E-20, 1.679406E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.275655E-01, 0.000000E+00 /
      DATA ( QY_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.275655E-01, 0.000000E+00 /
      DATA ( QY_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.275655E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.452234E-19, 7.297681E-20, 7.090584E-20, 8.592445E-20, 
     & 9.197928E-20, 1.679405E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.452234E-19, 7.297681E-20, 7.090584E-20, 8.592445E-20, 
     & 9.197928E-20, 1.679405E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.452234E-19, 7.297681E-20, 7.090584E-20, 8.592445E-20, 
     & 9.197928E-20, 1.679405E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999992E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999992E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBALD-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999992E-01, 0.000000E+00 /


C...AFG1
C..  Photolysis of lumped photoreactive aromatic unsaturated dicarbonyl product.
C..  Based on 31.6% BUTEDIAL and 68.4% 4OX2PEAL with QY(BUTEDIAL) = 0.723 and
C..  QY(4OX2PEAL) = 1.000
C..  From h:\mech\saprc06\phf.xls, "AFG1" sheet and based on
C..  h:\mech\saprc06\aroprods.xls
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IAFG1 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.731772E-18, 1.412651E-18, 1.107261E-18, 8.915006E-19, 
     & 5.026286E-19, 1.273167E-19, 1.232365E-21 /
      DATA ( CS_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.731772E-18, 1.412651E-18, 1.107261E-18, 8.915006E-19, 
     & 5.026286E-19, 1.273167E-19, 1.232365E-21 /
      DATA ( CS_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.731772E-18, 1.412651E-18, 1.107261E-18, 8.915006E-19, 
     & 5.026286E-19, 1.273167E-19, 1.232365E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /
      DATA ( QY_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /
      DATA ( QY_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.731772E-18, 1.412651E-18, 1.107261E-18, 8.915006E-19, 
     & 5.026286E-19, 1.273167E-19, 1.232365E-21 /
      DATA ( ECS_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.731772E-18, 1.412651E-18, 1.107261E-18, 8.915006E-19, 
     & 5.026286E-19, 1.273167E-19, 1.232365E-21 /
      DATA ( ECS_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.731772E-18, 1.412651E-18, 1.107261E-18, 8.915006E-19, 
     & 5.026286E-19, 1.273167E-19, 1.232365E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /
      DATA ( EQY_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /
      DATA ( EQY_REF( IAFG1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /


C...MACR-06
C..  Methacrolein total photolysis
C..  Absorption cross sections from IUPAC Subcommittee on Gas Kinetic Data Evaluatio
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/. This datasheet updated: 16th M
C..  The quantum yields were derived using the pressure and wavelength-dependent
C..  expression given by IUPAC (2005) for MVK, with the total pressure adjusted so
C..  that the radical forming photolysis rates for the chamber experiments are the
C..  same as those that fit the chamber data.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IMACR-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.639069E-20, 3.936243E-20, 4.989643E-20, 5.926217E-20, 
     & 6.528435E-20, 1.326859E-20, 0.000000E+00 /
      DATA ( CS_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.639069E-20, 3.936243E-20, 4.989643E-20, 5.926217E-20, 
     & 6.528435E-20, 1.326859E-20, 0.000000E+00 /
      DATA ( CS_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.639069E-20, 3.936243E-20, 4.989643E-20, 5.926217E-20, 
     & 6.528435E-20, 1.326859E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.901070E-02, 3.073936E-02, 2.101928E-02, 1.479159E-02, 
     & 6.251543E-03, 5.827899E-04, 0.000000E+00 /
      DATA ( QY_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.901070E-02, 3.073936E-02, 2.101928E-02, 1.479159E-02, 
     & 6.251543E-03, 5.827899E-04, 0.000000E+00 /
      DATA ( QY_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.901070E-02, 3.073936E-02, 2.101928E-02, 1.479159E-02, 
     & 6.251543E-03, 5.827899E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.276398E-21, 1.192333E-21, 1.045257E-21, 8.708165E-22, 
     & 4.101440E-22, 2.057185E-23, 0.000000E+00 /
      DATA ( ECS_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.276398E-21, 1.192333E-21, 1.045257E-21, 8.708165E-22, 
     & 4.101440E-22, 2.057185E-23, 0.000000E+00 /
      DATA ( ECS_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.276398E-21, 1.192333E-21, 1.045257E-21, 8.708165E-22, 
     & 4.101440E-22, 2.057185E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.836546E-02, 3.029115E-02, 2.094852E-02, 1.469431E-02, 
     & 6.282425E-03, 1.550417E-03, 0.000000E+00 /
      DATA ( EQY_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.836546E-02, 3.029115E-02, 2.094852E-02, 1.469431E-02, 
     & 6.282425E-03, 1.550417E-03, 0.000000E+00 /
      DATA ( EQY_REF( IMACR-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.836546E-02, 3.029115E-02, 2.094852E-02, 1.469431E-02, 
     & 6.282425E-03, 1.550417E-03, 0.000000E+00 /


C...MVK-06
C..  MVK total photolysis
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P10
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/
C..  This datasheet updated: 9th August 2002.
C..  Decline in absorption cross sections with wavelength above 395 nm estimated by
C..  linear interpolation.
C..  Quantum yields for all photodecomposition processes. IUPAC recommends ~60% for
C..  propene formation, rest radical forming routes.
C..  Quantum yields calculated for 1 atm overpredicts photolysis rates for chamber
C..  experiments that gives best fits to data. Effective pressure increased to 5 atm
C..  to give overall quantum yields that are consistent with modeling chamber data.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IMVK-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.041510E-20, 4.373023E-20, 5.436502E-20, 6.318696E-20, 
     & 6.828636E-20, 8.068906E-21, 0.000000E+00 /
      DATA ( CS_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.041510E-20, 4.373023E-20, 5.436502E-20, 6.318696E-20, 
     & 6.828636E-20, 8.068906E-21, 0.000000E+00 /
      DATA ( CS_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.041510E-20, 4.373023E-20, 5.436502E-20, 6.318696E-20, 
     & 6.828636E-20, 8.068906E-21, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.750364E-02, 1.113042E-02, 7.578346E-03, 5.286950E-03, 
     & 2.238235E-03, 1.619938E-04, 0.000000E+00 /
      DATA ( QY_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.750364E-02, 1.113042E-02, 7.578346E-03, 5.286950E-03, 
     & 2.238235E-03, 1.619938E-04, 0.000000E+00 /
      DATA ( QY_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.750364E-02, 1.113042E-02, 7.578346E-03, 5.286950E-03, 
     & 2.238235E-03, 1.619938E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.264576E-22, 4.799989E-22, 4.109027E-22, 3.323400E-22, 
     & 1.546971E-22, 7.827994E-24, 0.000000E+00 /
      DATA ( ECS_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.264576E-22, 4.799989E-22, 4.109027E-22, 3.323400E-22, 
     & 1.546971E-22, 7.827994E-24, 0.000000E+00 /
      DATA ( ECS_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.264576E-22, 4.799989E-22, 4.109027E-22, 3.323400E-22, 
     & 1.546971E-22, 7.827994E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.730908E-02, 1.097636E-02, 7.558218E-03, 5.259628E-03, 
     & 2.265417E-03, 9.701432E-04, 0.000000E+00 /
      DATA ( EQY_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.730908E-02, 1.097636E-02, 7.558218E-03, 5.259628E-03, 
     & 2.265417E-03, 9.701432E-04, 0.000000E+00 /
      DATA ( EQY_REF( IMVK-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.730908E-02, 1.097636E-02, 7.558218E-03, 5.259628E-03, 
     & 2.265417E-03, 9.701432E-04, 0.000000E+00 /


C...IC3ONO2
C..  ! I-C3H7ONO2 + HV = PRODUCTS
C..  IUPAC (1997).  Recommend assuming unit quantum yields.
C..  This has stronger absorption than n-C3-ONO2 and lower nitrates,
C..  but chosen as representative of lumped higher nitrates.
C..  wl       abs

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IIC3ONO2 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667190E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667190E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667190E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667181E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667181E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667181E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999965E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999965E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999965E-01, 0.000000E+00, 0.000000E+00 /


C...HOCCHO_IUPAC
C..  HOCH2CHO + hv ---> products		
C..  IUPAC (2002) based on Bacher et al (2001), J. Atm. Chem, 39, 171.
C..  quantum yield = 0.75+/-0.25		
C..  lambda(nm) xcross(1.E+20*cm2) yield	

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHOCCHO_IUPAC ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.704784E-20, 2.277375E-20, 1.257585E-20, 5.852386E-21, 
     & 6.512001E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.704784E-20, 2.277375E-20, 1.257585E-20, 5.852386E-21, 
     & 6.512001E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.704784E-20, 2.277375E-20, 1.257585E-20, 5.852386E-21, 
     & 6.512001E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 4.315647E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 4.315647E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 4.315647E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.778588E-20, 1.708031E-20, 9.431886E-21, 4.389290E-21, 
     & 4.882505E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.778588E-20, 1.708031E-20, 9.431886E-21, 4.389290E-21, 
     & 4.882505E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.778588E-20, 1.708031E-20, 9.431886E-21, 4.389290E-21, 
     & 4.882505E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 7.497704E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 7.497704E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHOCCHO_IUPAC, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 7.497704E-01, 0.000000E+00, 0.000000E+00 /


C...ACRO-09
C..  ! Chemical Kinetics and Photochemical Data for Use in Atmospheric Studies
C..  Evaluation Number 15. JPL Publication 06-2. July 10, 2006
C..  The quantum yields were derived using the pressure and wavelength-dependent
C..  expression given by IUPAC (2005) for MVK,
C..  with the total pressure adjusted so that the radical forming photolysis rates
C..  for the chamber experiments are the same as those that fit the chamber data.
C..  Adjusted pressure = 1.50 atm.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IACRO-09 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.856131E-20, 2.787986E-20, 3.602617E-20, 4.321522E-20, 
     & 5.371940E-20, 1.543856E-20, 0.000000E+00 /
      DATA ( CS_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.856131E-20, 2.787986E-20, 3.602617E-20, 4.321522E-20, 
     & 5.371940E-20, 1.543856E-20, 0.000000E+00 /
      DATA ( CS_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.856131E-20, 2.787986E-20, 3.602617E-20, 4.321522E-20, 
     & 5.371940E-20, 1.543856E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.332907E-02, 3.341171E-02, 2.275740E-02, 1.604886E-02, 
     & 6.814506E-03, 6.657416E-04, 0.000000E+00 /
      DATA ( QY_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.332907E-02, 3.341171E-02, 2.275740E-02, 1.604886E-02, 
     & 6.814506E-03, 6.657416E-04, 0.000000E+00 /
      DATA ( QY_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.332907E-02, 3.341171E-02, 2.275740E-02, 1.604886E-02, 
     & 6.814506E-03, 6.657416E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.780047E-22, 9.183047E-22, 8.156431E-22, 6.894450E-22, 
     & 3.635462E-22, 2.508367E-23, 0.000000E+00 /
      DATA ( ECS_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.780047E-22, 9.183047E-22, 8.156431E-22, 6.894450E-22, 
     & 3.635462E-22, 2.508367E-23, 0.000000E+00 /
      DATA ( ECS_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.780047E-22, 9.183047E-22, 8.156431E-22, 6.894450E-22, 
     & 3.635462E-22, 2.508367E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.269049E-02, 3.293793E-02, 2.264030E-02, 1.595376E-02, 
     & 6.767503E-03, 1.624741E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.269049E-02, 3.293793E-02, 2.264030E-02, 1.595376E-02, 
     & 6.767503E-03, 1.624741E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACRO-09, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.269049E-02, 3.293793E-02, 2.264030E-02, 1.595376E-02, 
     & 6.767503E-03, 1.624741E-03, 0.000000E+00 /


C...PAA
C..  Peroxy acetic acid absorption cross sections
C..  Orlando, J. J. and G. S. Tyndall (2003): "Gas phase UV absorption spectra
C..  for peracetic acid, and for acetic acid monomers and dimers," J. Photochem.
C..  Photobiol A, 157, 161-166.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IPAA ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.880975E-21, 9.984305E-22, 5.884868E-22, 3.735135E-22, 
     & 1.059119E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.880975E-21, 9.984305E-22, 5.884868E-22, 3.735135E-22, 
     & 1.059119E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.880975E-21, 9.984305E-22, 5.884868E-22, 3.735135E-22, 
     & 1.059119E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.624107E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.624107E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.624107E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.880975E-21, 9.984305E-22, 5.884868E-22, 3.735135E-22, 
     & 1.059117E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.880975E-21, 9.984305E-22, 5.884868E-22, 3.735135E-22, 
     & 1.059117E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.880975E-21, 9.984305E-22, 5.884868E-22, 3.735135E-22, 
     & 1.059117E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999983E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999983E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPAA, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999983E-01, 0.000000E+00, 0.000000E+00 /


C...CL2
C..  CL2 + HV = 2 CL 	
C.. 	FROM IUPAC EVALUATION (1996)
C.. 	RECOMMEND UNIT QUANTUM YIELD
C.. 	

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICL2 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.311134E-22 /
      DATA ( CS_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.311134E-22 /
      DATA ( CS_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.311134E-22 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.368807E-01 /
      DATA ( QY_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.368807E-01 /
      DATA ( QY_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.368807E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.311133E-22 /
      DATA ( ECS_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.311133E-22 /
      DATA ( ECS_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.311133E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999998E-01 /
      DATA ( EQY_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999998E-01 /
      DATA ( EQY_REF( ICL2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999998E-01 /


C...CLNO-06
C..  CLNO absorption cross sections recommended by IUPAC (2005)
C..  Wavelength where absorption goes to zero est'd by extrapolation
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICLNO-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.713535E-20, 1.043433E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /
      DATA ( CS_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.713535E-20, 1.043433E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /
      DATA ( CS_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.713535E-20, 1.043433E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.444760E-01 /
      DATA ( QY_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.444760E-01 /
      DATA ( QY_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.444760E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.713535E-20, 1.043433E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /
      DATA ( ECS_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.713535E-20, 1.043433E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /
      DATA ( ECS_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.713535E-20, 1.043433E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /
      DATA ( EQY_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /
      DATA ( EQY_REF( ICLNO-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /


C...CLONO
C..  CLONO + HV = CL + NO2	
C.. 	FROM IUPAC EVALUATION (1996)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICLONO ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205327E-19, 
     & 5.728580E-19, 7.394602E-20, 0.000000E+00 /
      DATA ( CS_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205327E-19, 
     & 5.728580E-19, 7.394602E-20, 0.000000E+00 /
      DATA ( CS_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205327E-19, 
     & 5.728580E-19, 7.394602E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.317809E-01, 0.000000E+00 /
      DATA ( QY_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.317809E-01, 0.000000E+00 /
      DATA ( QY_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.317809E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205327E-19, 
     & 5.728580E-19, 7.394601E-20, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205327E-19, 
     & 5.728580E-19, 7.394601E-20, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205327E-19, 
     & 5.728580E-19, 7.394601E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /


C...CLNO2
C..  CLNO2 + HV = CL + NO2	
C.. 	FROM IUPAC EVALUATION (1996)
C.. 	RECOMMEND UNIT QUANTUM YILED

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICLNO2 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258212E-20, 4.822256E-21, 0.000000E+00 /
      DATA ( CS_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258212E-20, 4.822256E-21, 0.000000E+00 /
      DATA ( CS_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258212E-20, 4.822256E-21, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.412513E-01, 0.000000E+00 /
      DATA ( QY_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.412513E-01, 0.000000E+00 /
      DATA ( QY_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.412513E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258212E-20, 4.822253E-21, 0.000000E+00 /
      DATA ( ECS_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258212E-20, 4.822253E-21, 0.000000E+00 /
      DATA ( ECS_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258212E-20, 4.822253E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLNO2, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /


C...CLONO2-1
C..  CLONO2 + hv = CLO. + NO2
C..  CLONO2 absorption cross sections and quantum yields recommended by IUPAC (2005)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICLONO2-1 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.653763E-20, 2.593172E-20, 1.621920E-20, 1.063004E-20, 
     & 4.392588E-21, 4.870605E-22, 0.000000E+00 /
      DATA ( CS_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.653763E-20, 2.593172E-20, 1.621920E-20, 1.063004E-20, 
     & 4.392588E-21, 4.870605E-22, 0.000000E+00 /
      DATA ( CS_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.653763E-20, 2.593172E-20, 1.621920E-20, 1.063004E-20, 
     & 4.392588E-21, 4.870605E-22, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.989906E-01, 3.830687E-01, 3.395569E-01, 
     & 2.198601E-01, 1.467998E-02, 0.000000E+00 /
      DATA ( QY_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.989906E-01, 3.830687E-01, 3.395569E-01, 
     & 2.198601E-01, 1.467998E-02, 0.000000E+00 /
      DATA ( QY_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.989906E-01, 3.830687E-01, 3.395569E-01, 
     & 2.198601E-01, 1.467998E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.861505E-20, 1.035219E-20, 6.224709E-21, 3.635981E-21, 
     & 1.037276E-21, 3.317179E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.861505E-20, 1.035219E-20, 6.224709E-21, 3.635981E-21, 
     & 1.037276E-21, 3.317179E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.861505E-20, 1.035219E-20, 6.224709E-21, 3.635981E-21, 
     & 1.037276E-21, 3.317179E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.992096E-01, 3.837864E-01, 3.420477E-01, 
     & 2.361424E-01, 6.810609E-02, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.992096E-01, 3.837864E-01, 3.420477E-01, 
     & 2.361424E-01, 6.810609E-02, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO2-1, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.992096E-01, 3.837864E-01, 3.420477E-01, 
     & 2.361424E-01, 6.810609E-02, 0.000000E+00 /


C...CLONO2-2
C..  CLONO2 + hv = CL. + NO3
C..  CLONO2 absorption cross sections and quantum yields recommended by IUPAC (2005)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICLONO2-2 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.653763E-20, 2.593172E-20, 1.621920E-20, 1.063004E-20, 
     & 4.392588E-21, 1.220202E-21, 9.579244E-24 /
      DATA ( CS_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.653763E-20, 2.593172E-20, 1.621920E-20, 1.063004E-20, 
     & 4.392588E-21, 1.220202E-21, 9.579244E-24 /
      DATA ( CS_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.653763E-20, 2.593172E-20, 1.621920E-20, 1.063004E-20, 
     & 4.392588E-21, 1.220202E-21, 9.579244E-24 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.010094E-01, 6.169314E-01, 6.604431E-01, 
     & 7.801399E-01, 9.853200E-01, 3.841701E-02 /
      DATA ( QY_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.010094E-01, 6.169314E-01, 6.604431E-01, 
     & 7.801399E-01, 9.853200E-01, 3.841701E-02 /
      DATA ( QY_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.010094E-01, 6.169314E-01, 6.604431E-01, 
     & 7.801399E-01, 9.853200E-01, 3.841701E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.792258E-20, 1.557953E-20, 9.994494E-21, 6.994062E-21, 
     & 3.355312E-21, 1.187030E-21, 9.579223E-24 /
      DATA ( ECS_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.792258E-20, 1.557953E-20, 9.994494E-21, 6.994062E-21, 
     & 3.355312E-21, 1.187030E-21, 9.579223E-24 /
      DATA ( ECS_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.792258E-20, 1.557953E-20, 9.994494E-21, 6.994062E-21, 
     & 3.355312E-21, 1.187030E-21, 9.579223E-24 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.007904E-01, 6.162136E-01, 6.579523E-01, 
     & 7.638576E-01, 9.728145E-01, 9.999978E-01 /
      DATA ( EQY_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.007904E-01, 6.162136E-01, 6.579523E-01, 
     & 7.638576E-01, 9.728145E-01, 9.999978E-01 /
      DATA ( EQY_REF( ICLONO2-2, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.007904E-01, 6.162136E-01, 6.579523E-01, 
     & 7.638576E-01, 9.728145E-01, 9.999978E-01 /


C...HOCL-06
C..  HOCL absorption cross sections recommended by IUPAC (2005)
C..
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHOCL-06 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377944E-20, 
     & 3.120856E-20, 6.545257E-21, 1.283779E-23 /
      DATA ( CS_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377944E-20, 
     & 3.120856E-20, 6.545257E-21, 1.283779E-23 /
      DATA ( CS_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377944E-20, 
     & 3.120856E-20, 6.545257E-21, 1.283779E-23 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.671194E-02 /
      DATA ( QY_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.671194E-02 /
      DATA ( QY_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.671194E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377944E-20, 
     & 3.120856E-20, 6.545257E-21, 1.283761E-23 /
      DATA ( ECS_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377944E-20, 
     & 3.120856E-20, 6.545257E-21, 1.283761E-23 /
      DATA ( ECS_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377944E-20, 
     & 3.120856E-20, 6.545257E-21, 1.283761E-23 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999858E-01 /
      DATA ( EQY_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999858E-01 /
      DATA ( EQY_REF( IHOCL-06, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999858E-01 /


C...CLCCHO
C..  Chloroacetaldehyde absorption cross sections
C..  NASA (2006) evaluation

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICLCCHO ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.379343E-20, 5.300274E-20, 4.849358E-20, 3.861001E-20, 
     & 1.286110E-20, 9.229084E-23, 0.000000E+00 /
      DATA ( CS_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.379343E-20, 5.300274E-20, 4.849358E-20, 3.861001E-20, 
     & 1.286110E-20, 9.229084E-23, 0.000000E+00 /
      DATA ( CS_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.379343E-20, 5.300274E-20, 4.849358E-20, 3.861001E-20, 
     & 1.286110E-20, 9.229084E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.478604E-01, 0.000000E+00 /
      DATA ( QY_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.478604E-01, 0.000000E+00 /
      DATA ( QY_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.478604E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.379343E-20, 5.300274E-20, 4.849358E-20, 3.861001E-20, 
     & 1.286110E-20, 9.229049E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.379343E-20, 5.300274E-20, 4.849358E-20, 3.861001E-20, 
     & 1.286110E-20, 9.229049E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.379343E-20, 5.300274E-20, 4.849358E-20, 3.861001E-20, 
     & 1.286110E-20, 9.229049E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999961E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999961E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLCCHO, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999961E-01, 0.000000E+00 /


C...CLACET
C..  Chloroacetone absorption cross sections
C..  Chloroacetone absorption cross sections
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICLACET ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000120E-19, 8.662040E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132987E-20, 6.893330E-23, 0.000000E+00 /
      DATA ( CS_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000120E-19, 8.662040E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132987E-20, 6.893330E-23, 0.000000E+00 /
      DATA ( CS_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000120E-19, 8.662040E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132987E-20, 6.893330E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /
      DATA ( QY_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /
      DATA ( QY_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000120E-19, 8.662040E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132987E-20, 6.893320E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000120E-19, 8.662040E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132987E-20, 6.893320E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000120E-19, 8.662040E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132987E-20, 6.893320E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999986E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999986E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLACET, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999986E-01, 0.000000E+00 /


C...NO2_SAPRC99
C..  NO2 + HV = NO + O
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, INO2_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478274E-19, 1.860793E-19, 2.249056E-19, 
     & 3.335557E-19, 5.492275E-19, 1.148338E-20 /
      DATA ( CS_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478274E-19, 1.860793E-19, 2.249056E-19, 
     & 3.335557E-19, 5.492275E-19, 1.148338E-20 /
      DATA ( CS_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478274E-19, 1.860793E-19, 2.249056E-19, 
     & 3.335557E-19, 5.492275E-19, 1.148338E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986849E-01, 9.908285E-01, 
     & 9.900000E-01, 7.922024E-01, 7.010691E-04 /
      DATA ( QY_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986849E-01, 9.908285E-01, 
     & 9.900000E-01, 7.922024E-01, 7.010691E-04 /
      DATA ( QY_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986849E-01, 9.908285E-01, 
     & 9.900000E-01, 7.922024E-01, 7.010691E-04 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478274E-19, 1.858225E-19, 2.228265E-19, 
     & 3.302201E-19, 4.272924E-19, 4.015397E-22 /
      DATA ( ECS_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478274E-19, 1.858225E-19, 2.228265E-19, 
     & 3.302201E-19, 4.272924E-19, 4.015397E-22 /
      DATA ( ECS_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478274E-19, 1.858225E-19, 2.228265E-19, 
     & 3.302201E-19, 4.272924E-19, 4.015397E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986200E-01, 9.907554E-01, 
     & 9.900000E-01, 7.779881E-01, 3.496704E-02 /
      DATA ( EQY_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986200E-01, 9.907554E-01, 
     & 9.900000E-01, 7.779881E-01, 3.496704E-02 /
      DATA ( EQY_REF( INO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986200E-01, 9.907554E-01, 
     & 9.900000E-01, 7.779881E-01, 3.496704E-02 /


C...NO3NO_SAPRC99
C..  NO3 + HV = NO + O2 (T=298)
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, INO3NO_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.695915E-19 /
      DATA ( CS_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.695915E-19 /
      DATA ( CS_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.695915E-19 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.027933E-02 /
      DATA ( QY_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.027933E-02 /
      DATA ( QY_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.027933E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.172137E-20 /
      DATA ( ECS_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.172137E-20 /
      DATA ( ECS_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.172137E-20 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.434736E-01 /
      DATA ( EQY_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.434736E-01 /
      DATA ( EQY_REF( INO3NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.434736E-01 /


C...NO3NO2_SAPRC99
C..  NO3 + HV = NO2 + O (T=298)
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, INO3NO2_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263254E-21, 1.112198E-18 /
      DATA ( CS_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263254E-21, 1.112198E-18 /
      DATA ( CS_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263254E-21, 1.112198E-18 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.818170E-01, 4.333254E-01 /
      DATA ( QY_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.818170E-01, 4.333254E-01 /
      DATA ( QY_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.818170E-01, 4.333254E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263254E-21, 7.608259E-19 /
      DATA ( ECS_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263254E-21, 7.608259E-19 /
      DATA ( ECS_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263254E-21, 7.608259E-19 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.000000E+00, 6.840739E-01 /
      DATA ( EQY_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.000000E+00, 6.840739E-01 /
      DATA ( EQY_REF( INO3NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.000000E+00, 6.840739E-01 /


C...O3O3P_SAPRC99
C..  O3 + HV = O1D + O2
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07
C..  Absorption cross sections from NASA (1999), using wavelength which is cente
C..  r of intervals shown.
C..  Quantum yields derived from O3->O1D quantum yields assuming total quantum y
C..  ield is 1, though this is not adequately discussed in the evaluations.
C..  Values given are interpolated for each 1 nm interval.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IO3O3P_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.244220E-19, 2.732263E-19, 1.048019E-19, 4.476736E-20, 
     & 6.115340E-21, 1.783091E-23, 1.650939E-21 /
      DATA ( CS_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.244220E-19, 2.732263E-19, 1.048019E-19, 4.476736E-20, 
     & 6.115340E-21, 1.783091E-23, 1.650939E-21 /
      DATA ( CS_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.244220E-19, 2.732263E-19, 1.048019E-19, 4.476736E-20, 
     & 6.115340E-21, 1.783091E-23, 1.650939E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.227037E-02, 4.316586E-02, 4.576474E-01, 7.880838E-01, 
     & 9.572254E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( QY_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.227037E-02, 4.316586E-02, 4.576474E-01, 7.880838E-01, 
     & 9.572254E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( QY_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.227037E-02, 4.316586E-02, 4.576474E-01, 7.880838E-01, 
     & 9.572254E-01, 1.000000E+00, 1.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.234563E-20, 1.108868E-20, 4.421222E-20, 3.478954E-20, 
     & 5.673124E-21, 1.783091E-23, 1.650939E-21 /
      DATA ( ECS_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.234563E-20, 1.108868E-20, 4.421222E-20, 3.478954E-20, 
     & 5.673124E-21, 1.783091E-23, 1.650939E-21 /
      DATA ( ECS_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.234563E-20, 1.108868E-20, 4.421222E-20, 3.478954E-20, 
     & 5.673124E-21, 1.783091E-23, 1.650939E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.349374E-02, 4.058423E-02, 4.218646E-01, 7.771183E-01, 
     & 9.276875E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.349374E-02, 4.058423E-02, 4.218646E-01, 7.771183E-01, 
     & 9.276875E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 6.349374E-02, 4.058423E-02, 4.218646E-01, 7.771183E-01, 
     & 9.276875E-01, 1.000000E+00, 1.000000E+00 /


C...O3O1D_SAPRC99
C..  O3 + HV = O1D + O2
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07
C..  Absorption cross sections from NASA (1999), using wavelength which is cente
C..  r of intervals shown.
C..  Quantum yields from IUPAC, Supplement VI (1997).
C..  No quantum yield recommendation is given for wl>335.  Assume they decrease
C..  linearly to zero at 340 nm.
C..  Values given are interpolated for each 1 nm interval.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IO3O1D_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.244220E-19, 2.732263E-19, 1.048019E-19, 4.476736E-20, 
     & 5.921096E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.244220E-19, 2.732263E-19, 1.048019E-19, 4.476736E-20, 
     & 5.921096E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.244220E-19, 2.732263E-19, 1.048019E-19, 4.476736E-20, 
     & 5.921096E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.377297E-01, 9.568341E-01, 5.423526E-01, 2.119162E-01, 
     & 4.277464E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.377297E-01, 9.568341E-01, 5.423526E-01, 2.119162E-01, 
     & 4.277464E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.377297E-01, 9.568341E-01, 5.423526E-01, 2.119162E-01, 
     & 4.277464E-02, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.720763E-19, 2.621376E-19, 6.058971E-20, 9.977826E-21, 
     & 4.422152E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.720763E-19, 2.621376E-19, 6.058971E-20, 9.977826E-21, 
     & 4.422152E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.720763E-19, 2.621376E-19, 6.058971E-20, 9.977826E-21, 
     & 4.422152E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.365063E-01, 9.594158E-01, 5.781354E-01, 2.228817E-01, 
     & 7.468469E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.365063E-01, 9.594158E-01, 5.781354E-01, 2.228817E-01, 
     & 7.468469E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.365063E-01, 9.594158E-01, 5.781354E-01, 2.228817E-01, 
     & 7.468469E-02, 0.000000E+00, 0.000000E+00 /


C...HONO_NO_SAPRC99
C..  HONO + HV = HO. + NO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHONO_NO_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218167E-20, 3.478063E-20, 
     & 1.090584E-19, 8.781866E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218167E-20, 3.478063E-20, 
     & 1.090584E-19, 8.781866E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218167E-20, 3.478063E-20, 
     & 1.090584E-19, 8.781866E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.840304E-01, 4.690968E-01, 
     & 6.487832E-01, 6.915469E-01, 0.000000E+00 /
      DATA ( QY_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.840304E-01, 4.690968E-01, 
     & 6.487832E-01, 6.915469E-01, 0.000000E+00 /
      DATA ( QY_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.840304E-01, 4.690968E-01, 
     & 6.487832E-01, 6.915469E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.061498E-21, 1.650524E-20, 
     & 7.360088E-20, 8.336914E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.061498E-21, 1.650524E-20, 
     & 7.360088E-20, 8.336914E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.061498E-21, 1.650524E-20, 
     & 7.360088E-20, 8.336914E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.155010E-01, 4.745526E-01, 
     & 6.748759E-01, 9.493328E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.155010E-01, 4.745526E-01, 
     & 6.748759E-01, 9.493328E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.155010E-01, 4.745526E-01, 
     & 6.748759E-01, 9.493328E-01, 0.000000E+00 /


C...HONO_NO2_SAPRC99
C..  HONO + HV = H. + NO2
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHONO_NO2_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218167E-20, 3.478063E-20, 
     & 1.090584E-19, 3.949920E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218167E-20, 3.478063E-20, 
     & 1.090584E-19, 3.949920E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218167E-20, 3.478063E-20, 
     & 1.090584E-19, 3.949920E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.022079E-01, 5.309032E-01, 
     & 3.512168E-01, 2.663612E-02, 0.000000E+00 /
      DATA ( QY_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.022079E-01, 5.309032E-01, 
     & 3.512168E-01, 2.663612E-02, 0.000000E+00 /
      DATA ( QY_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.022079E-01, 5.309032E-01, 
     & 3.512168E-01, 2.663612E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.119419E-21, 1.827539E-20, 
     & 3.545750E-20, 4.449506E-21, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.119419E-21, 1.827539E-20, 
     & 3.545750E-20, 4.449506E-21, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.119419E-21, 1.827539E-20, 
     & 3.545750E-20, 4.449506E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.844368E-01, 5.254474E-01, 
     & 3.251241E-01, 1.126480E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.844368E-01, 5.254474E-01, 
     & 3.251241E-01, 1.126480E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.844368E-01, 5.254474E-01, 
     & 3.251241E-01, 1.126480E-01, 0.000000E+00 /


C...HNO3_SAPRC99
C..  HNO3 + HV = products
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHNO3_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875286E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875286E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875286E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875253E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875253E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875253E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999914E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999914E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999914E-01, 0.000000E+00 /


C...HO2NO2_SAPRC99
C..  HO2NO2 + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHO2NO2_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327416E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327416E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327416E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327393E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327393E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327393E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999964E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999964E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999964E-01, 0.000000E+00, 0.000000E+00 /


C...H2O2_SAPRC99
C..  H2O2 + HV = 2 OH
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IH2O2_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606317E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606317E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606317E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606284E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606284E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.353273E-21, 5.771023E-21, 3.923019E-21, 2.721922E-21, 
     & 1.138124E-21, 3.606284E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999907E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999907E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999907E-01, 0.000000E+00 /


C...HCHO_R_SAPRC99
C..  HCHO + HV = HCO + H
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHCHO_R_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.131405E-20, 3.361006E-20, 1.633912E-20, 3.089530E-20, 
     & 1.387140E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.131405E-20, 3.361006E-20, 1.633912E-20, 3.089530E-20, 
     & 1.387140E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.131405E-20, 3.361006E-20, 1.633912E-20, 3.089530E-20, 
     & 1.387140E-20, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.530258E-01, 7.793078E-01, 7.694797E-01, 6.766384E-01, 
     & 2.101625E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.530258E-01, 7.793078E-01, 7.694797E-01, 6.766384E-01, 
     & 2.101625E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.530258E-01, 7.793078E-01, 7.694797E-01, 6.766384E-01, 
     & 2.101625E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.360343E-20, 2.619576E-20, 1.264516E-20, 2.110715E-20, 
     & 3.830308E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.360343E-20, 2.619576E-20, 1.264516E-20, 2.110715E-20, 
     & 3.830308E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.360343E-20, 2.619576E-20, 1.264516E-20, 2.110715E-20, 
     & 3.830308E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.537647E-01, 7.794024E-01, 7.739189E-01, 6.831833E-01, 
     & 2.761300E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.537647E-01, 7.794024E-01, 7.739189E-01, 6.831833E-01, 
     & 2.761300E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.537647E-01, 7.794024E-01, 7.739189E-01, 6.831833E-01, 
     & 2.761300E-01, 0.000000E+00, 0.000000E+00 /


C...HCHO_M_SAPRC99
C..  HCHO + HV = H2 + CO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHCHO_M_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.131405E-20, 3.361006E-20, 1.633912E-20, 3.089530E-20, 
     & 1.653419E-20, 7.220988E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.131405E-20, 3.361006E-20, 1.633912E-20, 3.089530E-20, 
     & 1.653419E-20, 7.220988E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.131405E-20, 3.361006E-20, 1.633912E-20, 3.089530E-20, 
     & 1.653419E-20, 7.220988E-22, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.261845E-01, 2.137268E-01, 2.298765E-01, 3.233059E-01, 
     & 5.540997E-01, 2.985818E-02, 0.000000E+00 /
      DATA ( QY_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.261845E-01, 2.137268E-01, 2.298765E-01, 3.233059E-01, 
     & 5.540997E-01, 2.985818E-02, 0.000000E+00 /
      DATA ( QY_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.261845E-01, 2.137268E-01, 2.298765E-01, 3.233059E-01, 
     & 5.540997E-01, 2.985818E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.068871E-21, 7.200778E-21, 3.680172E-21, 9.787415E-21, 
     & 9.269199E-21, 1.144566E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.068871E-21, 7.200778E-21, 3.680172E-21, 9.787415E-21, 
     & 9.269199E-21, 1.144566E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.068871E-21, 7.200778E-21, 3.680172E-21, 9.787415E-21, 
     & 9.269199E-21, 1.144566E-22, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.257411E-01, 2.142447E-01, 2.252368E-01, 3.167930E-01, 
     & 5.606080E-01, 1.585055E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.257411E-01, 2.142447E-01, 2.252368E-01, 3.167930E-01, 
     & 5.606080E-01, 1.585055E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_M_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.257411E-01, 2.142447E-01, 2.252368E-01, 3.167930E-01, 
     & 5.606080E-01, 1.585055E-01, 0.000000E+00 /


C...CCHO_R_SAPRC99
C..  CCHO + HV = CH3 + CHO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICCHO_R_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.432585E-20, 3.717942E-20, 2.933100E-20, 2.104718E-20, 
     & 3.715620E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.432585E-20, 3.717942E-20, 2.933100E-20, 2.104718E-20, 
     & 3.715620E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.432585E-20, 3.717942E-20, 2.933100E-20, 2.104718E-20, 
     & 3.715620E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902035E-01, 2.845210E-01, 1.538490E-01, 
     & 1.409418E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902035E-01, 2.845210E-01, 1.538490E-01, 
     & 1.409418E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902035E-01, 2.845210E-01, 1.538490E-01, 
     & 1.409418E-02, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461039E-20, 8.426933E-21, 3.330087E-21, 
     & 1.731238E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461039E-20, 8.426933E-21, 3.330087E-21, 
     & 1.731238E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461039E-20, 8.426933E-21, 3.330087E-21, 
     & 1.731238E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.845356E-01, 3.929698E-01, 2.873047E-01, 1.582202E-01, 
     & 4.659352E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.845356E-01, 3.929698E-01, 2.873047E-01, 1.582202E-01, 
     & 4.659352E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.845356E-01, 3.929698E-01, 2.873047E-01, 1.582202E-01, 
     & 4.659352E-02, 0.000000E+00, 0.000000E+00 /


C...C2CHO_SAPRC99
C..  C2CHO + HV = C2H5. + CHO.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IC2CHO_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634685E-20, 3.579653E-20, 2.441747E-20, 
     & 5.808840E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634685E-20, 3.579653E-20, 2.441747E-20, 
     & 5.808840E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634685E-20, 3.579653E-20, 2.441747E-20, 
     & 5.808840E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.112440E-01, 7.954015E-01, 5.951666E-01, 4.312301E-01, 
     & 1.520067E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.112440E-01, 7.954015E-01, 5.951666E-01, 4.312301E-01, 
     & 1.520067E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.112440E-01, 7.954015E-01, 5.951666E-01, 4.312301E-01, 
     & 1.520067E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.730259E-20, 3.713793E-20, 2.133678E-20, 1.077363E-20, 
     & 1.383935E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.730259E-20, 3.713793E-20, 2.133678E-20, 1.077363E-20, 
     & 1.383935E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.730259E-20, 3.713793E-20, 2.133678E-20, 1.077363E-20, 
     & 1.383935E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.769110E-01, 8.013043E-01, 5.960574E-01, 4.412262E-01, 
     & 2.382463E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.769110E-01, 8.013043E-01, 5.960574E-01, 4.412262E-01, 
     & 2.382463E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.769110E-01, 8.013043E-01, 5.960574E-01, 4.412262E-01, 
     & 2.382463E-01, 0.000000E+00, 0.000000E+00 /


C...ACETONE_SAPRC99
C..  ACETONE + HV = CH3CO. + CH3.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IACETONE_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.565268E-20, 2.347504E-20, 1.411215E-20, 7.530071E-21, 
     & 8.363713E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.565268E-20, 2.347504E-20, 1.411215E-20, 7.530071E-21, 
     & 8.363713E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.565268E-20, 2.347504E-20, 1.411215E-20, 7.530071E-21, 
     & 8.363713E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.242423E-01, 1.142597E-01, 5.803530E-02, 2.870068E-02, 
     & 4.434784E-03, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.242423E-01, 1.142597E-01, 5.803530E-02, 2.870068E-02, 
     & 4.434784E-03, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.242423E-01, 1.142597E-01, 5.803530E-02, 2.870068E-02, 
     & 4.434784E-03, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.097968E-21, 2.778263E-21, 8.357590E-22, 2.321768E-22, 
     & 8.431113E-24, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.097968E-21, 2.778263E-21, 8.357590E-22, 2.321768E-22, 
     & 8.431113E-24, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.097968E-21, 2.778263E-21, 8.357590E-22, 2.321768E-22, 
     & 8.431113E-24, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.271349E-01, 1.183496E-01, 5.922266E-02, 3.083328E-02, 
     & 1.008059E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.271349E-01, 1.183496E-01, 5.922266E-02, 3.083328E-02, 
     & 1.008059E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.271349E-01, 1.183496E-01, 5.922266E-02, 3.083328E-02, 
     & 1.008059E-02, 0.000000E+00, 0.000000E+00 /


C...KETONE_SAPRC99
C..  Methyl Ethyl Ketone Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IKETONE_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.255143E-20, 2.715759E-20, 1.567302E-20, 7.669485E-21, 
     & 7.479172E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.255143E-20, 2.715759E-20, 1.567302E-20, 7.669485E-21, 
     & 7.479172E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.255143E-20, 2.715759E-20, 1.567302E-20, 7.669485E-21, 
     & 7.479172E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.169248E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.169248E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.169248E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.255143E-20, 2.715759E-20, 1.567302E-20, 7.669485E-21, 
     & 7.479158E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.255143E-20, 2.715759E-20, 1.567302E-20, 7.669485E-21, 
     & 7.479158E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.255143E-20, 2.715759E-20, 1.567302E-20, 7.669485E-21, 
     & 7.479158E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999980E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999980E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999980E-01, 0.000000E+00, 0.000000E+00 /


C...COOH_SAPRC99
C..  CH3OOH + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICOOH_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395106E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395106E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395106E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /
      DATA ( QY_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /
      DATA ( QY_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395094E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395094E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524637E-21, 2.410332E-21, 1.699590E-21, 
     & 7.230011E-22, 5.395094E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999977E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999977E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999977E-01, 0.000000E+00 /


C...GLY_R_SAPRC99
C..  Glyoxal + hv = 2 HCO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IGLY_R_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 2.034957E-20, 6.237771E-22 /
      DATA ( CS_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 2.034957E-20, 6.237771E-22 /
      DATA ( CS_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 2.034957E-20, 6.237771E-22 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.446526E-01, 0.000000E+00 /
      DATA ( QY_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.446526E-01, 0.000000E+00 /
      DATA ( QY_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.446526E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 4.361282E-21, 0.000000E+00 /
      DATA ( ECS_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 4.361282E-21, 0.000000E+00 /
      DATA ( ECS_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 4.361282E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.143182E-01, 0.000000E+00 /
      DATA ( EQY_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.143182E-01, 0.000000E+00 /
      DATA ( EQY_REF( IGLY_R_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.143182E-01, 0.000000E+00 /


C...GLY_ABS_SAPRC99
C..  Glyoxal Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IGLY_ABS_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 2.034957E-20, 7.746852E-21 /
      DATA ( CS_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 2.034957E-20, 7.746852E-21 /
      DATA ( CS_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 2.034957E-20, 7.746852E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.534627E-02 /
      DATA ( QY_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.534627E-02 /
      DATA ( QY_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.534627E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 2.034957E-20, 7.746850E-21 /
      DATA ( ECS_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 2.034957E-20, 7.746850E-21 /
      DATA ( ECS_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.307063E-20, 3.064664E-20, 2.759451E-20, 2.066151E-20, 
     & 6.469825E-21, 2.034957E-20, 7.746850E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999998E-01 /
      DATA ( EQY_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999998E-01 /
      DATA ( EQY_REF( IGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999998E-01 /


C...MGLY_ADJ_SAPRC99
C..  MGLY + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IMGLY_ADJ_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.998245E-21, 3.697535E-20, 1.477384E-21 /
      DATA ( CS_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.998245E-21, 3.697535E-20, 1.477384E-21 /
      DATA ( CS_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.998245E-21, 3.697535E-20, 1.477384E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992940E-01, 3.801420E-01, 0.000000E+00 /
      DATA ( QY_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992940E-01, 3.801420E-01, 0.000000E+00 /
      DATA ( QY_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992940E-01, 3.801420E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.996184E-21, 6.110818E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.996184E-21, 6.110818E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.996184E-21, 6.110818E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.996565E-01, 1.652673E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.996565E-01, 1.652673E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.996565E-01, 1.652673E-01, 0.000000E+00 /


C...BACL_ADJ_SAPRC99
C..  BACL + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IBACL_ADJ_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589010E-20, 9.041891E-21, 6.004446E-21, 
     & 4.676509E-21, 3.224516E-20, 4.675101E-21 /
      DATA ( CS_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589010E-20, 9.041891E-21, 6.004446E-21, 
     & 4.676509E-21, 3.224516E-20, 4.675101E-21 /
      DATA ( CS_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589010E-20, 9.041891E-21, 6.004446E-21, 
     & 4.676509E-21, 3.224516E-20, 4.675101E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.303439E-01, 9.162503E-04 /
      DATA ( QY_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.303439E-01, 9.162503E-04 /
      DATA ( QY_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.303439E-01, 9.162503E-04 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589010E-20, 9.041891E-21, 6.004446E-21, 
     & 4.676509E-21, 1.215315E-20, 6.213340E-23 /
      DATA ( ECS_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589010E-20, 9.041891E-21, 6.004446E-21, 
     & 4.676509E-21, 1.215315E-20, 6.213340E-23 /
      DATA ( ECS_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589010E-20, 9.041891E-21, 6.004446E-21, 
     & 4.676509E-21, 1.215315E-20, 6.213340E-23 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.768983E-01, 1.329028E-02 /
      DATA ( EQY_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.768983E-01, 1.329028E-02 /
      DATA ( EQY_REF( IBACL_ADJ_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.768983E-01, 1.329028E-02 /


C...BZCHO_SAPRC99
C..  Benzaldehyde absorbtion coefs in n-Hexane
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IBZCHO_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.271380E-20, 6.609055E-20, 6.730978E-20, 
     & 8.248206E-20, 2.821755E-20, 0.000000E+00 /
      DATA ( CS_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.271380E-20, 6.609055E-20, 6.730978E-20, 
     & 8.248206E-20, 2.821755E-20, 0.000000E+00 /
      DATA ( CS_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.271380E-20, 6.609055E-20, 6.730978E-20, 
     & 8.248206E-20, 2.821755E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.366284E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.153002E-01, 0.000000E+00 /
      DATA ( QY_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.366284E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.153002E-01, 0.000000E+00 /
      DATA ( QY_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.366284E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.153002E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.247798E-20, 6.609055E-20, 6.730978E-20, 
     & 8.248206E-20, 2.821755E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.247798E-20, 6.609055E-20, 6.730978E-20, 
     & 8.248206E-20, 2.821755E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.247798E-20, 6.609055E-20, 6.730978E-20, 
     & 8.248206E-20, 2.821755E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.974566E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999997E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.974566E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999997E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBZCHO_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.974566E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999997E-01, 0.000000E+00 /


C...ACROLEIN_SAPRC99
C..  Absorption cross sections for Acrolein.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IACROLEIN_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.119557E-20, 3.145168E-20, 4.081551E-20, 4.837758E-20, 
     & 5.750342E-20, 1.189678E-20, 0.000000E+00 /
      DATA ( CS_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.119557E-20, 3.145168E-20, 4.081551E-20, 4.837758E-20, 
     & 5.750342E-20, 1.189678E-20, 0.000000E+00 /
      DATA ( CS_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.119557E-20, 3.145168E-20, 4.081551E-20, 4.837758E-20, 
     & 5.750342E-20, 1.189678E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.565694E-01, 0.000000E+00 /
      DATA ( QY_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.565694E-01, 0.000000E+00 /
      DATA ( QY_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.565694E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.119557E-20, 3.145168E-20, 4.081551E-20, 4.837758E-20, 
     & 5.750342E-20, 1.189678E-20, 0.000000E+00 /
      DATA ( ECS_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.119557E-20, 3.145168E-20, 4.081551E-20, 4.837758E-20, 
     & 5.750342E-20, 1.189678E-20, 0.000000E+00 /
      DATA ( ECS_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.119557E-20, 3.145168E-20, 4.081551E-20, 4.837758E-20, 
     & 5.750342E-20, 1.189678E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACROLEIN_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /


C...IC3ONO2_SAPRC99
C..  I-C3H7ONO2 + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IIC3ONO2_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667190E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667190E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667190E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667181E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667181E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.255485E-20, 6.352151E-21, 3.286583E-21, 1.709812E-21, 
     & 2.667181E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999965E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999965E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999965E-01, 0.000000E+00, 0.000000E+00 /


C...MGLY_ABS_SAPRC99
C..  Methyl Glyoxal Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IMGLY_ABS_SAPRC99 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.998245E-21, 3.697535E-20, 7.930062E-21 /
      DATA ( CS_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.998245E-21, 3.697535E-20, 7.930062E-21 /
      DATA ( CS_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.998245E-21, 3.697535E-20, 7.930062E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.689096E-01 /
      DATA ( QY_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.689096E-01 /
      DATA ( QY_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.689096E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.998245E-21, 3.697535E-20, 7.930062E-21 /
      DATA ( ECS_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.998245E-21, 3.697535E-20, 7.930062E-21 /
      DATA ( ECS_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.414883E-20, 3.510057E-20, 2.364220E-20, 1.814699E-20, 
     & 5.998245E-21, 3.697535E-20, 7.930062E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IMGLY_ABS_SAPRC99, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /


C...O3_O3P_IUPAC04
C.. O3 + HV = O(3P) + O2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet POx2, updated 2nd October 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IO3_O3P_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.237238E-19, 2.618511E-19, 1.031290E-19, 4.161018E-20, 
     & 6.126439E-21, 4.250345E-23, 1.550664E-21 /
      DATA ( CS_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.237238E-19, 2.618511E-19, 1.031290E-19, 4.161018E-20, 
     & 6.126439E-21, 4.250345E-23, 1.550664E-21 /
      DATA ( CS_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.237238E-19, 2.618511E-19, 1.031290E-19, 4.161018E-20, 
     & 6.126439E-21, 4.250345E-23, 1.550664E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070961E-01, 4.640082E-01, 7.781807E-01, 
     & 9.134409E-01, 9.634614E-01, 6.920660E-01 /
      DATA ( QY_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070961E-01, 4.640082E-01, 7.781807E-01, 
     & 9.134409E-01, 9.634614E-01, 6.920660E-01 /
      DATA ( QY_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070961E-01, 4.640082E-01, 7.781807E-01, 
     & 9.134409E-01, 9.634614E-01, 6.920660E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.237238E-20, 2.744997E-20, 4.605586E-20, 3.200319E-20, 
     & 5.504526E-21, 3.927087E-23, 1.550664E-21 /
      DATA ( ECS_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.237238E-20, 2.744997E-20, 4.605586E-20, 3.200319E-20, 
     & 5.504526E-21, 3.927087E-23, 1.550664E-21 /
      DATA ( ECS_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.237238E-20, 2.744997E-20, 4.605586E-20, 3.200319E-20, 
     & 5.504526E-21, 3.927087E-23, 1.550664E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.048305E-01, 4.465847E-01, 7.691193E-01, 
     & 8.984871E-01, 9.239455E-01, 9.999999E-01 /
      DATA ( EQY_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.048305E-01, 4.465847E-01, 7.691193E-01, 
     & 8.984871E-01, 9.239455E-01, 9.999999E-01 /
      DATA ( EQY_REF( IO3_O3P_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.048305E-01, 4.465847E-01, 7.691193E-01, 
     & 8.984871E-01, 9.239455E-01, 9.999999E-01 /


C...O3_O1D_IUPAC04
C..  O3 + HV = O(1D) + O2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet POx2, updated 2nd October 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IO3_O1D_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.237238E-19, 2.618511E-19, 1.031290E-19, 4.161018E-20, 
     & 6.126439E-21, 4.040724E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.237238E-19, 2.618511E-19, 1.031290E-19, 4.161018E-20, 
     & 6.126439E-21, 4.040724E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.237238E-19, 2.618511E-19, 1.031290E-19, 4.161018E-20, 
     & 6.126439E-21, 4.040724E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929039E-01, 5.359917E-01, 2.218193E-01, 
     & 8.655906E-02, 1.629167E-02, 0.000000E+00 /
      DATA ( QY_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929039E-01, 5.359917E-01, 2.218193E-01, 
     & 8.655906E-02, 1.629167E-02, 0.000000E+00 /
      DATA ( QY_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929039E-01, 5.359917E-01, 2.218193E-01, 
     & 8.655906E-02, 1.629167E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.413514E-19, 2.344011E-19, 5.707319E-20, 9.606989E-21, 
     & 6.219128E-22, 3.232562E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.413514E-19, 2.344011E-19, 5.707319E-20, 9.606989E-21, 
     & 6.219128E-22, 3.232562E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.413514E-19, 2.344011E-19, 5.707319E-20, 9.606989E-21, 
     & 6.219128E-22, 3.232562E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.951695E-01, 5.534152E-01, 2.308807E-01, 
     & 1.015129E-01, 7.999958E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.951695E-01, 5.534152E-01, 2.308807E-01, 
     & 1.015129E-01, 7.999958E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3_O1D_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.951695E-01, 5.534152E-01, 2.308807E-01, 
     & 1.015129E-01, 7.999958E-02, 0.000000E+00 /


C...HONO_IUPAC04
C.. HONO + HV = HO + NO
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx1_HONO, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHONO_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780390E-21, 1.602461E-20, 3.132017E-20, 
     & 9.264930E-20, 7.356076E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780390E-21, 1.602461E-20, 3.132017E-20, 
     & 9.264930E-20, 7.356076E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780390E-21, 1.602461E-20, 3.132017E-20, 
     & 9.264930E-20, 7.356076E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 8.334302E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.181830E-01, 0.000000E+00 /
      DATA ( QY_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 8.334302E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.181830E-01, 0.000000E+00 /
      DATA ( QY_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 8.334302E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.181830E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780352E-21, 1.602461E-20, 3.132017E-20, 
     & 9.264930E-20, 7.356075E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780352E-21, 1.602461E-20, 3.132017E-20, 
     & 9.264930E-20, 7.356075E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780352E-21, 1.602461E-20, 3.132017E-20, 
     & 9.264930E-20, 7.356075E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.999921E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.999921E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.999921E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /


C...HO2NO2_IUPAC04
C..  HOONO2 + HV = products
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx3_HO2NO2, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHO2NO2_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327416E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327416E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327416E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327393E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327393E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 2.591182E-20, 1.079995E-20, 5.527955E-21, 3.457478E-21, 
     & 6.327393E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999964E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999964E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999964E-01, 0.000000E+00, 0.000000E+00 /


C...HNO3_IUPAC04
C.. HONO2 + HV = OH + NO2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx2_HONO2, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHNO3_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875286E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875286E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875286E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875253E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875253E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.358854E-21, 1.954560E-21, 8.462490E-22, 3.649802E-22, 
     & 4.770221E-23, 3.875253E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999914E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999914E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999914E-01, 0.000000E+00 /


C...N2O5_IUPAC04
C.. N2O5 + HV = NO2 + NO3
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx7_N2O5, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IN2O5_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.288716E-20, 3.284370E-20, 2.192962E-20, 1.512788E-20, 
     & 6.068443E-21, 6.195751E-22, 9.185398E-26 /
      DATA ( CS_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.288716E-20, 3.284370E-20, 2.192962E-20, 1.512788E-20, 
     & 6.068443E-21, 6.195751E-22, 9.185398E-26 /
      DATA ( CS_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.288716E-20, 3.284370E-20, 2.192962E-20, 1.512788E-20, 
     & 6.068443E-21, 6.195751E-22, 9.185398E-26 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.222279E-01, 9.980621E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 4.477524E-03 /
      DATA ( QY_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.222279E-01, 9.980621E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 4.477524E-03 /
      DATA ( QY_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.222279E-01, 9.980621E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 4.477524E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.857826E-20, 3.276334E-20, 2.192962E-20, 1.512788E-20, 
     & 6.068443E-21, 6.195751E-22, 9.184478E-26 /
      DATA ( ECS_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.857826E-20, 3.276334E-20, 2.192962E-20, 1.512788E-20, 
     & 6.068443E-21, 6.195751E-22, 9.184478E-26 /
      DATA ( ECS_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.857826E-20, 3.276334E-20, 2.192962E-20, 1.512788E-20, 
     & 6.068443E-21, 6.195751E-22, 9.184478E-26 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.185266E-01, 9.975534E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.998998E-01 /
      DATA ( EQY_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.185266E-01, 9.975534E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.998998E-01 /
      DATA ( EQY_REF( IN2O5_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 9.185266E-01, 9.975534E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.998998E-01 /


C...NTR_IUPAC04
C.. i-C3H7ONO2 + HV = iC3H7O + NO2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet P17_i-C3H7ONO2+hv, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, INTR_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112268E-21, 3.012610E-21, 1.483086E-21, 
     & 2.223501E-22, 1.206809E-24, 0.000000E+00 /
      DATA ( CS_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112268E-21, 3.012610E-21, 1.483086E-21, 
     & 2.223501E-22, 1.206809E-24, 0.000000E+00 /
      DATA ( CS_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112268E-21, 3.012610E-21, 1.483086E-21, 
     & 2.223501E-22, 1.206809E-24, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /
      DATA ( QY_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /
      DATA ( QY_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112268E-21, 3.012610E-21, 1.483086E-21, 
     & 2.223501E-22, 1.206808E-24, 0.000000E+00 /
      DATA ( ECS_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112268E-21, 3.012610E-21, 1.483086E-21, 
     & 2.223501E-22, 1.206808E-24, 0.000000E+00 /
      DATA ( ECS_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112268E-21, 3.012610E-21, 1.483086E-21, 
     & 2.223501E-22, 1.206808E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999989E-01, 0.000000E+00 /
      DATA ( EQY_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999989E-01, 0.000000E+00 /
      DATA ( EQY_REF( INTR_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999989E-01, 0.000000E+00 /


C...PAN_IUPAC04
C.. CH3C(O)OONO2 + HV = CH3C(O)OO + NO2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet P21_CH3C(O)OONO2+hv, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IPAN_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.308075E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.308075E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.308075E-24, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.613498E-02, 0.000000E+00 /
      DATA ( QY_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.613498E-02, 0.000000E+00 /
      DATA ( QY_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.613498E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.308011E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.308011E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 3.530304E-21, 1.417995E-21, 6.732257E-22, 3.640895E-22, 
     & 9.144017E-23, 1.308011E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999513E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999513E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999513E-01, 0.000000E+00 /


C...PACD_CB05
C..  PACD   + HV = MEO2 + OH
C..  CB05 Photolysis data for PACD
C..  Supplied by Greg Yarwood, 11/16/2007
C..  Ref: Giguère, P. A. and A. W. Olmos. Sur le spectre ultraviolet de l'acide peracétique et l'hydrolyse des peracétates.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IPACD_CB05 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128915E-22, 2.430018E-22, 1.524875E-22, 
     & 2.678573E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128915E-22, 2.430018E-22, 1.524875E-22, 
     & 2.678573E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128915E-22, 2.430018E-22, 1.524875E-22, 
     & 2.678573E-23, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 3.584636E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 3.584636E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 3.584636E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128915E-22, 2.430018E-22, 1.524875E-22, 
     & 2.675543E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128915E-22, 2.430018E-22, 1.524875E-22, 
     & 2.675543E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128915E-22, 2.430018E-22, 1.524875E-22, 
     & 2.675543E-23, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.988688E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.988688E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPACD_CB05, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.988688E-01, 0.000000E+00, 0.000000E+00 /


C...MGLY_IUPAC04
C.. CH3COCHO + hv ---> CH3CO + HCO
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation; IUPAC Stern-Volmer expression
C..  Data Sheet P6_CH3COCHO+hv.pdf, updated 16th Jan, 2003
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IMGLY_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.380390E-20, 3.467067E-20, 2.430921E-20, 1.788231E-20, 
     & 6.183693E-21, 3.700272E-20, 7.900482E-21 /
      DATA ( CS_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.380390E-20, 3.467067E-20, 2.430921E-20, 1.788231E-20, 
     & 6.183693E-21, 3.700272E-20, 7.900482E-21 /
      DATA ( CS_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.380390E-20, 3.467067E-20, 2.430921E-20, 1.788231E-20, 
     & 6.183693E-21, 3.700272E-20, 7.900482E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.166999E-01, 2.503723E-03 /
      DATA ( QY_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.166999E-01, 2.503723E-03 /
      DATA ( QY_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.166999E-01, 2.503723E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.380390E-20, 3.467067E-20, 2.430921E-20, 1.788231E-20, 
     & 6.183693E-21, 8.170217E-21, 2.110402E-22 /
      DATA ( ECS_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.380390E-20, 3.467067E-20, 2.430921E-20, 1.788231E-20, 
     & 6.183693E-21, 8.170217E-21, 2.110402E-22 /
      DATA ( ECS_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 4.380390E-20, 3.467067E-20, 2.430921E-20, 1.788231E-20, 
     & 6.183693E-21, 8.170217E-21, 2.110402E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.208005E-01, 2.671232E-02 /
      DATA ( EQY_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.208005E-01, 2.671232E-02 /
      DATA ( EQY_REF( IMGLY_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.208005E-01, 2.671232E-02 /


C...CL2_IUPAC04
C..  CL2 + HV = 2*CL
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation; Data Sheet PCl11 Website: 15th December 2000
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk
C..  Assume these are point values (not specified in data source)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, ICL2_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.126823E-22 /
      DATA ( CS_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.126823E-22 /
      DATA ( CS_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.126823E-22 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /
      DATA ( QY_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /
      DATA ( QY_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.126375E-22 /
      DATA ( ECS_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.126375E-22 /
      DATA ( ECS_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841697E-19, 2.185163E-19, 
     & 2.410845E-19, 6.539245E-20, 6.126375E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999270E-01 /
      DATA ( EQY_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999270E-01 /
      DATA ( EQY_REF( ICL2_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999270E-01 /


C...HOCL_IUPAC04
C..  HOCL + HV = HO + CL
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation; Data Sheet PCl2 Website: 15th December 2000
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk
C..  Assume these are point values - not specified in data source

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IHOCL_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377943E-20, 
     & 3.120856E-20, 6.545257E-21, 1.172834E-23 /
      DATA ( CS_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377943E-20, 
     & 3.120856E-20, 6.545257E-21, 1.172834E-23 /
      DATA ( CS_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377943E-20, 
     & 3.120856E-20, 6.545257E-21, 1.172834E-23 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.319226E-02 /
      DATA ( QY_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.319226E-02 /
      DATA ( QY_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.319226E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377943E-20, 
     & 3.120856E-20, 6.545257E-21, 1.171487E-23 /
      DATA ( ECS_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377943E-20, 
     & 3.120856E-20, 6.545257E-21, 1.171487E-23 /
      DATA ( ECS_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.570116E-20, 6.066932E-20, 5.958157E-20, 5.377943E-20, 
     & 3.120856E-20, 6.545257E-21, 1.171487E-23 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.988515E-01 /
      DATA ( EQY_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.988515E-01 /
      DATA ( EQY_REF( IHOCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.988515E-01 /


C...FMCL_IUPAC04
C..  FMCL + hv -->   HCO + CL
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation; Data Sheet PCl28 Website: 15th December 2000
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk
C..  Reference: H. G. Libuda, F. Zabel, E. H. Fink, and K. H. Becker, J. Phys. Chem. 94, 5860 (1990)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITT, IFMCL_IUPAC04 ), ITT=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.320078E-21, 1.421314E-21, 2.242436E-22, 8.360540E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.320078E-21, 1.421314E-21, 2.242436E-22, 8.360540E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.320078E-21, 1.421314E-21, 2.242436E-22, 8.360540E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 7.811236E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 7.811236E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 7.811236E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.320078E-21, 1.421314E-21, 2.242436E-22, 8.349450E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.320078E-21, 1.421314E-21, 2.242436E-22, 8.349450E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 5.320078E-21, 1.421314E-21, 2.242436E-22, 8.349450E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 9.986735E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 9.986735E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IFMCL_IUPAC04, 1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 9.986735E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /


      END MODULE CSQY_DATA
