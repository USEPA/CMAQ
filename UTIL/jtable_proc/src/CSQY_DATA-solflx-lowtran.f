      MODULE CSQY_DATA

      IMPLICIT NONE

C.....PARAMETERS and their descriptions:

      INTEGER, PARAMETER :: NPHOT_REF =  84 ! # ref phot reactions 

      INTEGER, PARAMETER :: NTEMP_REF =   3 ! # ref temperatures 

      INTEGER, PARAMETER :: NWL_REF   =   7 ! # ref wavelengths 

C...Names of the mapped photolysis reactions (available to chemical)
C... mechanisms) and their pointers to the reference photolysis rxn


      INTEGER, PARAMETER :: INO2_06           =   1 ! pointer to NO2-06          
      INTEGER, PARAMETER :: INO3NO_06         =   2 ! pointer to NO3NO-06        
      INTEGER, PARAMETER :: INO3NO2_6         =   3 ! pointer to NO3NO2-6        
      INTEGER, PARAMETER :: IO3O1D_06         =   4 ! pointer to O3O1D-06        
      INTEGER, PARAMETER :: IO3O3P_06         =   5 ! pointer to O3O3P-06        
      INTEGER, PARAMETER :: IHONO_06          =   6 ! pointer to HONO-06         
      INTEGER, PARAMETER :: IHNO3             =   7 ! pointer to HNO3            
      INTEGER, PARAMETER :: IHNO4_06          =   8 ! pointer to HNO4-06         
      INTEGER, PARAMETER :: IH2O2             =   9 ! pointer to H2O2            
      INTEGER, PARAMETER :: INO2EX            =  10 ! pointer to NO2EX           
      INTEGER, PARAMETER :: IPAN              =  11 ! pointer to PAN             
      INTEGER, PARAMETER :: IHCHOR_06         =  12 ! pointer to HCHOR-06        
      INTEGER, PARAMETER :: IHCHOM_06         =  13 ! pointer to HCHOM-06        
      INTEGER, PARAMETER :: ICCHO_R           =  14 ! pointer to CCHO_R          
      INTEGER, PARAMETER :: IC2CHO            =  15 ! pointer to C2CHO           
      INTEGER, PARAMETER :: IACET_06          =  16 ! pointer to ACET-06         
      INTEGER, PARAMETER :: IMEK_06           =  17 ! pointer to MEK-06          
      INTEGER, PARAMETER :: ICOOH             =  18 ! pointer to COOH            
      INTEGER, PARAMETER :: IGLY_07R          =  19 ! pointer to GLY-07R         
      INTEGER, PARAMETER :: IGLY_07M          =  20 ! pointer to GLY-07M         
      INTEGER, PARAMETER :: IMGLY_06          =  21 ! pointer to MGLY-06         
      INTEGER, PARAMETER :: IBACL_07          =  22 ! pointer to BACL-07         
      INTEGER, PARAMETER :: IBALD_06          =  23 ! pointer to BALD-06         
      INTEGER, PARAMETER :: IAFG1             =  24 ! pointer to AFG1            
      INTEGER, PARAMETER :: IMACR_06          =  25 ! pointer to MACR-06         
      INTEGER, PARAMETER :: IMVK_06           =  26 ! pointer to MVK-06          
      INTEGER, PARAMETER :: IIC3ONO2          =  27 ! pointer to IC3ONO2         
      INTEGER, PARAMETER :: IHOCCHO_IUPAC     =  28 ! pointer to HOCCHO_IUPAC    
      INTEGER, PARAMETER :: IACRO_09          =  29 ! pointer to ACRO-09         
      INTEGER, PARAMETER :: IPAA              =  30 ! pointer to PAA             
      INTEGER, PARAMETER :: ICL2              =  31 ! pointer to CL2             
      INTEGER, PARAMETER :: ICLNO_06          =  32 ! pointer to CLNO-06         
      INTEGER, PARAMETER :: ICLONO            =  33 ! pointer to CLONO           
      INTEGER, PARAMETER :: ICLNO2            =  34 ! pointer to CLNO2           
      INTEGER, PARAMETER :: ICLONO2_1         =  35 ! pointer to CLONO2-1        
      INTEGER, PARAMETER :: ICLONO2_2         =  36 ! pointer to CLONO2-2        
      INTEGER, PARAMETER :: IHOCL_06          =  37 ! pointer to HOCL-06         
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
      INTEGER, PARAMETER :: INO2              =  78 ! pointer to NO2             
      INTEGER, PARAMETER :: IO3O1D            =  79 ! pointer to O3O1D           
      INTEGER, PARAMETER :: IO3O3P            =  80 ! pointer to O3O3P           
      INTEGER, PARAMETER :: IKETONE           =  81 ! pointer to KETONE          
      INTEGER, PARAMETER :: IMGLY_ABS         =  82 ! pointer to MGLY_ABS        
      INTEGER, PARAMETER :: IMGLY_ADJ         =  83 ! pointer to MGLY_ADJ        
      INTEGER, PARAMETER :: IACETONE          =  84 ! pointer to ACETONE         

      CHARACTER(16), SAVE :: PNAME_REF( NPHOT_REF )

      DATA PNAME_REF( INO2_06           ) / 'NO2-06          ' /
      DATA PNAME_REF( INO3NO_06         ) / 'NO3NO-06        ' /
      DATA PNAME_REF( INO3NO2_6         ) / 'NO3NO2-6        ' /
      DATA PNAME_REF( IO3O1D_06         ) / 'O3O1D-06        ' /
      DATA PNAME_REF( IO3O3P_06         ) / 'O3O3P-06        ' /
      DATA PNAME_REF( IHONO_06          ) / 'HONO-06         ' /
      DATA PNAME_REF( IHNO3             ) / 'HNO3            ' /
      DATA PNAME_REF( IHNO4_06          ) / 'HNO4-06         ' /
      DATA PNAME_REF( IH2O2             ) / 'H2O2            ' /
      DATA PNAME_REF( INO2EX            ) / 'NO2EX           ' /
      DATA PNAME_REF( IPAN              ) / 'PAN             ' /
      DATA PNAME_REF( IHCHOR_06         ) / 'HCHOR-06        ' /
      DATA PNAME_REF( IHCHOM_06         ) / 'HCHOM-06        ' /
      DATA PNAME_REF( ICCHO_R           ) / 'CCHO_R          ' /
      DATA PNAME_REF( IC2CHO            ) / 'C2CHO           ' /
      DATA PNAME_REF( IACET_06          ) / 'ACET-06         ' /
      DATA PNAME_REF( IMEK_06           ) / 'MEK-06          ' /
      DATA PNAME_REF( ICOOH             ) / 'COOH            ' /
      DATA PNAME_REF( IGLY_07R          ) / 'GLY-07R         ' /
      DATA PNAME_REF( IGLY_07M          ) / 'GLY-07M         ' /
      DATA PNAME_REF( IMGLY_06          ) / 'MGLY-06         ' /
      DATA PNAME_REF( IBACL_07          ) / 'BACL-07         ' /
      DATA PNAME_REF( IBALD_06          ) / 'BALD-06         ' /
      DATA PNAME_REF( IAFG1             ) / 'AFG1            ' /
      DATA PNAME_REF( IMACR_06          ) / 'MACR-06         ' /
      DATA PNAME_REF( IMVK_06           ) / 'MVK-06          ' /
      DATA PNAME_REF( IIC3ONO2          ) / 'IC3ONO2         ' /
      DATA PNAME_REF( IHOCCHO_IUPAC     ) / 'HOCCHO_IUPAC    ' /
      DATA PNAME_REF( IACRO_09          ) / 'ACRO-09         ' /
      DATA PNAME_REF( IPAA              ) / 'PAA             ' /
      DATA PNAME_REF( ICL2              ) / 'CL2             ' /
      DATA PNAME_REF( ICLNO_06          ) / 'CLNO-06         ' /
      DATA PNAME_REF( ICLONO            ) / 'CLONO           ' /
      DATA PNAME_REF( ICLNO2            ) / 'CLNO2           ' /
      DATA PNAME_REF( ICLONO2_1         ) / 'CLONO2-1        ' /
      DATA PNAME_REF( ICLONO2_2         ) / 'CLONO2-2        ' /
      DATA PNAME_REF( IHOCL_06          ) / 'HOCL-06         ' /
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
      DATA PNAME_REF( INO2              ) / 'NO2             ' /
      DATA PNAME_REF( IO3O1D            ) / 'O3O1D           ' /
      DATA PNAME_REF( IO3O3P            ) / 'O3O3P           ' /
      DATA PNAME_REF( IKETONE           ) / 'KETONE          ' /
      DATA PNAME_REF( IMGLY_ABS         ) / 'MGLY_ABS        ' /
      DATA PNAME_REF( IMGLY_ADJ         ) / 'MGLY_ADJ        ' /
      DATA PNAME_REF( IACETONE          ) / 'ACETONE         ' /

C...Setup the Mapping from CMAQ chemical reactions to the reference data

      INTEGER, PARAMETER :: NPHOT_MAP =  84 ! #  phot mapped reactions 

      CHARACTER(16), SAVE :: PNAME_MAP( NPHOT_MAP )
      INTEGER, SAVE       :: PHOT_MAP( NPHOT_MAP )

      DATA PNAME_MAP(   1 ),  PHOT_MAP(   1 )  / 'NO2-06          ', INO2_06           / 
      DATA PNAME_MAP(   2 ),  PHOT_MAP(   2 )  / 'NO3NO-06        ', INO3NO_06         / 
      DATA PNAME_MAP(   3 ),  PHOT_MAP(   3 )  / 'NO3NO2-6        ', INO3NO2_6         / 
      DATA PNAME_MAP(   4 ),  PHOT_MAP(   4 )  / 'O3O1D-06        ', IO3O1D_06         / 
      DATA PNAME_MAP(   5 ),  PHOT_MAP(   5 )  / 'O3O3P-06        ', IO3O3P_06         / 
      DATA PNAME_MAP(   6 ),  PHOT_MAP(   6 )  / 'HONO-06         ', IHONO_06          / 
      DATA PNAME_MAP(   7 ),  PHOT_MAP(   7 )  / 'HNO3            ', IHNO3             / 
      DATA PNAME_MAP(   8 ),  PHOT_MAP(   8 )  / 'HNO4-06         ', IHNO4_06          / 
      DATA PNAME_MAP(   9 ),  PHOT_MAP(   9 )  / 'H2O2            ', IH2O2             / 
      DATA PNAME_MAP(  10 ),  PHOT_MAP(  10 )  / 'NO2EX           ', INO2EX            / 
      DATA PNAME_MAP(  11 ),  PHOT_MAP(  11 )  / 'PAN             ', IPAN              / 
      DATA PNAME_MAP(  12 ),  PHOT_MAP(  12 )  / 'HCHOR-06        ', IHCHOR_06         / 
      DATA PNAME_MAP(  13 ),  PHOT_MAP(  13 )  / 'HCHOM-06        ', IHCHOM_06         / 
      DATA PNAME_MAP(  14 ),  PHOT_MAP(  14 )  / 'CCHO_R          ', ICCHO_R           / 
      DATA PNAME_MAP(  15 ),  PHOT_MAP(  15 )  / 'C2CHO           ', IC2CHO            / 
      DATA PNAME_MAP(  16 ),  PHOT_MAP(  16 )  / 'ACET-06         ', IACET_06          / 
      DATA PNAME_MAP(  17 ),  PHOT_MAP(  17 )  / 'MEK-06          ', IMEK_06           / 
      DATA PNAME_MAP(  18 ),  PHOT_MAP(  18 )  / 'COOH            ', ICOOH             / 
      DATA PNAME_MAP(  19 ),  PHOT_MAP(  19 )  / 'GLY-07R         ', IGLY_07R          / 
      DATA PNAME_MAP(  20 ),  PHOT_MAP(  20 )  / 'GLY-07M         ', IGLY_07M          / 
      DATA PNAME_MAP(  21 ),  PHOT_MAP(  21 )  / 'MGLY-06         ', IMGLY_06          / 
      DATA PNAME_MAP(  22 ),  PHOT_MAP(  22 )  / 'BACL-07         ', IBACL_07          / 
      DATA PNAME_MAP(  23 ),  PHOT_MAP(  23 )  / 'BALD-06         ', IBALD_06          / 
      DATA PNAME_MAP(  24 ),  PHOT_MAP(  24 )  / 'AFG1            ', IAFG1             / 
      DATA PNAME_MAP(  25 ),  PHOT_MAP(  25 )  / 'MACR-06         ', IMACR_06          / 
      DATA PNAME_MAP(  26 ),  PHOT_MAP(  26 )  / 'MVK-06          ', IMVK_06           / 
      DATA PNAME_MAP(  27 ),  PHOT_MAP(  27 )  / 'IC3ONO2         ', IIC3ONO2          / 
      DATA PNAME_MAP(  28 ),  PHOT_MAP(  28 )  / 'HOCCHO_IUPAC    ', IHOCCHO_IUPAC     / 
      DATA PNAME_MAP(  29 ),  PHOT_MAP(  29 )  / 'ACRO-09         ', IACRO_09          / 
      DATA PNAME_MAP(  30 ),  PHOT_MAP(  30 )  / 'PAA             ', IPAA              / 
      DATA PNAME_MAP(  31 ),  PHOT_MAP(  31 )  / 'CL2             ', ICL2              / 
      DATA PNAME_MAP(  32 ),  PHOT_MAP(  32 )  / 'CLNO-06         ', ICLNO_06          / 
      DATA PNAME_MAP(  33 ),  PHOT_MAP(  33 )  / 'CLONO           ', ICLONO            / 
      DATA PNAME_MAP(  34 ),  PHOT_MAP(  34 )  / 'CLNO2           ', ICLNO2            / 
      DATA PNAME_MAP(  35 ),  PHOT_MAP(  35 )  / 'CLONO2-1        ', ICLONO2_1         / 
      DATA PNAME_MAP(  36 ),  PHOT_MAP(  36 )  / 'CLONO2-2        ', ICLONO2_2         / 
      DATA PNAME_MAP(  37 ),  PHOT_MAP(  37 )  / 'HOCL-06         ', IHOCL_06          / 
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
      DATA PNAME_MAP(  78 ),  PHOT_MAP(  78 )  / 'NO2             ', INO2              / 
      DATA PNAME_MAP(  79 ),  PHOT_MAP(  79 )  / 'O3O1D           ', IO3O1D            / 
      DATA PNAME_MAP(  80 ),  PHOT_MAP(  80 )  / 'O3O3P           ', IO3O3P            / 
      DATA PNAME_MAP(  81 ),  PHOT_MAP(  81 )  / 'KETONE          ', IKETONE           / 
      DATA PNAME_MAP(  82 ),  PHOT_MAP(  82 )  / 'MGLY_ABS        ', IMGLY_ABS         / 
      DATA PNAME_MAP(  83 ),  PHOT_MAP(  83 )  / 'MGLY_ADJ        ', IMGLY_ADJ         / 
      DATA PNAME_MAP(  84 ),  PHOT_MAP(  84 )  / 'ACETONE         ', IACETONE          / 

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
      INTEGER  :: ITTR   ! temperature loop variable

C...NO2-06
C..  NO2 + HV = NO + O
C..  From NASA (2006).
C..  Absorption cross sections are averages for wavelength intervals given.
C..  Data alligned to smallest wavelength intervals for abs. coefs and qy's.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO2_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO2_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 5.707211E-19, 2.476857E-20 /
      DATA ( CS_REF( INO2_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 5.707211E-19, 2.476857E-20 /
      DATA ( CS_REF( INO2_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 5.707211E-19, 2.476857E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO2_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.354442E-01, 1.556473E-03 /
      DATA ( QY_REF(  INO2_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.354442E-01, 1.556473E-03 /
      DATA ( QY_REF(  INO2_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.354442E-01, 1.556473E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 4.705641E-19, 9.288722E-22 /
      DATA ( ECS_REF( INO2_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 4.705641E-19, 9.288722E-22 /
      DATA ( ECS_REF( INO2_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 4.705641E-19, 9.288722E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.245080E-01, 3.750205E-02 /
      DATA ( EQY_REF( INO2_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.245080E-01, 3.750205E-02 /
      DATA ( EQY_REF( INO2_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.245080E-01, 3.750205E-02 /


C...NO3NO-06
C..  NO3 + HV = NO + O2
C..  From NASA (2006), for 298 K.
C..  Quantum yields for wavelengths above 640 nm estimated by linear extrapolation.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO3NO_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.976742E-19 /
      DATA ( CS_REF( INO3NO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.976742E-19 /
      DATA ( CS_REF( INO3NO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.976742E-19 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO3NO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.239172E-02 /
      DATA ( QY_REF(  INO3NO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.239172E-02 /
      DATA ( QY_REF(  INO3NO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.239172E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 9.557256E-20 /
      DATA ( ECS_REF( INO3NO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 9.557256E-20 /
      DATA ( ECS_REF( INO3NO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 9.557256E-20 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.599074E-01 /
      DATA ( EQY_REF( INO3NO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.599074E-01 /
      DATA ( EQY_REF( INO3NO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.599074E-01 /


C...NO3NO2-6
C..  NO3 + HV = NO2 + O
C..  From NASA (2006), for 298 K.
C..  Quantum yields for wavelengths above 640 nm estimated by linear extrapolation.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO3NO2_6 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO2_6,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.643271E-21, 1.341307E-18 /
      DATA ( CS_REF( INO3NO2_6,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.643271E-21, 1.341307E-18 /
      DATA ( CS_REF( INO3NO2_6,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.643271E-21, 1.341307E-18 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO3NO2_6,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.904540E-01, 5.964973E-01 /
      DATA ( QY_REF(  INO3NO2_6,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.904540E-01, 5.964973E-01 /
      DATA ( QY_REF(  INO3NO2_6,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.904540E-01, 5.964973E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO2_6,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.637229E-21, 9.265006E-19 /
      DATA ( ECS_REF( INO3NO2_6,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.637229E-21, 9.265006E-19 /
      DATA ( ECS_REF( INO3NO2_6,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.637229E-21, 9.265006E-19 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO2_6,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 9.983416E-01, 6.907448E-01 /
      DATA ( EQY_REF( INO3NO2_6,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 9.983416E-01, 6.907448E-01 /
      DATA ( EQY_REF( INO3NO2_6,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 9.983416E-01, 6.907448E-01 /


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

      DATA ( TEMP_REF( ITTR, IO3O1D_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O1D_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.519092E-19, 2.756369E-19, 1.058103E-19, 4.660382E-20, 
     & 6.954922E-21, 6.413300E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.519092E-19, 2.756369E-19, 1.058103E-19, 4.660382E-20, 
     & 6.954922E-21, 6.413300E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.519092E-19, 2.756369E-19, 1.058103E-19, 4.660382E-20, 
     & 6.954922E-21, 6.413300E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O1D_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.926651E-01, 5.311664E-01, 2.227850E-01, 
     & 8.767726E-02, 4.585904E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.926651E-01, 5.311664E-01, 2.227850E-01, 
     & 8.767726E-02, 4.585904E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.926651E-01, 5.311664E-01, 2.227850E-01, 
     & 8.767726E-02, 4.585904E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O1D_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.667183E-19, 2.468925E-19, 5.994369E-20, 1.079729E-20, 
     & 7.086030E-22, 5.114812E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.667183E-19, 2.468925E-19, 5.994369E-20, 1.079729E-20, 
     & 7.086030E-22, 5.114812E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.667183E-19, 2.468925E-19, 5.994369E-20, 1.079729E-20, 
     & 7.086030E-22, 5.114812E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O1D_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.957165E-01, 5.665205E-01, 2.316824E-01, 
     & 1.018851E-01, 7.975321E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.957165E-01, 5.665205E-01, 2.316824E-01, 
     & 1.018851E-01, 7.975321E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.957165E-01, 5.665205E-01, 2.316824E-01, 
     & 1.018851E-01, 7.975321E-02, 0.000000E+00 /


C...O3O3P-06
C..  O3 + HV = O3P + O2
C..  Absorption cross sections from NASA (2006).
C..  Quantum yields are derived from the O1D quantum yields in the low wavelength re
C..  Unit quantum yields assumed in high wavelength region.
C..  Absorption cross sections below 829 nm are extrapolated
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IO3O3P_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O3P_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-19, 2.754982E-19, 1.058103E-19, 4.660382E-20, 
     & 7.174786E-21, 7.032893E-23, 1.701844E-21 /
      DATA ( CS_REF( IO3O3P_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-19, 2.754982E-19, 1.058103E-19, 4.660382E-20, 
     & 7.174786E-21, 7.032893E-23, 1.701844E-21 /
      DATA ( CS_REF( IO3O3P_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-19, 2.754982E-19, 1.058103E-19, 4.660382E-20, 
     & 7.174786E-21, 7.032893E-23, 1.701844E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O3P_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.073348E-01, 4.688335E-01, 7.772150E-01, 
     & 9.115356E-01, 9.541410E-01, 9.901482E-01 /
      DATA ( QY_REF(  IO3O3P_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.073348E-01, 4.688335E-01, 7.772150E-01, 
     & 9.115356E-01, 9.541410E-01, 9.901482E-01 /
      DATA ( QY_REF(  IO3O3P_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.073348E-01, 4.688335E-01, 7.772150E-01, 
     & 9.115356E-01, 9.541410E-01, 9.901482E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O3P_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-20, 2.873049E-20, 4.586660E-20, 3.580654E-20, 
     & 6.431878E-21, 6.521411E-23, 1.701844E-21 /
      DATA ( ECS_REF( IO3O3P_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-20, 2.873049E-20, 4.586660E-20, 3.580654E-20, 
     & 6.431878E-21, 6.521411E-23, 1.701844E-21 /
      DATA ( ECS_REF( IO3O3P_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-20, 2.873049E-20, 4.586660E-20, 3.580654E-20, 
     & 6.431878E-21, 6.521411E-23, 1.701844E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O3P_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.042856E-01, 4.334796E-01, 7.683176E-01, 
     & 8.964556E-01, 9.272730E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.042856E-01, 4.334796E-01, 7.683176E-01, 
     & 8.964556E-01, 9.272730E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.042856E-01, 4.334796E-01, 7.683176E-01, 
     & 8.964556E-01, 9.272730E-01, 1.000000E+00 /


C...HONO-06
C..  HONO + HV = HO. + NO
C..  NASA (2006) recommended absorption cross sections. No recommendation on quantum yields.
C..  IUPAC (2005) recommendation (Data Sheet PNOx1, July, 2001) recommends unit quantum yield
C..  for OH + NO.
C..  (IUPAC recommended absorption cross sections are low resolution, so NASA values are
C..   preferred)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHONO_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.474359E-21, 9.278287E-21, 1.890271E-20, 3.166180E-20, 
     & 8.854483E-20, 7.417380E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.474359E-21, 9.278287E-21, 1.890271E-20, 3.166180E-20, 
     & 8.854483E-20, 7.417380E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.474359E-21, 9.278287E-21, 1.890271E-20, 3.166180E-20, 
     & 8.854483E-20, 7.417380E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHONO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.070430E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.070430E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.070430E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.474359E-21, 9.278287E-21, 1.890271E-20, 3.166180E-20, 
     & 8.854483E-20, 7.417380E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.474359E-21, 9.278287E-21, 1.890271E-20, 3.166180E-20, 
     & 8.854483E-20, 7.417380E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.474359E-21, 9.278287E-21, 1.890271E-20, 3.166180E-20, 
     & 8.854483E-20, 7.417380E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /


C...HNO3
C..  HNO3 + HV = products
C..  IUPAC (1997) Recommendation

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHNO3 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO3,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637400E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637400E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637400E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHNO3,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO3,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637264E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637264E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637264E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO3,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999707E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999707E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999707E-01, 0.000000E+00 /


C...HNO4-06
C..  HO2NO2 + HV = PRODUCTS
C..  NASA (2006)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHNO4_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO4_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.680126E-20, 1.175613E-20, 5.689837E-21, 3.128481E-21, 
     & 8.548637E-22, 2.509431E-23, 0.000000E+00 /
      DATA ( CS_REF( IHNO4_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.680126E-20, 1.175613E-20, 5.689837E-21, 3.128481E-21, 
     & 8.548637E-22, 2.509431E-23, 0.000000E+00 /
      DATA ( CS_REF( IHNO4_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.680126E-20, 1.175613E-20, 5.689837E-21, 3.128481E-21, 
     & 8.548637E-22, 2.509431E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHNO4_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.964434E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO4_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.964434E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO4_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.964434E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO4_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.680126E-20, 1.175613E-20, 5.689837E-21, 3.128481E-21, 
     & 8.548637E-22, 2.509400E-23, 0.000000E+00 /
      DATA ( ECS_REF( IHNO4_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.680126E-20, 1.175613E-20, 5.689837E-21, 3.128481E-21, 
     & 8.548637E-22, 2.509400E-23, 0.000000E+00 /
      DATA ( ECS_REF( IHNO4_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.680126E-20, 1.175613E-20, 5.689837E-21, 3.128481E-21, 
     & 8.548637E-22, 2.509400E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO4_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999877E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO4_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999877E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO4_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999877E-01, 0.000000E+00 /


C...H2O2
C..  H2O2 + HV = 2 OH
C..  IUPAC (1997) Recommended.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IH2O2 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IH2O2,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309158E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309158E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309158E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IH2O2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /
      DATA ( QY_REF(  IH2O2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /
      DATA ( QY_REF(  IH2O2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IH2O2,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309022E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309022E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309022E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IH2O2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999685E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999685E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999685E-01, 0.000000E+00 /


C...NO2EX
C..  NO2 + HV = NO2*
C..  From NASA (2006), for 294 K.
C..  Absorption cross sections are averages for wavelength intervals given.
C..  Data alligned to smallest wavelength intervals for abs. coefs and qy's.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO2EX ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO2EX,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 5.707211E-19, 1.710791E-19 /
      DATA ( CS_REF( INO2EX,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 5.707211E-19, 1.710791E-19 /
      DATA ( CS_REF( INO2EX,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 5.707211E-19, 1.710791E-19 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO2EX,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.645558E-01, 7.353834E-01 /
      DATA ( QY_REF(  INO2EX,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.645558E-01, 7.353834E-01 /
      DATA ( QY_REF(  INO2EX,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.645558E-01, 7.353834E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2EX,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.001570E-19, 1.701502E-19 /
      DATA ( ECS_REF( INO2EX,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.001570E-19, 1.701502E-19 /
      DATA ( ECS_REF( INO2EX,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.001570E-19, 1.701502E-19 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2EX,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.754921E-01, 9.945703E-01 /
      DATA ( EQY_REF( INO2EX,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.754921E-01, 9.945703E-01 /
      DATA ( EQY_REF( INO2EX,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.754921E-01, 9.945703E-01 /


C...PAN
C..  PAN + HV = #.6 {MECO3 + NO2} + #.4 {MEO2 + CO2 + NO3}
C..  IUPAC Data Sheet P21.updated: 12/19/05
C..  Reaction reflects recommended quantum yields at 308 nm
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IPAN ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPAN,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.914260E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.914260E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.914260E-24, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IPAN,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.198098E-01, 0.000000E+00 /
      DATA ( QY_REF(  IPAN,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.198098E-01, 0.000000E+00 /
      DATA ( QY_REF(  IPAN,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.198098E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPAN,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.914185E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.914185E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.914185E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPAN,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999610E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999610E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999610E-01, 0.000000E+00 /


C...HCHOR-06
C..  HCHO + HV = HCO. + H.
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P1
C..  This datasheet updated: 16th May 2002.
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/.
C..  IUPAC recommendations used for both absorption cross sections and quantum yield
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHCHOR_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHOR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.165459E-20, 3.244126E-20, 1.504850E-20, 3.317451E-20, 
     & 1.553457E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHOR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.165459E-20, 3.244126E-20, 1.504850E-20, 3.317451E-20, 
     & 1.553457E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHOR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.165459E-20, 3.244126E-20, 1.504850E-20, 3.317451E-20, 
     & 1.553457E-20, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHCHOR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.849613E-01, 7.181136E-01, 7.157158E-01, 6.843758E-01, 
     & 2.950587E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHCHOR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.849613E-01, 7.181136E-01, 7.157158E-01, 6.843758E-01, 
     & 2.950587E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHCHOR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.849613E-01, 7.181136E-01, 7.157158E-01, 6.843758E-01, 
     & 2.950587E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHOR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.161663E-20, 2.354888E-20, 1.083089E-20, 2.277132E-20, 
     & 5.819424E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.161663E-20, 2.354888E-20, 1.083089E-20, 2.277132E-20, 
     & 5.819424E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.161663E-20, 2.354888E-20, 1.083089E-20, 2.277132E-20, 
     & 5.819424E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHOR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.828910E-01, 7.258928E-01, 7.197325E-01, 6.864101E-01, 
     & 3.746113E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.828910E-01, 7.258928E-01, 7.197325E-01, 6.864101E-01, 
     & 3.746113E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.828910E-01, 7.258928E-01, 7.197325E-01, 6.864101E-01, 
     & 3.746113E-01, 0.000000E+00, 0.000000E+00 /


C...HCHOM-06
C..  HCHO + HV = H2 + CO
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P1
C..  This datasheet updated: 16th May 2002.
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/.
C..  IUPAC recommendations used for both absorption cross sections and quantum yield
C..  Quantum yields for wl < 0.360 interpolated.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHCHOM_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHOM_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.165459E-20, 3.244126E-20, 1.504850E-20, 3.317451E-20, 
     & 1.826654E-20, 8.630682E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHOM_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.165459E-20, 3.244126E-20, 1.504850E-20, 3.317451E-20, 
     & 1.826654E-20, 8.630682E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHOM_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.165459E-20, 3.244126E-20, 1.504850E-20, 3.317451E-20, 
     & 1.826654E-20, 8.630682E-22, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHCHOM_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.957663E-01, 2.805459E-01, 2.844469E-01, 3.161261E-01, 
     & 4.858848E-01, 3.593407E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHCHOM_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.957663E-01, 2.805459E-01, 2.844469E-01, 3.161261E-01, 
     & 4.858848E-01, 3.593407E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHCHOM_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.957663E-01, 2.805459E-01, 2.844469E-01, 3.161261E-01, 
     & 4.858848E-01, 3.593407E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHOM_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.454901E-21, 8.857910E-21, 4.220348E-21, 1.041737E-20, 
     & 9.005958E-21, 1.384442E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOM_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.454901E-21, 8.857910E-21, 4.220348E-21, 1.041737E-20, 
     & 9.005958E-21, 1.384442E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOM_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.454901E-21, 8.857910E-21, 4.220348E-21, 1.041737E-20, 
     & 9.005958E-21, 1.384442E-22, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHOM_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.986898E-01, 2.730446E-01, 2.804497E-01, 3.140172E-01, 
     & 4.930303E-01, 1.604093E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOM_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.986898E-01, 2.730446E-01, 2.804497E-01, 3.140172E-01, 
     & 4.930303E-01, 1.604093E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOM_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.986898E-01, 2.730446E-01, 2.804497E-01, 3.140172E-01, 
     & 4.930303E-01, 1.604093E-01, 0.000000E+00 /


C...CCHO_R
C..  CCHO + HV = CH3 + CHO
C..  IUPAC (1997)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICCHO_R ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICCHO_R,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.431909E-20, 3.721389E-20, 2.926475E-20, 2.113278E-20, 
     & 4.007796E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.431909E-20, 3.721389E-20, 2.926475E-20, 2.113278E-20, 
     & 4.007796E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.431909E-20, 3.721389E-20, 2.926475E-20, 2.113278E-20, 
     & 4.007796E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICCHO_R,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.837319E-01, 3.903790E-01, 2.837039E-01, 1.548971E-01, 
     & 1.542570E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  ICCHO_R,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.837319E-01, 3.903790E-01, 2.837039E-01, 1.548971E-01, 
     & 1.542570E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  ICCHO_R,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.837319E-01, 3.903790E-01, 2.837039E-01, 1.548971E-01, 
     & 1.542570E-02, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICCHO_R,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.146492E-20, 1.463071E-20, 8.383887E-21, 3.363618E-21, 
     & 1.911978E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.146492E-20, 1.463071E-20, 8.383887E-21, 3.363618E-21, 
     & 1.911978E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.146492E-20, 1.463071E-20, 8.383887E-21, 3.363618E-21, 
     & 1.911978E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICCHO_R,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.843267E-01, 3.931520E-01, 2.864842E-01, 1.591659E-01, 
     & 4.770647E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.843267E-01, 3.931520E-01, 2.864842E-01, 1.591659E-01, 
     & 4.770647E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.843267E-01, 3.931520E-01, 2.864842E-01, 1.591659E-01, 
     & 4.770647E-02, 0.000000E+00, 0.000000E+00 /


C...C2CHO
C..  C2CHO + HV = C2H5. + CHO.
C..  IUPAC (1997)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IC2CHO ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IC2CHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.167935E-20, 4.637920E-20, 3.576927E-20, 2.454658E-20, 
     & 6.109313E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.167935E-20, 4.637920E-20, 3.576927E-20, 2.454658E-20, 
     & 6.109313E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.167935E-20, 4.637920E-20, 3.576927E-20, 2.454658E-20, 
     & 6.109313E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IC2CHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.201593E-01, 7.958030E-01, 5.938827E-01, 4.325821E-01, 
     & 1.575024E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IC2CHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.201593E-01, 7.958030E-01, 5.938827E-01, 4.325821E-01, 
     & 1.575024E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IC2CHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.201593E-01, 7.958030E-01, 5.938827E-01, 4.325821E-01, 
     & 1.575024E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IC2CHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.779956E-20, 3.718655E-20, 2.127446E-20, 1.085955E-20, 
     & 1.479962E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.779956E-20, 3.718655E-20, 2.127446E-20, 1.085955E-20, 
     & 1.479962E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.779956E-20, 3.718655E-20, 2.127446E-20, 1.085955E-20, 
     & 1.479962E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IC2CHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.775294E-01, 8.017938E-01, 5.947691E-01, 4.424057E-01, 
     & 2.422470E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.775294E-01, 8.017938E-01, 5.947691E-01, 4.424057E-01, 
     & 2.422470E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.775294E-01, 8.017938E-01, 5.947691E-01, 4.424057E-01, 
     & 2.422470E-01, 0.000000E+00, 0.000000E+00 /


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

      DATA ( TEMP_REF( ITTR, IACET_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACET_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.460821E-20, 2.278628E-20, 1.362906E-20, 7.428147E-21, 
     & 9.464437E-22, 9.957367E-25, 0.000000E+00 /
      DATA ( CS_REF( IACET_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.460821E-20, 2.278628E-20, 1.362906E-20, 7.428147E-21, 
     & 9.464437E-22, 9.957367E-25, 0.000000E+00 /
      DATA ( CS_REF( IACET_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.460821E-20, 2.278628E-20, 1.362906E-20, 7.428147E-21, 
     & 9.464437E-22, 9.957367E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACET_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.454557E-01, 2.098742E-01, 8.525195E-02, 3.909341E-02, 
     & 9.695551E-03, 1.622317E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACET_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.454557E-01, 2.098742E-01, 8.525195E-02, 3.909341E-02, 
     & 9.695551E-03, 1.622317E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACET_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.454557E-01, 2.098742E-01, 8.525195E-02, 3.909341E-02, 
     & 9.695551E-03, 1.622317E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACET_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.205182E-20, 4.966170E-21, 1.192478E-21, 3.105726E-22, 
     & 1.545213E-23, 2.812016E-27, 0.000000E+00 /
      DATA ( ECS_REF( IACET_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.205182E-20, 4.966170E-21, 1.192478E-21, 3.105726E-22, 
     & 1.545213E-23, 2.812016E-27, 0.000000E+00 /
      DATA ( ECS_REF( IACET_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.205182E-20, 4.966170E-21, 1.192478E-21, 3.105726E-22, 
     & 1.545213E-23, 2.812016E-27, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACET_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.482358E-01, 2.179456E-01, 8.749527E-02, 4.181024E-02, 
     & 1.632652E-02, 2.824056E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACET_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.482358E-01, 2.179456E-01, 8.749527E-02, 4.181024E-02, 
     & 1.632652E-02, 2.824056E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACET_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.482358E-01, 2.179456E-01, 8.749527E-02, 4.181024E-02, 
     & 1.632652E-02, 2.824056E-03, 0.000000E+00 /


C...MEK-06
C..  MEK absorption cross sections
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P8
C..  This datasheet updated: 5th December 2005.
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/.
C..  Note that recommended quantum yield is 0.34 and SAPRC07T sets
C..  value to 0.175 in mechanism definition file
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMEK_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMEK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.986233E-25, 0.000000E+00 /
      DATA ( CS_REF( IMEK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.986233E-25, 0.000000E+00 /
      DATA ( CS_REF( IMEK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.986233E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMEK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.689541E-02, 0.000000E+00 /
      DATA ( QY_REF(  IMEK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.689541E-02, 0.000000E+00 /
      DATA ( QY_REF(  IMEK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.689541E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMEK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.984605E-25, 0.000000E+00 /
      DATA ( ECS_REF( IMEK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.984605E-25, 0.000000E+00 /
      DATA ( ECS_REF( IMEK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.984605E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMEK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.991803E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMEK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.991803E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMEK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.991803E-01, 0.000000E+00 /


C...COOH
C..  CH3OOH + HV = PRODUCTS
C..  IUPAC (1997).  Also recommend unit quantum yields.
C..  wl      abs        qy

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICOOH ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICOOH,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232370E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232370E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232370E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICOOH,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.407560E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICOOH,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.407560E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICOOH,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.407560E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICOOH,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232326E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232326E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232326E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICOOH,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999929E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999929E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999929E-01, 0.000000E+00 /


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

      DATA ( TEMP_REF( ITTR, IGLY_07R ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY_07R,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.793907E-20, 3.576577E-20, 3.196227E-20, 2.588874E-20, 
     & 1.131968E-20, 2.589663E-20, 1.938750E-20 /
      DATA ( CS_REF( IGLY_07R,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.793907E-20, 3.576577E-20, 3.196227E-20, 2.588874E-20, 
     & 1.131968E-20, 2.589663E-20, 1.938750E-20 /
      DATA ( CS_REF( IGLY_07R,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.793907E-20, 3.576577E-20, 3.196227E-20, 2.588874E-20, 
     & 1.131968E-20, 2.589663E-20, 1.938750E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IGLY_07R,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.321870E-01, 3.435464E-01, 3.693566E-01, 4.280283E-01, 
     & 5.810962E-01, 2.622190E-01, 2.601158E-03 /
      DATA ( QY_REF(  IGLY_07R,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.321870E-01, 3.435464E-01, 3.693566E-01, 4.280283E-01, 
     & 5.810962E-01, 2.622190E-01, 2.601158E-03 /
      DATA ( QY_REF(  IGLY_07R,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.321870E-01, 3.435464E-01, 3.693566E-01, 4.280283E-01, 
     & 5.810962E-01, 2.622190E-01, 2.601158E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_07R,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.260643E-20, 1.227545E-20, 1.181427E-20, 1.097191E-20, 
     & 6.261264E-21, 3.119572E-21, 2.621976E-22 /
      DATA ( ECS_REF( IGLY_07R,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.260643E-20, 1.227545E-20, 1.181427E-20, 1.097191E-20, 
     & 6.261264E-21, 3.119572E-21, 2.621976E-22 /
      DATA ( ECS_REF( IGLY_07R,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.260643E-20, 1.227545E-20, 1.181427E-20, 1.097191E-20, 
     & 6.261264E-21, 3.119572E-21, 2.621976E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_07R,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.322810E-01, 3.432177E-01, 3.696319E-01, 4.238100E-01, 
     & 5.531310E-01, 1.204625E-01, 1.352405E-02 /
      DATA ( EQY_REF( IGLY_07R,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.322810E-01, 3.432177E-01, 3.696319E-01, 4.238100E-01, 
     & 5.531310E-01, 1.204625E-01, 1.352405E-02 /
      DATA ( EQY_REF( IGLY_07R,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.322810E-01, 3.432177E-01, 3.696319E-01, 4.238100E-01, 
     & 5.531310E-01, 1.204625E-01, 1.352405E-02 /


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

      DATA ( TEMP_REF( ITTR, IGLY_07M ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY_07M,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.793907E-20, 3.576577E-20, 3.196227E-20, 2.588874E-20, 
     & 1.131968E-20, 2.589663E-20, 7.146744E-21 /
      DATA ( CS_REF( IGLY_07M,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.793907E-20, 3.576577E-20, 3.196227E-20, 2.588874E-20, 
     & 1.131968E-20, 2.589663E-20, 7.146744E-21 /
      DATA ( CS_REF( IGLY_07M,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.793907E-20, 3.576577E-20, 3.196227E-20, 2.588874E-20, 
     & 1.131968E-20, 2.589663E-20, 7.146744E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IGLY_07M,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.678130E-01, 6.564536E-01, 6.306653E-01, 5.720885E-01, 
     & 4.189721E-01, 6.740002E-02, 1.018117E-04 /
      DATA ( QY_REF(  IGLY_07M,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.678130E-01, 6.564536E-01, 6.306653E-01, 5.720885E-01, 
     & 4.189721E-01, 6.740002E-02, 1.018117E-04 /
      DATA ( QY_REF(  IGLY_07M,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.678130E-01, 6.564536E-01, 6.306653E-01, 5.720885E-01, 
     & 4.189721E-01, 6.740002E-02, 1.018117E-04 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_07M,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.533264E-20, 2.349032E-20, 2.014873E-20, 1.492075E-20, 
     & 5.059183E-21, 5.596750E-22, 9.269946E-24 /
      DATA ( ECS_REF( IGLY_07M,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.533264E-20, 2.349032E-20, 2.014873E-20, 1.492075E-20, 
     & 5.059183E-21, 5.596750E-22, 9.269946E-24 /
      DATA ( ECS_REF( IGLY_07M,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.533264E-20, 2.349032E-20, 2.014873E-20, 1.492075E-20, 
     & 5.059183E-21, 5.596750E-22, 9.269946E-24 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_07M,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.677190E-01, 6.567823E-01, 6.303911E-01, 5.763413E-01, 
     & 4.469371E-01, 2.161189E-02, 1.297087E-03 /
      DATA ( EQY_REF( IGLY_07M,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.677190E-01, 6.567823E-01, 6.303911E-01, 5.763413E-01, 
     & 4.469371E-01, 2.161189E-02, 1.297087E-03 /
      DATA ( EQY_REF( IGLY_07M,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.677190E-01, 6.567823E-01, 6.303911E-01, 5.763413E-01, 
     & 4.469371E-01, 2.161189E-02, 1.297087E-03 /


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

      DATA ( TEMP_REF( ITTR, IMGLY_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.377988E-20, 3.467536E-20, 2.425079E-20, 1.793719E-20, 
     & 6.357002E-21, 3.467224E-20, 1.502724E-20 /
      DATA ( CS_REF( IMGLY_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.377988E-20, 3.467536E-20, 2.425079E-20, 1.793719E-20, 
     & 6.357002E-21, 3.467224E-20, 1.502724E-20 /
      DATA ( CS_REF( IMGLY_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.377988E-20, 3.467536E-20, 2.425079E-20, 1.793719E-20, 
     & 6.357002E-21, 3.467224E-20, 1.502724E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.564515E-01, 9.284549E-01, 8.960506E-01, 8.568918E-01, 
     & 7.103677E-01, 2.552682E-01, 7.488430E-03 /
      DATA ( QY_REF(  IMGLY_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.564515E-01, 9.284549E-01, 8.960506E-01, 8.568918E-01, 
     & 7.103677E-01, 2.552682E-01, 7.488430E-03 /
      DATA ( QY_REF(  IMGLY_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.564515E-01, 9.284549E-01, 8.960506E-01, 8.568918E-01, 
     & 7.103677E-01, 2.552682E-01, 7.488430E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.188608E-20, 3.222329E-20, 2.174746E-20, 1.539860E-20, 
     & 4.724494E-21, 5.167266E-21, 6.224983E-22 /
      DATA ( ECS_REF( IMGLY_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.188608E-20, 3.222329E-20, 2.174746E-20, 1.539860E-20, 
     & 4.724494E-21, 5.167266E-21, 6.224983E-22 /
      DATA ( ECS_REF( IMGLY_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.188608E-20, 3.222329E-20, 2.174746E-20, 1.539860E-20, 
     & 4.724494E-21, 5.167266E-21, 6.224983E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.567426E-01, 9.292849E-01, 8.967732E-01, 8.584732E-01, 
     & 7.431952E-01, 1.490318E-01, 4.142465E-02 /
      DATA ( EQY_REF( IMGLY_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.567426E-01, 9.292849E-01, 8.967732E-01, 8.584732E-01, 
     & 7.431952E-01, 1.490318E-01, 4.142465E-02 /
      DATA ( EQY_REF( IMGLY_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.567426E-01, 9.292849E-01, 8.967732E-01, 8.584732E-01, 
     & 7.431952E-01, 1.490318E-01, 4.142465E-02 /


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

      DATA ( TEMP_REF( ITTR, IBACL_07 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBACL_07,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.654940E-20, 1.614309E-20, 9.222299E-21, 6.084959E-21, 
     & 4.646638E-21, 3.039083E-20, 8.999122E-21 /
      DATA ( CS_REF( IBACL_07,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.654940E-20, 1.614309E-20, 9.222299E-21, 6.084959E-21, 
     & 4.646638E-21, 3.039083E-20, 8.999122E-21 /
      DATA ( CS_REF( IBACL_07,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.654940E-20, 1.614309E-20, 9.222299E-21, 6.084959E-21, 
     & 4.646638E-21, 3.039083E-20, 8.999122E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IBACL_07,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.894255E-01, 9.817211E-01, 9.728009E-01, 9.610599E-01, 
     & 9.081057E-01, 5.113288E-01, 1.829104E-02 /
      DATA ( QY_REF(  IBACL_07,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.894255E-01, 9.817211E-01, 9.728009E-01, 9.610599E-01, 
     & 9.081057E-01, 5.113288E-01, 1.829104E-02 /
      DATA ( QY_REF(  IBACL_07,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.894255E-01, 9.817211E-01, 9.728009E-01, 9.610599E-01, 
     & 9.081057E-01, 5.113288E-01, 1.829104E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBACL_07,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.627346E-20, 1.585460E-20, 8.974108E-21, 5.850922E-21, 
     & 4.205010E-21, 1.163110E-20, 9.600788E-22 /
      DATA ( ECS_REF( IBACL_07,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.627346E-20, 1.585460E-20, 8.974108E-21, 5.850922E-21, 
     & 4.205010E-21, 1.163110E-20, 9.600788E-22 /
      DATA ( ECS_REF( IBACL_07,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.627346E-20, 1.585460E-20, 8.974108E-21, 5.850922E-21, 
     & 4.205010E-21, 1.163110E-20, 9.600788E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBACL_07,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.896064E-01, 9.821288E-01, 9.730880E-01, 9.615384E-01, 
     & 9.049575E-01, 3.827173E-01, 1.066858E-01 /
      DATA ( EQY_REF( IBACL_07,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.896064E-01, 9.821288E-01, 9.730880E-01, 9.615384E-01, 
     & 9.049575E-01, 3.827173E-01, 1.066858E-01 /
      DATA ( EQY_REF( IBACL_07,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.896064E-01, 9.821288E-01, 9.730880E-01, 9.615384E-01, 
     & 9.049575E-01, 3.827173E-01, 1.066858E-01 /


C...BALD-06
C..  Benzaldehyde absorption cross sections recommended by Calvert et al (2002)
C..  From Lang (1961-1971) in hexane solution
C..
C..  Note that recommended quantum ranges from 0,14 to 0,4 and SAPRC07T sets
C..  value to 0.060 in mechanism definition file

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IBALD_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBALD_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.446840E-19, 7.294727E-20, 7.083062E-20, 8.588684E-20, 
     & 9.205959E-20, 1.899708E-20, 0.000000E+00 /
      DATA ( CS_REF( IBALD_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.446840E-19, 7.294727E-20, 7.083062E-20, 8.588684E-20, 
     & 9.205959E-20, 1.899708E-20, 0.000000E+00 /
      DATA ( CS_REF( IBALD_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.446840E-19, 7.294727E-20, 7.083062E-20, 8.588684E-20, 
     & 9.205959E-20, 1.899708E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IBALD_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.698323E-01, 0.000000E+00 /
      DATA ( QY_REF(  IBALD_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.698323E-01, 0.000000E+00 /
      DATA ( QY_REF(  IBALD_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.698323E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBALD_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.446840E-19, 7.294727E-20, 7.083062E-20, 8.588684E-20, 
     & 9.205959E-20, 1.899707E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBALD_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.446840E-19, 7.294727E-20, 7.083062E-20, 8.588684E-20, 
     & 9.205959E-20, 1.899707E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBALD_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.446840E-19, 7.294727E-20, 7.083062E-20, 8.588684E-20, 
     & 9.205959E-20, 1.899707E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBALD_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBALD_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBALD_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /


C...AFG1
C..  Photolysis of lumped photoreactive aromatic unsaturated dicarbonyl product.
C..  Based on 31.6% BUTEDIAL and 68.4% 4OX2PEAL with QY(BUTEDIAL) = 0.723 and
C..  QY(4OX2PEAL) = 1.000
C..  From h:\mech\saprc06\phf.xls, "AFG1" sheet and based on
C..  h:\mech\saprc06\aroprods.xls
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IAFG1 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IAFG1,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.731169E-18, 1.413427E-18, 1.105655E-18, 8.934788E-19, 
     & 5.096681E-19, 1.346181E-19, 2.441063E-21 /
      DATA ( CS_REF( IAFG1,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.731169E-18, 1.413427E-18, 1.105655E-18, 8.934788E-19, 
     & 5.096681E-19, 1.346181E-19, 2.441063E-21 /
      DATA ( CS_REF( IAFG1,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.731169E-18, 1.413427E-18, 1.105655E-18, 8.934788E-19, 
     & 5.096681E-19, 1.346181E-19, 2.441063E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IAFG1,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.073897E-01 /
      DATA ( QY_REF(  IAFG1,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.073897E-01 /
      DATA ( QY_REF(  IAFG1,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.073897E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IAFG1,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.731169E-18, 1.413427E-18, 1.105655E-18, 8.934788E-19, 
     & 5.096681E-19, 1.346181E-19, 2.441062E-21 /
      DATA ( ECS_REF( IAFG1,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.731169E-18, 1.413427E-18, 1.105655E-18, 8.934788E-19, 
     & 5.096681E-19, 1.346181E-19, 2.441062E-21 /
      DATA ( ECS_REF( IAFG1,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.731169E-18, 1.413427E-18, 1.105655E-18, 8.934788E-19, 
     & 5.096681E-19, 1.346181E-19, 2.441062E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IAFG1,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999996E-01 /
      DATA ( EQY_REF( IAFG1,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999996E-01 /
      DATA ( EQY_REF( IAFG1,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999996E-01 /


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

      DATA ( TEMP_REF( ITTR, IMACR_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMACR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.642473E-20, 3.933871E-20, 4.996084E-20, 5.919546E-20, 
     & 6.529635E-20, 1.485992E-20, 0.000000E+00 /
      DATA ( CS_REF( IMACR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.642473E-20, 3.933871E-20, 4.996084E-20, 5.919546E-20, 
     & 6.529635E-20, 1.485992E-20, 0.000000E+00 /
      DATA ( CS_REF( IMACR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.642473E-20, 3.933871E-20, 4.996084E-20, 5.919546E-20, 
     & 6.529635E-20, 1.485992E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMACR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.895972E-02, 3.077034E-02, 2.097494E-02, 1.484818E-02, 
     & 6.388199E-03, 6.587669E-04, 0.000000E+00 /
      DATA ( QY_REF(  IMACR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.895972E-02, 3.077034E-02, 2.097494E-02, 1.484818E-02, 
     & 6.388199E-03, 6.587669E-04, 0.000000E+00 /
      DATA ( QY_REF(  IMACR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.895972E-02, 3.077034E-02, 2.097494E-02, 1.484818E-02, 
     & 6.388199E-03, 6.587669E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMACR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.276483E-21, 1.192719E-21, 1.044423E-21, 8.732378E-22, 
     & 4.189941E-22, 2.383998E-23, 0.000000E+00 /
      DATA ( ECS_REF( IMACR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.276483E-21, 1.192719E-21, 1.044423E-21, 8.732378E-22, 
     & 4.189941E-22, 2.383998E-23, 0.000000E+00 /
      DATA ( ECS_REF( IMACR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.276483E-21, 1.192719E-21, 1.044423E-21, 8.732378E-22, 
     & 4.189941E-22, 2.383998E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMACR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.830637E-02, 3.031922E-02, 2.090483E-02, 1.475177E-02, 
     & 6.416808E-03, 1.604314E-03, 0.000000E+00 /
      DATA ( EQY_REF( IMACR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.830637E-02, 3.031922E-02, 2.090483E-02, 1.475177E-02, 
     & 6.416808E-03, 1.604314E-03, 0.000000E+00 /
      DATA ( EQY_REF( IMACR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.830637E-02, 3.031922E-02, 2.090483E-02, 1.475177E-02, 
     & 6.416808E-03, 1.604314E-03, 0.000000E+00 /


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

      DATA ( TEMP_REF( ITTR, IMVK_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMVK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.044929E-20, 4.370248E-20, 5.442438E-20, 6.311225E-20, 
     & 6.837229E-20, 9.456476E-21, 0.000000E+00 /
      DATA ( CS_REF( IMVK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.044929E-20, 4.370248E-20, 5.442438E-20, 6.311225E-20, 
     & 6.837229E-20, 9.456476E-21, 0.000000E+00 /
      DATA ( CS_REF( IMVK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.044929E-20, 4.370248E-20, 5.442438E-20, 6.311225E-20, 
     & 6.837229E-20, 9.456476E-21, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMVK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.748657E-02, 1.113743E-02, 7.554945E-03, 5.308024E-03, 
     & 2.286832E-03, 1.891282E-04, 0.000000E+00 /
      DATA ( QY_REF(  IMVK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.748657E-02, 1.113743E-02, 7.554945E-03, 5.308024E-03, 
     & 2.286832E-03, 1.891282E-04, 0.000000E+00 /
      DATA ( QY_REF(  IMVK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.748657E-02, 1.113743E-02, 7.554945E-03, 5.308024E-03, 
     & 2.286832E-03, 1.891282E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMVK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.264739E-22, 4.799790E-22, 4.100841E-22, 3.333177E-22, 
     & 1.581298E-22, 9.181583E-24, 0.000000E+00 /
      DATA ( ECS_REF( IMVK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.264739E-22, 4.799790E-22, 4.100841E-22, 3.333177E-22, 
     & 1.581298E-22, 9.181583E-24, 0.000000E+00 /
      DATA ( ECS_REF( IMVK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.264739E-22, 4.799790E-22, 4.100841E-22, 3.333177E-22, 
     & 1.581298E-22, 9.181583E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMVK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.729018E-02, 1.098288E-02, 7.534933E-03, 5.281347E-03, 
     & 2.312777E-03, 9.709307E-04, 0.000000E+00 /
      DATA ( EQY_REF( IMVK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.729018E-02, 1.098288E-02, 7.534933E-03, 5.281347E-03, 
     & 2.312777E-03, 9.709307E-04, 0.000000E+00 /
      DATA ( EQY_REF( IMVK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.729018E-02, 1.098288E-02, 7.534933E-03, 5.281347E-03, 
     & 2.312777E-03, 9.709307E-04, 0.000000E+00 /


C...IC3ONO2
C..  ! I-C3H7ONO2 + HV = PRODUCTS
C..  IUPAC (1997).  Recommend assuming unit quantum yields.
C..  This has stronger absorption than n-C3-ONO2 and lower nitrates,
C..  but chosen as representative of lumped higher nitrates.
C..  wl       abs

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IIC3ONO2 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IIC3ONO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837941E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837941E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837941E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IIC3ONO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IIC3ONO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IIC3ONO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IIC3ONO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837921E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837921E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837921E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IIC3ONO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999931E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999931E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999931E-01, 0.000000E+00, 0.000000E+00 /


C...HOCCHO_IUPAC
C..  HOCH2CHO + hv ---> products		
C..  IUPAC (2002) based on Bacher et al (2001), J. Atm. Chem, 39, 171.
C..  quantum yield = 0.75+/-0.25		
C..  lambda(nm) xcross(1.E+20*cm2) yield	

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHOCCHO_IUPAC ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHOCCHO_IUPAC,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.701644E-20, 2.281197E-20, 1.252918E-20, 5.900787E-21, 
     & 6.962136E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHOCCHO_IUPAC,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.701644E-20, 2.281197E-20, 1.252918E-20, 5.900787E-21, 
     & 6.962136E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHOCCHO_IUPAC,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.701644E-20, 2.281197E-20, 1.252918E-20, 5.900787E-21, 
     & 6.962136E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHOCCHO_IUPAC,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 4.462717E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHOCCHO_IUPAC,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 4.462717E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHOCCHO_IUPAC,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 4.462717E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHOCCHO_IUPAC,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.776233E-20, 1.710898E-20, 9.396888E-21, 4.425590E-21, 
     & 5.218385E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHOCCHO_IUPAC,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.776233E-20, 1.710898E-20, 9.396888E-21, 4.425590E-21, 
     & 5.218385E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHOCCHO_IUPAC,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.776233E-20, 1.710898E-20, 9.396888E-21, 4.425590E-21, 
     & 5.218385E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHOCCHO_IUPAC,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 7.495379E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHOCCHO_IUPAC,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 7.495379E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHOCCHO_IUPAC,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 7.495379E-01, 0.000000E+00, 0.000000E+00 /


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

      DATA ( TEMP_REF( ITTR, IACRO_09 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACRO_09,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.858116E-20, 2.786563E-20, 3.608658E-20, 4.315211E-20, 
     & 5.365798E-20, 1.717206E-20, 0.000000E+00 /
      DATA ( CS_REF( IACRO_09,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.858116E-20, 2.786563E-20, 3.608658E-20, 4.315211E-20, 
     & 5.365798E-20, 1.717206E-20, 0.000000E+00 /
      DATA ( CS_REF( IACRO_09,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.858116E-20, 2.786563E-20, 3.608658E-20, 4.315211E-20, 
     & 5.365798E-20, 1.717206E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACRO_09,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.327035E-02, 3.344404E-02, 2.270661E-02, 1.610625E-02, 
     & 6.961018E-03, 7.441319E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACRO_09,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.327035E-02, 3.344404E-02, 2.270661E-02, 1.610625E-02, 
     & 6.961018E-03, 7.441319E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACRO_09,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.327035E-02, 3.344404E-02, 2.270661E-02, 1.610625E-02, 
     & 6.961018E-03, 7.441319E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACRO_09,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.778219E-22, 9.186790E-22, 8.152126E-22, 6.910216E-22, 
     & 3.706286E-22, 2.881135E-23, 0.000000E+00 /
      DATA ( ECS_REF( IACRO_09,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.778219E-22, 9.186790E-22, 8.152126E-22, 6.910216E-22, 
     & 3.706286E-22, 2.881135E-23, 0.000000E+00 /
      DATA ( ECS_REF( IACRO_09,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.778219E-22, 9.186790E-22, 8.152126E-22, 6.910216E-22, 
     & 3.706286E-22, 2.881135E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACRO_09,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.262437E-02, 3.296818E-02, 2.259047E-02, 1.601362E-02, 
     & 6.907240E-03, 1.677804E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACRO_09,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.262437E-02, 3.296818E-02, 2.259047E-02, 1.601362E-02, 
     & 6.907240E-03, 1.677804E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACRO_09,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.262437E-02, 3.296818E-02, 2.259047E-02, 1.601362E-02, 
     & 6.907240E-03, 1.677804E-03, 0.000000E+00 /


C...PAA
C..  Peroxy acetic acid absorption cross sections
C..  Orlando, J. J. and G. S. Tyndall (2003): "Gas phase UV absorption spectra
C..  for peracetic acid, and for acetic acid monomers and dimers," J. Photochem.
C..  Photobiol A, 157, 161-166.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IPAA ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPAA,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.878198E-21, 9.997928E-22, 5.862126E-22, 3.756698E-22, 
     & 1.093339E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPAA,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.878198E-21, 9.997928E-22, 5.862126E-22, 3.756698E-22, 
     & 1.093339E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPAA,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.878198E-21, 9.997928E-22, 5.862126E-22, 3.756698E-22, 
     & 1.093339E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IPAA,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.677803E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IPAA,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.677803E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IPAA,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.677803E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPAA,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.878198E-21, 9.997928E-22, 5.862126E-22, 3.756698E-22, 
     & 1.093335E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPAA,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.878198E-21, 9.997928E-22, 5.862126E-22, 3.756698E-22, 
     & 1.093335E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPAA,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.878198E-21, 9.997928E-22, 5.862126E-22, 3.756698E-22, 
     & 1.093335E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPAA,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999964E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPAA,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999964E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPAA,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999964E-01, 0.000000E+00, 0.000000E+00 /


C...CL2
C..  CL2 + HV = 2 CL 	
C.. 	FROM IUPAC EVALUATION (1996)
C.. 	RECOMMEND UNIT QUANTUM YIELD
C.. 	

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICL2 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICL2,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.197690E-21 /
      DATA ( CS_REF( ICL2,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.197690E-21 /
      DATA ( CS_REF( ICL2,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.197690E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICL2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.435630E-01 /
      DATA ( QY_REF(  ICL2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.435630E-01 /
      DATA ( QY_REF(  ICL2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.435630E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICL2,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.197690E-21 /
      DATA ( ECS_REF( ICL2,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.197690E-21 /
      DATA ( ECS_REF( ICL2,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.197690E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICL2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999997E-01 /
      DATA ( EQY_REF( ICL2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999997E-01 /
      DATA ( EQY_REF( ICL2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999997E-01 /


C...CLNO-06
C..  CLNO absorption cross sections recommended by IUPAC (2005)
C..  Wavelength where absorption goes to zero est'd by extrapolation
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLNO_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLNO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.714401E-20, 1.043246E-19, 1.151118E-19, 1.265982E-19, 
     & 1.476980E-19, 8.786674E-20, 7.731566E-21 /
      DATA ( CS_REF( ICLNO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.714401E-20, 1.043246E-19, 1.151118E-19, 1.265982E-19, 
     & 1.476980E-19, 8.786674E-20, 7.731566E-21 /
      DATA ( CS_REF( ICLNO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.714401E-20, 1.043246E-19, 1.151118E-19, 1.265982E-19, 
     & 1.476980E-19, 8.786674E-20, 7.731566E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLNO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 3.938179E-01 /
      DATA ( QY_REF(  ICLNO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 3.938179E-01 /
      DATA ( QY_REF(  ICLNO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 3.938179E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLNO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.714401E-20, 1.043246E-19, 1.151118E-19, 1.265982E-19, 
     & 1.476980E-19, 8.786674E-20, 7.731564E-21 /
      DATA ( ECS_REF( ICLNO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.714401E-20, 1.043246E-19, 1.151118E-19, 1.265982E-19, 
     & 1.476980E-19, 8.786674E-20, 7.731564E-21 /
      DATA ( ECS_REF( ICLNO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.714401E-20, 1.043246E-19, 1.151118E-19, 1.265982E-19, 
     & 1.476980E-19, 8.786674E-20, 7.731564E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLNO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999998E-01 /
      DATA ( EQY_REF( ICLNO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999998E-01 /
      DATA ( EQY_REF( ICLNO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999998E-01 /


C...CLONO
C..  CLONO + HV = CL + NO2	
C.. 	FROM IUPAC EVALUATION (1996)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLONO ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLONO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.405210E-18, 1.199159E-18, 1.051876E-18, 9.226636E-19, 
     & 5.800030E-19, 8.253898E-20, 0.000000E+00 /
      DATA ( CS_REF( ICLONO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.405210E-18, 1.199159E-18, 1.051876E-18, 9.226636E-19, 
     & 5.800030E-19, 8.253898E-20, 0.000000E+00 /
      DATA ( CS_REF( ICLONO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.405210E-18, 1.199159E-18, 1.051876E-18, 9.226636E-19, 
     & 5.800030E-19, 8.253898E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLONO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.480834E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLONO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.480834E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLONO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.480834E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLONO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.405210E-18, 1.199159E-18, 1.051876E-18, 9.226636E-19, 
     & 5.800030E-19, 8.253895E-20, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.405210E-18, 1.199159E-18, 1.051876E-18, 9.226636E-19, 
     & 5.800030E-19, 8.253895E-20, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.405210E-18, 1.199159E-18, 1.051876E-18, 9.226636E-19, 
     & 5.800030E-19, 8.253895E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLONO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999997E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999997E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999997E-01, 0.000000E+00 /


C...CLNO2
C..  CLNO2 + HV = CL + NO2	
C.. 	FROM IUPAC EVALUATION (1996)
C.. 	RECOMMEND UNIT QUANTUM YILED

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLNO2 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLNO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.618957E-19, 1.401068E-19, 1.205566E-19, 1.003856E-19, 
     & 5.353105E-20, 5.485528E-21, 0.000000E+00 /
      DATA ( CS_REF( ICLNO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.618957E-19, 1.401068E-19, 1.205566E-19, 1.003856E-19, 
     & 5.353105E-20, 5.485528E-21, 0.000000E+00 /
      DATA ( CS_REF( ICLNO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.618957E-19, 1.401068E-19, 1.205566E-19, 1.003856E-19, 
     & 5.353105E-20, 5.485528E-21, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLNO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.832393E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLNO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.832393E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLNO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.832393E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLNO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.618957E-19, 1.401068E-19, 1.205566E-19, 1.003856E-19, 
     & 5.353105E-20, 5.485521E-21, 0.000000E+00 /
      DATA ( ECS_REF( ICLNO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.618957E-19, 1.401068E-19, 1.205566E-19, 1.003856E-19, 
     & 5.353105E-20, 5.485521E-21, 0.000000E+00 /
      DATA ( ECS_REF( ICLNO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.618957E-19, 1.401068E-19, 1.205566E-19, 1.003856E-19, 
     & 5.353105E-20, 5.485521E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLNO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999987E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLNO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999987E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLNO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999987E-01, 0.000000E+00 /


C...CLONO2-1
C..  CLONO2 + hv = CLO. + NO2
C..  CLONO2 absorption cross sections and quantum yields recommended by IUPAC (2005)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLONO2_1 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLONO2_1,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.646976E-20, 2.596369E-20, 1.617506E-20, 1.067463E-20, 
     & 4.477540E-21, 5.616671E-22, 0.000000E+00 /
      DATA ( CS_REF( ICLONO2_1,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.646976E-20, 2.596369E-20, 1.617506E-20, 1.067463E-20, 
     & 4.477540E-21, 5.616671E-22, 0.000000E+00 /
      DATA ( CS_REF( ICLONO2_1,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.646976E-20, 2.596369E-20, 1.617506E-20, 1.067463E-20, 
     & 4.477540E-21, 5.616671E-22, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLONO2_1,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.989724E-01, 3.828619E-01, 3.400473E-01, 
     & 2.225876E-01, 1.723396E-02, 0.000000E+00 /
      DATA ( QY_REF(  ICLONO2_1,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.989724E-01, 3.828619E-01, 3.400473E-01, 
     & 2.225876E-01, 1.723396E-02, 0.000000E+00 /
      DATA ( QY_REF(  ICLONO2_1,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.989724E-01, 3.828619E-01, 3.400473E-01, 
     & 2.225876E-01, 1.723396E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLONO2_1,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.858791E-20, 1.036466E-20, 6.204424E-21, 3.655838E-21, 
     & 1.070239E-21, 3.904645E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO2_1,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.858791E-20, 1.036466E-20, 6.204424E-21, 3.655838E-21, 
     & 1.070239E-21, 3.904645E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO2_1,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.858791E-20, 1.036466E-20, 6.204424E-21, 3.655838E-21, 
     & 1.070239E-21, 3.904645E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLONO2_1,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.991984E-01, 3.835795E-01, 3.424791E-01, 
     & 2.390239E-01, 6.951884E-02, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO2_1,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.991984E-01, 3.835795E-01, 3.424791E-01, 
     & 2.390239E-01, 6.951884E-02, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO2_1,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.991984E-01, 3.835795E-01, 3.424791E-01, 
     & 2.390239E-01, 6.951884E-02, 0.000000E+00 /


C...CLONO2-2
C..  CLONO2 + hv = CL. + NO3
C..  CLONO2 absorption cross sections and quantum yields recommended by IUPAC (2005)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLONO2_2 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLONO2_2,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.646976E-20, 2.596369E-20, 1.617506E-20, 1.067463E-20, 
     & 4.477540E-21, 1.279905E-21, 1.948483E-23 /
      DATA ( CS_REF( ICLONO2_2,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.646976E-20, 2.596369E-20, 1.617506E-20, 1.067463E-20, 
     & 4.477540E-21, 1.279905E-21, 1.948483E-23 /
      DATA ( CS_REF( ICLONO2_2,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.646976E-20, 2.596369E-20, 1.617506E-20, 1.067463E-20, 
     & 4.477540E-21, 1.279905E-21, 1.948483E-23 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLONO2_2,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.010277E-01, 6.171381E-01, 6.599527E-01, 
     & 7.774124E-01, 9.827660E-01, 7.729670E-02 /
      DATA ( QY_REF(  ICLONO2_2,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.010277E-01, 6.171381E-01, 6.599527E-01, 
     & 7.774124E-01, 9.827660E-01, 7.729670E-02 /
      DATA ( QY_REF(  ICLONO2_2,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.010277E-01, 6.171381E-01, 6.599527E-01, 
     & 7.774124E-01, 9.827660E-01, 7.729670E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLONO2_2,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.788186E-20, 1.559903E-20, 9.970641E-21, 7.018792E-21, 
     & 3.407301E-21, 1.240858E-21, 1.948451E-23 /
      DATA ( ECS_REF( ICLONO2_2,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.788186E-20, 1.559903E-20, 9.970641E-21, 7.018792E-21, 
     & 3.407301E-21, 1.240858E-21, 1.948451E-23 /
      DATA ( ECS_REF( ICLONO2_2,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.788186E-20, 1.559903E-20, 9.970641E-21, 7.018792E-21, 
     & 3.407301E-21, 1.240858E-21, 1.948451E-23 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLONO2_2,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.008017E-01, 6.164205E-01, 6.575208E-01, 
     & 7.609761E-01, 9.694926E-01, 9.999832E-01 /
      DATA ( EQY_REF( ICLONO2_2,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.008017E-01, 6.164205E-01, 6.575208E-01, 
     & 7.609761E-01, 9.694926E-01, 9.999832E-01 /
      DATA ( EQY_REF( ICLONO2_2,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.008017E-01, 6.164205E-01, 6.575208E-01, 
     & 7.609761E-01, 9.694926E-01, 9.999832E-01 /


C...HOCL-06
C..  HOCL absorption cross sections recommended by IUPAC (2005)
C..
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHOCL_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHOCL_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386087E-20, 
     & 3.171168E-20, 6.944975E-21, 2.670822E-23 /
      DATA ( CS_REF( IHOCL_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386087E-20, 
     & 3.171168E-20, 6.944975E-21, 2.670822E-23 /
      DATA ( CS_REF( IHOCL_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386087E-20, 
     & 3.171168E-20, 6.944975E-21, 2.670822E-23 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHOCL_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 3.488595E-02 /
      DATA ( QY_REF(  IHOCL_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 3.488595E-02 /
      DATA ( QY_REF(  IHOCL_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 3.488595E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHOCL_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386087E-20, 
     & 3.171168E-20, 6.944975E-21, 2.670613E-23 /
      DATA ( ECS_REF( IHOCL_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386087E-20, 
     & 3.171168E-20, 6.944975E-21, 2.670613E-23 /
      DATA ( ECS_REF( IHOCL_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386087E-20, 
     & 3.171168E-20, 6.944975E-21, 2.670613E-23 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHOCL_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999219E-01 /
      DATA ( EQY_REF( IHOCL_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999219E-01 /
      DATA ( EQY_REF( IHOCL_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999219E-01 /


C...CLCCHO
C..  Chloroacetaldehyde absorption cross sections
C..  NASA (2006) evaluation

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLCCHO ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLCCHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.382647E-20, 5.297188E-20, 4.837030E-20, 3.864029E-20, 
     & 1.335077E-20, 1.098278E-22, 0.000000E+00 /
      DATA ( CS_REF( ICLCCHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.382647E-20, 5.297188E-20, 4.837030E-20, 3.864029E-20, 
     & 1.335077E-20, 1.098278E-22, 0.000000E+00 /
      DATA ( CS_REF( ICLCCHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.382647E-20, 5.297188E-20, 4.837030E-20, 3.864029E-20, 
     & 1.335077E-20, 1.098278E-22, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLCCHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.735011E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLCCHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.735011E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLCCHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.735011E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLCCHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.382647E-20, 5.297188E-20, 4.837030E-20, 3.864029E-20, 
     & 1.335077E-20, 1.098275E-22, 0.000000E+00 /
      DATA ( ECS_REF( ICLCCHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.382647E-20, 5.297188E-20, 4.837030E-20, 3.864029E-20, 
     & 1.335077E-20, 1.098275E-22, 0.000000E+00 /
      DATA ( ECS_REF( ICLCCHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.382647E-20, 5.297188E-20, 4.837030E-20, 3.864029E-20, 
     & 1.335077E-20, 1.098275E-22, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLCCHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999973E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLCCHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999973E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLCCHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999973E-01, 0.000000E+00 /


C...CLACET
C..  Chloroacetone absorption cross sections
C..  Chloroacetone absorption cross sections
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLACET ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLACET,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.998559E-20, 8.666034E-20, 6.763288E-20, 4.788546E-20, 
     & 1.187528E-20, 8.168951E-23, 0.000000E+00 /
      DATA ( CS_REF( ICLACET,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.998559E-20, 8.666034E-20, 6.763288E-20, 4.788546E-20, 
     & 1.187528E-20, 8.168951E-23, 0.000000E+00 /
      DATA ( CS_REF( ICLACET,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.998559E-20, 8.666034E-20, 6.763288E-20, 4.788546E-20, 
     & 1.187528E-20, 8.168951E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLACET,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.623016E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLACET,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.623016E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLACET,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.623016E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLACET,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.998559E-20, 8.666034E-20, 6.763288E-20, 4.788546E-20, 
     & 1.187528E-20, 8.168905E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLACET,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.998559E-20, 8.666034E-20, 6.763288E-20, 4.788546E-20, 
     & 1.187528E-20, 8.168905E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLACET,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.998559E-20, 8.666034E-20, 6.763288E-20, 4.788546E-20, 
     & 1.187528E-20, 8.168905E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLACET,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999943E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLACET,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999943E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLACET,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999943E-01, 0.000000E+00 /


C...NO2_SAPRC99
C..  NO2 + HV = NO + O
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.080259E-19, 1.477440E-19, 1.863140E-19, 2.244636E-19, 
     & 3.311073E-19, 5.442337E-19, 2.379556E-20 /
      DATA ( CS_REF( INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.080259E-19, 1.477440E-19, 1.863140E-19, 2.244636E-19, 
     & 3.311073E-19, 5.442337E-19, 2.379556E-20 /
      DATA ( CS_REF( INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.080259E-19, 1.477440E-19, 1.863140E-19, 2.244636E-19, 
     & 3.311073E-19, 5.442337E-19, 2.379556E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986425E-01, 9.908484E-01, 
     & 9.900000E-01, 8.105520E-01, 1.455241E-03 /
      DATA ( QY_REF(  INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986425E-01, 9.908484E-01, 
     & 9.900000E-01, 8.105520E-01, 1.455241E-03 /
      DATA ( QY_REF(  INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986425E-01, 9.908484E-01, 
     & 9.900000E-01, 8.105520E-01, 1.455241E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.080259E-19, 1.477440E-19, 1.860489E-19, 2.223930E-19, 
     & 3.277963E-19, 4.331605E-19, 8.335073E-22 /
      DATA ( ECS_REF( INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.080259E-19, 1.477440E-19, 1.860489E-19, 2.223930E-19, 
     & 3.277963E-19, 4.331605E-19, 8.335073E-22 /
      DATA ( ECS_REF( INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.080259E-19, 1.477440E-19, 1.860489E-19, 2.223930E-19, 
     & 3.277963E-19, 4.331605E-19, 8.335073E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.985774E-01, 9.907753E-01, 
     & 9.900000E-01, 7.959090E-01, 3.502784E-02 /
      DATA ( EQY_REF( INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.985774E-01, 9.907753E-01, 
     & 9.900000E-01, 7.959090E-01, 3.502784E-02 /
      DATA ( EQY_REF( INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.985774E-01, 9.907753E-01, 
     & 9.900000E-01, 7.959090E-01, 3.502784E-02 /


C...NO3NO_SAPRC99
C..  NO3 + HV = NO + O2 (T=298)
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO3NO_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.442916E-19 /
      DATA ( CS_REF( INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.442916E-19 /
      DATA ( CS_REF( INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.442916E-19 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.990637E-02 /
      DATA ( QY_REF(  INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.990637E-02 /
      DATA ( QY_REF(  INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.990637E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.034345E-20 /
      DATA ( ECS_REF( INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.034345E-20 /
      DATA ( ECS_REF( INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.034345E-20 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.476111E-01 /
      DATA ( EQY_REF( INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.476111E-01 /
      DATA ( EQY_REF( INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.476111E-01 /


C...NO3NO2_SAPRC99
C..  NO3 + HV = NO2 + O (T=298)
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO3NO2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.854805E-21, 1.222858E-18 /
      DATA ( CS_REF( INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.854805E-21, 1.222858E-18 /
      DATA ( CS_REF( INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.854805E-21, 1.222858E-18 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.540388E-01, 6.027042E-01 /
      DATA ( QY_REF(  INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.540388E-01, 6.027042E-01 /
      DATA ( QY_REF(  INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.540388E-01, 6.027042E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.854805E-21, 8.924195E-19 /
      DATA ( ECS_REF( INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.854805E-21, 8.924195E-19 /
      DATA ( ECS_REF( INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 3.854805E-21, 8.924195E-19 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.000000E+00, 7.297817E-01 /
      DATA ( EQY_REF( INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.000000E+00, 7.297817E-01 /
      DATA ( EQY_REF( INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.000000E+00, 7.297817E-01 /


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

      DATA ( TEMP_REF( ITTR, IO3O3P_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.225669E-19, 2.738969E-19, 1.042280E-19, 4.515059E-20, 
     & 6.450503E-21, 2.109681E-23, 1.684420E-21 /
      DATA ( CS_REF( IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.225669E-19, 2.738969E-19, 1.042280E-19, 4.515059E-20, 
     & 6.450503E-21, 2.109681E-23, 1.684420E-21 /
      DATA ( CS_REF( IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.225669E-19, 2.738969E-19, 1.042280E-19, 4.515059E-20, 
     & 6.450503E-21, 2.109681E-23, 1.684420E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.221909E-02, 4.386296E-02, 4.630244E-01, 7.869666E-01, 
     & 9.554292E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( QY_REF(  IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.221909E-02, 4.386296E-02, 4.630244E-01, 7.869666E-01, 
     & 9.554292E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( QY_REF(  IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.221909E-02, 4.386296E-02, 4.630244E-01, 7.869666E-01, 
     & 9.554292E-01, 1.000000E+00, 1.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.220116E-20, 1.121303E-20, 4.453936E-20, 3.505516E-20, 
     & 5.972207E-21, 2.109681E-23, 1.684420E-21 /
      DATA ( ECS_REF( IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.220116E-20, 1.121303E-20, 4.453936E-20, 3.505516E-20, 
     & 5.972207E-21, 2.109681E-23, 1.684420E-21 /
      DATA ( ECS_REF( IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.220116E-20, 1.121303E-20, 4.453936E-20, 3.505516E-20, 
     & 5.972207E-21, 2.109681E-23, 1.684420E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.346130E-02, 4.093885E-02, 4.273264E-01, 7.764055E-01, 
     & 9.258513E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.346130E-02, 4.093885E-02, 4.273264E-01, 7.764055E-01, 
     & 9.258513E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.346130E-02, 4.093885E-02, 4.273264E-01, 7.764055E-01, 
     & 9.258513E-01, 1.000000E+00, 1.000000E+00 /


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

      DATA ( TEMP_REF( ITTR, IO3O1D_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.225669E-19, 2.738969E-19, 1.042280E-19, 4.515059E-20, 
     & 6.267941E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.225669E-19, 2.738969E-19, 1.042280E-19, 4.515059E-20, 
     & 6.267941E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.225669E-19, 2.738969E-19, 1.042280E-19, 4.515059E-20, 
     & 6.267941E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.377809E-01, 9.561371E-01, 5.369756E-01, 2.130334E-01, 
     & 4.457083E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.377809E-01, 9.561371E-01, 5.369756E-01, 2.130334E-01, 
     & 4.457083E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.377809E-01, 9.561371E-01, 5.369756E-01, 2.130334E-01, 
     & 4.457083E-02, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.703657E-19, 2.626839E-19, 5.968862E-20, 1.009543E-20, 
     & 4.782964E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.703657E-19, 2.626839E-19, 5.968862E-20, 1.009543E-20, 
     & 4.782964E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.703657E-19, 2.626839E-19, 5.968862E-20, 1.009543E-20, 
     & 4.782964E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.365387E-01, 9.590611E-01, 5.726736E-01, 2.235946E-01, 
     & 7.630838E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.365387E-01, 9.590611E-01, 5.726736E-01, 2.235946E-01, 
     & 7.630838E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.365387E-01, 9.590611E-01, 5.726736E-01, 2.235946E-01, 
     & 7.630838E-02, 0.000000E+00, 0.000000E+00 /


C...HONO_NO_SAPRC99
C..  HONO + HV = HO. + NO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHONO_NO_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.241728E-20, 3.445791E-20, 
     & 1.071895E-19, 9.424301E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.241728E-20, 3.445791E-20, 
     & 1.071895E-19, 9.424301E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.241728E-20, 3.445791E-20, 
     & 1.071895E-19, 9.424301E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.852223E-01, 4.683687E-01, 
     & 6.446996E-01, 7.147868E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.852223E-01, 4.683687E-01, 
     & 6.446996E-01, 7.147868E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.852223E-01, 4.683687E-01, 
     & 6.446996E-01, 7.147868E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.159218E-21, 1.631716E-20, 
     & 7.201626E-20, 8.904415E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.159218E-21, 1.631716E-20, 
     & 7.201626E-20, 8.904415E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.159218E-21, 1.631716E-20, 
     & 7.201626E-20, 8.904415E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.154868E-01, 4.735389E-01, 
     & 6.718590E-01, 9.448357E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.154868E-01, 4.735389E-01, 
     & 6.718590E-01, 9.448357E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.154868E-01, 4.735389E-01, 
     & 6.718590E-01, 9.448357E-01, 0.000000E+00 /


C...HONO_NO2_SAPRC99
C..  HONO + HV = H. + NO2
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHONO_NO2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.241728E-20, 3.445791E-20, 
     & 1.071895E-19, 4.548377E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.241728E-20, 3.445791E-20, 
     & 1.071895E-19, 4.548377E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.241728E-20, 3.445791E-20, 
     & 1.071895E-19, 4.548377E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.037597E-01, 5.316312E-01, 
     & 3.553004E-01, 3.117437E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.037597E-01, 5.316312E-01, 
     & 3.553004E-01, 3.117437E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.037597E-01, 5.316312E-01, 
     & 3.553004E-01, 3.117437E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.256632E-21, 1.814075E-20, 
     & 3.517327E-20, 5.198806E-21, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.256632E-21, 1.814075E-20, 
     & 3.517327E-20, 5.198806E-21, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.256632E-21, 1.814075E-20, 
     & 3.517327E-20, 5.198806E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.843977E-01, 5.264611E-01, 
     & 3.281410E-01, 1.143003E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.843977E-01, 5.264611E-01, 
     & 3.281410E-01, 1.143003E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.843977E-01, 5.264611E-01, 
     & 3.281410E-01, 1.143003E-01, 0.000000E+00 /


C...HNO3_SAPRC99
C..  HNO3 + HV = products
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHNO3_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637400E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637400E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637400E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637264E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637264E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637264E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999707E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999707E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999707E-01, 0.000000E+00 /


C...HO2NO2_SAPRC99
C..  HO2NO2 + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHO2NO2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721352E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721352E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721352E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721305E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721305E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721305E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999929E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999929E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999929E-01, 0.000000E+00, 0.000000E+00 /


C...H2O2_SAPRC99
C..  H2O2 + HV = 2 OH
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IH2O2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309158E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309158E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309158E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /
      DATA ( QY_REF(  IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /
      DATA ( QY_REF(  IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309022E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309022E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.342335E-21, 5.777204E-21, 3.914159E-21, 2.732004E-21, 
     & 1.162692E-21, 4.309022E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999685E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999685E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999685E-01, 0.000000E+00 /


C...HCHO_R_SAPRC99
C..  HCHO + HV = HCO + H
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHCHO_R_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.125415E-20, 3.307976E-20, 1.602210E-20, 3.138593E-20, 
     & 1.421334E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.125415E-20, 3.307976E-20, 1.602210E-20, 3.138593E-20, 
     & 1.421334E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.125415E-20, 3.307976E-20, 1.602210E-20, 3.138593E-20, 
     & 1.421334E-20, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.531340E-01, 7.793137E-01, 7.691399E-01, 6.777878E-01, 
     & 2.204757E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.531340E-01, 7.793137E-01, 7.691399E-01, 6.777878E-01, 
     & 2.204757E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.531340E-01, 7.793137E-01, 7.691399E-01, 6.777878E-01, 
     & 2.204757E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.356318E-20, 2.578273E-20, 1.239485E-20, 2.146144E-20, 
     & 4.031781E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.356318E-20, 2.578273E-20, 1.239485E-20, 2.146144E-20, 
     & 4.031781E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.356318E-20, 2.578273E-20, 1.239485E-20, 2.146144E-20, 
     & 4.031781E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.539216E-01, 7.794110E-01, 7.736096E-01, 6.837917E-01, 
     & 2.836617E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.539216E-01, 7.794110E-01, 7.736096E-01, 6.837917E-01, 
     & 2.836617E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.539216E-01, 7.794110E-01, 7.736096E-01, 6.837917E-01, 
     & 2.836617E-01, 0.000000E+00, 0.000000E+00 /


C...HCHO_M_SAPRC99
C..  HCHO + HV = H2 + CO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHCHO_M_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.125415E-20, 3.307976E-20, 1.602210E-20, 3.138593E-20, 
     & 1.672116E-20, 8.398104E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.125415E-20, 3.307976E-20, 1.602210E-20, 3.138593E-20, 
     & 1.672116E-20, 8.398104E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.125415E-20, 3.307976E-20, 1.602210E-20, 3.138593E-20, 
     & 1.672116E-20, 8.398104E-22, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.261196E-01, 2.137055E-01, 2.302355E-01, 3.221780E-01, 
     & 5.534477E-01, 3.537201E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.261196E-01, 2.137055E-01, 2.302355E-01, 3.221780E-01, 
     & 5.534477E-01, 3.537201E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.261196E-01, 2.137055E-01, 2.302355E-01, 3.221780E-01, 
     & 5.534477E-01, 3.537201E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.052405E-21, 7.086060E-21, 3.613880E-21, 9.924044E-21, 
     & 9.369489E-21, 1.337857E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.052405E-21, 7.086060E-21, 3.613880E-21, 9.924044E-21, 
     & 9.369489E-21, 1.337857E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.052405E-21, 7.086060E-21, 3.613880E-21, 9.924044E-21, 
     & 9.369489E-21, 1.337857E-22, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.256470E-01, 2.142113E-01, 2.255560E-01, 3.161941E-01, 
     & 5.603374E-01, 1.593047E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.256470E-01, 2.142113E-01, 2.255560E-01, 3.161941E-01, 
     & 5.603374E-01, 1.593047E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.256470E-01, 2.142113E-01, 2.255560E-01, 3.161941E-01, 
     & 5.603374E-01, 1.593047E-01, 0.000000E+00 /


C...CCHO_R_SAPRC99
C..  CCHO + HV = CH3 + CHO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICCHO_R_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.431909E-20, 3.721389E-20, 2.926475E-20, 2.113278E-20, 
     & 4.007796E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.431909E-20, 3.721389E-20, 2.926475E-20, 2.113278E-20, 
     & 4.007796E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.431909E-20, 3.721389E-20, 2.926475E-20, 2.113278E-20, 
     & 4.007796E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.837319E-01, 3.903790E-01, 2.837039E-01, 1.548971E-01, 
     & 1.542570E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.837319E-01, 3.903790E-01, 2.837039E-01, 1.548971E-01, 
     & 1.542570E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.837319E-01, 3.903790E-01, 2.837039E-01, 1.548971E-01, 
     & 1.542570E-02, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.146492E-20, 1.463071E-20, 8.383887E-21, 3.363618E-21, 
     & 1.911978E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.146492E-20, 1.463071E-20, 8.383887E-21, 3.363618E-21, 
     & 1.911978E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.146492E-20, 1.463071E-20, 8.383887E-21, 3.363618E-21, 
     & 1.911978E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.843267E-01, 3.931520E-01, 2.864842E-01, 1.591659E-01, 
     & 4.770647E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.843267E-01, 3.931520E-01, 2.864842E-01, 1.591659E-01, 
     & 4.770647E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.843267E-01, 3.931520E-01, 2.864842E-01, 1.591659E-01, 
     & 4.770647E-02, 0.000000E+00, 0.000000E+00 /


C...C2CHO_SAPRC99
C..  C2CHO + HV = C2H5. + CHO.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IC2CHO_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.167935E-20, 4.637920E-20, 3.576927E-20, 2.454658E-20, 
     & 6.109313E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.167935E-20, 4.637920E-20, 3.576927E-20, 2.454658E-20, 
     & 6.109313E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.167935E-20, 4.637920E-20, 3.576927E-20, 2.454658E-20, 
     & 6.109313E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.201593E-01, 7.958030E-01, 5.938827E-01, 4.325821E-01, 
     & 1.575024E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.201593E-01, 7.958030E-01, 5.938827E-01, 4.325821E-01, 
     & 1.575024E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.201593E-01, 7.958030E-01, 5.938827E-01, 4.325821E-01, 
     & 1.575024E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.779956E-20, 3.718655E-20, 2.127446E-20, 1.085955E-20, 
     & 1.479962E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.779956E-20, 3.718655E-20, 2.127446E-20, 1.085955E-20, 
     & 1.479962E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.779956E-20, 3.718655E-20, 2.127446E-20, 1.085955E-20, 
     & 1.479962E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.775294E-01, 8.017938E-01, 5.947691E-01, 4.424057E-01, 
     & 2.422470E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.775294E-01, 8.017938E-01, 5.947691E-01, 4.424057E-01, 
     & 2.422470E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.775294E-01, 8.017938E-01, 5.947691E-01, 4.424057E-01, 
     & 2.422470E-01, 0.000000E+00, 0.000000E+00 /


C...ACETONE_SAPRC99
C..  ACETONE + HV = CH3CO. + CH3.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IACETONE_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.561990E-20, 2.349588E-20, 1.406119E-20, 7.586603E-21, 
     & 8.979676E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.561990E-20, 2.349588E-20, 1.406119E-20, 7.586603E-21, 
     & 8.979676E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.561990E-20, 2.349588E-20, 1.406119E-20, 7.586603E-21, 
     & 8.979676E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.239195E-01, 1.144415E-01, 5.779590E-02, 2.891370E-02, 
     & 4.679137E-03, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.239195E-01, 1.144415E-01, 5.779590E-02, 2.891370E-02, 
     & 4.679137E-03, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.239195E-01, 1.144415E-01, 5.779590E-02, 2.891370E-02, 
     & 4.679137E-03, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.080394E-21, 2.785339E-21, 8.292822E-22, 2.351871E-22, 
     & 9.267896E-24, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.080394E-21, 2.785339E-21, 8.292822E-22, 2.351871E-22, 
     & 9.267896E-24, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.080394E-21, 2.785339E-21, 8.292822E-22, 2.351871E-22, 
     & 9.267896E-24, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.268505E-01, 1.185458E-01, 5.897669E-02, 3.100031E-02, 
     & 1.032097E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.268505E-01, 1.185458E-01, 5.897669E-02, 3.100031E-02, 
     & 1.032097E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.268505E-01, 1.185458E-01, 5.897669E-02, 3.100031E-02, 
     & 1.032097E-02, 0.000000E+00, 0.000000E+00 /


C...KETONE_SAPRC99
C..  Methyl Ethyl Ketone Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IKETONE_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.251486E-20, 2.719476E-20, 1.560863E-20, 7.734724E-21, 
     & 8.026588E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.251486E-20, 2.719476E-20, 1.560863E-20, 7.734724E-21, 
     & 8.026588E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.251486E-20, 2.719476E-20, 1.560863E-20, 7.734724E-21, 
     & 8.026588E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.251229E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.251229E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.251229E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.251486E-20, 2.719476E-20, 1.560863E-20, 7.734724E-21, 
     & 8.026587E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.251486E-20, 2.719476E-20, 1.560863E-20, 7.734724E-21, 
     & 8.026587E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.251486E-20, 2.719476E-20, 1.560863E-20, 7.734724E-21, 
     & 8.026587E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999999E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999999E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999999E-01, 0.000000E+00, 0.000000E+00 /


C...COOH_SAPRC99
C..  CH3OOH + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICOOH_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232370E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232370E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232370E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.407560E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.407560E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.407560E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232326E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232326E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.617454E-21, 3.528208E-21, 2.405066E-21, 1.705886E-21, 
     & 7.383624E-22, 6.232326E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999929E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999929E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999929E-01, 0.000000E+00 /


C...GLY_R_SAPRC99
C..  Glyoxal + hv = 2 HCO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IGLY_R_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 1.898477E-20, 1.296601E-21 /
      DATA ( CS_REF( IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 1.898477E-20, 1.296601E-21 /
      DATA ( CS_REF( IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 1.898477E-20, 1.296601E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.823931E-01, 0.000000E+00 /
      DATA ( QY_REF(  IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.823931E-01, 0.000000E+00 /
      DATA ( QY_REF(  IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.823931E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 4.341389E-21, 0.000000E+00 /
      DATA ( ECS_REF( IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 4.341389E-21, 0.000000E+00 /
      DATA ( ECS_REF( IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 4.341389E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.286774E-01, 0.000000E+00 /
      DATA ( EQY_REF( IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.286774E-01, 0.000000E+00 /
      DATA ( EQY_REF( IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.286774E-01, 0.000000E+00 /


C...GLY_ABS_SAPRC99
C..  Glyoxal Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IGLY_ABS_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 1.898477E-20, 1.424838E-20 /
      DATA ( CS_REF( IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 1.898477E-20, 1.424838E-20 /
      DATA ( CS_REF( IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 1.898477E-20, 1.424838E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.777996E-01 /
      DATA ( QY_REF(  IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.777996E-01 /
      DATA ( QY_REF(  IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.777996E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 1.898477E-20, 1.424838E-20 /
      DATA ( ECS_REF( IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 1.898477E-20, 1.424838E-20 /
      DATA ( ECS_REF( IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.308047E-20, 3.068909E-20, 2.760725E-20, 2.078190E-20, 
     & 6.738002E-21, 1.898477E-20, 1.424838E-20 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /


C...MGLY_ADJ_SAPRC99
C..  MGLY + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_ADJ_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 3.074720E-21 /
      DATA ( CS_REF( IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 3.074720E-21 /
      DATA ( CS_REF( IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 3.074720E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.993793E-01, 4.093204E-01, 0.000000E+00 /
      DATA ( QY_REF(  IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.993793E-01, 4.093204E-01, 0.000000E+00 /
      DATA ( QY_REF(  IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.993793E-01, 4.093204E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.162925E-21, 6.143782E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.162925E-21, 6.143782E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.162925E-21, 6.143782E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.997061E-01, 1.773227E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.997061E-01, 1.773227E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.997061E-01, 1.773227E-01, 0.000000E+00 /


C...BACL_ADJ_SAPRC99
C..  BACL + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IBACL_ADJ_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.614094E-20, 1.589884E-20, 9.010759E-21, 6.016450E-21, 
     & 4.659123E-21, 3.061080E-20, 8.938935E-21 /
      DATA ( CS_REF( IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.614094E-20, 1.589884E-20, 9.010759E-21, 6.016450E-21, 
     & 4.659123E-21, 3.061080E-20, 8.938935E-21 /
      DATA ( CS_REF( IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.614094E-20, 1.589884E-20, 9.010759E-21, 6.016450E-21, 
     & 4.659123E-21, 3.061080E-20, 8.938935E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.562430E-01, 1.901927E-03 /
      DATA ( QY_REF(  IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.562430E-01, 1.901927E-03 /
      DATA ( QY_REF(  IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.562430E-01, 1.901927E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.614094E-20, 1.589884E-20, 9.010759E-21, 6.016450E-21, 
     & 4.659123E-21, 1.199574E-20, 1.289835E-22 /
      DATA ( ECS_REF( IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.614094E-20, 1.589884E-20, 9.010759E-21, 6.016450E-21, 
     & 4.659123E-21, 1.199574E-20, 1.289835E-22 /
      DATA ( ECS_REF( IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.614094E-20, 1.589884E-20, 9.010759E-21, 6.016450E-21, 
     & 4.659123E-21, 1.199574E-20, 1.289835E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.918793E-01, 1.442940E-02 /
      DATA ( EQY_REF( IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.918793E-01, 1.442940E-02 /
      DATA ( EQY_REF( IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.918793E-01, 1.442940E-02 /


C...BZCHO_SAPRC99
C..  Benzaldehyde absorbtion coefs in n-Hexane
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IBZCHO_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.365009E-20, 6.611635E-20, 6.728674E-20, 
     & 8.232216E-20, 3.125745E-20, 0.000000E+00 /
      DATA ( CS_REF( IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.365009E-20, 6.611635E-20, 6.728674E-20, 
     & 8.232216E-20, 3.125745E-20, 0.000000E+00 /
      DATA ( CS_REF( IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.365009E-20, 6.611635E-20, 6.728674E-20, 
     & 8.232216E-20, 3.125745E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.392353E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.550116E-01, 0.000000E+00 /
      DATA ( QY_REF(  IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.392353E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.550116E-01, 0.000000E+00 /
      DATA ( QY_REF(  IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.392353E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.550116E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.361427E-20, 6.611635E-20, 6.728674E-20, 
     & 8.232216E-20, 3.125741E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.361427E-20, 6.611635E-20, 6.728674E-20, 
     & 8.232216E-20, 3.125741E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.361427E-20, 6.611635E-20, 6.728674E-20, 
     & 8.232216E-20, 3.125741E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.996175E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999987E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.996175E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999987E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.996175E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999987E-01, 0.000000E+00 /


C...ACROLEIN_SAPRC99
C..  Absorption cross sections for Acrolein.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IACROLEIN_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.121896E-20, 3.142855E-20, 4.087763E-20, 4.828621E-20, 
     & 5.748912E-20, 1.354352E-20, 0.000000E+00 /
      DATA ( CS_REF( IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.121896E-20, 3.142855E-20, 4.087763E-20, 4.828621E-20, 
     & 5.748912E-20, 1.354352E-20, 0.000000E+00 /
      DATA ( CS_REF( IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.121896E-20, 3.142855E-20, 4.087763E-20, 4.828621E-20, 
     & 5.748912E-20, 1.354352E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.990738E-01, 0.000000E+00 /
      DATA ( QY_REF(  IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.990738E-01, 0.000000E+00 /
      DATA ( QY_REF(  IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.990738E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.121896E-20, 3.142855E-20, 4.087763E-20, 4.828621E-20, 
     & 5.748912E-20, 1.354352E-20, 0.000000E+00 /
      DATA ( ECS_REF( IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.121896E-20, 3.142855E-20, 4.087763E-20, 4.828621E-20, 
     & 5.748912E-20, 1.354352E-20, 0.000000E+00 /
      DATA ( ECS_REF( IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.121896E-20, 3.142855E-20, 4.087763E-20, 4.828621E-20, 
     & 5.748912E-20, 1.354352E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /


C...IC3ONO2_SAPRC99
C..  I-C3H7ONO2 + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IIC3ONO2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837941E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837941E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837941E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837921E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837921E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.253602E-20, 6.362722E-21, 3.273374E-21, 1.721527E-21, 
     & 2.837921E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999931E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999931E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999931E-01, 0.000000E+00, 0.000000E+00 /


C...MGLY_ABS_SAPRC99
C..  Methyl Glyoxal Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_ABS_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /
      DATA ( CS_REF( IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /
      DATA ( CS_REF( IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.908495E-01 /
      DATA ( QY_REF(  IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.908495E-01 /
      DATA ( QY_REF(  IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.908495E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /
      DATA ( ECS_REF( IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /
      DATA ( ECS_REF( IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /


C...O3_O3P_IUPAC04
C.. O3 + HV = O(3P) + O2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet POx2, updated 2nd October 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IO3_O3P_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3_O3P_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.208109E-19, 2.619094E-19, 1.026330E-19, 4.193888E-20, 
     & 6.469928E-21, 4.985061E-23, 1.620752E-21 /
      DATA ( CS_REF( IO3_O3P_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.208109E-19, 2.619094E-19, 1.026330E-19, 4.193888E-20, 
     & 6.469928E-21, 4.985061E-23, 1.620752E-21 /
      DATA ( CS_REF( IO3_O3P_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.208109E-19, 2.619094E-19, 1.026330E-19, 4.193888E-20, 
     & 6.469928E-21, 4.985061E-23, 1.620752E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3_O3P_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.073348E-01, 4.688335E-01, 7.772150E-01, 
     & 9.126695E-01, 9.604713E-01, 8.193054E-01 /
      DATA ( QY_REF(  IO3_O3P_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.073348E-01, 4.688335E-01, 7.772150E-01, 
     & 9.126695E-01, 9.604713E-01, 8.193054E-01 /
      DATA ( QY_REF(  IO3_O3P_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.073348E-01, 4.688335E-01, 7.772150E-01, 
     & 9.126695E-01, 9.604713E-01, 8.193054E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3_O3P_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.208109E-20, 2.749129E-20, 4.637081E-20, 3.223425E-20, 
     & 5.803926E-21, 4.601414E-23, 1.620752E-21 /
      DATA ( ECS_REF( IO3_O3P_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.208109E-20, 2.749129E-20, 4.637081E-20, 3.223425E-20, 
     & 5.803926E-21, 4.601414E-23, 1.620752E-21 /
      DATA ( ECS_REF( IO3_O3P_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.208109E-20, 2.749129E-20, 4.637081E-20, 3.223425E-20, 
     & 5.803926E-21, 4.601414E-23, 1.620752E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3_O3P_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.049649E-01, 4.518118E-01, 7.686004E-01, 
     & 8.970618E-01, 9.230406E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3_O3P_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.049649E-01, 4.518118E-01, 7.686004E-01, 
     & 8.970618E-01, 9.230406E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3_O3P_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.049649E-01, 4.518118E-01, 7.686004E-01, 
     & 8.970618E-01, 9.230406E-01, 1.000000E+00 /


C...O3_O1D_IUPAC04
C..  O3 + HV = O(1D) + O2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet POx2, updated 2nd October 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IO3_O1D_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3_O1D_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.208109E-19, 2.619094E-19, 1.026330E-19, 4.193888E-20, 
     & 6.469928E-21, 4.795595E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3_O1D_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.208109E-19, 2.619094E-19, 1.026330E-19, 4.193888E-20, 
     & 6.469928E-21, 4.795595E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3_O1D_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.208109E-19, 2.619094E-19, 1.026330E-19, 4.193888E-20, 
     & 6.469928E-21, 4.795595E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3_O1D_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.926651E-01, 5.311664E-01, 2.227850E-01, 
     & 8.733054E-02, 1.887175E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3_O1D_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.926651E-01, 5.311664E-01, 2.227850E-01, 
     & 8.733054E-02, 1.887175E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3_O1D_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.926651E-01, 5.311664E-01, 2.227850E-01, 
     & 8.733054E-02, 1.887175E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3_O1D_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.387298E-19, 2.344181E-19, 5.626221E-20, 9.704640E-21, 
     & 6.660025E-22, 3.836402E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3_O1D_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.387298E-19, 2.344181E-19, 5.626221E-20, 9.704640E-21, 
     & 6.660025E-22, 3.836402E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3_O1D_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.387298E-19, 2.344181E-19, 5.626221E-20, 9.704640E-21, 
     & 6.660025E-22, 3.836402E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3_O1D_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.950351E-01, 5.481882E-01, 2.313996E-01, 
     & 1.029382E-01, 7.999845E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3_O1D_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.950351E-01, 5.481882E-01, 2.313996E-01, 
     & 1.029382E-01, 7.999845E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3_O1D_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.950351E-01, 5.481882E-01, 2.313996E-01, 
     & 1.029382E-01, 7.999845E-02, 0.000000E+00 /


C...HONO_IUPAC04
C.. HONO + HV = HO + NO
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx1_HONO, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHONO_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.758105E-21, 1.609743E-20, 3.108898E-20, 
     & 9.115838E-20, 7.878995E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.758105E-21, 1.609743E-20, 3.108898E-20, 
     & 9.115838E-20, 7.878995E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.758105E-21, 1.609743E-20, 3.108898E-20, 
     & 9.115838E-20, 7.878995E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHONO_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 8.285595E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.459611E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 8.285595E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.459611E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 8.285595E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.459611E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.758064E-21, 1.609743E-20, 3.108898E-20, 
     & 9.115838E-20, 7.878991E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.758064E-21, 1.609743E-20, 3.108898E-20, 
     & 9.115838E-20, 7.878991E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.758064E-21, 1.609743E-20, 3.108898E-20, 
     & 9.115838E-20, 7.878991E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.999914E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.999914E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.999914E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /


C...HO2NO2_IUPAC04
C..  HOONO2 + HV = products
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx3_HO2NO2, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHO2NO2_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHO2NO2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721352E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721352E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721352E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHO2NO2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHO2NO2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHO2NO2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.950289E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHO2NO2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721305E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721305E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.585970E-20, 1.082016E-20, 5.509709E-21, 3.477366E-21, 
     & 6.721305E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHO2NO2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999929E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999929E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999929E-01, 0.000000E+00, 0.000000E+00 /


C...HNO3_IUPAC04
C.. HONO2 + HV = OH + NO2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx2_HONO2, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHNO3_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO3_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637400E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637400E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637400E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHNO3_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.351202E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO3_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637264E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637264E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.351450E-21, 1.958802E-21, 8.418979E-22, 3.681581E-22, 
     & 5.035483E-23, 4.637264E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO3_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999707E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999707E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999707E-01, 0.000000E+00 /


C...N2O5_IUPAC04
C.. N2O5 + HV = NO2 + NO3
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx7_N2O5, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IN2O5_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IN2O5_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.282711E-20, 3.287780E-20, 2.187781E-20, 1.518603E-20, 
     & 6.206631E-21, 6.826251E-22, 1.900982E-25 /
      DATA ( CS_REF( IN2O5_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.282711E-20, 3.287780E-20, 2.187781E-20, 1.518603E-20, 
     & 6.206631E-21, 6.826251E-22, 1.900982E-25 /
      DATA ( CS_REF( IN2O5_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.282711E-20, 3.287780E-20, 2.187781E-20, 1.518603E-20, 
     & 6.206631E-21, 6.826251E-22, 1.900982E-25 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IN2O5_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.225487E-01, 9.980785E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.282069E-03 /
      DATA ( QY_REF(  IN2O5_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.225487E-01, 9.980785E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.282069E-03 /
      DATA ( QY_REF(  IN2O5_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.225487E-01, 9.980785E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.282069E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IN2O5_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.853734E-20, 3.279835E-20, 2.187781E-20, 1.518603E-20, 
     & 6.206631E-21, 6.826251E-22, 1.900108E-25 /
      DATA ( ECS_REF( IN2O5_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.853734E-20, 3.279835E-20, 2.187781E-20, 1.518603E-20, 
     & 6.206631E-21, 6.826251E-22, 1.900108E-25 /
      DATA ( ECS_REF( IN2O5_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.853734E-20, 3.279835E-20, 2.187781E-20, 1.518603E-20, 
     & 6.206631E-21, 6.826251E-22, 1.900108E-25 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IN2O5_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.187960E-01, 9.975836E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.995402E-01 /
      DATA ( EQY_REF( IN2O5_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.187960E-01, 9.975836E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.995402E-01 /
      DATA ( EQY_REF( IN2O5_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.187960E-01, 9.975836E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.995402E-01 /


C...NTR_IUPAC04
C.. i-C3H7ONO2 + HV = iC3H7O + NO2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet P17_i-C3H7ONO2+hv, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INTR_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INTR_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.226895E-20, 6.122152E-21, 2.999441E-21, 1.495104E-21, 
     & 2.339986E-22, 1.431077E-24, 0.000000E+00 /
      DATA ( CS_REF( INTR_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.226895E-20, 6.122152E-21, 2.999441E-21, 1.495104E-21, 
     & 2.339986E-22, 1.431077E-24, 0.000000E+00 /
      DATA ( CS_REF( INTR_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.226895E-20, 6.122152E-21, 2.999441E-21, 1.495104E-21, 
     & 2.339986E-22, 1.431077E-24, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INTR_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.623016E-01, 0.000000E+00 /
      DATA ( QY_REF(  INTR_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.623016E-01, 0.000000E+00 /
      DATA ( QY_REF(  INTR_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.623016E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INTR_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.226895E-20, 6.122152E-21, 2.999441E-21, 1.495104E-21, 
     & 2.339986E-22, 1.431070E-24, 0.000000E+00 /
      DATA ( ECS_REF( INTR_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.226895E-20, 6.122152E-21, 2.999441E-21, 1.495104E-21, 
     & 2.339986E-22, 1.431070E-24, 0.000000E+00 /
      DATA ( ECS_REF( INTR_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.226895E-20, 6.122152E-21, 2.999441E-21, 1.495104E-21, 
     & 2.339986E-22, 1.431070E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INTR_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999954E-01, 0.000000E+00 /
      DATA ( EQY_REF( INTR_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999954E-01, 0.000000E+00 /
      DATA ( EQY_REF( INTR_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999954E-01, 0.000000E+00 /


C...PAN_IUPAC04
C.. CH3C(O)OONO2 + HV = CH3C(O)OO + NO2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet P21_CH3C(O)OONO2+hv, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IPAN_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPAN_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.582222E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.582222E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.582222E-24, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IPAN_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.995215E-02, 0.000000E+00 /
      DATA ( QY_REF(  IPAN_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.995215E-02, 0.000000E+00 /
      DATA ( QY_REF(  IPAN_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.995215E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPAN_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.581996E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.581996E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.523355E-21, 1.420651E-21, 6.704418E-22, 3.663942E-22, 
     & 9.453910E-23, 1.581996E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPAN_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.998567E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.998567E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.998567E-01, 0.000000E+00 /


C...PACD_CB05
C..  PACD   + HV = MEO2 + OH
C..  CB05 Photolysis data for PACD
C..  Supplied by Greg Yarwood, 11/16/2007
C..  Ref: Giguère, P. A. and A. W. Olmos. Sur le spectre ultraviolet de l'acide peracétique et l'hydrolyse des peracétates.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IPACD_CB05 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPACD_CB05,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.747792E-22, 4.132827E-22, 2.422399E-22, 1.533387E-22, 
     & 2.881389E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPACD_CB05,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.747792E-22, 4.132827E-22, 2.422399E-22, 1.533387E-22, 
     & 2.881389E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPACD_CB05,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.747792E-22, 4.132827E-22, 2.422399E-22, 1.533387E-22, 
     & 2.881389E-23, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IPACD_CB05,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 3.826614E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IPACD_CB05,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 3.826614E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IPACD_CB05,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 3.826614E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPACD_CB05,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.747792E-22, 4.132827E-22, 2.422399E-22, 1.533387E-22, 
     & 2.876751E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPACD_CB05,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.747792E-22, 4.132827E-22, 2.422399E-22, 1.533387E-22, 
     & 2.876751E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPACD_CB05,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.747792E-22, 4.132827E-22, 2.422399E-22, 1.533387E-22, 
     & 2.876751E-23, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPACD_CB05,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.983906E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPACD_CB05,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.983906E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPACD_CB05,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.983906E-01, 0.000000E+00, 0.000000E+00 /


C...MGLY_IUPAC04
C.. CH3COCHO + hv ---> CH3CO + HCO
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation; IUPAC Stern-Volmer expression
C..  Data Sheet P6_CH3COCHO+hv.pdf, updated 16th Jan, 2003
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.377988E-20, 3.467536E-20, 2.425079E-20, 1.793719E-20, 
     & 6.357002E-21, 3.467224E-20, 1.501109E-20 /
      DATA ( CS_REF( IMGLY_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.377988E-20, 3.467536E-20, 2.425079E-20, 1.793719E-20, 
     & 6.357002E-21, 3.467224E-20, 1.501109E-20 /
      DATA ( CS_REF( IMGLY_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.377988E-20, 3.467536E-20, 2.425079E-20, 1.793719E-20, 
     & 6.357002E-21, 3.467224E-20, 1.501109E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.552968E-01, 4.758768E-03 /
      DATA ( QY_REF(  IMGLY_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.552968E-01, 4.758768E-03 /
      DATA ( QY_REF(  IMGLY_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.552968E-01, 4.758768E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.377988E-20, 3.467536E-20, 2.425079E-20, 1.793719E-20, 
     & 6.357002E-21, 8.277275E-21, 4.106772E-22 /
      DATA ( ECS_REF( IMGLY_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.377988E-20, 3.467536E-20, 2.425079E-20, 1.793719E-20, 
     & 6.357002E-21, 8.277275E-21, 4.106772E-22 /
      DATA ( ECS_REF( IMGLY_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.377988E-20, 3.467536E-20, 2.425079E-20, 1.793719E-20, 
     & 6.357002E-21, 8.277275E-21, 4.106772E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.387292E-01, 2.735824E-02 /
      DATA ( EQY_REF( IMGLY_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.387292E-01, 2.735824E-02 /
      DATA ( EQY_REF( IMGLY_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.387292E-01, 2.735824E-02 /


C...CL2_IUPAC04
C..  CL2 + HV = 2*CL
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation; Data Sheet PCl11 Website: 15th December 2000
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk
C..  Assume these are point values (not specified in data source)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICL2_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICL2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.168854E-21 /
      DATA ( CS_REF( ICL2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.168854E-21 /
      DATA ( CS_REF( ICL2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.168854E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICL2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.073897E-01 /
      DATA ( QY_REF(  ICL2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.073897E-01 /
      DATA ( QY_REF(  ICL2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.073897E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICL2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.168687E-21 /
      DATA ( ECS_REF( ICL2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.168687E-21 /
      DATA ( ECS_REF( ICL2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.837280E-20, 1.399567E-19, 1.844070E-19, 2.181722E-19, 
     & 2.414089E-19, 7.097299E-20, 1.168687E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICL2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.998571E-01 /
      DATA ( EQY_REF( ICL2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.998571E-01 /
      DATA ( EQY_REF( ICL2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.998571E-01 /


C...HOCL_IUPAC04
C..  HOCL + HV = HO + CL
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation; Data Sheet PCl2 Website: 15th December 2000
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk
C..  Assume these are point values - not specified in data source

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHOCL_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHOCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386086E-20, 
     & 3.171168E-20, 6.944975E-21, 2.435396E-23 /
      DATA ( CS_REF( IHOCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386086E-20, 
     & 3.171168E-20, 6.944975E-21, 2.435396E-23 /
      DATA ( CS_REF( IHOCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386086E-20, 
     & 3.171168E-20, 6.944975E-21, 2.435396E-23 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHOCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.741205E-02 /
      DATA ( QY_REF(  IHOCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.741205E-02 /
      DATA ( QY_REF(  IHOCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.741205E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHOCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386086E-20, 
     & 3.171168E-20, 6.944975E-21, 2.425303E-23 /
      DATA ( ECS_REF( IHOCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386086E-20, 
     & 3.171168E-20, 6.944975E-21, 2.425303E-23 /
      DATA ( ECS_REF( IHOCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.572037E-20, 6.066260E-20, 5.955837E-20, 5.386086E-20, 
     & 3.171168E-20, 6.944975E-21, 2.425303E-23 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHOCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.958555E-01 /
      DATA ( EQY_REF( IHOCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.958555E-01 /
      DATA ( EQY_REF( IHOCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.958555E-01 /


C...FMCL_IUPAC04
C..  FMCL + hv -->   HCO + CL
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation; Data Sheet PCl28 Website: 15th December 2000
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk
C..  Reference: H. G. Libuda, F. Zabel, E. H. Fink, and K. H. Becker, J. Phys. Chem. 94, 5860 (1990)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IFMCL_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IFMCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.296061E-21, 1.428731E-21, 2.229037E-22, 8.498279E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IFMCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.296061E-21, 1.428731E-21, 2.229037E-22, 8.498279E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IFMCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.296061E-21, 1.428731E-21, 2.229037E-22, 8.498279E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IFMCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 7.921222E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IFMCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 7.921222E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IFMCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 7.921222E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IFMCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.296061E-21, 1.428731E-21, 2.229037E-22, 8.480686E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IFMCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.296061E-21, 1.428731E-21, 2.229037E-22, 8.480686E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IFMCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.296061E-21, 1.428731E-21, 2.229037E-22, 8.480686E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IFMCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 9.979299E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IFMCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 9.979299E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IFMCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 9.979299E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /


C...NO2
C..  NO2 + HV = NO + O
C..  From NASA (2006).
C..  Absorption cross sections are averages for wavelength intervals given.
C..  Data alligned to smallest wavelength intervals for abs. coefs and qy's.
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO2 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 5.707211E-19, 2.476857E-20 /
      DATA ( CS_REF( INO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 5.707211E-19, 2.476857E-20 /
      DATA ( CS_REF( INO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 5.707211E-19, 2.476857E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.354442E-01, 1.556473E-03 /
      DATA ( QY_REF(  INO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.354442E-01, 1.556473E-03 /
      DATA ( QY_REF(  INO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.354442E-01, 1.556473E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 4.705641E-19, 9.288722E-22 /
      DATA ( ECS_REF( INO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 4.705641E-19, 9.288722E-22 /
      DATA ( ECS_REF( INO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.076237E-19, 1.482865E-19, 1.884280E-19, 2.269908E-19, 
     & 3.422504E-19, 4.705641E-19, 9.288722E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.245080E-01, 3.750205E-02 /
      DATA ( EQY_REF( INO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.245080E-01, 3.750205E-02 /
      DATA ( EQY_REF( INO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.245080E-01, 3.750205E-02 /


C...O3O1D
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

      DATA ( TEMP_REF( ITTR, IO3O1D ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O1D,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.519092E-19, 2.756369E-19, 1.058103E-19, 4.660382E-20, 
     & 6.954922E-21, 6.413300E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.519092E-19, 2.756369E-19, 1.058103E-19, 4.660382E-20, 
     & 6.954922E-21, 6.413300E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.519092E-19, 2.756369E-19, 1.058103E-19, 4.660382E-20, 
     & 6.954922E-21, 6.413300E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O1D,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.926651E-01, 5.311664E-01, 2.227850E-01, 
     & 8.767726E-02, 4.585904E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.926651E-01, 5.311664E-01, 2.227850E-01, 
     & 8.767726E-02, 4.585904E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.926651E-01, 5.311664E-01, 2.227850E-01, 
     & 8.767726E-02, 4.585904E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O1D,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.667183E-19, 2.468925E-19, 5.994369E-20, 1.079729E-20, 
     & 7.086030E-22, 5.114812E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.667183E-19, 2.468925E-19, 5.994369E-20, 1.079729E-20, 
     & 7.086030E-22, 5.114812E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.667183E-19, 2.468925E-19, 5.994369E-20, 1.079729E-20, 
     & 7.086030E-22, 5.114812E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O1D,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.957165E-01, 5.665205E-01, 2.316824E-01, 
     & 1.018851E-01, 7.975321E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.957165E-01, 5.665205E-01, 2.316824E-01, 
     & 1.018851E-01, 7.975321E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.957165E-01, 5.665205E-01, 2.316824E-01, 
     & 1.018851E-01, 7.975321E-02, 0.000000E+00 /


C...O3O3P
C..  O3 + HV = O3P + O2
C..  Absorption cross sections from NASA (2006).
C..  Quantum yields are derived from the O1D quantum yields in the low wavelength re
C..  Unit quantum yields assumed in high wavelength region.
C..  Absorption cross sections below 829 nm are extrapolated
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IO3O3P ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O3P,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-19, 2.754982E-19, 1.058103E-19, 4.660382E-20, 
     & 7.174786E-21, 7.032893E-23, 1.701844E-21 /
      DATA ( CS_REF( IO3O3P,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-19, 2.754982E-19, 1.058103E-19, 4.660382E-20, 
     & 7.174786E-21, 7.032893E-23, 1.701844E-21 /
      DATA ( CS_REF( IO3O3P,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-19, 2.754982E-19, 1.058103E-19, 4.660382E-20, 
     & 7.174786E-21, 7.032893E-23, 1.701844E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O3P,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.073348E-01, 4.688335E-01, 7.772150E-01, 
     & 9.115356E-01, 9.541410E-01, 9.901482E-01 /
      DATA ( QY_REF(  IO3O3P,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.073348E-01, 4.688335E-01, 7.772150E-01, 
     & 9.115356E-01, 9.541410E-01, 9.901482E-01 /
      DATA ( QY_REF(  IO3O3P,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.073348E-01, 4.688335E-01, 7.772150E-01, 
     & 9.115356E-01, 9.541410E-01, 9.901482E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O3P,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-20, 2.873049E-20, 4.586660E-20, 3.580654E-20, 
     & 6.431878E-21, 6.521411E-23, 1.701844E-21 /
      DATA ( ECS_REF( IO3O3P,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-20, 2.873049E-20, 4.586660E-20, 3.580654E-20, 
     & 6.431878E-21, 6.521411E-23, 1.701844E-21 /
      DATA ( ECS_REF( IO3O3P,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.387662E-20, 2.873049E-20, 4.586660E-20, 3.580654E-20, 
     & 6.431878E-21, 6.521411E-23, 1.701844E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O3P,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.042856E-01, 4.334796E-01, 7.683176E-01, 
     & 8.964556E-01, 9.272730E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.042856E-01, 4.334796E-01, 7.683176E-01, 
     & 8.964556E-01, 9.272730E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.042856E-01, 4.334796E-01, 7.683176E-01, 
     & 8.964556E-01, 9.272730E-01, 1.000000E+00 /


C...KETONE
C..  MEK absorption cross sections
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P8
C..  This datasheet updated: 5th December 2005.
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/.
C..  Note that recommended quantum yield is 0.34 and SAPRC07T sets
C..  value to 0.175 in mechanism definition file
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IKETONE ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IKETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.986233E-25, 0.000000E+00 /
      DATA ( CS_REF( IKETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.986233E-25, 0.000000E+00 /
      DATA ( CS_REF( IKETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.986233E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IKETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.689541E-02, 0.000000E+00 /
      DATA ( QY_REF(  IKETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.689541E-02, 0.000000E+00 /
      DATA ( QY_REF(  IKETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.689541E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IKETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.984605E-25, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.984605E-25, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.163253E-20, 2.690433E-20, 1.567194E-20, 7.784590E-21, 
     & 8.652938E-22, 1.984605E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IKETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.991803E-01, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.991803E-01, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.991803E-01, 0.000000E+00 /


C...MGLY_ABS
C..  Methyl Glyoxal Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_ABS ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ABS,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /
      DATA ( CS_REF( IMGLY_ABS,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /
      DATA ( CS_REF( IMGLY_ABS,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_ABS,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.908495E-01 /
      DATA ( QY_REF(  IMGLY_ABS,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.908495E-01 /
      DATA ( QY_REF(  IMGLY_ABS,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.908495E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ABS,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /
      DATA ( ECS_REF( IMGLY_ABS,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /
      DATA ( ECS_REF( IMGLY_ABS,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 1.505659E-20 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_ABS,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IMGLY_ABS,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IMGLY_ABS,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /


C...MGLY_ADJ
C..  MGLY + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_ADJ ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ADJ,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 3.074720E-21 /
      DATA ( CS_REF( IMGLY_ADJ,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 3.074720E-21 /
      DATA ( CS_REF( IMGLY_ADJ,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.164736E-21, 3.464746E-20, 3.074720E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_ADJ,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.993793E-01, 4.093204E-01, 0.000000E+00 /
      DATA ( QY_REF(  IMGLY_ADJ,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.993793E-01, 4.093204E-01, 0.000000E+00 /
      DATA ( QY_REF(  IMGLY_ADJ,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.993793E-01, 4.093204E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ADJ,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.162925E-21, 6.143782E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.162925E-21, 6.143782E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.410857E-20, 3.510266E-20, 2.357658E-20, 1.820124E-20, 
     & 6.162925E-21, 6.143782E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_ADJ,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.997061E-01, 1.773227E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.997061E-01, 1.773227E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.997061E-01, 1.773227E-01, 0.000000E+00 /


C...ACETONE
C..  CH3-CO-CH3 + HV = Radical products
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation – Data Sheet P7
C..  This datasheet updated: 19th December 2005.
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/.
C..  IUPAC recommendations used for both absorption cross sections and quantum yield
C..  Cross sections and quantum yields are for 298K only.
C..  Uncertain whether the cross sections are calculated correctly from the complex
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IACETONE ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.460821E-20, 2.278628E-20, 1.362906E-20, 7.428147E-21, 
     & 9.464437E-22, 9.957367E-25, 0.000000E+00 /
      DATA ( CS_REF( IACETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.460821E-20, 2.278628E-20, 1.362906E-20, 7.428147E-21, 
     & 9.464437E-22, 9.957367E-25, 0.000000E+00 /
      DATA ( CS_REF( IACETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.460821E-20, 2.278628E-20, 1.362906E-20, 7.428147E-21, 
     & 9.464437E-22, 9.957367E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.454557E-01, 2.098742E-01, 8.525195E-02, 3.909341E-02, 
     & 9.695551E-03, 1.622317E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.454557E-01, 2.098742E-01, 8.525195E-02, 3.909341E-02, 
     & 9.695551E-03, 1.622317E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.454557E-01, 2.098742E-01, 8.525195E-02, 3.909341E-02, 
     & 9.695551E-03, 1.622317E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.205182E-20, 4.966170E-21, 1.192478E-21, 3.105726E-22, 
     & 1.545213E-23, 2.812016E-27, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.205182E-20, 4.966170E-21, 1.192478E-21, 3.105726E-22, 
     & 1.545213E-23, 2.812016E-27, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.205182E-20, 4.966170E-21, 1.192478E-21, 3.105726E-22, 
     & 1.545213E-23, 2.812016E-27, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.482358E-01, 2.179456E-01, 8.749527E-02, 4.181024E-02, 
     & 1.632652E-02, 2.824056E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.482358E-01, 2.179456E-01, 8.749527E-02, 4.181024E-02, 
     & 1.632652E-02, 2.824056E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.482358E-01, 2.179456E-01, 8.749527E-02, 4.181024E-02, 
     & 1.632652E-02, 2.824056E-03, 0.000000E+00 /


      END MODULE CSQY_DATA
