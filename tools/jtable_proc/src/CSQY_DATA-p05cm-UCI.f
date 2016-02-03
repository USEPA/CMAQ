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
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 5.754674E-19, 1.195356E-20 /
      DATA ( CS_REF( INO2_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 5.754674E-19, 1.195356E-20 /
      DATA ( CS_REF( INO2_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 5.754674E-19, 1.195356E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO2_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.174767E-01, 7.500220E-04 /
      DATA ( QY_REF(  INO2_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.174767E-01, 7.500220E-04 /
      DATA ( QY_REF(  INO2_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.174767E-01, 7.500220E-04 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 4.644383E-19, 4.476030E-22 /
      DATA ( ECS_REF( INO2_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 4.644383E-19, 4.476030E-22 /
      DATA ( ECS_REF( INO2_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 4.644383E-19, 4.476030E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.070627E-01, 3.744516E-02 /
      DATA ( EQY_REF( INO2_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.070627E-01, 3.744516E-02 /
      DATA ( EQY_REF( INO2_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.070627E-01, 3.744516E-02 /


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
     & 0.000000E+00, 0.000000E+00, 6.269962E-19 /
      DATA ( CS_REF( INO3NO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 6.269962E-19 /
      DATA ( CS_REF( INO3NO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 6.269962E-19 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO3NO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.298857E-02 /
      DATA ( QY_REF(  INO3NO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.298857E-02 /
      DATA ( QY_REF(  INO3NO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.298857E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 9.811753E-20 /
      DATA ( ECS_REF( INO3NO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 9.811753E-20 /
      DATA ( ECS_REF( INO3NO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 9.811753E-20 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.564883E-01 /
      DATA ( EQY_REF( INO3NO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.564883E-01 /
      DATA ( EQY_REF( INO3NO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.564883E-01 /


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
     & 0.000000E+00, 4.027451E-21, 1.227490E-18 /
      DATA ( CS_REF( INO3NO2_6,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.027451E-21, 1.227490E-18 /
      DATA ( CS_REF( INO3NO2_6,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.027451E-21, 1.227490E-18 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO3NO2_6,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.130039E-01, 4.268056E-01 /
      DATA ( QY_REF(  INO3NO2_6,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.130039E-01, 4.268056E-01 /
      DATA ( QY_REF(  INO3NO2_6,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.130039E-01, 4.268056E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO2_6,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.021672E-21, 7.834468E-19 /
      DATA ( ECS_REF( INO3NO2_6,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.021672E-21, 7.834468E-19 /
      DATA ( ECS_REF( INO3NO2_6,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.021672E-21, 7.834468E-19 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO2_6,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 9.985652E-01, 6.382512E-01 /
      DATA ( EQY_REF( INO3NO2_6,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 9.985652E-01, 6.382512E-01 /
      DATA ( EQY_REF( INO3NO2_6,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 9.985652E-01, 6.382512E-01 /


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
     & 8.538728E-19, 2.750705E-19, 1.063739E-19, 4.625938E-20, 
     & 6.599046E-21, 5.460110E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.538728E-19, 2.750705E-19, 1.063739E-19, 4.625938E-20, 
     & 6.599046E-21, 5.460110E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.538728E-19, 2.750705E-19, 1.063739E-19, 4.625938E-20, 
     & 6.599046E-21, 5.460110E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O1D_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929066E-01, 5.359897E-01, 2.218182E-01, 
     & 8.688402E-02, 4.278573E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929066E-01, 5.359897E-01, 2.218182E-01, 
     & 8.688402E-02, 4.278573E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929066E-01, 5.359897E-01, 2.218182E-01, 
     & 8.688402E-02, 4.278573E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O1D_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.684855E-19, 2.464146E-19, 6.078741E-20, 1.068696E-20, 
     & 6.625140E-22, 4.350830E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.684855E-19, 2.464146E-19, 6.078741E-20, 1.068696E-20, 
     & 6.625140E-22, 4.350830E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.684855E-19, 2.464146E-19, 6.078741E-20, 1.068696E-20, 
     & 6.625140E-22, 4.350830E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O1D_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.958234E-01, 5.714505E-01, 2.310225E-01, 
     & 1.003954E-01, 7.968391E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.958234E-01, 5.714505E-01, 2.310225E-01, 
     & 1.003954E-01, 7.968391E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.958234E-01, 5.714505E-01, 2.310225E-01, 
     & 1.003954E-01, 7.968391E-02, 0.000000E+00 /


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
     & 8.407954E-19, 2.749065E-19, 1.063739E-19, 4.625938E-20, 
     & 6.803406E-21, 6.139220E-23, 1.666851E-21 /
      DATA ( CS_REF( IO3O3P_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.407954E-19, 2.749065E-19, 1.063739E-19, 4.625938E-20, 
     & 6.803406E-21, 6.139220E-23, 1.666851E-21 /
      DATA ( CS_REF( IO3O3P_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.407954E-19, 2.749065E-19, 1.063739E-19, 4.625938E-20, 
     & 6.803406E-21, 6.139220E-23, 1.666851E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O3P_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070934E-01, 4.640103E-01, 7.781818E-01, 
     & 9.123903E-01, 9.572143E-01, 9.795594E-01 /
      DATA ( QY_REF(  IO3O3P_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070934E-01, 4.640103E-01, 7.781818E-01, 
     & 9.123903E-01, 9.572143E-01, 9.795594E-01 /
      DATA ( QY_REF(  IO3O3P_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070934E-01, 4.640103E-01, 7.781818E-01, 
     & 9.123903E-01, 9.572143E-01, 9.795594E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O3P_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.407954E-20, 2.863951E-20, 4.558648E-20, 3.557242E-20, 
     & 6.109183E-21, 5.704138E-23, 1.666851E-21 /
      DATA ( ECS_REF( IO3O3P_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.407954E-20, 2.863951E-20, 4.558648E-20, 3.557242E-20, 
     & 6.109183E-21, 5.704138E-23, 1.666851E-21 /
      DATA ( ECS_REF( IO3O3P_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.407954E-20, 2.863951E-20, 4.558648E-20, 3.557242E-20, 
     & 6.109183E-21, 5.704138E-23, 1.666851E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O3P_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.041791E-01, 4.285495E-01, 7.689775E-01, 
     & 8.979595E-01, 9.291306E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.041791E-01, 4.285495E-01, 7.689775E-01, 
     & 8.979595E-01, 9.291306E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.041791E-01, 4.285495E-01, 7.689775E-01, 
     & 8.979595E-01, 9.291306E-01, 1.000000E+00 /


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
     & 5.486961E-21, 9.335561E-21, 1.881716E-20, 3.188688E-20, 
     & 9.007190E-20, 6.903974E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.486961E-21, 9.335561E-21, 1.881716E-20, 3.188688E-20, 
     & 9.007190E-20, 6.903974E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.486961E-21, 9.335561E-21, 1.881716E-20, 3.188688E-20, 
     & 9.007190E-20, 6.903974E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHONO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.771221E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.771221E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.771221E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.486961E-21, 9.335561E-21, 1.881716E-20, 3.188688E-20, 
     & 9.007190E-20, 6.903974E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.486961E-21, 9.335561E-21, 1.881716E-20, 3.188688E-20, 
     & 9.007190E-20, 6.903974E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.486961E-21, 9.335561E-21, 1.881716E-20, 3.188688E-20, 
     & 9.007190E-20, 6.903974E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /


C...HNO3
C..  HNO3 + HV = products
C..  IUPAC (1997) Recommendation

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHNO3 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO3,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875268E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875268E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875268E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHNO3,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO3,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875251E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875251E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875251E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO3,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999956E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999956E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999956E-01, 0.000000E+00 /


C...HNO4-06
C..  HO2NO2 + HV = PRODUCTS
C..  NASA (2006)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHNO4_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO4_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.685089E-20, 1.173542E-20, 5.713448E-21, 3.110138E-21, 
     & 8.269172E-22, 2.119340E-23, 0.000000E+00 /
      DATA ( CS_REF( IHNO4_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.685089E-20, 1.173542E-20, 5.713448E-21, 3.110138E-21, 
     & 8.269172E-22, 2.119340E-23, 0.000000E+00 /
      DATA ( CS_REF( IHNO4_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.685089E-20, 1.173542E-20, 5.713448E-21, 3.110138E-21, 
     & 8.269172E-22, 2.119340E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHNO4_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.684180E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO4_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.684180E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO4_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.684180E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO4_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.685089E-20, 1.173542E-20, 5.713448E-21, 3.110138E-21, 
     & 8.269172E-22, 2.119337E-23, 0.000000E+00 /
      DATA ( ECS_REF( IHNO4_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.685089E-20, 1.173542E-20, 5.713448E-21, 3.110138E-21, 
     & 8.269172E-22, 2.119337E-23, 0.000000E+00 /
      DATA ( ECS_REF( IHNO4_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.685089E-20, 1.173542E-20, 5.713448E-21, 3.110138E-21, 
     & 8.269172E-22, 2.119337E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO4_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999983E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO4_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999983E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO4_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999983E-01, 0.000000E+00 /


C...H2O2
C..  H2O2 + HV = 2 OH
C..  IUPAC (1997) Recommended.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IH2O2 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IH2O2,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606306E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606306E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606306E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IH2O2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF(  IH2O2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF(  IH2O2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IH2O2,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606289E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606289E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606289E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IH2O2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999954E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999954E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999954E-01, 0.000000E+00 /


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
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 5.754674E-19, 1.067281E-19 /
      DATA ( CS_REF( INO2EX,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 5.754674E-19, 1.067281E-19 /
      DATA ( CS_REF( INO2EX,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 5.754674E-19, 1.067281E-19 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO2EX,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.825233E-01, 5.823866E-01 /
      DATA ( QY_REF(  INO2EX,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.825233E-01, 5.823866E-01 /
      DATA ( QY_REF(  INO2EX,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.825233E-01, 5.823866E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2EX,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.110291E-19, 1.062805E-19 /
      DATA ( ECS_REF( INO2EX,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.110291E-19, 1.062805E-19 /
      DATA ( ECS_REF( INO2EX,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.110291E-19, 1.062805E-19 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2EX,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.929373E-01, 9.958061E-01 /
      DATA ( EQY_REF( INO2EX,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.929373E-01, 9.958061E-01 /
      DATA ( EQY_REF( INO2EX,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.929373E-01, 9.958061E-01 /


C...PAN
C..  PAN + HV = #.6 {MECO3 + NO2} + #.4 {MEO2 + CO2 + NO3}
C..  IUPAC Data Sheet P21.updated: 12/19/05
C..  Reaction reflects recommended quantum yields at 308 nm
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IPAN ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPAN,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.597520E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.597520E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.597520E-24, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IPAN,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.009612E-01, 0.000000E+00 /
      DATA ( QY_REF(  IPAN,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.009612E-01, 0.000000E+00 /
      DATA ( QY_REF(  IPAN,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.009612E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPAN,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.597511E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.597511E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.597511E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPAN,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999946E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999946E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999946E-01, 0.000000E+00 /


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
     & 3.170115E-20, 3.298361E-20, 1.534595E-20, 3.259295E-20, 
     & 1.516365E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHOR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.170115E-20, 3.298361E-20, 1.534595E-20, 3.259295E-20, 
     & 1.516365E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHOR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.170115E-20, 3.298361E-20, 1.534595E-20, 3.259295E-20, 
     & 1.516365E-20, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHCHOR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.851836E-01, 7.182512E-01, 7.159075E-01, 6.839373E-01, 
     & 2.830292E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHCHOR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.851836E-01, 7.182512E-01, 7.159075E-01, 6.839373E-01, 
     & 2.830292E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHCHOR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.851836E-01, 7.182512E-01, 7.159075E-01, 6.839373E-01, 
     & 2.830292E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHOR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.165864E-20, 2.395194E-20, 1.104754E-20, 2.236421E-20, 
     & 5.554628E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.165864E-20, 2.395194E-20, 1.104754E-20, 2.236421E-20, 
     & 5.554628E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.165864E-20, 2.395194E-20, 1.104754E-20, 2.236421E-20, 
     & 5.554628E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHOR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.832129E-01, 7.261769E-01, 7.198996E-01, 6.861672E-01, 
     & 3.663122E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.832129E-01, 7.261769E-01, 7.198996E-01, 6.861672E-01, 
     & 3.663122E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.832129E-01, 7.261769E-01, 7.198996E-01, 6.861672E-01, 
     & 3.663122E-01, 0.000000E+00, 0.000000E+00 /


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
     & 3.170115E-20, 3.298361E-20, 1.534595E-20, 3.259295E-20, 
     & 1.806860E-20, 7.417042E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHOM_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.170115E-20, 3.298361E-20, 1.534595E-20, 3.259295E-20, 
     & 1.806860E-20, 7.417042E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHOM_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.170115E-20, 3.298361E-20, 1.534595E-20, 3.259295E-20, 
     & 1.806860E-20, 7.417042E-22, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHCHOM_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.954926E-01, 2.804151E-01, 2.842610E-01, 3.165711E-01, 
     & 4.879651E-01, 3.037100E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHCHOM_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.954926E-01, 2.804151E-01, 2.842610E-01, 3.165711E-01, 
     & 4.879651E-01, 3.037100E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHCHOM_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.954926E-01, 2.804151E-01, 2.842610E-01, 3.165711E-01, 
     & 4.879651E-01, 3.037100E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHOM_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.456341E-21, 8.996975E-21, 4.301307E-21, 1.024271E-20, 
     & 8.938935E-21, 1.182654E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOM_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.456341E-21, 8.996975E-21, 4.301307E-21, 1.024271E-20, 
     & 8.938935E-21, 1.182654E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHOM_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.456341E-21, 8.996975E-21, 4.301307E-21, 1.024271E-20, 
     & 8.938935E-21, 1.182654E-22, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHOM_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.982965E-01, 2.727711E-01, 2.802895E-01, 3.142615E-01, 
     & 4.947222E-01, 1.594508E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOM_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.982965E-01, 2.727711E-01, 2.802895E-01, 3.142615E-01, 
     & 4.947222E-01, 1.594508E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHOM_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.982965E-01, 2.727711E-01, 2.802895E-01, 3.142615E-01, 
     & 4.947222E-01, 1.594508E-01, 0.000000E+00 /


C...CCHO_R
C..  CCHO + HV = CH3 + CHO
C..  IUPAC (1997)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICCHO_R ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICCHO_R,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.432586E-20, 3.717937E-20, 2.933103E-20, 2.104728E-20, 
     & 3.715596E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.432586E-20, 3.717937E-20, 2.933103E-20, 2.104728E-20, 
     & 3.715596E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.432586E-20, 3.717937E-20, 2.933103E-20, 2.104728E-20, 
     & 3.715596E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICCHO_R,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902037E-01, 2.845214E-01, 1.538485E-01, 
     & 1.409402E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  ICCHO_R,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902037E-01, 2.845214E-01, 1.538485E-01, 
     & 1.409402E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  ICCHO_R,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902037E-01, 2.845214E-01, 1.538485E-01, 
     & 1.409402E-02, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICCHO_R,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461038E-20, 8.426962E-21, 3.330092E-21, 
     & 1.731218E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461038E-20, 8.426962E-21, 3.330092E-21, 
     & 1.731218E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461038E-20, 8.426962E-21, 3.330092E-21, 
     & 1.731218E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICCHO_R,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.845355E-01, 3.929700E-01, 2.873054E-01, 1.582196E-01, 
     & 4.659327E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.845355E-01, 3.929700E-01, 2.873054E-01, 1.582196E-01, 
     & 4.659327E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.845355E-01, 3.929700E-01, 2.873054E-01, 1.582196E-01, 
     & 4.659327E-02, 0.000000E+00, 0.000000E+00 /


C...C2CHO
C..  C2CHO + HV = C2H5. + CHO.
C..  IUPAC (1997)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IC2CHO ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IC2CHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634689E-20, 3.579653E-20, 2.441742E-20, 
     & 5.808827E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634689E-20, 3.579653E-20, 2.441742E-20, 
     & 5.808827E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634689E-20, 3.579653E-20, 2.441742E-20, 
     & 5.808827E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IC2CHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.112442E-01, 7.954021E-01, 5.951666E-01, 4.312297E-01, 
     & 1.520064E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IC2CHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.112442E-01, 7.954021E-01, 5.951666E-01, 4.312297E-01, 
     & 1.520064E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IC2CHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.112442E-01, 7.954021E-01, 5.951666E-01, 4.312297E-01, 
     & 1.520064E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IC2CHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.730256E-20, 3.713801E-20, 2.133677E-20, 1.077360E-20, 
     & 1.383933E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.730256E-20, 3.713801E-20, 2.133677E-20, 1.077360E-20, 
     & 1.383933E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.730256E-20, 3.713801E-20, 2.133677E-20, 1.077360E-20, 
     & 1.383933E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IC2CHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.769099E-01, 8.013052E-01, 5.960570E-01, 4.412259E-01, 
     & 2.382466E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.769099E-01, 8.013052E-01, 5.960570E-01, 4.412259E-01, 
     & 2.382466E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.769099E-01, 8.013052E-01, 5.960570E-01, 4.412259E-01, 
     & 2.382466E-01, 0.000000E+00, 0.000000E+00 /


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
     & 3.464058E-20, 2.276631E-20, 1.367786E-20, 7.373137E-21, 
     & 8.850609E-22, 8.179470E-25, 0.000000E+00 /
      DATA ( CS_REF( IACET_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.464058E-20, 2.276631E-20, 1.367786E-20, 7.373137E-21, 
     & 8.850609E-22, 8.179470E-25, 0.000000E+00 /
      DATA ( CS_REF( IACET_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.464058E-20, 2.276631E-20, 1.367786E-20, 7.373137E-21, 
     & 8.850609E-22, 8.179470E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACET_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.457704E-01, 2.094483E-01, 8.573251E-02, 3.882141E-02, 
     & 9.387424E-03, 1.342100E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACET_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.457704E-01, 2.094483E-01, 8.573251E-02, 3.882141E-02, 
     & 9.387424E-03, 1.342100E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACET_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.457704E-01, 2.094483E-01, 8.573251E-02, 3.882141E-02, 
     & 9.387424E-03, 1.342100E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACET_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.207275E-20, 4.950919E-21, 1.203532E-21, 3.067005E-22, 
     & 1.419284E-23, 2.303166E-27, 0.000000E+00 /
      DATA ( ECS_REF( IACET_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.207275E-20, 4.950919E-21, 1.203532E-21, 3.067005E-22, 
     & 1.419284E-23, 2.303166E-27, 0.000000E+00 /
      DATA ( ECS_REF( IACET_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.207275E-20, 4.950919E-21, 1.203532E-21, 3.067005E-22, 
     & 1.419284E-23, 2.303166E-27, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACET_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.485148E-01, 2.174669E-01, 8.799125E-02, 4.159702E-02, 
     & 1.603601E-02, 2.815788E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACET_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.485148E-01, 2.174669E-01, 8.799125E-02, 4.159702E-02, 
     & 1.603601E-02, 2.815788E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACET_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.485148E-01, 2.174669E-01, 8.799125E-02, 4.159702E-02, 
     & 1.603601E-02, 2.815788E-03, 0.000000E+00 /


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
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609984E-25, 0.000000E+00 /
      DATA ( CS_REF( IMEK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609984E-25, 0.000000E+00 /
      DATA ( CS_REF( IMEK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609984E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMEK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.161979E-02, 0.000000E+00 /
      DATA ( QY_REF(  IMEK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.161979E-02, 0.000000E+00 /
      DATA ( QY_REF(  IMEK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.161979E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMEK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609804E-25, 0.000000E+00 /
      DATA ( ECS_REF( IMEK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609804E-25, 0.000000E+00 /
      DATA ( ECS_REF( IMEK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609804E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMEK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.998884E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMEK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.998884E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMEK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.998884E-01, 0.000000E+00 /


C...COOH
C..  CH3OOH + HV = PRODUCTS
C..  IUPAC (1997).  Also recommend unit quantum yields.
C..  wl      abs        qy

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICOOH ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICOOH,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395097E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395097E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395097E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICOOH,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICOOH,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICOOH,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICOOH,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395090E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395090E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395090E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICOOH,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999989E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999989E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999989E-01, 0.000000E+00 /


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
     & 3.792117E-20, 3.572108E-20, 3.194423E-20, 2.574261E-20, 
     & 1.106789E-20, 2.757124E-20, 1.054196E-20 /
      DATA ( CS_REF( IGLY_07R,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.792117E-20, 3.572108E-20, 3.194423E-20, 2.574261E-20, 
     & 1.106789E-20, 2.757124E-20, 1.054196E-20 /
      DATA ( CS_REF( IGLY_07R,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.792117E-20, 3.572108E-20, 3.194423E-20, 2.574261E-20, 
     & 1.106789E-20, 2.757124E-20, 1.054196E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IGLY_07R,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.321564E-01, 3.435688E-01, 3.690092E-01, 4.286592E-01, 
     & 5.846431E-01, 2.414760E-01, 1.331258E-03 /
      DATA ( QY_REF(  IGLY_07R,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.321564E-01, 3.435688E-01, 3.690092E-01, 4.286592E-01, 
     & 5.846431E-01, 2.414760E-01, 1.331258E-03 /
      DATA ( QY_REF(  IGLY_07R,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.321564E-01, 3.435688E-01, 3.690092E-01, 4.286592E-01, 
     & 5.846431E-01, 2.414760E-01, 1.331258E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_07R,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.259928E-20, 1.226096E-20, 1.179645E-20, 1.092393E-20, 
     & 6.158653E-21, 3.111813E-21, 1.360347E-22 /
      DATA ( ECS_REF( IGLY_07R,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.259928E-20, 1.226096E-20, 1.179645E-20, 1.092393E-20, 
     & 6.158653E-21, 3.111813E-21, 1.360347E-22 /
      DATA ( ECS_REF( IGLY_07R,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.259928E-20, 1.226096E-20, 1.179645E-20, 1.092393E-20, 
     & 6.158653E-21, 3.111813E-21, 1.360347E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_07R,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.322493E-01, 3.432416E-01, 3.692827E-01, 4.243520E-01, 
     & 5.564435E-01, 1.128644E-01, 1.290412E-02 /
      DATA ( EQY_REF( IGLY_07R,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.322493E-01, 3.432416E-01, 3.692827E-01, 4.243520E-01, 
     & 5.564435E-01, 1.128644E-01, 1.290412E-02 /
      DATA ( EQY_REF( IGLY_07R,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.322493E-01, 3.432416E-01, 3.692827E-01, 4.243520E-01, 
     & 5.564435E-01, 1.128644E-01, 1.290412E-02 /


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
     & 3.792117E-20, 3.572108E-20, 3.194423E-20, 2.574261E-20, 
     & 1.106789E-20, 2.757124E-20, 3.572554E-21 /
      DATA ( CS_REF( IGLY_07M,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.792117E-20, 3.572108E-20, 3.194423E-20, 2.574261E-20, 
     & 1.106789E-20, 2.757124E-20, 3.572554E-21 /
      DATA ( CS_REF( IGLY_07M,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.792117E-20, 3.572108E-20, 3.194423E-20, 2.574261E-20, 
     & 1.106789E-20, 2.757124E-20, 3.572554E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IGLY_07M,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.678436E-01, 6.564312E-01, 6.310096E-01, 5.714539E-01, 
     & 4.154251E-01, 6.007637E-02, 5.018706E-05 /
      DATA ( QY_REF(  IGLY_07M,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.678436E-01, 6.564312E-01, 6.310096E-01, 5.714539E-01, 
     & 4.154251E-01, 6.007637E-02, 5.018706E-05 /
      DATA ( QY_REF(  IGLY_07M,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.678436E-01, 6.564312E-01, 6.310096E-01, 5.714539E-01, 
     & 4.154251E-01, 6.007637E-02, 5.018706E-05 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_07M,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.532189E-20, 2.346012E-20, 2.014841E-20, 1.482248E-20, 
     & 4.909979E-21, 5.296770E-22, 4.592936E-24 /
      DATA ( ECS_REF( IGLY_07M,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.532189E-20, 2.346012E-20, 2.014841E-20, 1.482248E-20, 
     & 4.909979E-21, 5.296770E-22, 4.592936E-24 /
      DATA ( ECS_REF( IGLY_07M,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.532189E-20, 2.346012E-20, 2.014841E-20, 1.482248E-20, 
     & 4.909979E-21, 5.296770E-22, 4.592936E-24 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_07M,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.677508E-01, 6.567584E-01, 6.307371E-01, 5.757954E-01, 
     & 4.436239E-01, 1.921121E-02, 1.285617E-03 /
      DATA ( EQY_REF( IGLY_07M,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.677508E-01, 6.567584E-01, 6.307371E-01, 5.757954E-01, 
     & 4.436239E-01, 1.921121E-02, 1.285617E-03 /
      DATA ( EQY_REF( IGLY_07M,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.677508E-01, 6.567584E-01, 6.307371E-01, 5.757954E-01, 
     & 4.436239E-01, 1.921121E-02, 1.285617E-03 /


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
     & 4.380391E-20, 3.467071E-20, 2.430916E-20, 1.788232E-20, 
     & 6.183676E-21, 3.700269E-20, 7.910760E-21 /
      DATA ( CS_REF( IMGLY_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.380391E-20, 3.467071E-20, 2.430916E-20, 1.788232E-20, 
     & 6.183676E-21, 3.700269E-20, 7.910760E-21 /
      DATA ( CS_REF( IMGLY_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.380391E-20, 3.467071E-20, 2.430916E-20, 1.788232E-20, 
     & 6.183676E-21, 3.700269E-20, 7.910760E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.565113E-01, 9.283988E-01, 8.962741E-01, 8.564202E-01, 
     & 7.065107E-01, 2.411670E-01, 3.970909E-03 /
      DATA ( QY_REF(  IMGLY_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.565113E-01, 9.283988E-01, 8.962741E-01, 8.564202E-01, 
     & 7.065107E-01, 2.411670E-01, 3.970909E-03 /
      DATA ( QY_REF(  IMGLY_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.565113E-01, 9.283988E-01, 8.962741E-01, 8.564202E-01, 
     & 7.065107E-01, 2.411670E-01, 3.970909E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.191153E-20, 3.221663E-20, 2.180534E-20, 1.534382E-20, 
     & 4.571303E-21, 5.327231E-21, 3.199066E-22 /
      DATA ( ECS_REF( IMGLY_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.191153E-20, 3.221663E-20, 2.180534E-20, 1.534382E-20, 
     & 4.571303E-21, 5.327231E-21, 3.199066E-22 /
      DATA ( ECS_REF( IMGLY_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.191153E-20, 3.221663E-20, 2.180534E-20, 1.534382E-20, 
     & 4.571303E-21, 5.327231E-21, 3.199066E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.567987E-01, 9.292175E-01, 8.970010E-01, 8.580443E-01, 
     & 7.392533E-01, 1.439687E-01, 4.043944E-02 /
      DATA ( EQY_REF( IMGLY_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.567987E-01, 9.292175E-01, 8.970010E-01, 8.580443E-01, 
     & 7.392533E-01, 1.439687E-01, 4.043944E-02 /
      DATA ( EQY_REF( IMGLY_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.567987E-01, 9.292175E-01, 8.970010E-01, 8.580443E-01, 
     & 7.392533E-01, 1.439687E-01, 4.043944E-02 /


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
     & 2.658386E-20, 1.613232E-20, 9.254814E-21, 6.071191E-21, 
     & 4.661714E-21, 3.201369E-20, 4.709775E-21 /
      DATA ( CS_REF( IBACL_07,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.658386E-20, 1.613232E-20, 9.254814E-21, 6.071191E-21, 
     & 4.661714E-21, 3.201369E-20, 4.709775E-21 /
      DATA ( CS_REF( IBACL_07,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.658386E-20, 1.613232E-20, 9.254814E-21, 6.071191E-21, 
     & 4.661714E-21, 3.201369E-20, 4.709775E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IBACL_07,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.894450E-01, 9.817067E-01, 9.728644E-01, 9.609106E-01, 
     & 9.065533E-01, 4.913754E-01, 9.660118E-03 /
      DATA ( QY_REF(  IBACL_07,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.894450E-01, 9.817067E-01, 9.728644E-01, 9.609106E-01, 
     & 9.065533E-01, 4.913754E-01, 9.660118E-03 /
      DATA ( QY_REF(  IBACL_07,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.894450E-01, 9.817067E-01, 9.728644E-01, 9.609106E-01, 
     & 9.065533E-01, 4.913754E-01, 9.660118E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBACL_07,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.630803E-20, 1.584373E-20, 9.006348E-21, 5.836824E-21, 
     & 4.210301E-21, 1.187160E-20, 4.906409E-22 /
      DATA ( ECS_REF( IBACL_07,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.630803E-20, 1.584373E-20, 9.006348E-21, 5.836824E-21, 
     & 4.210301E-21, 1.187160E-20, 4.906409E-22 /
      DATA ( ECS_REF( IBACL_07,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.630803E-20, 1.584373E-20, 9.006348E-21, 5.836824E-21, 
     & 4.210301E-21, 1.187160E-20, 4.906409E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBACL_07,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.896241E-01, 9.821110E-01, 9.731528E-01, 9.613968E-01, 
     & 9.031659E-01, 3.708287E-01, 1.041750E-01 /
      DATA ( EQY_REF( IBACL_07,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.896241E-01, 9.821110E-01, 9.731528E-01, 9.613968E-01, 
     & 9.031659E-01, 3.708287E-01, 1.041750E-01 /
      DATA ( EQY_REF( IBACL_07,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.896241E-01, 9.821110E-01, 9.731528E-01, 9.613968E-01, 
     & 9.031659E-01, 3.708287E-01, 1.041750E-01 /


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
     & 4.451981E-19, 7.297634E-20, 7.090561E-20, 8.592461E-20, 
     & 9.197932E-20, 1.679405E-20, 0.000000E+00 /
      DATA ( CS_REF( IBALD_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.451981E-19, 7.297634E-20, 7.090561E-20, 8.592461E-20, 
     & 9.197932E-20, 1.679405E-20, 0.000000E+00 /
      DATA ( CS_REF( IBALD_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.451981E-19, 7.297634E-20, 7.090561E-20, 8.592461E-20, 
     & 9.197932E-20, 1.679405E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IBALD_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.275655E-01, 0.000000E+00 /
      DATA ( QY_REF(  IBALD_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.275655E-01, 0.000000E+00 /
      DATA ( QY_REF(  IBALD_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.275655E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBALD_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.451981E-19, 7.297634E-20, 7.090561E-20, 8.592461E-20, 
     & 9.197932E-20, 1.679404E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBALD_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.451981E-19, 7.297634E-20, 7.090561E-20, 8.592461E-20, 
     & 9.197932E-20, 1.679404E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBALD_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.451981E-19, 7.297634E-20, 7.090561E-20, 8.592461E-20, 
     & 9.197932E-20, 1.679404E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBALD_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999996E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBALD_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999996E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBALD_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999996E-01, 0.000000E+00 /


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
     & 1.731773E-18, 1.412652E-18, 1.107261E-18, 8.915003E-19, 
     & 5.026284E-19, 1.273166E-19, 1.232364E-21 /
      DATA ( CS_REF( IAFG1,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.731773E-18, 1.412652E-18, 1.107261E-18, 8.915003E-19, 
     & 5.026284E-19, 1.273166E-19, 1.232364E-21 /
      DATA ( CS_REF( IAFG1,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.731773E-18, 1.412652E-18, 1.107261E-18, 8.915003E-19, 
     & 5.026284E-19, 1.273166E-19, 1.232364E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IAFG1,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /
      DATA ( QY_REF(  IAFG1,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /
      DATA ( QY_REF(  IAFG1,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IAFG1,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.731773E-18, 1.412652E-18, 1.107261E-18, 8.915003E-19, 
     & 5.026284E-19, 1.273166E-19, 1.232364E-21 /
      DATA ( ECS_REF( IAFG1,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.731773E-18, 1.412652E-18, 1.107261E-18, 8.915003E-19, 
     & 5.026284E-19, 1.273166E-19, 1.232364E-21 /
      DATA ( ECS_REF( IAFG1,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.731773E-18, 1.412652E-18, 1.107261E-18, 8.915003E-19, 
     & 5.026284E-19, 1.273166E-19, 1.232364E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IAFG1,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IAFG1,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IAFG1,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /


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
     & 2.639069E-20, 3.936243E-20, 4.989644E-20, 5.926228E-20, 
     & 6.528440E-20, 1.326860E-20, 0.000000E+00 /
      DATA ( CS_REF( IMACR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.639069E-20, 3.936243E-20, 4.989644E-20, 5.926228E-20, 
     & 6.528440E-20, 1.326860E-20, 0.000000E+00 /
      DATA ( CS_REF( IMACR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.639069E-20, 3.936243E-20, 4.989644E-20, 5.926228E-20, 
     & 6.528440E-20, 1.326860E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMACR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.901060E-02, 3.073938E-02, 2.101921E-02, 1.479159E-02, 
     & 6.251532E-03, 5.827893E-04, 0.000000E+00 /
      DATA ( QY_REF(  IMACR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.901060E-02, 3.073938E-02, 2.101921E-02, 1.479159E-02, 
     & 6.251532E-03, 5.827893E-04, 0.000000E+00 /
      DATA ( QY_REF(  IMACR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.901060E-02, 3.073938E-02, 2.101921E-02, 1.479159E-02, 
     & 6.251532E-03, 5.827893E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMACR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.276395E-21, 1.192334E-21, 1.045253E-21, 8.708182E-22, 
     & 4.101432E-22, 2.057185E-23, 0.000000E+00 /
      DATA ( ECS_REF( IMACR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.276395E-21, 1.192334E-21, 1.045253E-21, 8.708182E-22, 
     & 4.101432E-22, 2.057185E-23, 0.000000E+00 /
      DATA ( ECS_REF( IMACR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.276395E-21, 1.192334E-21, 1.045253E-21, 8.708182E-22, 
     & 4.101432E-22, 2.057185E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMACR_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.836535E-02, 3.029118E-02, 2.094846E-02, 1.469431E-02, 
     & 6.282408E-03, 1.550416E-03, 0.000000E+00 /
      DATA ( EQY_REF( IMACR_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.836535E-02, 3.029118E-02, 2.094846E-02, 1.469431E-02, 
     & 6.282408E-03, 1.550416E-03, 0.000000E+00 /
      DATA ( EQY_REF( IMACR_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.836535E-02, 3.029118E-02, 2.094846E-02, 1.469431E-02, 
     & 6.282408E-03, 1.550416E-03, 0.000000E+00 /


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
     & 3.041510E-20, 4.373022E-20, 5.436504E-20, 6.318700E-20, 
     & 6.828638E-20, 8.068900E-21, 0.000000E+00 /
      DATA ( CS_REF( IMVK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.041510E-20, 4.373022E-20, 5.436504E-20, 6.318700E-20, 
     & 6.828638E-20, 8.068900E-21, 0.000000E+00 /
      DATA ( CS_REF( IMVK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.041510E-20, 4.373022E-20, 5.436504E-20, 6.318700E-20, 
     & 6.828638E-20, 8.068900E-21, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMVK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.750365E-02, 1.113042E-02, 7.578317E-03, 5.286919E-03, 
     & 2.238234E-03, 1.619926E-04, 0.000000E+00 /
      DATA ( QY_REF(  IMVK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.750365E-02, 1.113042E-02, 7.578317E-03, 5.286919E-03, 
     & 2.238234E-03, 1.619926E-04, 0.000000E+00 /
      DATA ( QY_REF(  IMVK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.750365E-02, 1.113042E-02, 7.578317E-03, 5.286919E-03, 
     & 2.238234E-03, 1.619926E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMVK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.264579E-22, 4.799992E-22, 4.109015E-22, 3.323382E-22, 
     & 1.546971E-22, 7.828037E-24, 0.000000E+00 /
      DATA ( ECS_REF( IMVK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.264579E-22, 4.799992E-22, 4.109015E-22, 3.323382E-22, 
     & 1.546971E-22, 7.828037E-24, 0.000000E+00 /
      DATA ( ECS_REF( IMVK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.264579E-22, 4.799992E-22, 4.109015E-22, 3.323382E-22, 
     & 1.546971E-22, 7.828037E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMVK_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.730910E-02, 1.097637E-02, 7.558194E-03, 5.259598E-03, 
     & 2.265416E-03, 9.701491E-04, 0.000000E+00 /
      DATA ( EQY_REF( IMVK_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.730910E-02, 1.097637E-02, 7.558194E-03, 5.259598E-03, 
     & 2.265416E-03, 9.701491E-04, 0.000000E+00 /
      DATA ( EQY_REF( IMVK_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.730910E-02, 1.097637E-02, 7.558194E-03, 5.259598E-03, 
     & 2.265416E-03, 9.701491E-04, 0.000000E+00 /


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
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667180E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667180E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667180E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IIC3ONO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IIC3ONO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IIC3ONO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IIC3ONO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667175E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667175E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667175E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IIC3ONO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /


C...HOCCHO_IUPAC
C..  HOCH2CHO + hv ---> products		
C..  IUPAC (2002) based on Bacher et al (2001), J. Atm. Chem, 39, 171.
C..  quantum yield = 0.75+/-0.25		
C..  lambda(nm) xcross(1.E+20*cm2) yield	

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHOCCHO_IUPAC ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHOCCHO_IUPAC,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.704782E-20, 2.277373E-20, 1.257586E-20, 5.852344E-21, 
     & 6.511950E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHOCCHO_IUPAC,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.704782E-20, 2.277373E-20, 1.257586E-20, 5.852344E-21, 
     & 6.511950E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHOCCHO_IUPAC,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.704782E-20, 2.277373E-20, 1.257586E-20, 5.852344E-21, 
     & 6.511950E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHOCCHO_IUPAC,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 4.315647E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHOCCHO_IUPAC,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 4.315647E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHOCCHO_IUPAC,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 4.315647E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHOCCHO_IUPAC,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.778587E-20, 1.708030E-20, 9.431893E-21, 4.389258E-21, 
     & 4.882468E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHOCCHO_IUPAC,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.778587E-20, 1.708030E-20, 9.431893E-21, 4.389258E-21, 
     & 4.882468E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHOCCHO_IUPAC,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.778587E-20, 1.708030E-20, 9.431893E-21, 4.389258E-21, 
     & 4.882468E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHOCCHO_IUPAC,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 7.497705E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHOCCHO_IUPAC,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 7.497705E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHOCCHO_IUPAC,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.500000E-01, 7.500000E-01, 7.500000E-01, 7.500000E-01, 
     & 7.497705E-01, 0.000000E+00, 0.000000E+00 /


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
     & 1.856128E-20, 2.787986E-20, 3.602616E-20, 4.321518E-20, 
     & 5.371941E-20, 1.543857E-20, 0.000000E+00 /
      DATA ( CS_REF( IACRO_09,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.856128E-20, 2.787986E-20, 3.602616E-20, 4.321518E-20, 
     & 5.371941E-20, 1.543857E-20, 0.000000E+00 /
      DATA ( CS_REF( IACRO_09,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.856128E-20, 2.787986E-20, 3.602616E-20, 4.321518E-20, 
     & 5.371941E-20, 1.543857E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACRO_09,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.332902E-02, 3.341169E-02, 2.275737E-02, 1.604885E-02, 
     & 6.814500E-03, 6.657408E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACRO_09,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.332902E-02, 3.341169E-02, 2.275737E-02, 1.604885E-02, 
     & 6.814500E-03, 6.657408E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACRO_09,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.332902E-02, 3.341169E-02, 2.275737E-02, 1.604885E-02, 
     & 6.814500E-03, 6.657408E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACRO_09,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.780021E-22, 9.183044E-22, 8.156418E-22, 6.894441E-22, 
     & 3.635461E-22, 2.508367E-23, 0.000000E+00 /
      DATA ( ECS_REF( IACRO_09,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.780021E-22, 9.183044E-22, 8.156418E-22, 6.894441E-22, 
     & 3.635461E-22, 2.508367E-23, 0.000000E+00 /
      DATA ( ECS_REF( IACRO_09,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.780021E-22, 9.183044E-22, 8.156418E-22, 6.894441E-22, 
     & 3.635461E-22, 2.508367E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACRO_09,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.269045E-02, 3.293790E-02, 2.264026E-02, 1.595375E-02, 
     & 6.767500E-03, 1.624741E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACRO_09,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.269045E-02, 3.293790E-02, 2.264026E-02, 1.595375E-02, 
     & 6.767500E-03, 1.624741E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACRO_09,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.269045E-02, 3.293790E-02, 2.264026E-02, 1.595375E-02, 
     & 6.767500E-03, 1.624741E-03, 0.000000E+00 /


C...PAA
C..  Peroxy acetic acid absorption cross sections
C..  Orlando, J. J. and G. S. Tyndall (2003): "Gas phase UV absorption spectra
C..  for peracetic acid, and for acetic acid monomers and dimers," J. Photochem.
C..  Photobiol A, 157, 161-166.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IPAA ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPAA,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.880973E-21, 9.984294E-22, 5.884834E-22, 3.735140E-22, 
     & 1.059116E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPAA,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.880973E-21, 9.984294E-22, 5.884834E-22, 3.735140E-22, 
     & 1.059116E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPAA,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.880973E-21, 9.984294E-22, 5.884834E-22, 3.735140E-22, 
     & 1.059116E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IPAA,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.624107E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IPAA,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.624107E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IPAA,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.624107E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPAA,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.880973E-21, 9.984294E-22, 5.884834E-22, 3.735140E-22, 
     & 1.059115E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPAA,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.880973E-21, 9.984294E-22, 5.884834E-22, 3.735140E-22, 
     & 1.059115E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPAA,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.880973E-21, 9.984294E-22, 5.884834E-22, 3.735140E-22, 
     & 1.059115E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPAA,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999991E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPAA,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999991E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPAA,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999991E-01, 0.000000E+00, 0.000000E+00 /


C...CL2
C..  CL2 + HV = 2 CL 	
C.. 	FROM IUPAC EVALUATION (1996)
C.. 	RECOMMEND UNIT QUANTUM YIELD
C.. 	

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICL2 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICL2,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.311132E-22 /
      DATA ( CS_REF( ICL2,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.311132E-22 /
      DATA ( CS_REF( ICL2,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.311132E-22 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICL2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.368807E-01 /
      DATA ( QY_REF(  ICL2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.368807E-01 /
      DATA ( QY_REF(  ICL2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.368807E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICL2,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.311132E-22 /
      DATA ( ECS_REF( ICL2,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.311132E-22 /
      DATA ( ECS_REF( ICL2,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.311132E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICL2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /
      DATA ( EQY_REF( ICL2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /
      DATA ( EQY_REF( ICL2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /


C...CLNO-06
C..  CLNO absorption cross sections recommended by IUPAC (2005)
C..  Wavelength where absorption goes to zero est'd by extrapolation
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLNO_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLNO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.713532E-20, 1.043432E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /
      DATA ( CS_REF( ICLNO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.713532E-20, 1.043432E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /
      DATA ( CS_REF( ICLNO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.713532E-20, 1.043432E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLNO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.444760E-01 /
      DATA ( QY_REF(  ICLNO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.444760E-01 /
      DATA ( QY_REF(  ICLNO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 2.444760E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLNO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.713532E-20, 1.043432E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /
      DATA ( ECS_REF( ICLNO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.713532E-20, 1.043432E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /
      DATA ( ECS_REF( ICLNO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.713532E-20, 1.043432E-19, 1.150344E-19, 1.267321E-19, 
     & 1.479858E-19, 8.438991E-20, 4.594130E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLNO_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /
      DATA ( EQY_REF( ICLNO_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /
      DATA ( EQY_REF( ICLNO_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /


C...CLONO
C..  CLONO + HV = CL + NO2	
C.. 	FROM IUPAC EVALUATION (1996)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLONO ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLONO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205324E-19, 
     & 5.728580E-19, 7.394595E-20, 0.000000E+00 /
      DATA ( CS_REF( ICLONO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205324E-19, 
     & 5.728580E-19, 7.394595E-20, 0.000000E+00 /
      DATA ( CS_REF( ICLONO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205324E-19, 
     & 5.728580E-19, 7.394595E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLONO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.317809E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLONO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.317809E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLONO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.317809E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLONO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205324E-19, 
     & 5.728580E-19, 7.394595E-20, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205324E-19, 
     & 5.728580E-19, 7.394595E-20, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.405580E-18, 1.198578E-18, 1.052515E-18, 9.205324E-19, 
     & 5.728580E-19, 7.394595E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLONO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /


C...CLNO2
C..  CLNO2 + HV = CL + NO2	
C.. 	FROM IUPAC EVALUATION (1996)
C.. 	RECOMMEND UNIT QUANTUM YILED

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLNO2 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLNO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258209E-20, 4.822250E-21, 0.000000E+00 /
      DATA ( CS_REF( ICLNO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258209E-20, 4.822250E-21, 0.000000E+00 /
      DATA ( CS_REF( ICLNO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258209E-20, 4.822250E-21, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLNO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.412513E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLNO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.412513E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLNO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.412513E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLNO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258209E-20, 4.822249E-21, 0.000000E+00 /
      DATA ( ECS_REF( ICLNO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258209E-20, 4.822249E-21, 0.000000E+00 /
      DATA ( ECS_REF( ICLNO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.619476E-19, 1.400598E-19, 1.206790E-19, 1.001660E-19, 
     & 5.258209E-20, 4.822249E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLNO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999997E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLNO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999997E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLNO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999997E-01, 0.000000E+00 /


C...CLONO2-1
C..  CLONO2 + hv = CLO. + NO2
C..  CLONO2 absorption cross sections and quantum yields recommended by IUPAC (2005)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLONO2_1 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLONO2_1,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.653757E-20, 2.593168E-20, 1.621918E-20, 1.063002E-20, 
     & 4.392583E-21, 4.870603E-22, 0.000000E+00 /
      DATA ( CS_REF( ICLONO2_1,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.653757E-20, 2.593168E-20, 1.621918E-20, 1.063002E-20, 
     & 4.392583E-21, 4.870603E-22, 0.000000E+00 /
      DATA ( CS_REF( ICLONO2_1,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.653757E-20, 2.593168E-20, 1.621918E-20, 1.063002E-20, 
     & 4.392583E-21, 4.870603E-22, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLONO2_1,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.989907E-01, 3.830689E-01, 3.395569E-01, 
     & 2.198601E-01, 1.467996E-02, 0.000000E+00 /
      DATA ( QY_REF(  ICLONO2_1,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.989907E-01, 3.830689E-01, 3.395569E-01, 
     & 2.198601E-01, 1.467996E-02, 0.000000E+00 /
      DATA ( QY_REF(  ICLONO2_1,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.989907E-01, 3.830689E-01, 3.395569E-01, 
     & 2.198601E-01, 1.467996E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLONO2_1,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.861503E-20, 1.035218E-20, 6.224706E-21, 3.635974E-21, 
     & 1.037275E-21, 3.317175E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO2_1,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.861503E-20, 1.035218E-20, 6.224706E-21, 3.635974E-21, 
     & 1.037275E-21, 3.317175E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLONO2_1,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.861503E-20, 1.035218E-20, 6.224706E-21, 3.635974E-21, 
     & 1.037275E-21, 3.317175E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLONO2_1,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.992096E-01, 3.837866E-01, 3.420477E-01, 
     & 2.361424E-01, 6.810604E-02, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO2_1,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.992096E-01, 3.837866E-01, 3.420477E-01, 
     & 2.361424E-01, 6.810604E-02, 0.000000E+00 /
      DATA ( EQY_REF( ICLONO2_1,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.000000E-01, 3.992096E-01, 3.837866E-01, 3.420477E-01, 
     & 2.361424E-01, 6.810604E-02, 0.000000E+00 /


C...CLONO2-2
C..  CLONO2 + hv = CL. + NO3
C..  CLONO2 absorption cross sections and quantum yields recommended by IUPAC (2005)
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLONO2_2 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLONO2_2,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.653757E-20, 2.593168E-20, 1.621918E-20, 1.063002E-20, 
     & 4.392583E-21, 1.220202E-21, 9.579231E-24 /
      DATA ( CS_REF( ICLONO2_2,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.653757E-20, 2.593168E-20, 1.621918E-20, 1.063002E-20, 
     & 4.392583E-21, 1.220202E-21, 9.579231E-24 /
      DATA ( CS_REF( ICLONO2_2,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.653757E-20, 2.593168E-20, 1.621918E-20, 1.063002E-20, 
     & 4.392583E-21, 1.220202E-21, 9.579231E-24 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLONO2_2,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.010093E-01, 6.169312E-01, 6.604431E-01, 
     & 7.801399E-01, 9.853200E-01, 3.841701E-02 /
      DATA ( QY_REF(  ICLONO2_2,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.010093E-01, 6.169312E-01, 6.604431E-01, 
     & 7.801399E-01, 9.853200E-01, 3.841701E-02 /
      DATA ( QY_REF(  ICLONO2_2,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.010093E-01, 6.169312E-01, 6.604431E-01, 
     & 7.801399E-01, 9.853200E-01, 3.841701E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLONO2_2,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.792254E-20, 1.557950E-20, 9.994480E-21, 6.994048E-21, 
     & 3.355308E-21, 1.187030E-21, 9.579221E-24 /
      DATA ( ECS_REF( ICLONO2_2,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.792254E-20, 1.557950E-20, 9.994480E-21, 6.994048E-21, 
     & 3.355308E-21, 1.187030E-21, 9.579221E-24 /
      DATA ( ECS_REF( ICLONO2_2,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.792254E-20, 1.557950E-20, 9.994480E-21, 6.994048E-21, 
     & 3.355308E-21, 1.187030E-21, 9.579221E-24 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLONO2_2,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.007904E-01, 6.162134E-01, 6.579523E-01, 
     & 7.638576E-01, 9.728145E-01, 9.999989E-01 /
      DATA ( EQY_REF( ICLONO2_2,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.007904E-01, 6.162134E-01, 6.579523E-01, 
     & 7.638576E-01, 9.728145E-01, 9.999989E-01 /
      DATA ( EQY_REF( ICLONO2_2,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.000000E-01, 6.007904E-01, 6.162134E-01, 6.579523E-01, 
     & 7.638576E-01, 9.728145E-01, 9.999989E-01 /


C...HOCL-06
C..  HOCL absorption cross sections recommended by IUPAC (2005)
C..
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHOCL_06 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHOCL_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066935E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.283773E-23 /
      DATA ( CS_REF( IHOCL_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066935E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.283773E-23 /
      DATA ( CS_REF( IHOCL_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066935E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.283773E-23 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHOCL_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.671194E-02 /
      DATA ( QY_REF(  IHOCL_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.671194E-02 /
      DATA ( QY_REF(  IHOCL_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.671194E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHOCL_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066935E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.283764E-23 /
      DATA ( ECS_REF( IHOCL_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066935E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.283764E-23 /
      DATA ( ECS_REF( IHOCL_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066935E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.283764E-23 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHOCL_06,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999929E-01 /
      DATA ( EQY_REF( IHOCL_06,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999929E-01 /
      DATA ( EQY_REF( IHOCL_06,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999929E-01 /


C...CLCCHO
C..  Chloroacetaldehyde absorption cross sections
C..  NASA (2006) evaluation

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLCCHO ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLCCHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.379352E-20, 5.300264E-20, 4.849378E-20, 3.861017E-20, 
     & 1.286108E-20, 9.229011E-23, 0.000000E+00 /
      DATA ( CS_REF( ICLCCHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.379352E-20, 5.300264E-20, 4.849378E-20, 3.861017E-20, 
     & 1.286108E-20, 9.229011E-23, 0.000000E+00 /
      DATA ( CS_REF( ICLCCHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.379352E-20, 5.300264E-20, 4.849378E-20, 3.861017E-20, 
     & 1.286108E-20, 9.229011E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLCCHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.478604E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLCCHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.478604E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLCCHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.478604E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLCCHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.379352E-20, 5.300264E-20, 4.849378E-20, 3.861017E-20, 
     & 1.286108E-20, 9.228993E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLCCHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.379352E-20, 5.300264E-20, 4.849378E-20, 3.861017E-20, 
     & 1.286108E-20, 9.228993E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLCCHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.379352E-20, 5.300264E-20, 4.849378E-20, 3.861017E-20, 
     & 1.286108E-20, 9.228993E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLCCHO,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999981E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLCCHO,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999981E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLCCHO,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999981E-01, 0.000000E+00 /


C...CLACET
C..  Chloroacetone absorption cross sections
C..  Chloroacetone absorption cross sections
C..

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICLACET ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICLACET,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000121E-19, 8.662045E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132983E-20, 6.893275E-23, 0.000000E+00 /
      DATA ( CS_REF( ICLACET,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000121E-19, 8.662045E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132983E-20, 6.893275E-23, 0.000000E+00 /
      DATA ( CS_REF( ICLACET,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000121E-19, 8.662045E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132983E-20, 6.893275E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICLACET,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLACET,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICLACET,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICLACET,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000121E-19, 8.662045E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132983E-20, 6.893270E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLACET,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000121E-19, 8.662045E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132983E-20, 6.893270E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICLACET,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000121E-19, 8.662045E-20, 6.774627E-20, 4.765676E-20, 
     & 1.132983E-20, 6.893270E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICLACET,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999993E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLACET,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999993E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICLACET,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999993E-01, 0.000000E+00 /


C...NO2_SAPRC99
C..  NO2 + HV = NO + O
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.860793E-19, 2.249056E-19, 
     & 3.335557E-19, 5.492276E-19, 1.148337E-20 /
      DATA ( CS_REF( INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.860793E-19, 2.249056E-19, 
     & 3.335557E-19, 5.492276E-19, 1.148337E-20 /
      DATA ( CS_REF( INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.860793E-19, 2.249056E-19, 
     & 3.335557E-19, 5.492276E-19, 1.148337E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986851E-01, 9.908283E-01, 
     & 9.900000E-01, 7.922024E-01, 7.010630E-04 /
      DATA ( QY_REF(  INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986851E-01, 9.908283E-01, 
     & 9.900000E-01, 7.922024E-01, 7.010630E-04 /
      DATA ( QY_REF(  INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986851E-01, 9.908283E-01, 
     & 9.900000E-01, 7.922024E-01, 7.010630E-04 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.858225E-19, 2.228264E-19, 
     & 3.302202E-19, 4.272925E-19, 4.015366E-22 /
      DATA ( ECS_REF( INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.858225E-19, 2.228264E-19, 
     & 3.302202E-19, 4.272925E-19, 4.015366E-22 /
      DATA ( ECS_REF( INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.858225E-19, 2.228264E-19, 
     & 3.302202E-19, 4.272925E-19, 4.015366E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986201E-01, 9.907553E-01, 
     & 9.900000E-01, 7.779881E-01, 3.496679E-02 /
      DATA ( EQY_REF( INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986201E-01, 9.907553E-01, 
     & 9.900000E-01, 7.779881E-01, 3.496679E-02 /
      DATA ( EQY_REF( INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986201E-01, 9.907553E-01, 
     & 9.900000E-01, 7.779881E-01, 3.496679E-02 /


C...NO3NO_SAPRC99
C..  NO3 + HV = NO + O2 (T=298)
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO3NO_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.695913E-19 /
      DATA ( CS_REF( INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.695913E-19 /
      DATA ( CS_REF( INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.695913E-19 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.027933E-02 /
      DATA ( QY_REF(  INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.027933E-02 /
      DATA ( QY_REF(  INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.027933E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.172153E-20 /
      DATA ( ECS_REF( INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.172153E-20 /
      DATA ( ECS_REF( INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.172153E-20 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.434740E-01 /
      DATA ( EQY_REF( INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.434740E-01 /
      DATA ( EQY_REF( INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 1.434740E-01 /


C...NO3NO2_SAPRC99
C..  NO3 + HV = NO2 + O (T=298)
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO3NO2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 1.112198E-18 /
      DATA ( CS_REF( INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 1.112198E-18 /
      DATA ( CS_REF( INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 1.112198E-18 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.818170E-01, 4.333254E-01 /
      DATA ( QY_REF(  INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.818170E-01, 4.333254E-01 /
      DATA ( QY_REF(  INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.818170E-01, 4.333254E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 7.608258E-19 /
      DATA ( ECS_REF( INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 7.608258E-19 /
      DATA ( ECS_REF( INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 7.608258E-19 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.000000E+00, 6.840740E-01 /
      DATA ( EQY_REF( INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.000000E+00, 6.840740E-01 /
      DATA ( EQY_REF( INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 1.000000E+00, 6.840740E-01 /


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
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 6.115310E-21, 1.783046E-23, 1.650939E-21 /
      DATA ( CS_REF( IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 6.115310E-21, 1.783046E-23, 1.650939E-21 /
      DATA ( CS_REF( IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 6.115310E-21, 1.783046E-23, 1.650939E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.227041E-02, 4.316183E-02, 4.576553E-01, 7.880843E-01, 
     & 9.572257E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( QY_REF(  IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.227041E-02, 4.316183E-02, 4.576553E-01, 7.880843E-01, 
     & 9.572257E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( QY_REF(  IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.227041E-02, 4.316183E-02, 4.576553E-01, 7.880843E-01, 
     & 9.572257E-01, 1.000000E+00, 1.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.234537E-20, 1.108793E-20, 4.421269E-20, 3.478935E-20, 
     & 5.673101E-21, 1.783046E-23, 1.650939E-21 /
      DATA ( ECS_REF( IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.234537E-20, 1.108793E-20, 4.421269E-20, 3.478935E-20, 
     & 5.673101E-21, 1.783046E-23, 1.650939E-21 /
      DATA ( ECS_REF( IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.234537E-20, 1.108793E-20, 4.421269E-20, 3.478935E-20, 
     & 5.673101E-21, 1.783046E-23, 1.650939E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.349377E-02, 4.058169E-02, 4.218709E-01, 7.771195E-01, 
     & 9.276882E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.349377E-02, 4.058169E-02, 4.218709E-01, 7.771195E-01, 
     & 9.276882E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.349377E-02, 4.058169E-02, 4.218709E-01, 7.771195E-01, 
     & 9.276882E-01, 1.000000E+00, 1.000000E+00 /


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
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 5.921064E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 5.921064E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 5.921064E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.377296E-01, 9.568382E-01, 5.423447E-01, 2.119156E-01, 
     & 4.277430E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.377296E-01, 9.568382E-01, 5.423447E-01, 2.119156E-01, 
     & 4.277430E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.377296E-01, 9.568382E-01, 5.423447E-01, 2.119156E-01, 
     & 4.277430E-02, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.720721E-19, 2.621370E-19, 6.058880E-20, 9.977705E-21, 
     & 4.422089E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.720721E-19, 2.621370E-19, 6.058880E-20, 9.977705E-21, 
     & 4.422089E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.720721E-19, 2.621370E-19, 6.058880E-20, 9.977705E-21, 
     & 4.422089E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.365062E-01, 9.594183E-01, 5.781292E-01, 2.228805E-01, 
     & 7.468402E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.365062E-01, 9.594183E-01, 5.781292E-01, 2.228805E-01, 
     & 7.468402E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.365062E-01, 9.594183E-01, 5.781292E-01, 2.228805E-01, 
     & 7.468402E-02, 0.000000E+00, 0.000000E+00 /


C...HONO_NO_SAPRC99
C..  HONO + HV = HO. + NO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHONO_NO_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218199E-20, 3.477931E-20, 
     & 1.090586E-19, 8.781877E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218199E-20, 3.477931E-20, 
     & 1.090586E-19, 8.781877E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218199E-20, 3.477931E-20, 
     & 1.090586E-19, 8.781877E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.840296E-01, 4.690968E-01, 
     & 6.487832E-01, 6.915470E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.840296E-01, 4.690968E-01, 
     & 6.487832E-01, 6.915470E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.840296E-01, 4.690968E-01, 
     & 6.487832E-01, 6.915470E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.061778E-21, 1.650458E-20, 
     & 7.360098E-20, 8.336920E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.061778E-21, 1.650458E-20, 
     & 7.360098E-20, 8.336920E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.061778E-21, 1.650458E-20, 
     & 7.360098E-20, 8.336920E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.155132E-01, 4.745518E-01, 
     & 6.748757E-01, 9.493324E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.155132E-01, 4.745518E-01, 
     & 6.748757E-01, 9.493324E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.155132E-01, 4.745518E-01, 
     & 6.748757E-01, 9.493324E-01, 0.000000E+00 /


C...HONO_NO2_SAPRC99
C..  HONO + HV = H. + NO2
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHONO_NO2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218199E-20, 3.477931E-20, 
     & 1.090586E-19, 3.949962E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218199E-20, 3.477931E-20, 
     & 1.090586E-19, 3.949962E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218199E-20, 3.477931E-20, 
     & 1.090586E-19, 3.949962E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.022087E-01, 5.309032E-01, 
     & 3.512168E-01, 2.663607E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.022087E-01, 5.309032E-01, 
     & 3.512168E-01, 2.663607E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 4.022087E-01, 5.309032E-01, 
     & 3.512168E-01, 2.663607E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.119835E-21, 1.827472E-20, 
     & 3.545759E-20, 4.449558E-21, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.119835E-21, 1.827472E-20, 
     & 3.545759E-20, 4.449558E-21, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.119835E-21, 1.827472E-20, 
     & 3.545759E-20, 4.449558E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.844557E-01, 5.254481E-01, 
     & 3.251243E-01, 1.126481E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.844557E-01, 5.254481E-01, 
     & 3.251243E-01, 1.126481E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.844557E-01, 5.254481E-01, 
     & 3.251243E-01, 1.126481E-01, 0.000000E+00 /


C...HNO3_SAPRC99
C..  HNO3 + HV = products
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHNO3_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875268E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875268E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875268E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875251E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875251E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875251E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999956E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999956E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999956E-01, 0.000000E+00 /


C...HO2NO2_SAPRC99
C..  HO2NO2 + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHO2NO2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327396E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327396E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327396E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327384E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327384E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327384E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /


C...H2O2_SAPRC99
C..  H2O2 + HV = 2 OH
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IH2O2_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606306E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606306E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606306E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF(  IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF(  IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606289E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606289E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606289E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999954E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999954E-01, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999954E-01, 0.000000E+00 /


C...HCHO_R_SAPRC99
C..  HCHO + HV = HCO + H
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHCHO_R_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.387170E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.387170E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.387170E-20, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.530258E-01, 7.793080E-01, 7.694805E-01, 6.766393E-01, 
     & 2.101619E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.530258E-01, 7.793080E-01, 7.694805E-01, 6.766393E-01, 
     & 2.101619E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.530258E-01, 7.793080E-01, 7.694805E-01, 6.766393E-01, 
     & 2.101619E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.360363E-20, 2.619580E-20, 1.264450E-20, 2.110757E-20, 
     & 3.830408E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.360363E-20, 2.619580E-20, 1.264450E-20, 2.110757E-20, 
     & 3.830408E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.360363E-20, 2.619580E-20, 1.264450E-20, 2.110757E-20, 
     & 3.830408E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.537656E-01, 7.794024E-01, 7.739203E-01, 6.831840E-01, 
     & 2.761312E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.537656E-01, 7.794024E-01, 7.739203E-01, 6.831840E-01, 
     & 2.761312E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.537656E-01, 7.794024E-01, 7.739203E-01, 6.831840E-01, 
     & 2.761312E-01, 0.000000E+00, 0.000000E+00 /


C...HCHO_M_SAPRC99
C..  HCHO + HV = H2 + CO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHCHO_M_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.653456E-20, 7.220900E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.653456E-20, 7.220900E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.653456E-20, 7.220900E-22, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.261845E-01, 2.137267E-01, 2.298757E-01, 3.233053E-01, 
     & 5.541007E-01, 2.985801E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.261845E-01, 2.137267E-01, 2.298757E-01, 3.233053E-01, 
     & 5.541007E-01, 2.985801E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.261845E-01, 2.137267E-01, 2.298757E-01, 3.233053E-01, 
     & 5.541007E-01, 2.985801E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.068908E-21, 7.200789E-21, 3.679953E-21, 9.787576E-21, 
     & 9.269445E-21, 1.144538E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.068908E-21, 7.200789E-21, 3.679953E-21, 9.787576E-21, 
     & 9.269445E-21, 1.144538E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.068908E-21, 7.200789E-21, 3.679953E-21, 9.787576E-21, 
     & 9.269445E-21, 1.144538E-22, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.257407E-01, 2.142447E-01, 2.252355E-01, 3.167923E-01, 
     & 5.606101E-01, 1.585035E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.257407E-01, 2.142447E-01, 2.252355E-01, 3.167923E-01, 
     & 5.606101E-01, 1.585035E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.257407E-01, 2.142447E-01, 2.252355E-01, 3.167923E-01, 
     & 5.606101E-01, 1.585035E-01, 0.000000E+00 /


C...CCHO_R_SAPRC99
C..  CCHO + HV = CH3 + CHO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICCHO_R_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.432586E-20, 3.717937E-20, 2.933103E-20, 2.104728E-20, 
     & 3.715596E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.432586E-20, 3.717937E-20, 2.933103E-20, 2.104728E-20, 
     & 3.715596E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.432586E-20, 3.717937E-20, 2.933103E-20, 2.104728E-20, 
     & 3.715596E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902037E-01, 2.845214E-01, 1.538485E-01, 
     & 1.409402E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902037E-01, 2.845214E-01, 1.538485E-01, 
     & 1.409402E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902037E-01, 2.845214E-01, 1.538485E-01, 
     & 1.409402E-02, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461038E-20, 8.426962E-21, 3.330092E-21, 
     & 1.731218E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461038E-20, 8.426962E-21, 3.330092E-21, 
     & 1.731218E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461038E-20, 8.426962E-21, 3.330092E-21, 
     & 1.731218E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.845355E-01, 3.929700E-01, 2.873054E-01, 1.582196E-01, 
     & 4.659327E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.845355E-01, 3.929700E-01, 2.873054E-01, 1.582196E-01, 
     & 4.659327E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.845355E-01, 3.929700E-01, 2.873054E-01, 1.582196E-01, 
     & 4.659327E-02, 0.000000E+00, 0.000000E+00 /


C...C2CHO_SAPRC99
C..  C2CHO + HV = C2H5. + CHO.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IC2CHO_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634689E-20, 3.579653E-20, 2.441742E-20, 
     & 5.808827E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634689E-20, 3.579653E-20, 2.441742E-20, 
     & 5.808827E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.113497E-20, 4.634689E-20, 3.579653E-20, 2.441742E-20, 
     & 5.808827E-21, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.112442E-01, 7.954021E-01, 5.951666E-01, 4.312297E-01, 
     & 1.520064E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.112442E-01, 7.954021E-01, 5.951666E-01, 4.312297E-01, 
     & 1.520064E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.112442E-01, 7.954021E-01, 5.951666E-01, 4.312297E-01, 
     & 1.520064E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.730256E-20, 3.713801E-20, 2.133677E-20, 1.077360E-20, 
     & 1.383933E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.730256E-20, 3.713801E-20, 2.133677E-20, 1.077360E-20, 
     & 1.383933E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.730256E-20, 3.713801E-20, 2.133677E-20, 1.077360E-20, 
     & 1.383933E-21, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.769099E-01, 8.013052E-01, 5.960570E-01, 4.412259E-01, 
     & 2.382466E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.769099E-01, 8.013052E-01, 5.960570E-01, 4.412259E-01, 
     & 2.382466E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.769099E-01, 8.013052E-01, 5.960570E-01, 4.412259E-01, 
     & 2.382466E-01, 0.000000E+00, 0.000000E+00 /


C...ACETONE_SAPRC99
C..  ACETONE + HV = CH3CO. + CH3.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IACETONE_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.565269E-20, 2.347503E-20, 1.411211E-20, 7.530059E-21, 
     & 8.363643E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.565269E-20, 2.347503E-20, 1.411211E-20, 7.530059E-21, 
     & 8.363643E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.565269E-20, 2.347503E-20, 1.411211E-20, 7.530059E-21, 
     & 8.363643E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.242420E-01, 1.142595E-01, 5.803515E-02, 2.870061E-02, 
     & 4.434764E-03, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.242420E-01, 1.142595E-01, 5.803515E-02, 2.870061E-02, 
     & 4.434764E-03, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.242420E-01, 1.142595E-01, 5.803515E-02, 2.870061E-02, 
     & 4.434764E-03, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.097962E-21, 2.778257E-21, 8.357552E-22, 2.321761E-22, 
     & 8.431038E-24, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.097962E-21, 2.778257E-21, 8.357552E-22, 2.321761E-22, 
     & 8.431038E-24, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.097962E-21, 2.778257E-21, 8.357552E-22, 2.321761E-22, 
     & 8.431038E-24, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.271347E-01, 1.183495E-01, 5.922253E-02, 3.083324E-02, 
     & 1.008058E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.271347E-01, 1.183495E-01, 5.922253E-02, 3.083324E-02, 
     & 1.008058E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.271347E-01, 1.183495E-01, 5.922253E-02, 3.083324E-02, 
     & 1.008058E-02, 0.000000E+00, 0.000000E+00 /


C...KETONE_SAPRC99
C..  Methyl Ethyl Ketone Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IKETONE_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479082E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479082E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479082E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.169248E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.169248E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.169248E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479075E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479075E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479075E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999990E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999990E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999990E-01, 0.000000E+00, 0.000000E+00 /


C...COOH_SAPRC99
C..  CH3OOH + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICOOH_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395097E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395097E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395097E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.016710E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395090E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395090E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395090E-23, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999989E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999989E-01, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999989E-01, 0.000000E+00 /


C...GLY_R_SAPRC99
C..  Glyoxal + hv = 2 HCO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IGLY_R_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 6.237997E-22 /
      DATA ( CS_REF( IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 6.237997E-22 /
      DATA ( CS_REF( IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 6.237997E-22 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.446526E-01, 0.000000E+00 /
      DATA ( QY_REF(  IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.446526E-01, 0.000000E+00 /
      DATA ( QY_REF(  IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.446526E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 4.361289E-21, 0.000000E+00 /
      DATA ( ECS_REF( IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 4.361289E-21, 0.000000E+00 /
      DATA ( ECS_REF( IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 4.361289E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.143213E-01, 0.000000E+00 /
      DATA ( EQY_REF( IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.143213E-01, 0.000000E+00 /
      DATA ( EQY_REF( IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.143213E-01, 0.000000E+00 /


C...GLY_ABS_SAPRC99
C..  Glyoxal Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IGLY_ABS_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746832E-21 /
      DATA ( CS_REF( IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746832E-21 /
      DATA ( CS_REF( IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746832E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.534627E-02 /
      DATA ( QY_REF(  IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.534627E-02 /
      DATA ( QY_REF(  IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.534627E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746832E-21 /
      DATA ( ECS_REF( IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746832E-21 /
      DATA ( ECS_REF( IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746832E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /
      DATA ( EQY_REF( IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /
      DATA ( EQY_REF( IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999999E-01 /


C...MGLY_ADJ_SAPRC99
C..  MGLY + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_ADJ_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 1.477388E-21 /
      DATA ( CS_REF( IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 1.477388E-21 /
      DATA ( CS_REF( IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 1.477388E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992942E-01, 3.801419E-01, 0.000000E+00 /
      DATA ( QY_REF(  IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992942E-01, 3.801419E-01, 0.000000E+00 /
      DATA ( QY_REF(  IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992942E-01, 3.801419E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.996164E-21, 6.110804E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.996164E-21, 6.110804E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.996164E-21, 6.110804E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.996566E-01, 1.652672E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.996566E-01, 1.652672E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.996566E-01, 1.652672E-01, 0.000000E+00 /


C...BACL_ADJ_SAPRC99
C..  BACL + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IBACL_ADJ_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 3.224516E-20, 4.675102E-21 /
      DATA ( CS_REF( IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 3.224516E-20, 4.675102E-21 /
      DATA ( CS_REF( IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 3.224516E-20, 4.675102E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.303440E-01, 9.162429E-04 /
      DATA ( QY_REF(  IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.303440E-01, 9.162429E-04 /
      DATA ( QY_REF(  IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.303440E-01, 9.162429E-04 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 1.215315E-20, 6.213299E-23 /
      DATA ( ECS_REF( IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 1.215315E-20, 6.213299E-23 /
      DATA ( ECS_REF( IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 1.215315E-20, 6.213299E-23 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.768983E-01, 1.329019E-02 /
      DATA ( EQY_REF( IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.768983E-01, 1.329019E-02 /
      DATA ( EQY_REF( IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.768983E-01, 1.329019E-02 /


C...BZCHO_SAPRC99
C..  Benzaldehyde absorbtion coefs in n-Hexane
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IBZCHO_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.271376E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821757E-20, 0.000000E+00 /
      DATA ( CS_REF( IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.271376E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821757E-20, 0.000000E+00 /
      DATA ( CS_REF( IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.271376E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821757E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.366284E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.153002E-01, 0.000000E+00 /
      DATA ( QY_REF(  IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.366284E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.153002E-01, 0.000000E+00 /
      DATA ( QY_REF(  IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.366284E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.153002E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.247760E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821757E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.247760E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821757E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.247760E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821757E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.974529E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999998E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.974529E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999998E-01, 0.000000E+00 /
      DATA ( EQY_REF( IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.974529E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999998E-01, 0.000000E+00 /


C...ACROLEIN_SAPRC99
C..  Absorption cross sections for Acrolein.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IACROLEIN_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /
      DATA ( CS_REF( IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /
      DATA ( CS_REF( IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.565694E-01, 0.000000E+00 /
      DATA ( QY_REF(  IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.565694E-01, 0.000000E+00 /
      DATA ( QY_REF(  IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.565694E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /
      DATA ( ECS_REF( IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /
      DATA ( ECS_REF( IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /

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
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667180E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667180E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667180E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667175E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667175E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667175E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /


C...MGLY_ABS_SAPRC99
C..  Methyl Glyoxal Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_ABS_SAPRC99 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( CS_REF( IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( CS_REF( IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.689096E-01 /
      DATA ( QY_REF(  IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.689096E-01 /
      DATA ( QY_REF(  IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.689096E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( ECS_REF( IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( ECS_REF( IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /

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
     & 8.237372E-19, 2.618435E-19, 1.031278E-19, 4.160843E-20, 
     & 6.126474E-21, 4.250346E-23, 1.550664E-21 /
      DATA ( CS_REF( IO3_O3P_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.237372E-19, 2.618435E-19, 1.031278E-19, 4.160843E-20, 
     & 6.126474E-21, 4.250346E-23, 1.550664E-21 /
      DATA ( CS_REF( IO3_O3P_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.237372E-19, 2.618435E-19, 1.031278E-19, 4.160843E-20, 
     & 6.126474E-21, 4.250346E-23, 1.550664E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3_O3P_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070934E-01, 4.640103E-01, 7.781818E-01, 
     & 9.134412E-01, 9.634615E-01, 6.920660E-01 /
      DATA ( QY_REF(  IO3_O3P_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070934E-01, 4.640103E-01, 7.781818E-01, 
     & 9.134412E-01, 9.634615E-01, 6.920660E-01 /
      DATA ( QY_REF(  IO3_O3P_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070934E-01, 4.640103E-01, 7.781818E-01, 
     & 9.134412E-01, 9.634615E-01, 6.920660E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3_O3P_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.237372E-20, 2.744881E-20, 4.605711E-20, 3.200195E-20, 
     & 5.504558E-21, 3.927088E-23, 1.550664E-21 /
      DATA ( ECS_REF( IO3_O3P_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.237372E-20, 2.744881E-20, 4.605711E-20, 3.200195E-20, 
     & 5.504558E-21, 3.927088E-23, 1.550664E-21 /
      DATA ( ECS_REF( IO3_O3P_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.237372E-20, 2.744881E-20, 4.605711E-20, 3.200195E-20, 
     & 5.504558E-21, 3.927088E-23, 1.550664E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3_O3P_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.048291E-01, 4.466023E-01, 7.691218E-01, 
     & 8.984872E-01, 9.239455E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3_O3P_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.048291E-01, 4.466023E-01, 7.691218E-01, 
     & 8.984872E-01, 9.239455E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3_O3P_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.048291E-01, 4.466023E-01, 7.691218E-01, 
     & 8.984872E-01, 9.239455E-01, 1.000000E+00 /


C...O3_O1D_IUPAC04
C..  O3 + HV = O(1D) + O2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet POx2, updated 2nd October 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IO3_O1D_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3_O1D_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.237372E-19, 2.618435E-19, 1.031278E-19, 4.160843E-20, 
     & 6.126474E-21, 4.040726E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3_O1D_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.237372E-19, 2.618435E-19, 1.031278E-19, 4.160843E-20, 
     & 6.126474E-21, 4.040726E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3_O1D_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.237372E-19, 2.618435E-19, 1.031278E-19, 4.160843E-20, 
     & 6.126474E-21, 4.040726E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3_O1D_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929066E-01, 5.359897E-01, 2.218182E-01, 
     & 8.655884E-02, 1.629167E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3_O1D_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929066E-01, 5.359897E-01, 2.218182E-01, 
     & 8.655884E-02, 1.629167E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3_O1D_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929066E-01, 5.359897E-01, 2.218182E-01, 
     & 8.655884E-02, 1.629167E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3_O1D_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.413634E-19, 2.343947E-19, 5.707068E-20, 9.606477E-21, 
     & 6.219156E-22, 3.232572E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3_O1D_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.413634E-19, 2.343947E-19, 5.707068E-20, 9.606477E-21, 
     & 6.219156E-22, 3.232572E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3_O1D_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.413634E-19, 2.343947E-19, 5.707068E-20, 9.606477E-21, 
     & 6.219156E-22, 3.232572E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3_O1D_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.951709E-01, 5.533977E-01, 2.308782E-01, 
     & 1.015128E-01, 7.999979E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3_O1D_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.951709E-01, 5.533977E-01, 2.308782E-01, 
     & 1.015128E-01, 7.999979E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3_O1D_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.951709E-01, 5.533977E-01, 2.308782E-01, 
     & 1.015128E-01, 7.999979E-02, 0.000000E+00 /


C...HONO_IUPAC04
C.. HONO + HV = HO + NO
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx1_HONO, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHONO_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780336E-21, 1.602461E-20, 3.132021E-20, 
     & 9.264939E-20, 7.356084E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780336E-21, 1.602461E-20, 3.132021E-20, 
     & 9.264939E-20, 7.356084E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780336E-21, 1.602461E-20, 3.132021E-20, 
     & 9.264939E-20, 7.356084E-20, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHONO_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 8.334302E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.181830E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 8.334302E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.181830E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 8.334302E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 7.181830E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780317E-21, 1.602461E-20, 3.132021E-20, 
     & 9.264939E-20, 7.356084E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780317E-21, 1.602461E-20, 3.132021E-20, 
     & 9.264939E-20, 7.356084E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 4.780317E-21, 1.602461E-20, 3.132021E-20, 
     & 9.264939E-20, 7.356084E-20, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.999960E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.999960E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.999960E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999999E-01, 0.000000E+00 /


C...HO2NO2_IUPAC04
C..  HOONO2 + HV = products
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx3_HO2NO2, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHO2NO2_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHO2NO2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327396E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327396E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327396E-22, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHO2NO2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHO2NO2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHO2NO2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.754197E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHO2NO2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327384E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327384E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327384E-22, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHO2NO2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.999982E-01, 0.000000E+00, 0.000000E+00 /


C...HNO3_IUPAC04
C.. HONO2 + HV = OH + NO2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx2_HONO2, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHNO3_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO3_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875268E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875268E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875268E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHNO3_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.142428E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO3_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875251E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875251E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875251E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO3_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999956E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999956E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999956E-01, 0.000000E+00 /


C...N2O5_IUPAC04
C.. N2O5 + HV = NO2 + NO3
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet PNOx7_N2O5, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IN2O5_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IN2O5_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.288713E-20, 3.284367E-20, 2.192960E-20, 1.512786E-20, 
     & 6.068436E-21, 6.195746E-22, 9.184480E-26 /
      DATA ( CS_REF( IN2O5_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.288713E-20, 3.284367E-20, 2.192960E-20, 1.512786E-20, 
     & 6.068436E-21, 6.195746E-22, 9.184480E-26 /
      DATA ( CS_REF( IN2O5_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.288713E-20, 3.284367E-20, 2.192960E-20, 1.512786E-20, 
     & 6.068436E-21, 6.195746E-22, 9.184480E-26 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IN2O5_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.222279E-01, 9.980624E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 4.477524E-03 /
      DATA ( QY_REF(  IN2O5_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.222279E-01, 9.980624E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 4.477524E-03 /
      DATA ( QY_REF(  IN2O5_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.222279E-01, 9.980624E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 4.477524E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IN2O5_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.857824E-20, 3.276333E-20, 2.192960E-20, 1.512786E-20, 
     & 6.068436E-21, 6.195746E-22, 9.184020E-26 /
      DATA ( ECS_REF( IN2O5_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.857824E-20, 3.276333E-20, 2.192960E-20, 1.512786E-20, 
     & 6.068436E-21, 6.195746E-22, 9.184020E-26 /
      DATA ( ECS_REF( IN2O5_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.857824E-20, 3.276333E-20, 2.192960E-20, 1.512786E-20, 
     & 6.068436E-21, 6.195746E-22, 9.184020E-26 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IN2O5_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.185266E-01, 9.975538E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999499E-01 /
      DATA ( EQY_REF( IN2O5_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.185266E-01, 9.975538E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999499E-01 /
      DATA ( EQY_REF( IN2O5_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.185266E-01, 9.975538E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999499E-01 /


C...NTR_IUPAC04
C.. i-C3H7ONO2 + HV = iC3H7O + NO2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet P17_i-C3H7ONO2+hv, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INTR_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INTR_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112254E-21, 3.012601E-21, 1.483079E-21, 
     & 2.223491E-22, 1.206802E-24, 0.000000E+00 /
      DATA ( CS_REF( INTR_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112254E-21, 3.012601E-21, 1.483079E-21, 
     & 2.223491E-22, 1.206802E-24, 0.000000E+00 /
      DATA ( CS_REF( INTR_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112254E-21, 3.012601E-21, 1.483079E-21, 
     & 2.223491E-22, 1.206802E-24, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INTR_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /
      DATA ( QY_REF(  INTR_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /
      DATA ( QY_REF(  INTR_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.282282E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INTR_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112254E-21, 3.012601E-21, 1.483079E-21, 
     & 2.223491E-22, 1.206801E-24, 0.000000E+00 /
      DATA ( ECS_REF( INTR_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112254E-21, 3.012601E-21, 1.483079E-21, 
     & 2.223491E-22, 1.206801E-24, 0.000000E+00 /
      DATA ( ECS_REF( INTR_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.228679E-20, 6.112254E-21, 3.012601E-21, 1.483079E-21, 
     & 2.223491E-22, 1.206801E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INTR_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /
      DATA ( EQY_REF( INTR_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /
      DATA ( EQY_REF( INTR_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999995E-01, 0.000000E+00 /


C...PAN_IUPAC04
C.. CH3C(O)OONO2 + HV = CH3C(O)OO + NO2
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation
C..  Data Sheet P21_CH3C(O)OONO2+hv, updated 16th July 2001
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IPAN_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPAN_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.308070E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.308070E-24, 0.000000E+00 /
      DATA ( CS_REF( IPAN_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.308070E-24, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IPAN_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.613498E-02, 0.000000E+00 /
      DATA ( QY_REF(  IPAN_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.613498E-02, 0.000000E+00 /
      DATA ( QY_REF(  IPAN_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 6.613498E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPAN_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.308038E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.308038E-24, 0.000000E+00 /
      DATA ( ECS_REF( IPAN_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.530296E-21, 1.417990E-21, 6.732224E-22, 3.640893E-22, 
     & 9.143990E-23, 1.308038E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPAN_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999756E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999756E-01, 0.000000E+00 /
      DATA ( EQY_REF( IPAN_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.999756E-01, 0.000000E+00 /


C...PACD_CB05
C..  PACD   + HV = MEO2 + OH
C..  CB05 Photolysis data for PACD
C..  Supplied by Greg Yarwood, 11/16/2007
C..  Ref: Giguère, P. A. and A. W. Olmos. Sur le spectre ultraviolet de l'acide peracétique et l'hydrolyse des peracétates.

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IPACD_CB05 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IPACD_CB05,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128908E-22, 2.430012E-22, 1.524872E-22, 
     & 2.678565E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPACD_CB05,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128908E-22, 2.430012E-22, 1.524872E-22, 
     & 2.678565E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IPACD_CB05,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128908E-22, 2.430012E-22, 1.524872E-22, 
     & 2.678565E-23, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IPACD_CB05,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 3.584636E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IPACD_CB05,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 3.584636E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IPACD_CB05,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 3.584636E-01, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IPACD_CB05,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128908E-22, 2.430012E-22, 1.524872E-22, 
     & 2.675539E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPACD_CB05,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128908E-22, 2.430012E-22, 1.524872E-22, 
     & 2.675539E-23, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IPACD_CB05,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.759271E-22, 4.128908E-22, 2.430012E-22, 1.524872E-22, 
     & 2.675539E-23, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IPACD_CB05,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.988704E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPACD_CB05,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.988704E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IPACD_CB05,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.988704E-01, 0.000000E+00, 0.000000E+00 /


C...MGLY_IUPAC04
C.. CH3COCHO + hv ---> CH3CO + HCO
C..  From IUPAC Subcommittee on Gas Kinetic Data Evaluation; IUPAC Stern-Volmer expression
C..  Data Sheet P6_CH3COCHO+hv.pdf, updated 16th Jan, 2003
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk/

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.380391E-20, 3.467071E-20, 2.430916E-20, 1.788232E-20, 
     & 6.183676E-21, 3.700269E-20, 7.900482E-21 /
      DATA ( CS_REF( IMGLY_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.380391E-20, 3.467071E-20, 2.430916E-20, 1.788232E-20, 
     & 6.183676E-21, 3.700269E-20, 7.900482E-21 /
      DATA ( CS_REF( IMGLY_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.380391E-20, 3.467071E-20, 2.430916E-20, 1.788232E-20, 
     & 6.183676E-21, 3.700269E-20, 7.900482E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.166998E-01, 2.503722E-03 /
      DATA ( QY_REF(  IMGLY_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.166998E-01, 2.503722E-03 /
      DATA ( QY_REF(  IMGLY_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.166998E-01, 2.503722E-03 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.380391E-20, 3.467071E-20, 2.430916E-20, 1.788232E-20, 
     & 6.183676E-21, 8.170202E-21, 2.110403E-22 /
      DATA ( ECS_REF( IMGLY_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.380391E-20, 3.467071E-20, 2.430916E-20, 1.788232E-20, 
     & 6.183676E-21, 8.170202E-21, 2.110403E-22 /
      DATA ( ECS_REF( IMGLY_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.380391E-20, 3.467071E-20, 2.430916E-20, 1.788232E-20, 
     & 6.183676E-21, 8.170202E-21, 2.110403E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.208002E-01, 2.671233E-02 /
      DATA ( EQY_REF( IMGLY_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.208002E-01, 2.671233E-02 /
      DATA ( EQY_REF( IMGLY_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.208002E-01, 2.671233E-02 /


C...CL2_IUPAC04
C..  CL2 + HV = 2*CL
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation; Data Sheet PCl11 Website: 15th December 2000
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk
C..  Assume these are point values (not specified in data source)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICL2_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICL2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.126821E-22 /
      DATA ( CS_REF( ICL2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.126821E-22 /
      DATA ( CS_REF( ICL2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.126821E-22 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICL2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /
      DATA ( QY_REF(  ICL2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /
      DATA ( QY_REF(  ICL2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.135570E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICL2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.126374E-22 /
      DATA ( ECS_REF( ICL2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.126374E-22 /
      DATA ( ECS_REF( ICL2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.824935E-20, 1.400673E-19, 1.841698E-19, 2.185164E-19, 
     & 2.410846E-19, 6.539243E-20, 6.126374E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICL2_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999270E-01 /
      DATA ( EQY_REF( ICL2_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999270E-01 /
      DATA ( EQY_REF( ICL2_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.999270E-01 /


C...HOCL_IUPAC04
C..  HOCL + HV = HO + CL
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation; Data Sheet PCl2 Website: 15th December 2000
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk
C..  Assume these are point values - not specified in data source

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHOCL_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHOCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066934E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.172828E-23 /
      DATA ( CS_REF( IHOCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066934E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.172828E-23 /
      DATA ( CS_REF( IHOCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066934E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.172828E-23 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHOCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.319226E-02 /
      DATA ( QY_REF(  IHOCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.319226E-02 /
      DATA ( QY_REF(  IHOCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.319226E-02 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHOCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066934E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.171483E-23 /
      DATA ( ECS_REF( IHOCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066934E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.171483E-23 /
      DATA ( ECS_REF( IHOCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.570117E-20, 6.066934E-20, 5.958161E-20, 5.377945E-20, 
     & 3.120855E-20, 6.545254E-21, 1.171483E-23 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHOCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.988528E-01 /
      DATA ( EQY_REF( IHOCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.988528E-01 /
      DATA ( EQY_REF( IHOCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.988528E-01 /


C...FMCL_IUPAC04
C..  FMCL + hv -->   HCO + CL
C..  IUPAC Subcommittee on Gas Kinetic Data Evaluation; Data Sheet PCl28 Website: 15th December 2000
C..  Website: http://www.iupac-kinetic.ch.cam.ac.uk
C..  Reference: H. G. Libuda, F. Zabel, E. H. Fink, and K. H. Becker, J. Phys. Chem. 94, 5860 (1990)

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IFMCL_IUPAC04 ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IFMCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.320050E-21, 1.421323E-21, 2.242342E-22, 8.360462E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IFMCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.320050E-21, 1.421323E-21, 2.242342E-22, 8.360462E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IFMCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.320050E-21, 1.421323E-21, 2.242342E-22, 8.360462E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IFMCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 7.811255E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IFMCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 7.811255E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IFMCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 7.811255E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IFMCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.320050E-21, 1.421323E-21, 2.242342E-22, 8.349381E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IFMCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.320050E-21, 1.421323E-21, 2.242342E-22, 8.349381E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IFMCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.320050E-21, 1.421323E-21, 2.242342E-22, 8.349381E-23, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IFMCL_IUPAC04,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 9.986746E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IFMCL_IUPAC04,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 9.986746E-01, 
     & 0.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IFMCL_IUPAC04,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 9.986746E-01, 
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
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 5.754674E-19, 1.195356E-20 /
      DATA ( CS_REF( INO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 5.754674E-19, 1.195356E-20 /
      DATA ( CS_REF( INO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 5.754674E-19, 1.195356E-20 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.174767E-01, 7.500220E-04 /
      DATA ( QY_REF(  INO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.174767E-01, 7.500220E-04 /
      DATA ( QY_REF(  INO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.174767E-01, 7.500220E-04 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 4.644383E-19, 4.476030E-22 /
      DATA ( ECS_REF( INO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 4.644383E-19, 4.476030E-22 /
      DATA ( ECS_REF( INO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.075248E-19, 1.484195E-19, 1.882053E-19, 2.274755E-19, 
     & 3.449308E-19, 4.644383E-19, 4.476030E-22 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.070627E-01, 3.744516E-02 /
      DATA ( EQY_REF( INO2,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.070627E-01, 3.744516E-02 /
      DATA ( EQY_REF( INO2,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 8.070627E-01, 3.744516E-02 /


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
     & 8.538728E-19, 2.750705E-19, 1.063739E-19, 4.625938E-20, 
     & 6.599046E-21, 5.460110E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.538728E-19, 2.750705E-19, 1.063739E-19, 4.625938E-20, 
     & 6.599046E-21, 5.460110E-23, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.538728E-19, 2.750705E-19, 1.063739E-19, 4.625938E-20, 
     & 6.599046E-21, 5.460110E-23, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O1D,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929066E-01, 5.359897E-01, 2.218182E-01, 
     & 8.688402E-02, 4.278573E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929066E-01, 5.359897E-01, 2.218182E-01, 
     & 8.688402E-02, 4.278573E-02, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.929066E-01, 5.359897E-01, 2.218182E-01, 
     & 8.688402E-02, 4.278573E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O1D,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.684855E-19, 2.464146E-19, 6.078741E-20, 1.068696E-20, 
     & 6.625140E-22, 4.350830E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.684855E-19, 2.464146E-19, 6.078741E-20, 1.068696E-20, 
     & 6.625140E-22, 4.350830E-24, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.684855E-19, 2.464146E-19, 6.078741E-20, 1.068696E-20, 
     & 6.625140E-22, 4.350830E-24, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O1D,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.958234E-01, 5.714505E-01, 2.310225E-01, 
     & 1.003954E-01, 7.968391E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.958234E-01, 5.714505E-01, 2.310225E-01, 
     & 1.003954E-01, 7.968391E-02, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.000000E-01, 8.958234E-01, 5.714505E-01, 2.310225E-01, 
     & 1.003954E-01, 7.968391E-02, 0.000000E+00 /


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
     & 8.407954E-19, 2.749065E-19, 1.063739E-19, 4.625938E-20, 
     & 6.803406E-21, 6.139220E-23, 1.666851E-21 /
      DATA ( CS_REF( IO3O3P,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.407954E-19, 2.749065E-19, 1.063739E-19, 4.625938E-20, 
     & 6.803406E-21, 6.139220E-23, 1.666851E-21 /
      DATA ( CS_REF( IO3O3P,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.407954E-19, 2.749065E-19, 1.063739E-19, 4.625938E-20, 
     & 6.803406E-21, 6.139220E-23, 1.666851E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O3P,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070934E-01, 4.640103E-01, 7.781818E-01, 
     & 9.123903E-01, 9.572143E-01, 9.795594E-01 /
      DATA ( QY_REF(  IO3O3P,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070934E-01, 4.640103E-01, 7.781818E-01, 
     & 9.123903E-01, 9.572143E-01, 9.795594E-01 /
      DATA ( QY_REF(  IO3O3P,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.070934E-01, 4.640103E-01, 7.781818E-01, 
     & 9.123903E-01, 9.572143E-01, 9.795594E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O3P,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.407954E-20, 2.863951E-20, 4.558648E-20, 3.557242E-20, 
     & 6.109183E-21, 5.704138E-23, 1.666851E-21 /
      DATA ( ECS_REF( IO3O3P,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.407954E-20, 2.863951E-20, 4.558648E-20, 3.557242E-20, 
     & 6.109183E-21, 5.704138E-23, 1.666851E-21 /
      DATA ( ECS_REF( IO3O3P,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.407954E-20, 2.863951E-20, 4.558648E-20, 3.557242E-20, 
     & 6.109183E-21, 5.704138E-23, 1.666851E-21 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O3P,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.041791E-01, 4.285495E-01, 7.689775E-01, 
     & 8.979595E-01, 9.291306E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.041791E-01, 4.285495E-01, 7.689775E-01, 
     & 8.979595E-01, 9.291306E-01, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E-01, 1.041791E-01, 4.285495E-01, 7.689775E-01, 
     & 8.979595E-01, 9.291306E-01, 1.000000E+00 /


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
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609984E-25, 0.000000E+00 /
      DATA ( CS_REF( IKETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609984E-25, 0.000000E+00 /
      DATA ( CS_REF( IKETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609984E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IKETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.161979E-02, 0.000000E+00 /
      DATA ( QY_REF(  IKETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.161979E-02, 0.000000E+00 /
      DATA ( QY_REF(  IKETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 2.161979E-02, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IKETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609804E-25, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609804E-25, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.166522E-20, 2.686872E-20, 1.573254E-20, 7.723365E-21, 
     & 8.066992E-22, 1.609804E-25, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IKETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.998884E-01, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.998884E-01, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 9.998884E-01, 0.000000E+00 /


C...MGLY_ABS
C..  Methyl Glyoxal Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_ABS ), ITTR=1,3 ) / 298.0, 298.0, 298.0 /

C...  absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ABS,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( CS_REF( IMGLY_ABS,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( CS_REF( IMGLY_ABS,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_ABS,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.689096E-01 /
      DATA ( QY_REF(  IMGLY_ABS,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.689096E-01 /
      DATA ( QY_REF(  IMGLY_ABS,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.689096E-01 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ABS,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( ECS_REF( IMGLY_ABS,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( ECS_REF( IMGLY_ABS,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /

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
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 1.477388E-21 /
      DATA ( CS_REF( IMGLY_ADJ,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 1.477388E-21 /
      DATA ( CS_REF( IMGLY_ADJ,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 1.477388E-21 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_ADJ,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992942E-01, 3.801419E-01, 0.000000E+00 /
      DATA ( QY_REF(  IMGLY_ADJ,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992942E-01, 3.801419E-01, 0.000000E+00 /
      DATA ( QY_REF(  IMGLY_ADJ,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992942E-01, 3.801419E-01, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ADJ,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.996164E-21, 6.110804E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.996164E-21, 6.110804E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.996164E-21, 6.110804E-21, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_ADJ,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.996566E-01, 1.652672E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.996566E-01, 1.652672E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.996566E-01, 1.652672E-01, 0.000000E+00 /


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
     & 3.464058E-20, 2.276631E-20, 1.367786E-20, 7.373137E-21, 
     & 8.850609E-22, 8.179470E-25, 0.000000E+00 /
      DATA ( CS_REF( IACETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.464058E-20, 2.276631E-20, 1.367786E-20, 7.373137E-21, 
     & 8.850609E-22, 8.179470E-25, 0.000000E+00 /
      DATA ( CS_REF( IACETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.464058E-20, 2.276631E-20, 1.367786E-20, 7.373137E-21, 
     & 8.850609E-22, 8.179470E-25, 0.000000E+00 /

C...  quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.457704E-01, 2.094483E-01, 8.573251E-02, 3.882141E-02, 
     & 9.387424E-03, 1.342100E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.457704E-01, 2.094483E-01, 8.573251E-02, 3.882141E-02, 
     & 9.387424E-03, 1.342100E-04, 0.000000E+00 /
      DATA ( QY_REF(  IACETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.457704E-01, 2.094483E-01, 8.573251E-02, 3.882141E-02, 
     & 9.387424E-03, 1.342100E-04, 0.000000E+00 /

C...  ! CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.207275E-20, 4.950919E-21, 1.203532E-21, 3.067005E-22, 
     & 1.419284E-23, 2.303166E-27, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.207275E-20, 4.950919E-21, 1.203532E-21, 3.067005E-22, 
     & 1.419284E-23, 2.303166E-27, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.207275E-20, 4.950919E-21, 1.203532E-21, 3.067005E-22, 
     & 1.419284E-23, 2.303166E-27, 0.000000E+00 /

C...  ! eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACETONE,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.485148E-01, 2.174669E-01, 8.799125E-02, 4.159702E-02, 
     & 1.603601E-02, 2.815788E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.485148E-01, 2.174669E-01, 8.799125E-02, 4.159702E-02, 
     & 1.603601E-02, 2.815788E-03, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.485148E-01, 2.174669E-01, 8.799125E-02, 4.159702E-02, 
     & 1.603601E-02, 2.815788E-03, 0.000000E+00 /


      END MODULE CSQY_DATA
