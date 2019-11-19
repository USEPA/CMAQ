      MODULE CSQY_DATA

      IMPLICIT NONE

C.....PARAMETERS and their descriptions:

      INTEGER, PARAMETER :: NPHOT_REF =  25 ! # ref phot reactions 

      INTEGER, PARAMETER :: NTEMP_REF =   3 ! # ref temperatures 

      INTEGER, PARAMETER :: NWL_REF   =   7 ! # ref wavelengths 

C...Names of the mapped photolysis reactions (available to chemical)
C... mechanisms) and their pointers to the reference photolysis rxn


      INTEGER, PARAMETER :: INO2_SAPRC99      =   1 ! pointer to NO2_SAPRC99     
      INTEGER, PARAMETER :: INO3NO_SAPRC99    =   2 ! pointer to NO3NO_SAPRC99   
      INTEGER, PARAMETER :: INO3NO2_SAPRC99   =   3 ! pointer to NO3NO2_SAPRC99  
      INTEGER, PARAMETER :: IO3O3P_SAPRC99    =   4 ! pointer to O3O3P_SAPRC99   
      INTEGER, PARAMETER :: IO3O1D_SAPRC99    =   5 ! pointer to O3O1D_SAPRC99   
      INTEGER, PARAMETER :: IHONO_NO_SAPRC99  =   6 ! pointer to HONO_NO_SAPRC99 
      INTEGER, PARAMETER :: IHONO_NO2_SAPRC99 =   7 ! pointer to HONO_NO2_SAPRC99
      INTEGER, PARAMETER :: IHNO3_SAPRC99     =   8 ! pointer to HNO3_SAPRC99    
      INTEGER, PARAMETER :: IHO2NO2_SAPRC99   =   9 ! pointer to HO2NO2_SAPRC99  
      INTEGER, PARAMETER :: IH2O2_SAPRC99     =  10 ! pointer to H2O2_SAPRC99    
      INTEGER, PARAMETER :: IHCHO_R_SAPRC99   =  11 ! pointer to HCHO_R_SAPRC99  
      INTEGER, PARAMETER :: IHCHO_M_SAPRC99   =  12 ! pointer to HCHO_M_SAPRC99  
      INTEGER, PARAMETER :: ICCHO_R_SAPRC99   =  13 ! pointer to CCHO_R_SAPRC99  
      INTEGER, PARAMETER :: IC2CHO_SAPRC99    =  14 ! pointer to C2CHO_SAPRC99   
      INTEGER, PARAMETER :: IACETONE_SAPRC99  =  15 ! pointer to ACETONE_SAPRC99 
      INTEGER, PARAMETER :: IKETONE_SAPRC99   =  16 ! pointer to KETONE_SAPRC99  
      INTEGER, PARAMETER :: ICOOH_SAPRC99     =  17 ! pointer to COOH_SAPRC99    
      INTEGER, PARAMETER :: IGLY_R_SAPRC99    =  18 ! pointer to GLY_R_SAPRC99   
      INTEGER, PARAMETER :: IGLY_ABS_SAPRC99  =  19 ! pointer to GLY_ABS_SAPRC99 
      INTEGER, PARAMETER :: IMGLY_ADJ_SAPRC99 =  20 ! pointer to MGLY_ADJ_SAPRC99
      INTEGER, PARAMETER :: IBACL_ADJ_SAPRC99 =  21 ! pointer to BACL_ADJ_SAPRC99
      INTEGER, PARAMETER :: IBZCHO_SAPRC99    =  22 ! pointer to BZCHO_SAPRC99   
      INTEGER, PARAMETER :: IACROLEIN_SAPRC99 =  23 ! pointer to ACROLEIN_SAPRC99
      INTEGER, PARAMETER :: IIC3ONO2_SAPRC99  =  24 ! pointer to IC3ONO2_SAPRC99 
      INTEGER, PARAMETER :: IMGLY_ABS_SAPRC99 =  25 ! pointer to MGLY_ABS_SAPRC99

      CHARACTER(16), SAVE :: PNAME_REF( NPHOT_REF )

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

C...Setup the Mapping from CMAQ chemical reactions to the reference data

      INTEGER, PARAMETER :: NPHOT_MAP =  25 ! #  phot mapped reactions 

      CHARACTER(16), SAVE :: PNAME_MAP( NPHOT_MAP )
      INTEGER, SAVE       :: PHOT_MAP( NPHOT_MAP )

      DATA PNAME_MAP(   1 ),  PHOT_MAP(   1 )  / 'NO2_SAPRC99     ', INO2_SAPRC99      / 
      DATA PNAME_MAP(   2 ),  PHOT_MAP(   2 )  / 'NO3NO_SAPRC99   ', INO3NO_SAPRC99    / 
      DATA PNAME_MAP(   3 ),  PHOT_MAP(   3 )  / 'NO3NO2_SAPRC99  ', INO3NO2_SAPRC99   / 
      DATA PNAME_MAP(   4 ),  PHOT_MAP(   4 )  / 'O3O3P_SAPRC99   ', IO3O3P_SAPRC99    / 
      DATA PNAME_MAP(   5 ),  PHOT_MAP(   5 )  / 'O3O1D_SAPRC99   ', IO3O1D_SAPRC99    / 
      DATA PNAME_MAP(   6 ),  PHOT_MAP(   6 )  / 'HONO_NO_SAPRC99 ', IHONO_NO_SAPRC99  / 
      DATA PNAME_MAP(   7 ),  PHOT_MAP(   7 )  / 'HONO_NO2_SAPRC99', IHONO_NO2_SAPRC99 / 
      DATA PNAME_MAP(   8 ),  PHOT_MAP(   8 )  / 'HNO3_SAPRC99    ', IHNO3_SAPRC99     / 
      DATA PNAME_MAP(   9 ),  PHOT_MAP(   9 )  / 'HO2NO2_SAPRC99  ', IHO2NO2_SAPRC99   / 
      DATA PNAME_MAP(  10 ),  PHOT_MAP(  10 )  / 'H2O2_SAPRC99    ', IH2O2_SAPRC99     / 
      DATA PNAME_MAP(  11 ),  PHOT_MAP(  11 )  / 'HCHO_R_SAPRC99  ', IHCHO_R_SAPRC99   / 
      DATA PNAME_MAP(  12 ),  PHOT_MAP(  12 )  / 'HCHO_M_SAPRC99  ', IHCHO_M_SAPRC99   / 
      DATA PNAME_MAP(  13 ),  PHOT_MAP(  13 )  / 'CCHO_R_SAPRC99  ', ICCHO_R_SAPRC99   / 
      DATA PNAME_MAP(  14 ),  PHOT_MAP(  14 )  / 'C2CHO_SAPRC99   ', IC2CHO_SAPRC99    / 
      DATA PNAME_MAP(  15 ),  PHOT_MAP(  15 )  / 'ACETONE_SAPRC99 ', IACETONE_SAPRC99  / 
      DATA PNAME_MAP(  16 ),  PHOT_MAP(  16 )  / 'KETONE_SAPRC99  ', IKETONE_SAPRC99   / 
      DATA PNAME_MAP(  17 ),  PHOT_MAP(  17 )  / 'COOH_SAPRC99    ', ICOOH_SAPRC99     / 
      DATA PNAME_MAP(  18 ),  PHOT_MAP(  18 )  / 'GLY_R_SAPRC99   ', IGLY_R_SAPRC99    / 
      DATA PNAME_MAP(  19 ),  PHOT_MAP(  19 )  / 'GLY_ABS_SAPRC99 ', IGLY_ABS_SAPRC99  / 
      DATA PNAME_MAP(  20 ),  PHOT_MAP(  20 )  / 'MGLY_ADJ_SAPRC99', IMGLY_ADJ_SAPRC99 / 
      DATA PNAME_MAP(  21 ),  PHOT_MAP(  21 )  / 'BACL_ADJ_SAPRC99', IBACL_ADJ_SAPRC99 / 
      DATA PNAME_MAP(  22 ),  PHOT_MAP(  22 )  / 'BZCHO_SAPRC99   ', IBZCHO_SAPRC99    / 
      DATA PNAME_MAP(  23 ),  PHOT_MAP(  23 )  / 'ACROLEIN_SAPRC99', IACROLEIN_SAPRC99 / 
      DATA PNAME_MAP(  24 ),  PHOT_MAP(  24 )  / 'IC3ONO2_SAPRC99 ', IIC3ONO2_SAPRC99  / 
      DATA PNAME_MAP(  25 ),  PHOT_MAP(  25 )  / 'MGLY_ABS_SAPRC99', IMGLY_ABS_SAPRC99 / 

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

C...NO2_SAPRC99
C..  NO2 + HV = NO + O
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO2_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.860793E-19, 2.249056E-19, 
     & 3.335557E-19, 5.492276E-19, 1.146035E-20 /
      DATA ( CS_REF( INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.860793E-19, 2.249056E-19, 
     & 3.335557E-19, 5.492276E-19, 1.146035E-20 /
      DATA ( CS_REF( INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.860793E-19, 2.249056E-19, 
     & 3.335557E-19, 5.492276E-19, 1.146035E-20 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986851E-01, 9.908283E-01, 
     & 9.900000E-01, 7.922024E-01, 7.010609E-04 /
      DATA ( QY_REF(  INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986851E-01, 9.908283E-01, 
     & 9.900000E-01, 7.922024E-01, 7.010609E-04 /
      DATA ( QY_REF(  INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 9.986851E-01, 9.908283E-01, 
     & 9.900000E-01, 7.922024E-01, 7.010609E-04 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.858225E-19, 2.228264E-19, 
     & 3.302202E-19, 4.272925E-19, 4.015361E-22 /
      DATA ( ECS_REF( INO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.858225E-19, 2.228264E-19, 
     & 3.302202E-19, 4.272925E-19, 4.015361E-22 /
      DATA ( ECS_REF( INO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.079359E-19, 1.478273E-19, 1.858225E-19, 2.228264E-19, 
     & 3.302202E-19, 4.272925E-19, 4.015361E-22 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO2_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 9.900000E-01, 7.779881E-01, 3.503697E-02 /
      DATA ( EQY_REF( INO2_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 9.900000E-01, 7.779881E-01, 3.503697E-02 /
      DATA ( EQY_REF( INO2_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 9.900000E-01, 7.779881E-01, 3.503697E-02 /


C...NO3NO_SAPRC99
C..  NO3 + HV = NO + O2 (T=298)
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO3NO_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.693338E-19 /
      DATA ( CS_REF( INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.693338E-19 /
      DATA ( CS_REF( INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 5.693338E-19 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.027929E-02 /
      DATA ( QY_REF(  INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.027929E-02 /
      DATA ( QY_REF(  INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 2.027929E-02 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.172148E-20 /
      DATA ( ECS_REF( INO3NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.172148E-20 /
      DATA ( ECS_REF( INO3NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 0.000000E+00, 8.172148E-20 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 0.000000E+00, 0.000000E+00, 1.435388E-01 /
      DATA ( EQY_REF( INO3NO_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 0.000000E+00, 0.000000E+00, 1.435388E-01 /
      DATA ( EQY_REF( INO3NO_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 0.000000E+00, 0.000000E+00, 1.435388E-01 /


C...NO3NO2_SAPRC99
C..  NO3 + HV = NO2 + O (T=298)
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, INO3NO2_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 1.112108E-18 /
      DATA ( CS_REF( INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 1.112108E-18 /
      DATA ( CS_REF( INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 1.112108E-18 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.812680E-01, 4.333254E-01 /
      DATA ( QY_REF(  INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.812680E-01, 4.333254E-01 /
      DATA ( QY_REF(  INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 2.812680E-01, 4.333254E-01 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 7.608258E-19 /
      DATA ( ECS_REF( INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 7.608258E-19 /
      DATA ( ECS_REF( INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 0.000000E+00, 0.000000E+00, 
     & 0.000000E+00, 4.263211E-21, 7.608258E-19 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( INO3NO2_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 0.000000E+00, 1.000000E+00, 6.841297E-01 /
      DATA ( EQY_REF( INO3NO2_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 0.000000E+00, 1.000000E+00, 6.841297E-01 /
      DATA ( EQY_REF( INO3NO2_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 0.000000E+00, 1.000000E+00, 6.841297E-01 /


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

      DATA ( TEMP_REF( ITTR, IO3O3P_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 6.115310E-21, 1.783046E-23, 1.650939E-21 /
      DATA ( CS_REF( IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 6.115310E-21, 1.783046E-23, 1.650939E-21 /
      DATA ( CS_REF( IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 6.115310E-21, 1.783046E-23, 1.650939E-21 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 6.227041E-02, 4.316183E-02, 4.576553E-01, 7.880843E-01, 
     & 9.572257E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( QY_REF(  IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 6.227041E-02, 4.316183E-02, 4.576553E-01, 7.880843E-01, 
     & 9.572257E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( QY_REF(  IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 6.227041E-02, 4.316183E-02, 4.576553E-01, 7.880843E-01, 
     & 9.572257E-01, 1.000000E+00, 1.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.234537E-20, 1.108793E-20, 4.421269E-20, 3.478935E-20, 
     & 5.673101E-21, 1.783046E-23, 1.650939E-21 /
      DATA ( ECS_REF( IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.234537E-20, 1.108793E-20, 4.421269E-20, 3.478935E-20, 
     & 5.673101E-21, 1.783046E-23, 1.650939E-21 /
      DATA ( ECS_REF( IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.234537E-20, 1.108793E-20, 4.421269E-20, 3.478935E-20, 
     & 5.673101E-21, 1.783046E-23, 1.650939E-21 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O3P_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 9.276882E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 9.276882E-01, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IO3O3P_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
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

      DATA ( TEMP_REF( ITTR, IO3O1D_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 5.919636E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 5.919636E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.244175E-19, 2.732249E-19, 1.048015E-19, 4.476706E-20, 
     & 5.919636E-21, 0.000000E+00, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.377296E-01, 9.568382E-01, 5.423447E-01, 2.119156E-01, 
     & 4.277415E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.377296E-01, 9.568382E-01, 5.423447E-01, 2.119156E-01, 
     & 4.277415E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.377296E-01, 9.568382E-01, 5.423447E-01, 2.119156E-01, 
     & 4.277415E-02, 0.000000E+00, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.720721E-19, 2.621370E-19, 6.058880E-20, 9.977705E-21, 
     & 4.422088E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.720721E-19, 2.621370E-19, 6.058880E-20, 9.977705E-21, 
     & 4.422088E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.720721E-19, 2.621370E-19, 6.058880E-20, 9.977705E-21, 
     & 4.422088E-22, 0.000000E+00, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IO3O1D_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 7.470202E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 7.470202E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IO3O1D_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 7.470202E-02, 0.000000E+00, 0.000000E+00 /


C...HONO_NO_SAPRC99
C..  HONO + HV = HO. + NO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHONO_NO_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218124E-20, 3.477931E-20, 
     & 1.090586E-19, 8.781876E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218124E-20, 3.477931E-20, 
     & 1.090586E-19, 8.781876E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218124E-20, 3.477931E-20, 
     & 1.090586E-19, 8.781876E-20, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.821204E-01, 4.690968E-01, 
     & 6.487832E-01, 6.909981E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.821204E-01, 4.690968E-01, 
     & 6.487832E-01, 6.909981E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 2.821204E-01, 4.690968E-01, 
     & 6.487832E-01, 6.909981E-01, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.061623E-21, 1.650458E-20, 
     & 7.360098E-20, 8.336919E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.061623E-21, 1.650458E-20, 
     & 7.360098E-20, 8.336919E-20, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 5.061623E-21, 1.650458E-20, 
     & 7.360098E-20, 8.336919E-20, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_NO_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 6.748757E-01, 9.493324E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 6.748757E-01, 9.493324E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 6.748757E-01, 9.493324E-01, 0.000000E+00 /


C...HONO_NO2_SAPRC99
C..  HONO + HV = H. + NO2
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHONO_NO2_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218124E-20, 3.477931E-20, 
     & 1.090586E-19, 3.942653E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218124E-20, 3.477931E-20, 
     & 1.090586E-19, 3.942653E-20, 0.000000E+00 /
      DATA ( CS_REF( IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 1.218124E-20, 3.477931E-20, 
     & 1.090586E-19, 3.942653E-20, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 3.994613E-01, 5.309032E-01, 
     & 3.512168E-01, 2.663601E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 3.994613E-01, 5.309032E-01, 
     & 3.512168E-01, 2.663601E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 3.994613E-01, 5.309032E-01, 
     & 3.512168E-01, 2.663601E-02, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.119612E-21, 1.827472E-20, 
     & 3.545759E-20, 4.449553E-21, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.119612E-21, 1.827472E-20, 
     & 3.545759E-20, 4.449553E-21, 0.000000E+00 /
      DATA ( ECS_REF( IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 0.000000E+00, 7.119612E-21, 1.827472E-20, 
     & 3.545759E-20, 4.449553E-21, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHONO_NO2_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 3.251243E-01, 1.128568E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO2_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 3.251243E-01, 1.128568E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHONO_NO2_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 3.251243E-01, 1.128568E-01, 0.000000E+00 /


C...HNO3_SAPRC99
C..  HNO3 + HV = products
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHNO3_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875234E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875234E-25, 0.000000E+00 /
      DATA ( CS_REF( IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875234E-25, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.139071E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.139071E-01, 0.000000E+00 /
      DATA ( QY_REF(  IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.139071E-01, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHNO3_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875234E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875234E-25, 0.000000E+00 /
      DATA ( ECS_REF( IHNO3_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.358850E-21, 1.954554E-21, 8.462462E-22, 3.649777E-22, 
     & 4.770195E-23, 3.875234E-25, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHNO3_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHNO3_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /


C...HO2NO2_SAPRC99
C..  HO2NO2 + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHO2NO2_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327373E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327373E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327373E-22, 0.000000E+00, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.743980E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.743980E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.743980E-01, 0.000000E+00, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327373E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327373E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.591178E-20, 1.079991E-20, 5.527937E-21, 3.457473E-21, 
     & 6.327373E-22, 0.000000E+00, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHO2NO2_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHO2NO2_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 0.000000E+00, 0.000000E+00 /


C...H2O2_SAPRC99
C..  H2O2 + HV = 2 OH
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IH2O2_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606273E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606273E-23, 0.000000E+00 /
      DATA ( CS_REF( IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606273E-23, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.139071E-01, 0.000000E+00 /
      DATA ( QY_REF(  IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.139071E-01, 0.000000E+00 /
      DATA ( QY_REF(  IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.139071E-01, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IH2O2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606273E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606273E-23, 0.000000E+00 /
      DATA ( ECS_REF( IH2O2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 9.353266E-21, 5.771016E-21, 3.923017E-21, 2.721918E-21, 
     & 1.138123E-21, 3.606273E-23, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IH2O2_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IH2O2_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /


C...HCHO_R_SAPRC99
C..  HCHO + HV = HCO + H
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHCHO_R_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.384120E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.384120E-20, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.384120E-20, 0.000000E+00, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.530258E-01, 7.793080E-01, 7.694805E-01, 6.766393E-01, 
     & 2.101615E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.530258E-01, 7.793080E-01, 7.694805E-01, 6.766393E-01, 
     & 2.101615E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.530258E-01, 7.793080E-01, 7.694805E-01, 6.766393E-01, 
     & 2.101615E-01, 0.000000E+00, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.360363E-20, 2.619580E-20, 1.264450E-20, 2.110757E-20, 
     & 3.830403E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.360363E-20, 2.619580E-20, 1.264450E-20, 2.110757E-20, 
     & 3.830403E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.360363E-20, 2.619580E-20, 1.264450E-20, 2.110757E-20, 
     & 3.830403E-21, 0.000000E+00, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHO_R_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 2.767393E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_R_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 2.767393E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_R_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 2.767393E-01, 0.000000E+00, 0.000000E+00 /


C...HCHO_M_SAPRC99
C..  HCHO + HV = H2 + CO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IHCHO_M_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.653456E-20, 7.218154E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.653456E-20, 7.218154E-22, 0.000000E+00 /
      DATA ( CS_REF( IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.131429E-20, 3.361011E-20, 1.633825E-20, 3.089588E-20, 
     & 1.653456E-20, 7.218154E-22, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.261845E-01, 2.137267E-01, 2.298757E-01, 3.233053E-01, 
     & 5.541007E-01, 2.985781E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.261845E-01, 2.137267E-01, 2.298757E-01, 3.233053E-01, 
     & 5.541007E-01, 2.985781E-02, 0.000000E+00 /
      DATA ( QY_REF(  IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.261845E-01, 2.137267E-01, 2.298757E-01, 3.233053E-01, 
     & 5.541007E-01, 2.985781E-02, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 7.068908E-21, 7.200789E-21, 3.679953E-21, 9.787576E-21, 
     & 9.269445E-21, 1.144537E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 7.068908E-21, 7.200789E-21, 3.679953E-21, 9.787576E-21, 
     & 9.269445E-21, 1.144537E-22, 0.000000E+00 /
      DATA ( ECS_REF( IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 7.068908E-21, 7.200789E-21, 3.679953E-21, 9.787576E-21, 
     & 9.269445E-21, 1.144537E-22, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IHCHO_M_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 5.606101E-01, 1.585636E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_M_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 5.606101E-01, 1.585636E-01, 0.000000E+00 /
      DATA ( EQY_REF( IHCHO_M_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 5.606101E-01, 1.585636E-01, 0.000000E+00 /


C...CCHO_R_SAPRC99
C..  CCHO + HV = CH3 + CHO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICCHO_R_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.432586E-20, 3.717937E-20, 2.933103E-20, 2.104728E-20, 
     & 3.707365E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.432586E-20, 3.717937E-20, 2.933103E-20, 2.104728E-20, 
     & 3.707365E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.432586E-20, 3.717937E-20, 2.933103E-20, 2.104728E-20, 
     & 3.707365E-21, 0.000000E+00, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902037E-01, 2.845214E-01, 1.538485E-01, 
     & 1.409389E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902037E-01, 2.845214E-01, 1.538485E-01, 
     & 1.409389E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.839485E-01, 3.902037E-01, 2.845214E-01, 1.538485E-01, 
     & 1.409389E-02, 0.000000E+00, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461038E-20, 8.426962E-21, 3.330092E-21, 
     & 1.731214E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461038E-20, 8.426962E-21, 3.330092E-21, 
     & 1.731214E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.147745E-20, 1.461038E-20, 8.426962E-21, 3.330092E-21, 
     & 1.731214E-22, 0.000000E+00, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICCHO_R_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 4.669661E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 4.669661E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICCHO_R_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 4.669661E-02, 0.000000E+00, 0.000000E+00 /


C...C2CHO_SAPRC99
C..  C2CHO + HV = C2H5. + CHO.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IC2CHO_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.094719E-20, 4.634689E-20, 3.579653E-20, 2.441742E-20, 
     & 5.808274E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.094719E-20, 4.634689E-20, 3.579653E-20, 2.441742E-20, 
     & 5.808274E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.094719E-20, 4.634689E-20, 3.579653E-20, 2.441742E-20, 
     & 5.808274E-21, 0.000000E+00, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.083616E-01, 7.954021E-01, 5.951666E-01, 4.312297E-01, 
     & 1.520061E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.083616E-01, 7.954021E-01, 5.951666E-01, 4.312297E-01, 
     & 1.520061E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.083616E-01, 7.954021E-01, 5.951666E-01, 4.312297E-01, 
     & 1.520061E-01, 0.000000E+00, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.721900E-20, 3.713801E-20, 2.133677E-20, 1.077360E-20, 
     & 1.383933E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.721900E-20, 3.713801E-20, 2.133677E-20, 1.077360E-20, 
     & 1.383933E-21, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.721900E-20, 3.713801E-20, 2.133677E-20, 1.077360E-20, 
     & 1.383933E-21, 0.000000E+00, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IC2CHO_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 2.382693E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 2.382693E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IC2CHO_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 2.382693E-01, 0.000000E+00, 0.000000E+00 /


C...ACETONE_SAPRC99
C..  ACETONE + HV = CH3CO. + CH3.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IACETONE_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.565269E-20, 2.347503E-20, 1.411211E-20, 7.530059E-21, 
     & 8.363443E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.565269E-20, 2.347503E-20, 1.411211E-20, 7.530059E-21, 
     & 8.363443E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.565269E-20, 2.347503E-20, 1.411211E-20, 7.530059E-21, 
     & 8.363443E-22, 0.000000E+00, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.242420E-01, 1.142595E-01, 5.803515E-02, 2.870061E-02, 
     & 4.434752E-03, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.242420E-01, 1.142595E-01, 5.803515E-02, 2.870061E-02, 
     & 4.434752E-03, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.242420E-01, 1.142595E-01, 5.803515E-02, 2.870061E-02, 
     & 4.434752E-03, 0.000000E+00, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 8.097962E-21, 2.778257E-21, 8.357552E-22, 2.321761E-22, 
     & 8.431038E-24, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 8.097962E-21, 2.778257E-21, 8.357552E-22, 2.321761E-22, 
     & 8.431038E-24, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IACETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 8.097962E-21, 2.778257E-21, 8.357552E-22, 2.321761E-22, 
     & 8.431038E-24, 0.000000E+00, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACETONE_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.008082E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.008082E-02, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACETONE_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.008082E-02, 0.000000E+00, 0.000000E+00 /


C...KETONE_SAPRC99
C..  Methyl Ethyl Ketone Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IKETONE_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479068E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479068E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479068E-22, 0.000000E+00, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.157063E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.157063E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.157063E-01, 0.000000E+00, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IKETONE_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479068E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479068E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IKETONE_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.255141E-20, 2.715762E-20, 1.567299E-20, 7.669451E-21, 
     & 7.479068E-22, 0.000000E+00, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IKETONE_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IKETONE_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 0.000000E+00, 0.000000E+00 /


C...COOH_SAPRC99
C..  CH3OOH + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, ICOOH_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395084E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395084E-23, 0.000000E+00 /
      DATA ( CS_REF( ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395084E-23, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.012673E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.012673E-01, 0.000000E+00 /
      DATA ( QY_REF(  ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 3.012673E-01, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( ICOOH_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395084E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395084E-23, 0.000000E+00 /
      DATA ( ECS_REF( ICOOH_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 5.623467E-21, 3.524633E-21, 2.410330E-21, 1.699588E-21, 
     & 7.230005E-22, 5.395084E-23, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( ICOOH_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( ICOOH_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /


C...GLY_R_SAPRC99
C..  Glyoxal + hv = 2 HCO
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IGLY_R_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 6.211823E-22 /
      DATA ( CS_REF( IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 6.211823E-22 /
      DATA ( CS_REF( IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 6.211823E-22 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.446526E-01, 0.000000E+00 /
      DATA ( QY_REF(  IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.446526E-01, 0.000000E+00 /
      DATA ( QY_REF(  IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.446526E-01, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 4.361289E-21, 0.000000E+00 /
      DATA ( ECS_REF( IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 4.361289E-21, 0.000000E+00 /
      DATA ( ECS_REF( IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 4.361289E-21, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_R_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 2.143213E-01, 0.000000E+00 /
      DATA ( EQY_REF( IGLY_R_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 2.143213E-01, 0.000000E+00 /
      DATA ( EQY_REF( IGLY_R_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 2.143213E-01, 0.000000E+00 /


C...GLY_ABS_SAPRC99
C..  Glyoxal Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IGLY_ABS_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746831E-21 /
      DATA ( CS_REF( IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746831E-21 /
      DATA ( CS_REF( IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746831E-21 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.529012E-02 /
      DATA ( QY_REF(  IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.529012E-02 /
      DATA ( QY_REF(  IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 9.529012E-02 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746831E-21 /
      DATA ( ECS_REF( IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746831E-21 /
      DATA ( ECS_REF( IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 3.307062E-20, 3.064663E-20, 2.759448E-20, 2.066155E-20, 
     & 6.469823E-21, 2.034930E-20, 7.746831E-21 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /


C...MGLY_ADJ_SAPRC99
C..  MGLY + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_ADJ_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 1.472530E-21 /
      DATA ( CS_REF( IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 1.472530E-21 /
      DATA ( CS_REF( IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 1.472530E-21 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992942E-01, 3.801419E-01, 0.000000E+00 /
      DATA ( QY_REF(  IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992942E-01, 3.801419E-01, 0.000000E+00 /
      DATA ( QY_REF(  IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 9.992942E-01, 3.801419E-01, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.996164E-21, 6.110804E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.996164E-21, 6.110804E-21, 0.000000E+00 /
      DATA ( ECS_REF( IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.996164E-21, 6.110804E-21, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 9.996566E-01, 1.652672E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 9.996566E-01, 1.652672E-01, 0.000000E+00 /
      DATA ( EQY_REF( IMGLY_ADJ_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 9.996566E-01, 1.652672E-01, 0.000000E+00 /


C...BACL_ADJ_SAPRC99
C..  BACL + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IBACL_ADJ_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 3.224516E-20, 4.675102E-21 /
      DATA ( CS_REF( IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 3.224516E-20, 4.675102E-21 /
      DATA ( CS_REF( IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 3.224516E-20, 4.675102E-21 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.303440E-01, 9.162429E-04 /
      DATA ( QY_REF(  IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.303440E-01, 9.162429E-04 /
      DATA ( QY_REF(  IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.303440E-01, 9.162429E-04 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 1.215315E-20, 6.213299E-23 /
      DATA ( ECS_REF( IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 1.215315E-20, 6.213299E-23 /
      DATA ( ECS_REF( IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.617461E-20, 1.589011E-20, 9.041847E-21, 6.004408E-21, 
     & 4.676505E-21, 1.215315E-20, 6.213299E-23 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBACL_ADJ_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 3.768983E-01, 1.329019E-02 /
      DATA ( EQY_REF( IBACL_ADJ_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 3.768983E-01, 1.329019E-02 /
      DATA ( EQY_REF( IBACL_ADJ_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 3.768983E-01, 1.329019E-02 /


C...BZCHO_SAPRC99
C..  Benzaldehyde absorbtion coefs in n-Hexane
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IBZCHO_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.224145E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821756E-20, 0.000000E+00 /
      DATA ( CS_REF( IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.224145E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821756E-20, 0.000000E+00 /
      DATA ( CS_REF( IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.224145E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821756E-20, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.339651E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.150171E-01, 0.000000E+00 /
      DATA ( QY_REF(  IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.339651E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.150171E-01, 0.000000E+00 /
      DATA ( QY_REF(  IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.339651E-01, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 5.150171E-01, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.224145E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821756E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.224145E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821756E-20, 0.000000E+00 /
      DATA ( ECS_REF( IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 0.000000E+00, 9.224145E-20, 6.609039E-20, 6.730973E-20, 
     & 8.248212E-20, 2.821756E-20, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IBZCHO_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IBZCHO_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IBZCHO_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /


C...ACROLEIN_SAPRC99
C..  Absorption cross sections for Acrolein.
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IACROLEIN_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /
      DATA ( CS_REF( IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /
      DATA ( CS_REF( IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.561667E-01, 0.000000E+00 /
      DATA ( QY_REF(  IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.561667E-01, 0.000000E+00 /
      DATA ( QY_REF(  IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 4.561667E-01, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /
      DATA ( ECS_REF( IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /
      DATA ( ECS_REF( IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 2.119555E-20, 3.145168E-20, 4.081558E-20, 4.837755E-20, 
     & 5.750342E-20, 1.189679E-20, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IACROLEIN_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACROLEIN_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IACROLEIN_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 0.000000E+00 /


C...IC3ONO2_SAPRC99
C..  I-C3H7ONO2 + HV = PRODUCTS
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IIC3ONO2_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667170E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667170E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( CS_REF( IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667170E-22, 0.000000E+00, 0.000000E+00 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.743980E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.743980E-01, 0.000000E+00, 0.000000E+00 /
      DATA ( QY_REF(  IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 5.743980E-01, 0.000000E+00, 0.000000E+00 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667170E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667170E-22, 0.000000E+00, 0.000000E+00 /
      DATA ( ECS_REF( IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.255484E-20, 6.352138E-21, 3.286576E-21, 1.709805E-21, 
     & 2.667170E-22, 0.000000E+00, 0.000000E+00 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IIC3ONO2_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 0.000000E+00, 0.000000E+00 /
      DATA ( EQY_REF( IIC3ONO2_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 0.000000E+00, 0.000000E+00 /


C...MGLY_ABS_SAPRC99
C..  Methyl Glyoxal Absorption Cross Sections
C..  SAPRC-99 Photolysis data.  Supplied by William P. L. Carter.
C..  Created from PhotDat.xls on 29-Jan-2000 10:07

C...  reference temperatures (K)

      DATA ( TEMP_REF( ITTR, IMGLY_ABS_SAPRC99 ), ITTR=1,  3 ) / 248.0, 273.0, 298.0 /

!...  CS  = absorption cross sections averaged over UCI Solar Flux

      DATA ( CS_REF( IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( CS_REF( IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( CS_REF( IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /

!...  QY  = quantum yields averaged over UCI Solar Flux

      DATA ( QY_REF(  IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.688528E-01 /
      DATA ( QY_REF(  IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.688528E-01 /
      DATA ( QY_REF(  IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00, 1.000000E+00, 
     & 1.000000E+00, 1.000000E+00, 1.688528E-01 /

!...  ECS = CS*QY averaged over UCI Solar Flux

      DATA ( ECS_REF( IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( ECS_REF( IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /
      DATA ( ECS_REF( IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1, 7 ) /
     & 4.414885E-20, 3.510064E-20, 2.364210E-20, 1.814704E-20, 
     & 5.998223E-21, 3.697529E-20, 7.930063E-21 /

!...  EQY = eCS*eQY/CS averaged over Solar Flux and 77 bins in UCI Model

      DATA ( EQY_REF( IMGLY_ABS_SAPRC99,   1, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IMGLY_ABS_SAPRC99,   2, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /
      DATA ( EQY_REF( IMGLY_ABS_SAPRC99,   3, IWLR ), IWLR = 1,   7 ) /
     & 1.000000E+00, 1.000000E+00, 1.000000E+00 /


      END MODULE CSQY_DATA
