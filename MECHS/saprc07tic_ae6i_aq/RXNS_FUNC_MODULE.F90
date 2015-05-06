       MODULE RXNS_FUNCTION


       IMPLICIT NONE



! Name of Mechanism SAPRC07TIC_AE6I_AQ

       PUBLIC             :: CALC_RCONST, SPECIAL_RATES, MAP_CHEMISTRY_SPECIES

       CONTAINS


       REAL( 8 ) FUNCTION POWER_T02( TEMPOT300,A0,B0 )
         IMPLICIT NONE
! rate constant for CMAQ Arrhenuis reaction type 2
! Arguements:
         REAL( 8 ), INTENT( IN ) :: TEMPOT300
         REAL( 8 ), INTENT( IN ) :: A0
         REAL( 8 ), INTENT( IN ) :: B0
         ! Local: None
         POWER_T02 =  A0 * TEMPOT300**B0
         RETURN
       END FUNCTION POWER_T02
       REAL( 8 ) FUNCTION ARRHENUIS_T04( INV_TEMP,TEMPOT300,A0,B0,C0 )
         IMPLICIT NONE
! rate constant for CMAQ Arrhenuis reaction type 4
! Arguements:
         REAL( 8 ), INTENT( IN ) :: INV_TEMP
         REAL( 8 ), INTENT( IN ) :: TEMPOT300
         REAL( 8 ), INTENT( IN ) :: A0
         REAL( 8 ), INTENT( IN ) :: B0
         REAL( 8 ), INTENT( IN ) :: C0
         ! Local:
         INTRINSIC DEXP
         ARRHENUIS_T04 =  A0 * DEXP( B0 * INV_TEMP ) * TEMPOT300**C0
         RETURN
       END FUNCTION ARRHENUIS_T04
       REAL( 8 ) FUNCTION ARRHENUIS_T03( INV_TEMP,A0,B0 )
! rate constant for CMAQ Arrhenuis reaction type 3
         IMPLICIT NONE
! Arguements:
         REAL( 8 ),   INTENT( IN ) ::  INV_TEMP
         REAL( 8 ),     INTENT(IN) ::  A0
         REAL( 8 ),     INTENT(IN) ::  B0
         ! Local:
         INTRINSIC DEXP
         ARRHENUIS_T03 =  A0 * DEXP( B0 * INV_TEMP )
         RETURN
       END FUNCTION ARRHENUIS_T03 
       REAL( 8 ) FUNCTION FALLOFF_T08(INV_TEMP,CAIR,A0,C0,A2,C2,A3,C3)
! rate constant for CMAQ fall off reaction type 8
         IMPLICIT NONE
! Arguements:
         REAL( 8 ), INTENT( IN ) :: INV_TEMP
         REAL( 8 ), INTENT( IN ) :: CAIR
         REAL( 8 ), INTENT( IN ) :: A0
         REAL( 8 ), INTENT( IN ) :: C0
         REAL( 8 ), INTENT( IN ) :: A2
         REAL( 8 ), INTENT( IN ) :: C2
         REAL( 8 ), INTENT( IN ) :: A3
         REAL( 8 ), INTENT( IN ) :: C3
         ! Local:
         REAL( 8 ) K0
         REAL( 8 ) K2
         REAL( 8 ) K3
         INTRINSIC DEXP
         K0 = A0 * DEXP( C0 * INV_TEMP )
         K2 = A2 * DEXP( C2 * INV_TEMP )
         K3 = A3 * DEXP( C3 * INV_TEMP )
         K3 = K3 * CAIR
         FALLOFF_T08 = K0 + K3/( 1.0D0 + K3/K2 )
         RETURN
       END FUNCTION FALLOFF_T08
       REAL( 8 ) FUNCTION FALLOFF_T09(INV_TEMP,CAIR,A1,C1,A2,C2)
! rate constant for CMAQ fall off reaction type 9
         IMPLICIT NONE
! Arguements:
         REAL( 8 ), INTENT( IN ) :: INV_TEMP
         REAL( 8 ), INTENT( IN ) :: CAIR
         REAL( 8 ), INTENT( IN ) :: A1
         REAL( 8 ), INTENT( IN ) :: C1
         REAL( 8 ), INTENT( IN ) :: A2
         REAL( 8 ), INTENT( IN ) :: C2
         !  Local:
         REAL( 8 ) K1
         REAL( 8 ) K2
         INTRINSIC DEXP
         K1 = A1 * DEXP( C1 * INV_TEMP )
         K2 = A2 * DEXP( C2 * INV_TEMP )
         FALLOFF_T09 = K1 + K2 * CAIR
         RETURN
       END FUNCTION FALLOFF_T09
       REAL( 8 ) FUNCTION FALLOFF_T10(INV_TEMP,TEMPOT300,CAIR,A0,B0,C0,A1,B1,C1,CE,CF)
         IMPLICIT NONE
! rate constant for CMAQ fall off reaction type 10
! Arguements:
         REAL( 8 ), INTENT( IN ) :: INV_TEMP
         REAL( 8 ), INTENT( IN ) :: TEMPOT300
         REAL( 8 ), INTENT( IN ) :: CAIR
         REAL( 8 ), INTENT( IN ) :: A0
         REAL( 8 ), INTENT( IN ) :: B0
         REAL( 8 ), INTENT( IN ) :: C0
         REAL( 8 ), INTENT( IN ) :: A1
         REAL( 8 ), INTENT( IN ) :: B1
         REAL( 8 ), INTENT( IN ) :: C1
         REAL( 8 ), INTENT( IN ) :: CE
         REAL( 8 ), INTENT( IN ) :: CF
         ! Local:
         REAL( 8 ) K0
         REAL( 8 ) K1
         REAL( 8 ) KEND
         K0 = A0 * CAIR * DEXP(B0*INV_TEMP)* TEMPOT300**C0
         K1 = A1 * DEXP(B1*INV_TEMP) * TEMPOT300**C1
         KEND = ( ( 1.0D0 + ( ( 1.0D0 / CE ) * DLOG10( K0 / K1 ) ) ** 2.0D0 ) )
         KEND = 1.0D0 / KEND
         FALLOFF_T10 = ( K0 / ( 1.0D0 + K0/K1 ) ) * CF ** KEND
         RETURN
       END FUNCTION FALLOFF_T10
       REAL( 8 ) FUNCTION FALLOFF_T11(INV_TEMP,TEMPOT300,CAIR,A1,B1,C1,A2, B2, C2, D1, D2)
! rate constant for CMAQ fall off reaction type 11
! actually expanded form of type 9
         IMPLICIT NONE
! Arguements:
         REAL( 8 ), INTENT( IN ) :: INV_TEMP
         REAL( 8 ), INTENT( IN ) :: TEMPOT300
         REAL( 8 ), INTENT( IN ) :: CAIR
         REAL( 8 ), INTENT( IN ) :: A1
         REAL( 8 ), INTENT( IN ) :: B1
         REAL( 8 ), INTENT( IN ) :: C1
         REAL( 8 ), INTENT( IN ) :: A2
         REAL( 8 ), INTENT( IN ) :: B2
         REAL( 8 ), INTENT( IN ) :: C2
         REAL( 8 ), INTENT( IN ) :: D1
         REAL( 8 ), INTENT( IN ) :: D2
         !  Local:
         REAL( 8 ) K1
         REAL( 8 ) K2
         REAL( 8 ) K3
         INTRINSIC DEXP
         K1 = A1 * DEXP( C1 * INV_TEMP ) * TEMPOT300**B1
         K2 = A2 * DEXP( C2 * INV_TEMP ) * TEMPOT300**B2
         K3 = D1 * DEXP( D2 * INV_TEMP )
         FALLOFF_T11 = K1 + K2 * CAIR + K3
         RETURN
       END FUNCTION FALLOFF_T11
       REAL( 8 ) FUNCTION HALOGEN_FALLOFF(PRESS,A1,B1,A2,B2)
         IMPLICIT NONE
         REAL( 8 ), INTENT( IN ) :: PRESS
         REAL( 8 ), INTENT( IN ) :: A1
         REAL( 8 ), INTENT( IN ) :: B1
         REAL( 8 ), INTENT( IN ) :: A2
         REAL( 8 ), INTENT( IN ) :: B2
         INTRINSIC DEXP
         HALOGEN_FALLOFF = A1 * DEXP( B1 * PRESS ) + A2 * DEXP( B2 * PRESS )
         RETURN
       END FUNCTION HALOGEN_FALLOFF

       SUBROUTINE SPECIAL_RATES( NUMCELLS, IOLD2NEW, NCS, Y, RKI )
! Purpose: calculate special rate operators and update
!         appropriate rate constants

       USE RXNS_DATA
       IMPLICIT NONE

! Arguments:
       INTEGER,      INTENT( IN  )   :: NUMCELLS        ! Number of cells in block 
       INTEGER,      INTENT( IN  )   :: IOLD2NEW( :,: ) ! species map
       INTEGER,      INTENT( IN  )   :: NCS             ! index for which reaction set
       REAL( 8 ),    INTENT( IN )    :: Y( :, : )       ! species concs
       REAL( 8 ),    INTENT( INOUT ) :: RKI( :, : )     ! reaction rate constant, ppm/min 
! Local:
       INTEGER  NCELL
! special rate operators listed below



       DO NCELL = 1, NUMCELLS

! define special rate operators


! define rate constants in terms of special rate operators 

       END DO

       RETURN
       END SUBROUTINE SPECIAL_RATES
 
       SUBROUTINE CALC_RCONST( BLKTEMP, BLKPRES, BLKH2O, RJBLK, BLKHET, LSUNLIGHT, LAND, RKI, NUMCELLS )

!**********************************************************************

!  Function: To compute thermal and photolytic reaction rate
!            coefficients for each reaction.

!  Preconditions: Photolysis rates for individual species must have
!                 been calculated and stored in RJPHOT. Expects
!                 temperature in deg K, pressure in atm., water
!                 vapor in ppmV, and J-values in /min.
!  Key Subroutines/Functions Called: POWER_02, ARRHRENUIS_T0*, FALLOFF_T*, HALOGEN_FALLOFF 
!***********************************************************************




       USE RXNS_DATA

        IMPLICIT NONE  

!  Arguements: None 

        REAL( 8 ),           INTENT( IN  ) :: BLKTEMP( : )      ! temperature, deg K 
        REAL( 8 ),           INTENT( IN  ) :: BLKPRES( : )      ! pressure, Atm
        REAL( 8 ),           INTENT( IN  ) :: BLKH2O ( : )      ! water mixing ratio, ppm 
        REAL( 8 ),           INTENT( IN  ) :: RJBLK  ( :, : )   ! photolysis rates, 1/min 
        REAL( 8 ),           INTENT( IN  ) :: BLKHET ( :, : )   ! heterogeneous rate constants, ???/min
        INTEGER,             INTENT( IN  ) :: NUMCELLS          ! Number of cells in block 
        LOGICAL,             INTENT( IN  ) :: LSUNLIGHT         ! Is there sunlight? 
        LOGICAL,             INTENT( IN  ) :: LAND( : )         ! Is the surface totally land? 
        REAL( 8 ),           INTENT( OUT ) :: RKI ( :, : )      ! reaction rate constant, ppm/min 
!..Parameters: 

        REAL( 8 ), PARAMETER :: COEF1  = 7.33981D+15     ! Molec/cc to ppm conv factor 
        REAL( 8 ), PARAMETER :: CONSTC = 0.6D+0          ! Constant for reaction type 7
        REAL( 8 ), PARAMETER :: TI300  = 1.0D+0/300.0D+0 ! reciprocal of 300 deg K
        REAL( 8 ), PARAMETER :: SFACT  = 60.D+0          ! seconds per minute 
!..External Functions: None

!..Local Variables:

        INTEGER   :: NRT           ! Loop index for reaction types 
        INTEGER   :: IRXN          ! Reaction number
        INTEGER   :: JNUM          ! J-value species # from PHOT)
        INTEGER   :: KNUM          ! Reaction # for a relative rate coeff.
        INTEGER   :: N             ! Loop index for reactions
        INTEGER   :: NCELL         ! Loop index for # of cells in the block
        REAL( 8 ) :: CAIR          ! air number density (wet) [molec/cm^3]
        REAL( 8 ) :: CFACT         ! Convertor cm^3/(molec*sec) to 1/(ppm*min)
        REAL( 8 ) :: CFACT_SQU     ! Convertor cm^6/(molec^2*sec) to 1/(ppm^2*min)
        REAL( 8 ) :: INV_CFACT     ! ppm/min to molec/(cm^3*sec)
        REAL( 8 ) :: TEMPOT300     ! temperature divided by 300 K, dimensionaless 
        REAL( 8 ) :: INV_TEMP      ! reciprocal of air temperature, K-1
        REAL( 8 ) :: INV_CAIR      ! reciprocal of air number density (wet), [cm^3/molec]
        REAL( 8 ) :: TEMP          ! air temperature, K
        REAL( 8 ) :: PRESS         ! pressure [Atm] 
        REAL( 8 ) :: INV_RFACT     ! ppm/min to molec/(cm^3*min)
        REAL( 8 ) :: RFACT_SQU     ! cm^6/(molec^2*min) to 1/(ppm^2*min)
        REAL( 8 ) :: RFACT         ! cm^3/(molec*min) to 1/(ppm*min)
        REAL      :: H2O           ! Cell H2O mixing ratio (ppmV)

        RKI = 0.0D0 

! All rate constants converted from  molec/cm3 to ppm
! and 1/sec to 1/min

        IF( LSUNLIGHT )THEN 
            DO NCELL = 1, NUMCELLS 

!  Reaction Label 1               
                RKI( NCELL,    1) =  RJBLK( NCELL, IJ_NO2_06 )
!  Reaction Label 16              
                RKI( NCELL,   16) =  RJBLK( NCELL, IJ_NO3NO_06 )
!  Reaction Label 17              
                RKI( NCELL,   17) =  RJBLK( NCELL, IJ_NO3NO2_6 )
!  Reaction Label 18              
                RKI( NCELL,   18) =  RJBLK( NCELL, IJ_O3O1D_06 )
!  Reaction Label 19              
                RKI( NCELL,   19) =  RJBLK( NCELL, IJ_O3O3P_06 )
!  Reaction Label 23              
                RKI( NCELL,   23) =  RJBLK( NCELL, IJ_HONO_06 )
!  Reaction Label 28              
                RKI( NCELL,   28) =  RJBLK( NCELL, IJ_HNO3 )
!  Reaction Label 34              
                RKI( NCELL,   34) =  RJBLK( NCELL, IJ_HNO4_06 )
!  Reaction Label 41              
                RKI( NCELL,   41) =  RJBLK( NCELL, IJ_H2O2 )
!  Reaction Label BR20            
                RKI( NCELL,   65) =  RJBLK( NCELL, IJ_PAN )
!  Reaction Label BR30            
                RKI( NCELL,   75) =  RJBLK( NCELL, IJ_PAN )
!  Reaction Label BR41            
                RKI( NCELL,   86) =  RJBLK( NCELL, IJ_PAN )
!  Reaction Label BR53            
                RKI( NCELL,   98) =  RJBLK( NCELL, IJ_PAN )
!  Reaction Label BP01            
                RKI( NCELL,  194) =  RJBLK( NCELL, IJ_HCHOR_06 )
!  Reaction Label BP02            
                RKI( NCELL,  195) =  RJBLK( NCELL, IJ_HCHOM_06 )
!  Reaction Label BP09            
                RKI( NCELL,  199) =  RJBLK( NCELL, IJ_CCHO_R )
!  Reaction Label BP12            
                RKI( NCELL,  202) =  RJBLK( NCELL, IJ_C2CHO )
!  Reaction Label BP15            
                RKI( NCELL,  205) =   5.0000D-01 * RJBLK( NCELL, IJ_ACET_06 )
!  Reaction Label BP17            
                RKI( NCELL,  207) =   1.7500D-01 * RJBLK( NCELL, IJ_MEK_06 )
!  Reaction Label BP23            
                RKI( NCELL,  213) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label BP25            
                RKI( NCELL,  215) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label BP27            
                RKI( NCELL,  217) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label BP29            
                RKI( NCELL,  219) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label BP30            
                RKI( NCELL,  220) =  RJBLK( NCELL, IJ_GLY_07R )
!  Reaction Label BP31            
                RKI( NCELL,  221) =  RJBLK( NCELL, IJ_GLY_07M )
!  Reaction Label BP34            
                RKI( NCELL,  224) =  RJBLK( NCELL, IJ_MGLY_06 )
!  Reaction Label BP37            
                RKI( NCELL,  227) =  RJBLK( NCELL, IJ_BACL_07 )
!  Reaction Label BP41            
                RKI( NCELL,  231) =   1.5000D-03 * RJBLK( NCELL, IJ_NO2_06 )
!  Reaction Label BP42            
                RKI( NCELL,  232) =   1.5000D-02 * RJBLK( NCELL, IJ_NO2_06 )
!  Reaction Label BP44            
                RKI( NCELL,  234) =   6.0000D-02 * RJBLK( NCELL, IJ_BALD_06 )
!  Reaction Label BP48            
                RKI( NCELL,  238) =  RJBLK( NCELL, IJ_AFG1 )
!  Reaction Label BP51            
                RKI( NCELL,  241) =  RJBLK( NCELL, IJ_AFG1 )
!  Reaction Label BP63            
                RKI( NCELL,  248) =  RJBLK( NCELL, IJ_MVK_06 )
!  Reaction Label BP67            
                RKI( NCELL,  252) =  RJBLK( NCELL, IJ_MACR_06 )
!  Reaction Label BP69            
                RKI( NCELL,  254) =   4.8600D-03 * RJBLK( NCELL, IJ_MEK_06 )
!  Reaction Label BP71            
                RKI( NCELL,  256) =  RJBLK( NCELL, IJ_IC3ONO2 )
!  Reaction Label BP73            
                RKI( NCELL,  257) =  RJBLK( NCELL, IJ_HOCCHO_IUPAC )
!  Reaction Label BP79            
                RKI( NCELL,  263) =  RJBLK( NCELL, IJ_ACRO_09 )
!  Reaction Label BP81            
                RKI( NCELL,  265) =  RJBLK( NCELL, IJ_PAA )
!  Reaction Label BP83            
                RKI( NCELL,  267) =  RJBLK( NCELL, IJ_PAA )
!  Reaction Label CI01            
                RKI( NCELL,  554) =  RJBLK( NCELL, IJ_CL2 )
!  Reaction Label CI03            
                RKI( NCELL,  556) =  RJBLK( NCELL, IJ_CLNO_06 )
!  Reaction Label CI06            
                RKI( NCELL,  559) =  RJBLK( NCELL, IJ_CLONO )
!  Reaction Label CI07            
                RKI( NCELL,  560) =  RJBLK( NCELL, IJ_CLNO2 )
!  Reaction Label CI14            
                RKI( NCELL,  567) =  RJBLK( NCELL, IJ_CLONO2_1 )
!  Reaction Label CI15            
                RKI( NCELL,  568) =  RJBLK( NCELL, IJ_CLONO2_2 )
!  Reaction Label CI19            
                RKI( NCELL,  572) =  RJBLK( NCELL, IJ_HOCL_06 )
!  Reaction Label CP19            
                RKI( NCELL,  594) =  RJBLK( NCELL, IJ_CLCCHO )
!  Reaction Label CP22            
                RKI( NCELL,  597) =   5.0000D-01 * RJBLK( NCELL, IJ_CLACET )
!  Reaction Label TR01            
                RKI( NCELL,  661) =  RJBLK( NCELL, IJ_HCHOR_06 )
!  Reaction Label TR02            
                RKI( NCELL,  662) =  RJBLK( NCELL, IJ_HCHOM_06 )
!  Reaction Label TR08            
                RKI( NCELL,  667) =  RJBLK( NCELL, IJ_CCHO_R )
!  Reaction Label TR15            
                RKI( NCELL,  674) =  RJBLK( NCELL, IJ_ACRO_09 )
!  Reaction Label IS137           
                RKI( NCELL,  684) =  RJBLK( NCELL, IJ_HPALD )
!  Reaction Label IS81            
                RKI( NCELL,  765) =   1.7500D-01 * RJBLK( NCELL, IJ_MEK_06 )
!  Reaction Label IS111           
                RKI( NCELL,  767) =  RJBLK( NCELL, IJ_NOA )
!  Reaction Label IS97            
                RKI( NCELL,  770) =  RJBLK( NCELL, IJ_NOA )
!  Reaction Label IS98            
                RKI( NCELL,  771) =  RJBLK( NCELL, IJ_NOA )
!  Reaction Label IS106           
                RKI( NCELL,  773) =  RJBLK( NCELL, IJ_NOA )
!  Reaction Label IS110           
                RKI( NCELL,  775) =  RJBLK( NCELL, IJ_C2CHO )
!  Reaction Label IS87            
                RKI( NCELL,  777) =  RJBLK( NCELL, IJ_MGLY_06 )
!  Reaction Label IS92            
                RKI( NCELL,  786) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label BP58            
                RKI( NCELL,  792) =  RJBLK( NCELL, IJ_MACR_06 )
!  Reaction Label IA53            
                RKI( NCELL,  808) =  RJBLK( NCELL, IJ_PAN )

                IF( .NOT. LAND( NCELL ) )THEN
!  Reaction Label HAL_Ozone       
                   RKI( NCELL,  857) =  SFACT * HALOGEN_FALLOFF( BLKPRES( NCELL ),   8.3300D-47,   9.3220D+01,  & 
     &                                                           1.4500D-08,         3.8400D+00 )
                END IF

            END DO 
        END IF 

        DO NCELL = 1, NUMCELLS 
!  Set-up conversion factors 
             INV_TEMP  = 1.0D+00 / BLKTEMP( NCELL ) 
             CAIR      = 1.0D+06 * COEF1 * BLKPRES( NCELL ) * INV_TEMP 
             CFACT     = 6.0D-05 * CAIR
             CFACT_SQU = 6.0D-11 * CAIR * CAIR 
             INV_CAIR  = 1.0D0 / CAIR 
             INV_CFACT = 6.0D+07 * INV_CAIR 
             TEMP      = BLKTEMP( NCELL ) 
             TEMPOT300 = BLKTEMP( NCELL ) * TI300 
             RFACT     = 1.0D+06 * INV_CAIR 
             RFACT_SQU = 1.0D+12 * INV_CAIR * INV_CAIR 

!  Reaction Label 2               
             RKI( NCELL,    2) =  CFACT_SQU * POWER_T02( TEMPOT300,   5.6800D-34,  -2.6000D+00 )
!  Reaction Label 3               
             RKI( NCELL,    3) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.0000D-12,  -2.0600D+03 )
!  Reaction Label 4               
             RKI( NCELL,    4) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 9.0000D-32,   0.0000D+00,  -1.5000D+00,  & 
     &                                                 3.0000D-11,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 5               
             RKI( NCELL,    5) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.5000D-12,   1.8800D+02 )
!  Reaction Label 6               
             RKI( NCELL,    6) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.5000D-31,   0.0000D+00,  -1.8000D+00,  & 
     &                                                 2.2000D-11,   0.0000D+00,  -7.0000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 7               
             RKI( NCELL,    7) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0000D-12,  -1.5000D+03 )
!  Reaction Label 8               
             RKI( NCELL,    8) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-13,  -2.4700D+03 )
!  Reaction Label 9               
             RKI( NCELL,    9) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-11,   1.1000D+02 )
!  Reaction Label 10              
             RKI( NCELL,   10) =  CFACT_SQU * ARRHENUIS_T03( INV_TEMP,  3.3000D-39,   5.3000D+02 )
!  Reaction Label 11              
             RKI( NCELL,   11) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 3.6000D-30,   0.0000D+00,  -4.1000D+00,  & 
     &                                                 1.9000D-12,   0.0000D+00,   2.0000D-01,  & 
     &                                                 1.3300D+00,   3.5000D-01 )
!  Reaction Label 12              
             RKI( NCELL,   12) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.3000D-03,  -1.1000D+04,  -3.5000D+00,  & 
     &                                                 9.7000D+14,  -1.1080D+04,   1.0000D-01,  & 
     &                                                 1.3300D+00,   3.5000D-01 )
!  Reaction Label 13              
             RKI( NCELL,   13) =   1.0000D-22 * CFACT 
!  Reaction Label 14              
             RKI( NCELL,   14) =   0.0000D+00 * CFACT_SQU 
!  Reaction Label 15              
             RKI( NCELL,   15) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.5000D-14,  -1.2600D+03 )
!  Reaction Label 20              
             RKI( NCELL,   20) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6300D-10,   6.0000D+01 )
!  Reaction Label 21              
             RKI( NCELL,   21) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3800D-11,   9.6000D+01 )
!  Reaction Label 22              
             RKI( NCELL,   22) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 7.0000D-31,   0.0000D+00,  -2.6000D+00,  & 
     &                                                 3.6000D-11,   0.0000D+00,  -1.0000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 24              
             RKI( NCELL,   24) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5000D-12,   2.6000D+02 )
!  Reaction Label 25              
             RKI( NCELL,   25) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 3.2000D-30,   0.0000D+00,  -4.5000D+00,  & 
     &                                                 3.0000D-11,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.2400D+00,   4.1000D-01 )
!  Reaction Label 26              
             RKI( NCELL,   26) =   2.0000D-11 * CFACT 
!  Reaction Label 27              
             RKI( NCELL,   27) =  CFACT * FALLOFF_T08( INV_TEMP,  CAIR, & 
     &                                                 2.4000D-14,   4.6000D+02,   2.7000D-17,  & 
     &                                                 2.1990D+03,   6.5000D-34,   1.3350D+03 )
!  Reaction Label 29              
             RKI( NCELL,   29) =  CFACT * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 1.4400D-13,   0.0000D+00,   3.4300D-33,  & 
     &                                                 0.0000D+00 )
!  Reaction Label 30              
             RKI( NCELL,   30) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7000D-12,  -9.4000D+02 )
!  Reaction Label 31              
             RKI( NCELL,   31) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.6000D-12,   2.7000D+02 )
!  Reaction Label 32              
             RKI( NCELL,   32) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.0000D-31,   0.0000D+00,  -3.4000D+00,  & 
     &                                                 2.9000D-12,   0.0000D+00,  -1.1000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 33              
             RKI( NCELL,   33) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 3.7200D-05,  -1.0650D+04,  -2.4000D+00,  & 
     &                                                 5.4200D+15,  -1.1170D+04,  -2.3000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 35              
             RKI( NCELL,   35) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3000D-12,   3.8000D+02 )
!  Reaction Label 36              
             RKI( NCELL,   36) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   2.0300D-16,   6.9300D+02,   4.5700D+00 )
!  Reaction Label 37              
             RKI( NCELL,   37) =  CFACT * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 2.2000D-13,   6.0000D+02,   1.9000D-33,  & 
     &                                                 9.8000D+02 )
!  Reaction Label 38              
             RKI( NCELL,   38) =  CFACT_SQU * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 3.0800D-34,   2.8000D+03,   2.6600D-54,  & 
     &                                                 3.1800D+03 )
!  Reaction Label 39              
             RKI( NCELL,   39) =   4.0000D-12 * CFACT 
!  Reaction Label 40              
             RKI( NCELL,   40) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5000D-13,  -2.4500D+03 )
!  Reaction Label 42              
             RKI( NCELL,   42) =   1.8000D-12 * CFACT 
!  Reaction Label 43              
             RKI( NCELL,   43) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.8000D-11,   2.5000D+02 )
!  Reaction Label 44              
             RKI( NCELL,   44) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 3.3000D-31,   0.0000D+00,  -4.3000D+00,  & 
     &                                                 1.6000D-12,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 45              
             RKI( NCELL,   45) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.7000D-12,  -2.1000D+03 )
!  Reaction Label BR01            
             RKI( NCELL,   46) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3000D-12,   3.6000D+02 )
!  Reaction Label BR02            
             RKI( NCELL,   47) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   3.4600D-13,   7.8000D+02,   3.6000D-01 )
!  Reaction Label BR03            
             RKI( NCELL,   48) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   3.3400D-14,   7.8000D+02,  -3.5300D+00 )
!  Reaction Label BR04            
             RKI( NCELL,   49) =   1.3000D-12 * CFACT 
!  Reaction Label BR05            
             RKI( NCELL,   50) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   6.3900D-14,   3.6500D+02,  -1.8000D+00 )
!  Reaction Label BR06            
             RKI( NCELL,   51) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,  -5.2000D+02 )
!  Reaction Label BR07            
             RKI( NCELL,   52) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label BR08            
             RKI( NCELL,   53) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.8000D-13,   9.0000D+02 )
!  Reaction Label BR09            
             RKI( NCELL,   54) =   2.3000D-12 * CFACT 
!  Reaction Label BR10            
             RKI( NCELL,   55) =   2.0000D-13 * CFACT 
!  Reaction Label BR11            
             RKI( NCELL,   56) =   3.5000D-14 * CFACT 
!  Reaction Label BR12            
             RKI( NCELL,   57) =   RKI( NCELL,   52 ) 
!  Reaction Label BR13            
             RKI( NCELL,   58) =   RKI( NCELL,   53 ) 
!  Reaction Label BR14            
             RKI( NCELL,   59) =   RKI( NCELL,   54 ) 
!  Reaction Label BR15            
             RKI( NCELL,   60) =   RKI( NCELL,   55 ) 
!  Reaction Label BR16            
             RKI( NCELL,   61) =   RKI( NCELL,   56 ) 
!  Reaction Label BR17            
             RKI( NCELL,   62) =   RKI( NCELL,   56 ) 
!  Reaction Label BR18            
             RKI( NCELL,   63) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.7000D-28,   0.0000D+00,  -7.1000D+00,  & 
     &                                                 1.2100D-11,   0.0000D+00,  -9.0000D-01,  & 
     &                                                 1.4100D+00,   3.0000D-01 )
!  Reaction Label BR19            
             RKI( NCELL,   64) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.9000D-03,  -1.2100D+04,   0.0000D+00,  & 
     &                                                 4.0000D+16,  -1.3600D+04,   0.0000D+00,  & 
     &                                                 1.4100D+00,   3.0000D-01 )
!  Reaction Label BR21            
             RKI( NCELL,   66) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.5000D-12,   2.9000D+02 )
!  Reaction Label BR22            
             RKI( NCELL,   67) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.2000D-13,   9.8000D+02 )
!  Reaction Label BR23            
             RKI( NCELL,   68) =   RKI( NCELL,   54 ) 
!  Reaction Label BR24            
             RKI( NCELL,   69) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-12,   5.0000D+02 )
!  Reaction Label BR25            
             RKI( NCELL,   70) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label BR26            
             RKI( NCELL,   71) =   RKI( NCELL,   70 ) 
!  Reaction Label BR27            
             RKI( NCELL,   72) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D-12,   5.0000D+02 )
!  Reaction Label BR28            
             RKI( NCELL,   73) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.2100D-11,   0.0000D+00,  -1.0700D+00 )
!  Reaction Label BR29            
             RKI( NCELL,   74) =  SFACT * ARRHENUIS_T03( INV_TEMP,  8.3000D+16,  -1.3940D+04 )
!  Reaction Label BR31            
             RKI( NCELL,   76) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.7000D-12,   3.4000D+02 )
!  Reaction Label BR32            
             RKI( NCELL,   77) =   RKI( NCELL,   67 ) 
!  Reaction Label BR33            
             RKI( NCELL,   78) =   RKI( NCELL,   54 ) 
!  Reaction Label BR34            
             RKI( NCELL,   79) =   RKI( NCELL,   69 ) 
!  Reaction Label BR35            
             RKI( NCELL,   80) =   RKI( NCELL,   70 ) 
!  Reaction Label BR36            
             RKI( NCELL,   81) =   RKI( NCELL,   70 ) 
!  Reaction Label BR37            
             RKI( NCELL,   82) =   RKI( NCELL,   72 ) 
!  Reaction Label BR38            
             RKI( NCELL,   83) =   RKI( NCELL,   72 ) 
!  Reaction Label BR39            
             RKI( NCELL,   84) =   1.3700D-11 * CFACT 
!  Reaction Label BR40            
             RKI( NCELL,   85) =  SFACT * ARRHENUIS_T03( INV_TEMP,  7.9000D+16,  -1.4000D+04 )
!  Reaction Label BR42            
             RKI( NCELL,   87) =   RKI( NCELL,   76 ) 
!  Reaction Label BR43            
             RKI( NCELL,   88) =   RKI( NCELL,   67 ) 
!  Reaction Label BR44            
             RKI( NCELL,   89) =   RKI( NCELL,   54 ) 
!  Reaction Label BR45            
             RKI( NCELL,   90) =   RKI( NCELL,   69 ) 
!  Reaction Label BR46            
             RKI( NCELL,   91) =   RKI( NCELL,   70 ) 
!  Reaction Label BR47            
             RKI( NCELL,   92) =   RKI( NCELL,   70 ) 
!  Reaction Label BR48            
             RKI( NCELL,   93) =   RKI( NCELL,   72 ) 
!  Reaction Label BR49            
             RKI( NCELL,   94) =   RKI( NCELL,   72 ) 
!  Reaction Label BR50            
             RKI( NCELL,   95) =   RKI( NCELL,   72 ) 
!  Reaction Label BR51            
             RKI( NCELL,   96) =   RKI( NCELL,   73 ) 
!  Reaction Label BR52            
             RKI( NCELL,   97) =  SFACT * ARRHENUIS_T03( INV_TEMP,  1.6000D+16,  -1.3486D+04 )
!  Reaction Label BR64            
             RKI( NCELL,   99) =   2.4000D-11 * CFACT 
!  Reaction Label BR65            
             RKI( NCELL,  100) =  SFACT * ARRHENUIS_T03( INV_TEMP,  7.5000D+14,  -8.1520D+03 )
!  Reaction Label BR66            
             RKI( NCELL,  101) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3000D-11,   1.5000D+02 )
!  Reaction Label BR67            
             RKI( NCELL,  102) =   RKI( NCELL,   53 ) 
!  Reaction Label BR68            
             RKI( NCELL,  103) =   1.0000D-03 * SFACT 
!  Reaction Label R019            
             RKI( NCELL,  104) =   RKI( NCELL,   52 ) 
!  Reaction Label R020            
             RKI( NCELL,  105) =   RKI( NCELL,   53 ) 
!  Reaction Label R021            
             RKI( NCELL,  106) =   RKI( NCELL,   54 ) 
!  Reaction Label R022            
             RKI( NCELL,  107) =   RKI( NCELL,   55 ) 
!  Reaction Label R023            
             RKI( NCELL,  108) =   RKI( NCELL,   56 ) 
!  Reaction Label R024            
             RKI( NCELL,  109) =   RKI( NCELL,   56 ) 
!  Reaction Label R025            
             RKI( NCELL,  110) =   RKI( NCELL,   70 ) 
!  Reaction Label R026            
             RKI( NCELL,  111) =   RKI( NCELL,   70 ) 
!  Reaction Label R027            
             RKI( NCELL,  112) =   RKI( NCELL,   70 ) 
!  Reaction Label R028            
             RKI( NCELL,  113) =   RKI( NCELL,   70 ) 
!  Reaction Label R029            
             RKI( NCELL,  114) =   RKI( NCELL,   52 ) 
!  Reaction Label R030            
             RKI( NCELL,  115) =   RKI( NCELL,   53 ) 
!  Reaction Label R031            
             RKI( NCELL,  116) =   RKI( NCELL,   54 ) 
!  Reaction Label R032            
             RKI( NCELL,  117) =   RKI( NCELL,   55 ) 
!  Reaction Label R033            
             RKI( NCELL,  118) =   RKI( NCELL,   56 ) 
!  Reaction Label R034            
             RKI( NCELL,  119) =   RKI( NCELL,   56 ) 
!  Reaction Label R035            
             RKI( NCELL,  120) =   RKI( NCELL,   70 ) 
!  Reaction Label R036            
             RKI( NCELL,  121) =   RKI( NCELL,   70 ) 
!  Reaction Label R037            
             RKI( NCELL,  122) =   RKI( NCELL,   70 ) 
!  Reaction Label R038            
             RKI( NCELL,  123) =   RKI( NCELL,   70 ) 
!  Reaction Label R039            
             RKI( NCELL,  124) =   RKI( NCELL,   52 ) 
!  Reaction Label R040            
             RKI( NCELL,  125) =   RKI( NCELL,   53 ) 
!  Reaction Label R041            
             RKI( NCELL,  126) =   RKI( NCELL,   54 ) 
!  Reaction Label R042            
             RKI( NCELL,  127) =   RKI( NCELL,   55 ) 
!  Reaction Label R043            
             RKI( NCELL,  128) =   RKI( NCELL,   56 ) 
!  Reaction Label R044            
             RKI( NCELL,  129) =   RKI( NCELL,   56 ) 
!  Reaction Label R045            
             RKI( NCELL,  130) =   RKI( NCELL,   70 ) 
!  Reaction Label R046            
             RKI( NCELL,  131) =   RKI( NCELL,   70 ) 
!  Reaction Label R047            
             RKI( NCELL,  132) =   RKI( NCELL,   70 ) 
!  Reaction Label R048            
             RKI( NCELL,  133) =   RKI( NCELL,   70 ) 
!  Reaction Label R049            
             RKI( NCELL,  134) =   RKI( NCELL,   52 ) 
!  Reaction Label R050            
             RKI( NCELL,  135) =   RKI( NCELL,   53 ) 
!  Reaction Label R051            
             RKI( NCELL,  136) =   RKI( NCELL,   54 ) 
!  Reaction Label R052            
             RKI( NCELL,  137) =   RKI( NCELL,   55 ) 
!  Reaction Label R053            
             RKI( NCELL,  138) =   RKI( NCELL,   56 ) 
!  Reaction Label R054            
             RKI( NCELL,  139) =   RKI( NCELL,   56 ) 
!  Reaction Label R055            
             RKI( NCELL,  140) =   RKI( NCELL,   70 ) 
!  Reaction Label R056            
             RKI( NCELL,  141) =   RKI( NCELL,   70 ) 
!  Reaction Label R057            
             RKI( NCELL,  142) =   RKI( NCELL,   70 ) 
!  Reaction Label R058            
             RKI( NCELL,  143) =   RKI( NCELL,   70 ) 
!  Reaction Label R059            
             RKI( NCELL,  144) =   RKI( NCELL,   52 ) 
!  Reaction Label R060            
             RKI( NCELL,  145) =   RKI( NCELL,   53 ) 
!  Reaction Label R061            
             RKI( NCELL,  146) =   RKI( NCELL,   54 ) 
!  Reaction Label R062            
             RKI( NCELL,  147) =   RKI( NCELL,   55 ) 
!  Reaction Label R063            
             RKI( NCELL,  148) =   RKI( NCELL,   56 ) 
!  Reaction Label R064            
             RKI( NCELL,  149) =   RKI( NCELL,   56 ) 
!  Reaction Label R065            
             RKI( NCELL,  150) =   RKI( NCELL,   70 ) 
!  Reaction Label R066            
             RKI( NCELL,  151) =   RKI( NCELL,   70 ) 
!  Reaction Label R067            
             RKI( NCELL,  152) =   RKI( NCELL,   70 ) 
!  Reaction Label R068            
             RKI( NCELL,  153) =   RKI( NCELL,   70 ) 
!  Reaction Label R069            
             RKI( NCELL,  154) =   RKI( NCELL,   52 ) 
!  Reaction Label R070            
             RKI( NCELL,  155) =   RKI( NCELL,   53 ) 
!  Reaction Label R071            
             RKI( NCELL,  156) =   RKI( NCELL,   54 ) 
!  Reaction Label R072            
             RKI( NCELL,  157) =   RKI( NCELL,   55 ) 
!  Reaction Label R073            
             RKI( NCELL,  158) =   RKI( NCELL,   56 ) 
!  Reaction Label R074            
             RKI( NCELL,  159) =   RKI( NCELL,   56 ) 
!  Reaction Label R075            
             RKI( NCELL,  160) =   RKI( NCELL,   70 ) 
!  Reaction Label R076            
             RKI( NCELL,  161) =   RKI( NCELL,   70 ) 
!  Reaction Label R077            
             RKI( NCELL,  162) =   RKI( NCELL,   70 ) 
!  Reaction Label R078            
             RKI( NCELL,  163) =   RKI( NCELL,   70 ) 
!  Reaction Label R079            
             RKI( NCELL,  164) =   RKI( NCELL,   52 ) 
!  Reaction Label R080            
             RKI( NCELL,  165) =   RKI( NCELL,   53 ) 
!  Reaction Label R081            
             RKI( NCELL,  166) =   RKI( NCELL,   54 ) 
!  Reaction Label R082            
             RKI( NCELL,  167) =   RKI( NCELL,   55 ) 
!  Reaction Label R083            
             RKI( NCELL,  168) =   RKI( NCELL,   56 ) 
!  Reaction Label R084            
             RKI( NCELL,  169) =   RKI( NCELL,   56 ) 
!  Reaction Label R085            
             RKI( NCELL,  170) =   RKI( NCELL,   70 ) 
!  Reaction Label R086            
             RKI( NCELL,  171) =   RKI( NCELL,   70 ) 
!  Reaction Label R087            
             RKI( NCELL,  172) =   RKI( NCELL,   70 ) 
!  Reaction Label R088            
             RKI( NCELL,  173) =   RKI( NCELL,   70 ) 
!  Reaction Label R089            
             RKI( NCELL,  174) =   RKI( NCELL,   52 ) 
!  Reaction Label R090            
             RKI( NCELL,  175) =   RKI( NCELL,   53 ) 
!  Reaction Label R091            
             RKI( NCELL,  176) =   RKI( NCELL,   54 ) 
!  Reaction Label R092            
             RKI( NCELL,  177) =   RKI( NCELL,   55 ) 
!  Reaction Label R093            
             RKI( NCELL,  178) =   RKI( NCELL,   56 ) 
!  Reaction Label R094            
             RKI( NCELL,  179) =   RKI( NCELL,   56 ) 
!  Reaction Label R095            
             RKI( NCELL,  180) =   RKI( NCELL,   70 ) 
!  Reaction Label R096            
             RKI( NCELL,  181) =   RKI( NCELL,   70 ) 
!  Reaction Label R097            
             RKI( NCELL,  182) =   RKI( NCELL,   70 ) 
!  Reaction Label R098            
             RKI( NCELL,  183) =   RKI( NCELL,   70 ) 
!  Reaction Label R099            
             RKI( NCELL,  184) =   RKI( NCELL,   52 ) 
!  Reaction Label R100            
             RKI( NCELL,  185) =   RKI( NCELL,   53 ) 
!  Reaction Label R101            
             RKI( NCELL,  186) =   RKI( NCELL,   54 ) 
!  Reaction Label R102            
             RKI( NCELL,  187) =   RKI( NCELL,   55 ) 
!  Reaction Label R103            
             RKI( NCELL,  188) =   RKI( NCELL,   56 ) 
!  Reaction Label R104            
             RKI( NCELL,  189) =   RKI( NCELL,   56 ) 
!  Reaction Label R105            
             RKI( NCELL,  190) =   RKI( NCELL,   70 ) 
!  Reaction Label R106            
             RKI( NCELL,  191) =   RKI( NCELL,   70 ) 
!  Reaction Label R107            
             RKI( NCELL,  192) =   RKI( NCELL,   70 ) 
!  Reaction Label R108            
             RKI( NCELL,  193) =   RKI( NCELL,   70 ) 
!  Reaction Label BP03            
             RKI( NCELL,  196) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.4000D-12,   1.3500D+02 )
!  Reaction Label BP07            
             RKI( NCELL,  197) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-12,  -2.4310D+03 )
!  Reaction Label BP08            
             RKI( NCELL,  198) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-12,   3.6500D+02 )
!  Reaction Label BP10            
             RKI( NCELL,  200) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label BP11            
             RKI( NCELL,  201) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.1000D-12,   4.0500D+02 )
!  Reaction Label BP13            
             RKI( NCELL,  203) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.6010D+03 )
!  Reaction Label BP14            
             RKI( NCELL,  204) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   4.5600D-14,   4.2900D+02,   3.6500D+00 )
!  Reaction Label BP16            
             RKI( NCELL,  206) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.3000D-12,  -2.5000D+01,   2.0000D+00 )
!  Reaction Label BP18            
             RKI( NCELL,  208) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8500D-12,  -3.4500D+02 )
!  Reaction Label BP19            
             RKI( NCELL,  209) =   4.5000D-13 * CFACT 
!  Reaction Label BP20            
             RKI( NCELL,  210) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.2000D-14,   8.5500D+02 )
!  Reaction Label BP21            
             RKI( NCELL,  211) =   1.2000D-12 * CFACT 
!  Reaction Label BP22            
             RKI( NCELL,  212) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.8000D-12,   2.0000D+02 )
!  Reaction Label BP24            
             RKI( NCELL,  214) =   2.5000D-11 * CFACT 
!  Reaction Label BP26            
             RKI( NCELL,  216) =   5.6000D-11 * CFACT 
!  Reaction Label BP28            
             RKI( NCELL,  218) =   1.4100D-10 * CFACT 
!  Reaction Label BP32            
             RKI( NCELL,  222) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.1000D-12,   3.4220D+02 )
!  Reaction Label BP33            
             RKI( NCELL,  223) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-12,  -2.3900D+03 )
!  Reaction Label BP35            
             RKI( NCELL,  225) =   1.5000D-11 * CFACT 
!  Reaction Label BP36            
             RKI( NCELL,  226) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8950D+03 )
!  Reaction Label BP38            
             RKI( NCELL,  228) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7000D-12,   9.5000D+02 )
!  Reaction Label BP39            
             RKI( NCELL,  229) =   1.4000D-11 * CFACT 
!  Reaction Label BP40            
             RKI( NCELL,  230) =   3.5000D-12 * CFACT 
!  Reaction Label BP43            
             RKI( NCELL,  233) =   1.2000D-11 * CFACT 
!  Reaction Label BP45            
             RKI( NCELL,  235) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3400D-12,  -1.8600D+03 )
!  Reaction Label BP46            
             RKI( NCELL,  236) =   7.4000D-11 * CFACT 
!  Reaction Label BP47            
             RKI( NCELL,  237) =   9.6600D-18 * CFACT 
!  Reaction Label BP49            
             RKI( NCELL,  239) =   7.4000D-11 * CFACT 
!  Reaction Label BP50            
             RKI( NCELL,  240) =   9.6600D-18 * CFACT 
!  Reaction Label BP52            
             RKI( NCELL,  242) =   9.3500D-11 * CFACT 
!  Reaction Label BP53            
             RKI( NCELL,  243) =   1.4300D-17 * CFACT 
!  Reaction Label BP55            
             RKI( NCELL,  244) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-15,  -2.1000D+03 )
!  Reaction Label BP57            
             RKI( NCELL,  245) =   6.3400D-12 * CFACT 
!  Reaction Label BP60            
             RKI( NCELL,  246) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5000D-16,  -1.5200D+03 )
!  Reaction Label BP62            
             RKI( NCELL,  247) =   4.3200D-12 * CFACT 
!  Reaction Label BP64            
             RKI( NCELL,  249) =   6.1900D-11 * CFACT 
!  Reaction Label BP65            
             RKI( NCELL,  250) =   4.1800D-18 * CFACT 
!  Reaction Label BP66            
             RKI( NCELL,  251) =   1.0000D-13 * CFACT 
!  Reaction Label BP68            
             RKI( NCELL,  253) =   1.5500D-11 * CFACT 
!  Reaction Label BP70            
             RKI( NCELL,  255) =   7.2000D-12 * CFACT 
!  Reaction Label BP74            
             RKI( NCELL,  258) =   RKI( NCELL,  200 ) 
!  Reaction Label BP75            
             RKI( NCELL,  259) =   1.9900D-11 * CFACT 
!  Reaction Label BP76            
             RKI( NCELL,  260) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-15,  -2.5280D+03 )
!  Reaction Label BP77            
             RKI( NCELL,  261) =   1.1800D-15 * CFACT 
!  Reaction Label BP78            
             RKI( NCELL,  262) =   2.3700D-12 * CFACT 
!  Reaction Label BP80            
             RKI( NCELL,  264) =   5.2800D-12 * CFACT 
!  Reaction Label BP82            
             RKI( NCELL,  266) =   6.4200D-12 * CFACT 
!  Reaction Label BP84            
             RKI( NCELL,  268) =   RKI( NCELL,   76 ) 
!  Reaction Label BP85            
             RKI( NCELL,  269) =   RKI( NCELL,   73 ) 
!  Reaction Label BP86            
             RKI( NCELL,  270) =   RKI( NCELL,   67 ) 
!  Reaction Label P001            
             RKI( NCELL,  271) =   RKI( NCELL,   52 ) 
!  Reaction Label P002            
             RKI( NCELL,  272) =   RKI( NCELL,   53 ) 
!  Reaction Label P003            
             RKI( NCELL,  273) =   RKI( NCELL,   54 ) 
!  Reaction Label P004            
             RKI( NCELL,  274) =   RKI( NCELL,   55 ) 
!  Reaction Label P005            
             RKI( NCELL,  275) =   RKI( NCELL,   56 ) 
!  Reaction Label P006            
             RKI( NCELL,  276) =   RKI( NCELL,   56 ) 
!  Reaction Label P007            
             RKI( NCELL,  277) =   RKI( NCELL,   70 ) 
!  Reaction Label P008            
             RKI( NCELL,  278) =   RKI( NCELL,   70 ) 
!  Reaction Label P009            
             RKI( NCELL,  279) =   RKI( NCELL,   70 ) 
!  Reaction Label P010            
             RKI( NCELL,  280) =   RKI( NCELL,   70 ) 
!  Reaction Label P011            
             RKI( NCELL,  281) =   RKI( NCELL,   52 ) 
!  Reaction Label P012            
             RKI( NCELL,  282) =   RKI( NCELL,   53 ) 
!  Reaction Label P013            
             RKI( NCELL,  283) =   RKI( NCELL,   54 ) 
!  Reaction Label P014            
             RKI( NCELL,  284) =   RKI( NCELL,   55 ) 
!  Reaction Label P015            
             RKI( NCELL,  285) =   RKI( NCELL,   56 ) 
!  Reaction Label P016            
             RKI( NCELL,  286) =   RKI( NCELL,   56 ) 
!  Reaction Label P017            
             RKI( NCELL,  287) =   RKI( NCELL,   70 ) 
!  Reaction Label P018            
             RKI( NCELL,  288) =   RKI( NCELL,   70 ) 
!  Reaction Label P019            
             RKI( NCELL,  289) =   RKI( NCELL,   70 ) 
!  Reaction Label P020            
             RKI( NCELL,  290) =   RKI( NCELL,   70 ) 
!  Reaction Label P021            
             RKI( NCELL,  291) =   RKI( NCELL,   52 ) 
!  Reaction Label P022            
             RKI( NCELL,  292) =   RKI( NCELL,   53 ) 
!  Reaction Label P023            
             RKI( NCELL,  293) =   RKI( NCELL,   54 ) 
!  Reaction Label P024            
             RKI( NCELL,  294) =   RKI( NCELL,   55 ) 
!  Reaction Label P025            
             RKI( NCELL,  295) =   RKI( NCELL,   56 ) 
!  Reaction Label P026            
             RKI( NCELL,  296) =   RKI( NCELL,   56 ) 
!  Reaction Label P027            
             RKI( NCELL,  297) =   RKI( NCELL,   70 ) 
!  Reaction Label P028            
             RKI( NCELL,  298) =   RKI( NCELL,   70 ) 
!  Reaction Label P029            
             RKI( NCELL,  299) =   RKI( NCELL,   70 ) 
!  Reaction Label P030            
             RKI( NCELL,  300) =   RKI( NCELL,   70 ) 
!  Reaction Label P031            
             RKI( NCELL,  301) =   RKI( NCELL,   52 ) 
!  Reaction Label P032            
             RKI( NCELL,  302) =   RKI( NCELL,   53 ) 
!  Reaction Label P033            
             RKI( NCELL,  303) =   RKI( NCELL,   54 ) 
!  Reaction Label P034            
             RKI( NCELL,  304) =   RKI( NCELL,   55 ) 
!  Reaction Label P035            
             RKI( NCELL,  305) =   RKI( NCELL,   56 ) 
!  Reaction Label P036            
             RKI( NCELL,  306) =   RKI( NCELL,   56 ) 
!  Reaction Label P037            
             RKI( NCELL,  307) =   RKI( NCELL,   70 ) 
!  Reaction Label P038            
             RKI( NCELL,  308) =   RKI( NCELL,   70 ) 
!  Reaction Label P039            
             RKI( NCELL,  309) =   RKI( NCELL,   70 ) 
!  Reaction Label P040            
             RKI( NCELL,  310) =   RKI( NCELL,   70 ) 
!  Reaction Label P041            
             RKI( NCELL,  311) =   RKI( NCELL,   52 ) 
!  Reaction Label P042            
             RKI( NCELL,  312) =   RKI( NCELL,   53 ) 
!  Reaction Label P043            
             RKI( NCELL,  313) =   RKI( NCELL,   54 ) 
!  Reaction Label P044            
             RKI( NCELL,  314) =   RKI( NCELL,   55 ) 
!  Reaction Label P045            
             RKI( NCELL,  315) =   RKI( NCELL,   56 ) 
!  Reaction Label P046            
             RKI( NCELL,  316) =   RKI( NCELL,   56 ) 
!  Reaction Label P047            
             RKI( NCELL,  317) =   RKI( NCELL,   70 ) 
!  Reaction Label P048            
             RKI( NCELL,  318) =   RKI( NCELL,   70 ) 
!  Reaction Label P049            
             RKI( NCELL,  319) =   RKI( NCELL,   70 ) 
!  Reaction Label P050            
             RKI( NCELL,  320) =   RKI( NCELL,   70 ) 
!  Reaction Label P051            
             RKI( NCELL,  321) =   RKI( NCELL,   52 ) 
!  Reaction Label P052            
             RKI( NCELL,  322) =   RKI( NCELL,   53 ) 
!  Reaction Label P053            
             RKI( NCELL,  323) =   RKI( NCELL,   54 ) 
!  Reaction Label P054            
             RKI( NCELL,  324) =   RKI( NCELL,   55 ) 
!  Reaction Label P055            
             RKI( NCELL,  325) =   RKI( NCELL,   56 ) 
!  Reaction Label P056            
             RKI( NCELL,  326) =   RKI( NCELL,   56 ) 
!  Reaction Label P057            
             RKI( NCELL,  327) =   RKI( NCELL,   70 ) 
!  Reaction Label P058            
             RKI( NCELL,  328) =   RKI( NCELL,   70 ) 
!  Reaction Label P059            
             RKI( NCELL,  329) =   RKI( NCELL,   70 ) 
!  Reaction Label P060            
             RKI( NCELL,  330) =   RKI( NCELL,   70 ) 
!  Reaction Label P061            
             RKI( NCELL,  331) =   RKI( NCELL,   52 ) 
!  Reaction Label P062            
             RKI( NCELL,  332) =   RKI( NCELL,   53 ) 
!  Reaction Label P063            
             RKI( NCELL,  333) =   RKI( NCELL,   54 ) 
!  Reaction Label P064            
             RKI( NCELL,  334) =   RKI( NCELL,   55 ) 
!  Reaction Label P065            
             RKI( NCELL,  335) =   RKI( NCELL,   56 ) 
!  Reaction Label P066            
             RKI( NCELL,  336) =   RKI( NCELL,   56 ) 
!  Reaction Label P067            
             RKI( NCELL,  337) =   RKI( NCELL,   70 ) 
!  Reaction Label P068            
             RKI( NCELL,  338) =   RKI( NCELL,   70 ) 
!  Reaction Label P069            
             RKI( NCELL,  339) =   RKI( NCELL,   70 ) 
!  Reaction Label P070            
             RKI( NCELL,  340) =   RKI( NCELL,   70 ) 
!  Reaction Label P071            
             RKI( NCELL,  341) =   RKI( NCELL,   52 ) 
!  Reaction Label P072            
             RKI( NCELL,  342) =   RKI( NCELL,   53 ) 
!  Reaction Label P073            
             RKI( NCELL,  343) =   RKI( NCELL,   54 ) 
!  Reaction Label P074            
             RKI( NCELL,  344) =   RKI( NCELL,   55 ) 
!  Reaction Label P075            
             RKI( NCELL,  345) =   RKI( NCELL,   56 ) 
!  Reaction Label P076            
             RKI( NCELL,  346) =   RKI( NCELL,   56 ) 
!  Reaction Label P077            
             RKI( NCELL,  347) =   RKI( NCELL,   70 ) 
!  Reaction Label P078            
             RKI( NCELL,  348) =   RKI( NCELL,   70 ) 
!  Reaction Label P079            
             RKI( NCELL,  349) =   RKI( NCELL,   70 ) 
!  Reaction Label P080            
             RKI( NCELL,  350) =   RKI( NCELL,   70 ) 
!  Reaction Label P081            
             RKI( NCELL,  351) =   RKI( NCELL,   52 ) 
!  Reaction Label P082            
             RKI( NCELL,  352) =   RKI( NCELL,   53 ) 
!  Reaction Label P083            
             RKI( NCELL,  353) =   RKI( NCELL,   54 ) 
!  Reaction Label P084            
             RKI( NCELL,  354) =   RKI( NCELL,   55 ) 
!  Reaction Label P085            
             RKI( NCELL,  355) =   RKI( NCELL,   56 ) 
!  Reaction Label P086            
             RKI( NCELL,  356) =   RKI( NCELL,   56 ) 
!  Reaction Label P087            
             RKI( NCELL,  357) =   RKI( NCELL,   70 ) 
!  Reaction Label P088            
             RKI( NCELL,  358) =   RKI( NCELL,   70 ) 
!  Reaction Label P089            
             RKI( NCELL,  359) =   RKI( NCELL,   70 ) 
!  Reaction Label P090            
             RKI( NCELL,  360) =   RKI( NCELL,   70 ) 
!  Reaction Label P091            
             RKI( NCELL,  361) =   RKI( NCELL,   52 ) 
!  Reaction Label P092            
             RKI( NCELL,  362) =   RKI( NCELL,   53 ) 
!  Reaction Label P093            
             RKI( NCELL,  363) =   RKI( NCELL,   54 ) 
!  Reaction Label P094            
             RKI( NCELL,  364) =   RKI( NCELL,   55 ) 
!  Reaction Label P095            
             RKI( NCELL,  365) =   RKI( NCELL,   56 ) 
!  Reaction Label P096            
             RKI( NCELL,  366) =   RKI( NCELL,   56 ) 
!  Reaction Label P097            
             RKI( NCELL,  367) =   RKI( NCELL,   70 ) 
!  Reaction Label P098            
             RKI( NCELL,  368) =   RKI( NCELL,   70 ) 
!  Reaction Label P099            
             RKI( NCELL,  369) =   RKI( NCELL,   70 ) 
!  Reaction Label P100            
             RKI( NCELL,  370) =   RKI( NCELL,   70 ) 
!  Reaction Label P101            
             RKI( NCELL,  371) =   RKI( NCELL,   52 ) 
!  Reaction Label P102            
             RKI( NCELL,  372) =   RKI( NCELL,   53 ) 
!  Reaction Label P103            
             RKI( NCELL,  373) =   RKI( NCELL,   54 ) 
!  Reaction Label P104            
             RKI( NCELL,  374) =   RKI( NCELL,   55 ) 
!  Reaction Label P105            
             RKI( NCELL,  375) =   RKI( NCELL,   56 ) 
!  Reaction Label P106            
             RKI( NCELL,  376) =   RKI( NCELL,   56 ) 
!  Reaction Label P107            
             RKI( NCELL,  377) =   RKI( NCELL,   70 ) 
!  Reaction Label P108            
             RKI( NCELL,  378) =   RKI( NCELL,   70 ) 
!  Reaction Label P109            
             RKI( NCELL,  379) =   RKI( NCELL,   70 ) 
!  Reaction Label P110            
             RKI( NCELL,  380) =   RKI( NCELL,   70 ) 
!  Reaction Label P111            
             RKI( NCELL,  381) =   RKI( NCELL,   52 ) 
!  Reaction Label P112            
             RKI( NCELL,  382) =   RKI( NCELL,   53 ) 
!  Reaction Label P113            
             RKI( NCELL,  383) =   RKI( NCELL,   54 ) 
!  Reaction Label P114            
             RKI( NCELL,  384) =   RKI( NCELL,   55 ) 
!  Reaction Label P115            
             RKI( NCELL,  385) =   RKI( NCELL,   56 ) 
!  Reaction Label P116            
             RKI( NCELL,  386) =   RKI( NCELL,   56 ) 
!  Reaction Label P117            
             RKI( NCELL,  387) =   RKI( NCELL,   70 ) 
!  Reaction Label P118            
             RKI( NCELL,  388) =   RKI( NCELL,   70 ) 
!  Reaction Label P119            
             RKI( NCELL,  389) =   RKI( NCELL,   70 ) 
!  Reaction Label P120            
             RKI( NCELL,  390) =   RKI( NCELL,   70 ) 
!  Reaction Label P121            
             RKI( NCELL,  391) =   RKI( NCELL,   52 ) 
!  Reaction Label P122            
             RKI( NCELL,  392) =   RKI( NCELL,   53 ) 
!  Reaction Label P123            
             RKI( NCELL,  393) =   RKI( NCELL,   54 ) 
!  Reaction Label P124            
             RKI( NCELL,  394) =   RKI( NCELL,   55 ) 
!  Reaction Label P125            
             RKI( NCELL,  395) =   RKI( NCELL,   56 ) 
!  Reaction Label P126            
             RKI( NCELL,  396) =   RKI( NCELL,   56 ) 
!  Reaction Label P127            
             RKI( NCELL,  397) =   RKI( NCELL,   70 ) 
!  Reaction Label P128            
             RKI( NCELL,  398) =   RKI( NCELL,   70 ) 
!  Reaction Label P129            
             RKI( NCELL,  399) =   RKI( NCELL,   70 ) 
!  Reaction Label P130            
             RKI( NCELL,  400) =   RKI( NCELL,   70 ) 
!  Reaction Label P131            
             RKI( NCELL,  401) =   RKI( NCELL,   52 ) 
!  Reaction Label P132            
             RKI( NCELL,  402) =   RKI( NCELL,   53 ) 
!  Reaction Label P133            
             RKI( NCELL,  403) =   RKI( NCELL,   54 ) 
!  Reaction Label P134            
             RKI( NCELL,  404) =   RKI( NCELL,   55 ) 
!  Reaction Label P135            
             RKI( NCELL,  405) =   RKI( NCELL,   56 ) 
!  Reaction Label P136            
             RKI( NCELL,  406) =   RKI( NCELL,   56 ) 
!  Reaction Label P137            
             RKI( NCELL,  407) =   RKI( NCELL,   70 ) 
!  Reaction Label P138            
             RKI( NCELL,  408) =   RKI( NCELL,   70 ) 
!  Reaction Label P139            
             RKI( NCELL,  409) =   RKI( NCELL,   70 ) 
!  Reaction Label P140            
             RKI( NCELL,  410) =   RKI( NCELL,   70 ) 
!  Reaction Label P141            
             RKI( NCELL,  411) =   RKI( NCELL,   52 ) 
!  Reaction Label P142            
             RKI( NCELL,  412) =   RKI( NCELL,   53 ) 
!  Reaction Label P143            
             RKI( NCELL,  413) =   RKI( NCELL,   54 ) 
!  Reaction Label P144            
             RKI( NCELL,  414) =   RKI( NCELL,   55 ) 
!  Reaction Label P145            
             RKI( NCELL,  415) =   RKI( NCELL,   56 ) 
!  Reaction Label P146            
             RKI( NCELL,  416) =   RKI( NCELL,   56 ) 
!  Reaction Label P147            
             RKI( NCELL,  417) =   RKI( NCELL,   70 ) 
!  Reaction Label P148            
             RKI( NCELL,  418) =   RKI( NCELL,   70 ) 
!  Reaction Label P149            
             RKI( NCELL,  419) =   RKI( NCELL,   70 ) 
!  Reaction Label P150            
             RKI( NCELL,  420) =   RKI( NCELL,   70 ) 
!  Reaction Label P151            
             RKI( NCELL,  421) =   RKI( NCELL,   52 ) 
!  Reaction Label P152            
             RKI( NCELL,  422) =   RKI( NCELL,   53 ) 
!  Reaction Label P153            
             RKI( NCELL,  423) =   RKI( NCELL,   54 ) 
!  Reaction Label P154            
             RKI( NCELL,  424) =   RKI( NCELL,   55 ) 
!  Reaction Label P155            
             RKI( NCELL,  425) =   RKI( NCELL,   56 ) 
!  Reaction Label P156            
             RKI( NCELL,  426) =   RKI( NCELL,   56 ) 
!  Reaction Label P157            
             RKI( NCELL,  427) =   RKI( NCELL,   70 ) 
!  Reaction Label P158            
             RKI( NCELL,  428) =   RKI( NCELL,   70 ) 
!  Reaction Label P159            
             RKI( NCELL,  429) =   RKI( NCELL,   70 ) 
!  Reaction Label P160            
             RKI( NCELL,  430) =   RKI( NCELL,   70 ) 
!  Reaction Label P161            
             RKI( NCELL,  431) =   RKI( NCELL,   52 ) 
!  Reaction Label P162            
             RKI( NCELL,  432) =   RKI( NCELL,   53 ) 
!  Reaction Label P163            
             RKI( NCELL,  433) =   RKI( NCELL,   54 ) 
!  Reaction Label P164            
             RKI( NCELL,  434) =   RKI( NCELL,   55 ) 
!  Reaction Label P165            
             RKI( NCELL,  435) =   RKI( NCELL,   56 ) 
!  Reaction Label P166            
             RKI( NCELL,  436) =   RKI( NCELL,   56 ) 
!  Reaction Label P167            
             RKI( NCELL,  437) =   RKI( NCELL,   70 ) 
!  Reaction Label P168            
             RKI( NCELL,  438) =   RKI( NCELL,   70 ) 
!  Reaction Label P169            
             RKI( NCELL,  439) =   RKI( NCELL,   70 ) 
!  Reaction Label P170            
             RKI( NCELL,  440) =   RKI( NCELL,   70 ) 
!  Reaction Label P171            
             RKI( NCELL,  441) =   RKI( NCELL,   52 ) 
!  Reaction Label P172            
             RKI( NCELL,  442) =   RKI( NCELL,   53 ) 
!  Reaction Label P173            
             RKI( NCELL,  443) =   RKI( NCELL,   54 ) 
!  Reaction Label P174            
             RKI( NCELL,  444) =   RKI( NCELL,   55 ) 
!  Reaction Label P175            
             RKI( NCELL,  445) =   RKI( NCELL,   56 ) 
!  Reaction Label P176            
             RKI( NCELL,  446) =   RKI( NCELL,   56 ) 
!  Reaction Label P177            
             RKI( NCELL,  447) =   RKI( NCELL,   70 ) 
!  Reaction Label P178            
             RKI( NCELL,  448) =   RKI( NCELL,   70 ) 
!  Reaction Label P179            
             RKI( NCELL,  449) =   RKI( NCELL,   70 ) 
!  Reaction Label P180            
             RKI( NCELL,  450) =   RKI( NCELL,   70 ) 
!  Reaction Label P181            
             RKI( NCELL,  451) =   RKI( NCELL,   52 ) 
!  Reaction Label P182            
             RKI( NCELL,  452) =   RKI( NCELL,   53 ) 
!  Reaction Label P183            
             RKI( NCELL,  453) =   RKI( NCELL,   54 ) 
!  Reaction Label P184            
             RKI( NCELL,  454) =   RKI( NCELL,   55 ) 
!  Reaction Label P185            
             RKI( NCELL,  455) =   RKI( NCELL,   56 ) 
!  Reaction Label P186            
             RKI( NCELL,  456) =   RKI( NCELL,   56 ) 
!  Reaction Label P187            
             RKI( NCELL,  457) =   RKI( NCELL,   70 ) 
!  Reaction Label P188            
             RKI( NCELL,  458) =   RKI( NCELL,   70 ) 
!  Reaction Label P189            
             RKI( NCELL,  459) =   RKI( NCELL,   70 ) 
!  Reaction Label P190            
             RKI( NCELL,  460) =   RKI( NCELL,   70 ) 
!  Reaction Label P191            
             RKI( NCELL,  461) =   RKI( NCELL,   52 ) 
!  Reaction Label P192            
             RKI( NCELL,  462) =   RKI( NCELL,   53 ) 
!  Reaction Label P193            
             RKI( NCELL,  463) =   RKI( NCELL,   54 ) 
!  Reaction Label P194            
             RKI( NCELL,  464) =   RKI( NCELL,   55 ) 
!  Reaction Label P195            
             RKI( NCELL,  465) =   RKI( NCELL,   56 ) 
!  Reaction Label P196            
             RKI( NCELL,  466) =   RKI( NCELL,   56 ) 
!  Reaction Label P197            
             RKI( NCELL,  467) =   RKI( NCELL,   70 ) 
!  Reaction Label P198            
             RKI( NCELL,  468) =   RKI( NCELL,   70 ) 
!  Reaction Label P199            
             RKI( NCELL,  469) =   RKI( NCELL,   70 ) 
!  Reaction Label P200            
             RKI( NCELL,  470) =   RKI( NCELL,   70 ) 
!  Reaction Label P201            
             RKI( NCELL,  471) =   RKI( NCELL,   52 ) 
!  Reaction Label P202            
             RKI( NCELL,  472) =   RKI( NCELL,   53 ) 
!  Reaction Label P203            
             RKI( NCELL,  473) =   RKI( NCELL,   54 ) 
!  Reaction Label P204            
             RKI( NCELL,  474) =   RKI( NCELL,   55 ) 
!  Reaction Label P205            
             RKI( NCELL,  475) =   RKI( NCELL,   56 ) 
!  Reaction Label P206            
             RKI( NCELL,  476) =   RKI( NCELL,   56 ) 
!  Reaction Label P207            
             RKI( NCELL,  477) =   RKI( NCELL,   70 ) 
!  Reaction Label P208            
             RKI( NCELL,  478) =   RKI( NCELL,   70 ) 
!  Reaction Label P209            
             RKI( NCELL,  479) =   RKI( NCELL,   70 ) 
!  Reaction Label P210            
             RKI( NCELL,  480) =   RKI( NCELL,   70 ) 
!  Reaction Label P211            
             RKI( NCELL,  481) =   RKI( NCELL,   52 ) 
!  Reaction Label P212            
             RKI( NCELL,  482) =   RKI( NCELL,   53 ) 
!  Reaction Label P213            
             RKI( NCELL,  483) =   RKI( NCELL,   54 ) 
!  Reaction Label P214            
             RKI( NCELL,  484) =   RKI( NCELL,   55 ) 
!  Reaction Label P215            
             RKI( NCELL,  485) =   RKI( NCELL,   56 ) 
!  Reaction Label P216            
             RKI( NCELL,  486) =   RKI( NCELL,   56 ) 
!  Reaction Label P217            
             RKI( NCELL,  487) =   RKI( NCELL,   70 ) 
!  Reaction Label P218            
             RKI( NCELL,  488) =   RKI( NCELL,   70 ) 
!  Reaction Label P219            
             RKI( NCELL,  489) =   RKI( NCELL,   70 ) 
!  Reaction Label P220            
             RKI( NCELL,  490) =   RKI( NCELL,   70 ) 
!  Reaction Label P221            
             RKI( NCELL,  491) =   RKI( NCELL,   52 ) 
!  Reaction Label P222            
             RKI( NCELL,  492) =   RKI( NCELL,   53 ) 
!  Reaction Label P223            
             RKI( NCELL,  493) =   RKI( NCELL,   54 ) 
!  Reaction Label P224            
             RKI( NCELL,  494) =   RKI( NCELL,   55 ) 
!  Reaction Label P225            
             RKI( NCELL,  495) =   RKI( NCELL,   56 ) 
!  Reaction Label P226            
             RKI( NCELL,  496) =   RKI( NCELL,   56 ) 
!  Reaction Label P227            
             RKI( NCELL,  497) =   RKI( NCELL,   70 ) 
!  Reaction Label P228            
             RKI( NCELL,  498) =   RKI( NCELL,   70 ) 
!  Reaction Label P229            
             RKI( NCELL,  499) =   RKI( NCELL,   70 ) 
!  Reaction Label P230            
             RKI( NCELL,  500) =   RKI( NCELL,   70 ) 
!  Reaction Label BE01            
             RKI( NCELL,  501) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8500D-12,  -1.6900D+03 )
!  Reaction Label BE02            
             RKI( NCELL,  502) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.0000D-28,   0.0000D+00,  -4.5000D+00,  & 
     &                                                 8.8000D-12,   0.0000D+00,  -8.5000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label BE03            
             RKI( NCELL,  503) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.1400D-15,  -2.5800D+03 )
!  Reaction Label BE04            
             RKI( NCELL,  504) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.3000D-12,  -2.8800D+03 )
!  Reaction Label BE05            
             RKI( NCELL,  505) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0700D-11,  -8.0000D+02 )
!  Reaction Label BT01            
             RKI( NCELL,  506) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.8500D-12,   5.0400D+02 )
!  Reaction Label BT02            
             RKI( NCELL,  507) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.5100D-15,  -1.8780D+03 )
!  Reaction Label BT03            
             RKI( NCELL,  508) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.5900D-13,  -1.1560D+03 )
!  Reaction Label BT04            
             RKI( NCELL,  509) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0200D-11,  -2.8000D+02 )
!  Reaction Label BT05            
             RKI( NCELL,  510) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4800D-11,   4.4800D+02 )
!  Reaction Label BT06            
             RKI( NCELL,  511) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3400D-14,  -2.2830D+03 )
!  Reaction Label BT07            
             RKI( NCELL,  512) =   1.0000D-13 * CFACT 
!  Reaction Label BT08            
             RKI( NCELL,  513) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2600D-11,  -4.0000D+01 )
!  Reaction Label BE07            
             RKI( NCELL,  514) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.8600D-15,  -1.9120D+03 )
!  Reaction Label BE09            
             RKI( NCELL,  515) =   3.5000D-11 * CFACT 
!  Reaction Label BT09            
             RKI( NCELL,  516) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2100D-11,   4.3600D+02 )
!  Reaction Label BT10            
             RKI( NCELL,  517) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.0000D-16,  -5.3000D+02 )
!  Reaction Label BT11            
             RKI( NCELL,  518) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1900D-12,   4.9000D+02 )
!  Reaction Label BT12            
             RKI( NCELL,  519) =   3.2000D-11 * CFACT 
!  Reaction Label BE10            
             RKI( NCELL,  520) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 5.5000D-30,   0.0000D+00,   0.0000D+00,  & 
     &                                                 8.3000D-13,   0.0000D+00,  -2.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label BE11            
             RKI( NCELL,  521) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0000D-14,  -4.1000D+03 )
!  Reaction Label BE12            
             RKI( NCELL,  522) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3300D-12,  -1.9300D+02 )
!  Reaction Label BT13            
             RKI( NCELL,  523) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8100D-12,   3.3800D+02 )
!  Reaction Label BT14            
             RKI( NCELL,  524) =   2.3100D-11 * CFACT 
!  Reaction Label BT15            
             RKI( NCELL,  525) =   1.3600D-11 * CFACT 
!  Reaction Label BT16            
             RKI( NCELL,  526) =   1.4300D-11 * CFACT 
!  Reaction Label BT17            
             RKI( NCELL,  527) =   3.2500D-11 * CFACT 
!  Reaction Label BT18            
             RKI( NCELL,  528) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   5.4900D-13,   5.3000D+02,   2.0000D+00 )
!  Reaction Label BL01            
             RKI( NCELL,  529) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.3400D-12,  -4.9900D+02,   2.0000D+00 )
!  Reaction Label BL02            
             RKI( NCELL,  530) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.4900D-12,  -8.7000D+01,   2.0000D+00 )
!  Reaction Label BL03            
             RKI( NCELL,  531) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5100D-12,   1.2600D+02 )
!  Reaction Label BL04            
             RKI( NCELL,  532) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.7500D-12,   4.4000D+01 )
!  Reaction Label BL05            
             RKI( NCELL,  533) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.7400D+02 )
!  Reaction Label AALK            
             RKI( NCELL,  534) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.7400D+02 )
!  Reaction Label BL06            
             RKI( NCELL,  535) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.7200D-12,   5.0100D+02 )
!  Reaction Label BL07            
             RKI( NCELL,  536) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.1900D-15,  -1.7010D+03 )
!  Reaction Label BL08            
             RKI( NCELL,  537) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.3700D-13,  -1.0470D+03 )
!  Reaction Label BL09            
             RKI( NCELL,  538) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6100D-11,  -3.2600D+02 )
!  Reaction Label BL10            
             RKI( NCELL,  539) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2600D-11,   4.8800D+02 )
!  Reaction Label BL11            
             RKI( NCELL,  540) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5900D-15,  -1.2550D+03 )
!  Reaction Label BL12            
             RKI( NCELL,  541) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3100D-13,   3.8200D+02 )
!  Reaction Label BL13            
             RKI( NCELL,  542) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4300D-11,   1.1100D+02 )
!  Reaction Label BL14            
             RKI( NCELL,  543) =   7.8400D-12 * CFACT 
!  Reaction Label BL15a           
             RKI( NCELL,  544) =   3.0900D-11 * CFACT 
!  Reaction Label BL15b           
             RKI( NCELL,  545) =   3.0900D-11 * CFACT 
!  Reaction Label BL16            
             RKI( NCELL,  546) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2700D-11,   4.3500D+02 )
!  Reaction Label BL17            
             RKI( NCELL,  547) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.2800D-16,  -7.8500D+02 )
!  Reaction Label BL18            
             RKI( NCELL,  548) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3300D-12,   4.9000D+02 )
!  Reaction Label BL19            
             RKI( NCELL,  549) =   4.0200D-11 * CFACT 
!  Reaction Label BT19            
             RKI( NCELL,  550) =   RKI( NCELL,  546 ) 
!  Reaction Label BT20            
             RKI( NCELL,  551) =   RKI( NCELL,  547 ) 
!  Reaction Label BT21            
             RKI( NCELL,  552) =   RKI( NCELL,  548 ) 
!  Reaction Label BT22            
             RKI( NCELL,  553) =   RKI( NCELL,  549 ) 
!  Reaction Label CI02            
             RKI( NCELL,  555) =  CFACT_SQU * POWER_T02( TEMPOT300,   7.6000D-32,  -1.8000D+00 )
!  Reaction Label CI04            
             RKI( NCELL,  557) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.3000D-30,   0.0000D+00,  -2.0000D+00,  & 
     &                                                 1.0000D-10,   0.0000D+00,  -1.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label CI05            
             RKI( NCELL,  558) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.8000D-31,   0.0000D+00,  -2.0000D+00,  & 
     &                                                 1.0000D-10,   0.0000D+00,  -1.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label CI08            
             RKI( NCELL,  561) =  CFACT * POWER_T02( TEMPOT300,   3.4400D-11,  -5.6000D-01 )
!  Reaction Label CI09            
             RKI( NCELL,  562) =  CFACT * POWER_T02( TEMPOT300,   9.4100D-12,   2.1000D+00 )
!  Reaction Label CI10            
             RKI( NCELL,  563) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-11,  -2.5000D+02 )
!  Reaction Label CI11            
             RKI( NCELL,  564) =   2.4000D-11 * CFACT 
!  Reaction Label CI12            
             RKI( NCELL,  565) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.2000D-12,   2.9500D+02 )
!  Reaction Label CI13            
             RKI( NCELL,  566) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.8000D-31,   0.0000D+00,  -3.4000D+00,  & 
     &                                                 1.5000D-11,   0.0000D+00,  -1.9000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label CI16            
             RKI( NCELL,  569) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.4800D-05,  -1.2530D+04,  -1.0000D+00,  & 
     &                                                 3.7100D+15,  -1.2530D+04,   3.5000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label CI17            
             RKI( NCELL,  570) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.2000D-12,   1.4500D+02 )
!  Reaction Label CI18            
             RKI( NCELL,  571) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2000D-12,   3.4000D+02 )
!  Reaction Label CI20            
             RKI( NCELL,  573) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2500D-11,  -1.9600D+03 )
!  Reaction Label CI21            
             RKI( NCELL,  574) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7000D-12,  -2.3000D+02 )
!  Reaction Label CI22            
             RKI( NCELL,  575) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.9000D-11,  -2.3100D+03 )
!  Reaction Label CP01            
             RKI( NCELL,  576) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.1000D-11,  -3.0000D+01 )
!  Reaction Label CP02            
             RKI( NCELL,  577) =   8.0000D-11 * CFACT 
!  Reaction Label CP03            
             RKI( NCELL,  578) =   5.5000D-11 * CFACT 
!  Reaction Label CP04            
             RKI( NCELL,  579) =   1.2300D-10 * CFACT 
!  Reaction Label CP05            
             RKI( NCELL,  580) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.7000D-11,  -1.0000D+03 )
!  Reaction Label CP06            
             RKI( NCELL,  581) =   3.6000D-11 * CFACT 
!  Reaction Label CP07            
             RKI( NCELL,  582) =   1.9200D-10 * CFACT 
!  Reaction Label CP08            
             RKI( NCELL,  583) =   2.0000D-10 * CFACT 
!  Reaction Label CP09            
             RKI( NCELL,  584) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.1000D-11,  -3.0000D+01 )
!  Reaction Label CP10            
             RKI( NCELL,  585) =   8.0000D-11 * CFACT 
!  Reaction Label CP11            
             RKI( NCELL,  586) =   6.2000D-11 * CFACT 
!  Reaction Label CP12            
             RKI( NCELL,  587) =   8.0000D-11 * CFACT 
!  Reaction Label CP13            
             RKI( NCELL,  588) =   1.6600D-10 * CFACT 
!  Reaction Label CP14            
             RKI( NCELL,  589) =   3.0000D-10 * CFACT 
!  Reaction Label CP15            
             RKI( NCELL,  590) =   4.2900D-10 * CFACT 
!  Reaction Label TP01            
             RKI( NCELL,  591) =   2.9400D-10 * CFACT 
!  Reaction Label CP17            
             RKI( NCELL,  592) =   2.3200D-10 * CFACT 
!  Reaction Label CP18            
             RKI( NCELL,  593) =   4.1200D-10 * CFACT 
!  Reaction Label CP20            
             RKI( NCELL,  595) =   3.1000D-12 * CFACT 
!  Reaction Label CP21            
             RKI( NCELL,  596) =   1.2900D-11 * CFACT 
!  Reaction Label CP29            
             RKI( NCELL,  598) =   RKI( NCELL,   52 ) 
!  Reaction Label CP30            
             RKI( NCELL,  599) =   RKI( NCELL,   53 ) 
!  Reaction Label CP31            
             RKI( NCELL,  600) =   RKI( NCELL,   54 ) 
!  Reaction Label CP32            
             RKI( NCELL,  601) =   RKI( NCELL,   55 ) 
!  Reaction Label CP33            
             RKI( NCELL,  602) =   RKI( NCELL,   56 ) 
!  Reaction Label CP34            
             RKI( NCELL,  603) =   RKI( NCELL,   56 ) 
!  Reaction Label CP35            
             RKI( NCELL,  604) =   RKI( NCELL,   70 ) 
!  Reaction Label CP36            
             RKI( NCELL,  605) =   RKI( NCELL,   70 ) 
!  Reaction Label CP37            
             RKI( NCELL,  606) =   RKI( NCELL,   70 ) 
!  Reaction Label CP38            
             RKI( NCELL,  607) =   RKI( NCELL,   70 ) 
!  Reaction Label CP39            
             RKI( NCELL,  608) =   RKI( NCELL,   52 ) 
!  Reaction Label CP40            
             RKI( NCELL,  609) =   RKI( NCELL,   53 ) 
!  Reaction Label CP41            
             RKI( NCELL,  610) =   RKI( NCELL,   54 ) 
!  Reaction Label CP42            
             RKI( NCELL,  611) =   RKI( NCELL,   55 ) 
!  Reaction Label CP43            
             RKI( NCELL,  612) =   RKI( NCELL,   56 ) 
!  Reaction Label CP44            
             RKI( NCELL,  613) =   RKI( NCELL,   56 ) 
!  Reaction Label CP45            
             RKI( NCELL,  614) =   RKI( NCELL,   70 ) 
!  Reaction Label CP46            
             RKI( NCELL,  615) =   RKI( NCELL,   70 ) 
!  Reaction Label CP47            
             RKI( NCELL,  616) =   RKI( NCELL,   70 ) 
!  Reaction Label CP48            
             RKI( NCELL,  617) =   RKI( NCELL,   70 ) 
!  Reaction Label CP49            
             RKI( NCELL,  618) =   RKI( NCELL,   52 ) 
!  Reaction Label CP50            
             RKI( NCELL,  619) =   RKI( NCELL,   53 ) 
!  Reaction Label CP51            
             RKI( NCELL,  620) =   RKI( NCELL,   54 ) 
!  Reaction Label CP52            
             RKI( NCELL,  621) =   RKI( NCELL,   55 ) 
!  Reaction Label CP53            
             RKI( NCELL,  622) =   RKI( NCELL,   56 ) 
!  Reaction Label CP54            
             RKI( NCELL,  623) =   RKI( NCELL,   56 ) 
!  Reaction Label CP55            
             RKI( NCELL,  624) =   RKI( NCELL,   70 ) 
!  Reaction Label CP56            
             RKI( NCELL,  625) =   RKI( NCELL,   70 ) 
!  Reaction Label CP57            
             RKI( NCELL,  626) =   RKI( NCELL,   70 ) 
!  Reaction Label CP58            
             RKI( NCELL,  627) =   RKI( NCELL,   70 ) 
!  Reaction Label CE01            
             RKI( NCELL,  628) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.3000D-12,  -1.2800D+03 )
!  Reaction Label CE02            
             RKI( NCELL,  629) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.6000D-29,   0.0000D+00,  -3.3000D+00,  & 
     &                                                 3.1000D-10,   0.0000D+00,  -1.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label TE01            
             RKI( NCELL,  630) =   2.6700D-10 * CFACT 
!  Reaction Label TE02            
             RKI( NCELL,  631) =   4.9000D-10 * CFACT 
!  Reaction Label CE03            
             RKI( NCELL,  632) =   4.8000D-10 * CFACT 
!  Reaction Label TE03            
             RKI( NCELL,  633) =   5.4600D-10 * CFACT 
!  Reaction Label CE04            
             RKI( NCELL,  634) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 5.2000D-30,   0.0000D+00,  -2.4000D+00,  & 
     &                                                 2.2000D-10,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label TE04            
             RKI( NCELL,  635) =   6.2000D-11 * CFACT 
!  Reaction Label TE05            
             RKI( NCELL,  636) =   1.3500D-10 * CFACT 
!  Reaction Label TE06            
             RKI( NCELL,  637) =   1.4000D-10 * CFACT 
!  Reaction Label TE07            
             RKI( NCELL,  638) =   1.4400D-10 * CFACT 
!  Reaction Label TE08            
             RKI( NCELL,  639) =   2.4200D-10 * CFACT 
!  Reaction Label TE09            
             RKI( NCELL,  640) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.6000D-11,   4.5000D+01 )
!  Reaction Label BC01            
             RKI( NCELL,  641) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.3000D-11,  -1.0000D+02 )
!  Reaction Label BC02            
             RKI( NCELL,  642) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2000D-10,   4.0000D+01 )
!  Reaction Label BC03            
             RKI( NCELL,  643) =   1.8600D-10 * CFACT 
!  Reaction Label BC04            
             RKI( NCELL,  644) =   2.6300D-10 * CFACT 
!  Reaction Label BC05            
             RKI( NCELL,  645) =   4.2100D-10 * CFACT 
!  Reaction Label BC06            
             RKI( NCELL,  646) =   3.9200D-10 * CFACT 
!  Reaction Label BC07            
             RKI( NCELL,  647) =   3.7700D-10 * CFACT 
!  Reaction Label BC08            
             RKI( NCELL,  648) =   2.1600D-10 * CFACT 
!  Reaction Label BC09a           
             RKI( NCELL,  649) =   2.6600D-10 * CFACT 
!  Reaction Label BC09b           
             RKI( NCELL,  650) =   2.6600D-10 * CFACT 
!  Reaction Label BC10            
             RKI( NCELL,  651) =   5.4600D-10 * CFACT 
!  Reaction Label BC11            
             RKI( NCELL,  652) =   RKI( NCELL,  651 ) 
!  Reaction Label AE51            
             RKI( NCELL,  653) =   RKI( NCELL,   52 ) 
!  Reaction Label AE52            
             RKI( NCELL,  654) =   RKI( NCELL,   53 ) 
!  Reaction Label AE53            
             RKI( NCELL,  655) =   RKI( NCELL,   52 ) 
!  Reaction Label AE54            
             RKI( NCELL,  656) =   RKI( NCELL,   53 ) 
!  Reaction Label AE55            
             RKI( NCELL,  657) =   RKI( NCELL,   52 ) 
!  Reaction Label AE56            
             RKI( NCELL,  658) =   RKI( NCELL,   53 ) 
!  Reaction Label AE57            
             RKI( NCELL,  659) =   RKI( NCELL,   52 ) 
!  Reaction Label AE58            
             RKI( NCELL,  660) =   RKI( NCELL,   53 ) 
!  Reaction Label TR03            
             RKI( NCELL,  663) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.4000D-12,   1.3500D+02 )
!  Reaction Label TR05            
             RKI( NCELL,  664) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-12,  -2.4310D+03 )
!  Reaction Label TR06            
             RKI( NCELL,  665) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.1000D-11,  -3.0000D+01 )
!  Reaction Label TR07            
             RKI( NCELL,  666) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-12,   3.6500D+02 )
!  Reaction Label TR09            
             RKI( NCELL,  668) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label TR10            
             RKI( NCELL,  669) =   8.0000D-11 * CFACT 
!  Reaction Label TR11            
             RKI( NCELL,  670) =   1.9900D-11 * CFACT 
!  Reaction Label TR12            
             RKI( NCELL,  671) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-15,  -2.5280D+03 )
!  Reaction Label TR13            
             RKI( NCELL,  672) =   1.1800D-15 * CFACT 
!  Reaction Label TR14            
             RKI( NCELL,  673) =   2.3700D-12 * CFACT 
!  Reaction Label TR16            
             RKI( NCELL,  675) =   2.9400D-10 * CFACT 
!  Reaction Label IS1             
             RKI( NCELL,  676) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5400D-11,   4.1000D+02 )
!  Reaction Label IS2             
             RKI( NCELL,  677) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label IS3             
             RKI( NCELL,  678) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0600D-13,   1.3000D+03 )
!  Reaction Label IS4             
             RKI( NCELL,  679) =   1.8000D-12 * CFACT 
!  Reaction Label IS5             
             RKI( NCELL,  680) =   6.8000D-13 * CFACT 
!  Reaction Label IS6             
             RKI( NCELL,  681) =   2.3000D-12 * CFACT 
!  Reaction Label IS7             
             RKI( NCELL,  682) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS107           
             RKI( NCELL,  683) =  SFACT * ARRHENUIS_T03( INV_TEMP,  4.0700D+08,  -7.6940D+03 )
!  Reaction Label IS138           
             RKI( NCELL,  685) =   4.6000D-11 * CFACT 
!  Reaction Label IS9             
             RKI( NCELL,  686) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0300D-12,  -4.4800D+02 )
!  Reaction Label IS10            
             RKI( NCELL,  687) =   2.3000D-12 * CFACT 
!  Reaction Label IS11            
             RKI( NCELL,  688) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label IS12            
             RKI( NCELL,  689) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0600D-13,   1.3000D+03 )
!  Reaction Label IS13            
             RKI( NCELL,  690) =   1.3000D-12 * CFACT 
!  Reaction Label IS14            
             RKI( NCELL,  691) =   6.0400D-13 * CFACT 
!  Reaction Label IS140           
             RKI( NCELL,  692) =   1.2000D-12 * CFACT 
!  Reaction Label IS15            
             RKI( NCELL,  693) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS17            
             RKI( NCELL,  694) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4200D-11,   6.1000D+02 )
!  Reaction Label IS18            
             RKI( NCELL,  695) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label IS19            
             RKI( NCELL,  696) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0600D-13,   1.3000D+03 )
!  Reaction Label IS20            
             RKI( NCELL,  697) =   2.0000D-13 * CFACT 
!  Reaction Label IS21            
             RKI( NCELL,  698) =   3.5000D-14 * CFACT 
!  Reaction Label IS22            
             RKI( NCELL,  699) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS24            
             RKI( NCELL,  700) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.9400D-15,  -1.5200D+03 )
!  Reaction Label IS25            
             RKI( NCELL,  701) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2000D-11,   6.5200D+02 )
!  Reaction Label IS26            
             RKI( NCELL,  702) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.4000D-12,   3.6000D+02 )
!  Reaction Label IS141           
             RKI( NCELL,  703) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0600D-13,   1.3000D+03 )
!  Reaction Label IS142           
             RKI( NCELL,  704) =   2.0000D-13 * CFACT 
!  Reaction Label IS143           
             RKI( NCELL,  705) =   3.5000D-14 * CFACT 
!  Reaction Label IS144           
             RKI( NCELL,  706) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS27            
             RKI( NCELL,  707) =   2.9000D-17 * CFACT 
!  Reaction Label IS28            
             RKI( NCELL,  708) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.4000D-12,   7.4500D+02 )
!  Reaction Label IS29            
             RKI( NCELL,  709) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.4000D-12,   3.6000D+02 )
!  Reaction Label IS145           
             RKI( NCELL,  710) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0600D-13,   1.3000D+03 )
!  Reaction Label IS146           
             RKI( NCELL,  711) =   2.0000D-13 * CFACT 
!  Reaction Label IS147           
             RKI( NCELL,  712) =   3.5000D-14 * CFACT 
!  Reaction Label IS148           
             RKI( NCELL,  713) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS30            
             RKI( NCELL,  714) =   3.7000D-19 * CFACT 
!  Reaction Label IS31            
             RKI( NCELL,  715) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.1500D-13,  -4.4800D+02 )
!  Reaction Label IS32            
             RKI( NCELL,  716) =   4.0000D-12 * CFACT 
!  Reaction Label IS34            
             RKI( NCELL,  717) =   RKI( NCELL,   76 ) 
!  Reaction Label IS109           
             RKI( NCELL,  718) =   RKI( NCELL,   73 ) 
!  Reaction Label IS36            
             RKI( NCELL,  719) =   RKI( NCELL,   67 ) 
!  Reaction Label IS38            
             RKI( NCELL,  720) =   RKI( NCELL,   70 ) 
!  Reaction Label IS40            
             RKI( NCELL,  721) =   RKI( NCELL,   69 ) 
!  Reaction Label IS41            
             RKI( NCELL,  722) =   RKI( NCELL,   72 ) 
!  Reaction Label IS33            
             RKI( NCELL,  723) =   2.3000D-12 * CFACT 
!  Reaction Label IS35            
             RKI( NCELL,  724) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label IS37            
             RKI( NCELL,  725) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0600D-13,   1.3000D+03 )
!  Reaction Label IS39            
             RKI( NCELL,  726) =   3.5000D-14 * CFACT 
!  Reaction Label IS43            
             RKI( NCELL,  727) =   2.0000D-13 * CFACT 
!  Reaction Label IS44            
             RKI( NCELL,  728) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS46            
             RKI( NCELL,  729) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.1500D-15,  -1.5200D+03 )
!  Reaction Label IS47            
             RKI( NCELL,  730) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4800D-12,   4.1000D+02 )
!  Reaction Label IS48            
             RKI( NCELL,  731) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label IS50            
             RKI( NCELL,  732) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0600D-13,   1.3000D+03 )
!  Reaction Label IS51            
             RKI( NCELL,  733) =   3.5000D-14 * CFACT 
!  Reaction Label IS52            
             RKI( NCELL,  734) =   2.0000D-13 * CFACT 
!  Reaction Label IS53            
             RKI( NCELL,  735) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS55            
             RKI( NCELL,  736) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label IS102           
             RKI( NCELL,  737) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0600D-13,   1.3000D+03 )
!  Reaction Label IS103           
             RKI( NCELL,  738) =   2.0000D-13 * CFACT 
!  Reaction Label IS104           
             RKI( NCELL,  739) =   3.5000D-14 * CFACT 
!  Reaction Label IS105           
             RKI( NCELL,  740) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS56            
             RKI( NCELL,  741) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   6.1000D+02 )
!  Reaction Label IS57            
             RKI( NCELL,  742) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label IS58            
             RKI( NCELL,  743) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8200D-13,   1.3000D+03 )
!  Reaction Label IS59            
             RKI( NCELL,  744) =   2.0000D-13 * CFACT 
!  Reaction Label IS60            
             RKI( NCELL,  745) =   3.5000D-14 * CFACT 
!  Reaction Label IS61            
             RKI( NCELL,  746) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS63            
             RKI( NCELL,  747) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label IS64            
             RKI( NCELL,  748) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8200D-13,   1.3000D+03 )
!  Reaction Label IS65            
             RKI( NCELL,  749) =   2.0000D-13 * CFACT 
!  Reaction Label IS66            
             RKI( NCELL,  750) =   3.5000D-14 * CFACT 
!  Reaction Label IS67            
             RKI( NCELL,  751) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS69            
             RKI( NCELL,  752) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.7000D-12,   3.4000D+02 )
!  Reaction Label IS70            
             RKI( NCELL,  753) =   RKI( NCELL,   67 ) 
!  Reaction Label IS71            
             RKI( NCELL,  754) =   4.0000D-12 * CFACT 
!  Reaction Label IS72            
             RKI( NCELL,  755) =   RKI( NCELL,   69 ) 
!  Reaction Label IS73            
             RKI( NCELL,  756) =   RKI( NCELL,   70 ) 
!  Reaction Label IS74            
             RKI( NCELL,  757) =   RKI( NCELL,   70 ) 
!  Reaction Label IS75            
             RKI( NCELL,  758) =   RKI( NCELL,   72 ) 
!  Reaction Label IS76            
             RKI( NCELL,  759) =   RKI( NCELL,   72 ) 
!  Reaction Label IS77            
             RKI( NCELL,  760) =   RKI( NCELL,   72 ) 
!  Reaction Label IS78            
             RKI( NCELL,  761) =   RKI( NCELL,   72 ) 
!  Reaction Label IS108           
             RKI( NCELL,  762) =   2.9000D-11 * CFACT 
!  Reaction Label IS79            
             RKI( NCELL,  763) =   8.0000D-12 * CFACT 
!  Reaction Label IS80            
             RKI( NCELL,  764) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.1500D-12,   3.0500D+02 )
!  Reaction Label IS82            
             RKI( NCELL,  766) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9400D-12,   3.6500D+02 )
!  Reaction Label IS83            
             RKI( NCELL,  768) =   4.0000D-13 * CFACT 
!  Reaction Label IS93            
             RKI( NCELL,  769) =   4.0000D-13 * CFACT 
!  Reaction Label IS84            
             RKI( NCELL,  772) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5000D-12,   1.4000D+02 )
!  Reaction Label IS85            
             RKI( NCELL,  774) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2800D-11,   4.0500D+02 )
!  Reaction Label IS86            
             RKI( NCELL,  776) =   1.0000D-11 * CFACT 
!  Reaction Label IS88            
             RKI( NCELL,  778) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-11,   3.9000D+02 )
!  Reaction Label IS89            
             RKI( NCELL,  779) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.7500D-12,   2.0000D+02 )
!  Reaction Label IS90            
             RKI( NCELL,  780) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.7800D-11,  -4.0000D+02 )
!  Reaction Label IS91            
             RKI( NCELL,  781) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0600D-13,   1.3000D+03 )
!  Reaction Label IS96            
             RKI( NCELL,  782) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label IS112           
             RKI( NCELL,  783) =   2.0000D-13 * CFACT 
!  Reaction Label IS113           
             RKI( NCELL,  784) =   3.5000D-14 * CFACT 
!  Reaction Label IS114           
             RKI( NCELL,  785) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label IS94            
             RKI( NCELL,  787) =   8.0000D-12 * CFACT 
!  Reaction Label IS99            
             RKI( NCELL,  788) =   5.0000D-11 * CFACT 
!  Reaction Label IS139           
             RKI( NCELL,  789) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.8000D-12,   2.0000D+02 )
!  Reaction Label IS00            
             RKI( NCELL,  790) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.0000D-12,   3.8000D+02 )
!  Reaction Label BP56            
             RKI( NCELL,  791) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5000D-12,  -1.8150D+03 )
!  Reaction Label CP16            
             RKI( NCELL,  793) =   3.8500D-10 * CFACT 
!  Reaction Label IA69            
             RKI( NCELL,  794) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.7000D-12,   3.4000D+02 )
!  Reaction Label IA70            
             RKI( NCELL,  795) =   RKI( NCELL,   67 ) 
!  Reaction Label IA71            
             RKI( NCELL,  796) =   4.0000D-12 * CFACT 
!  Reaction Label IA72            
             RKI( NCELL,  797) =   RKI( NCELL,   69 ) 
!  Reaction Label IA73            
             RKI( NCELL,  798) =   RKI( NCELL,   70 ) 
!  Reaction Label IA74            
             RKI( NCELL,  799) =   RKI( NCELL,   70 ) 
!  Reaction Label IA75            
             RKI( NCELL,  800) =   RKI( NCELL,   72 ) 
!  Reaction Label IA76            
             RKI( NCELL,  801) =   RKI( NCELL,   72 ) 
!  Reaction Label IA77            
             RKI( NCELL,  802) =   RKI( NCELL,   72 ) 
!  Reaction Label IA78            
             RKI( NCELL,  803) =   RKI( NCELL,   72 ) 
!  Reaction Label IA79            
             RKI( NCELL,  804) =   RKI( NCELL,   72 ) 
!  Reaction Label IA80            
             RKI( NCELL,  805) =  SFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D+07,  -5.2970D+03 )
!  Reaction Label IA51            
             RKI( NCELL,  806) =   RKI( NCELL,   73 ) 
!  Reaction Label IA52            
             RKI( NCELL,  807) =  SFACT * ARRHENUIS_T03( INV_TEMP,  1.6000D+16,  -1.3486D+04 )
!  Reaction Label IC01            
             RKI( NCELL,  809) =   RKI( NCELL,   70 ) 
!  Reaction Label IC02            
             RKI( NCELL,  810) =   RKI( NCELL,   70 ) 
!  Reaction Label IC03            
             RKI( NCELL,  811) =   RKI( NCELL,   70 ) 
!  Reaction Label IC04            
             RKI( NCELL,  812) =   RKI( NCELL,   70 ) 
!  Reaction Label IC05            
             RKI( NCELL,  813) =   RKI( NCELL,   70 ) 
!  Reaction Label IC06            
             RKI( NCELL,  814) =   RKI( NCELL,   70 ) 
!  Reaction Label IC07            
             RKI( NCELL,  815) =   RKI( NCELL,   70 ) 
!  Reaction Label IC08            
             RKI( NCELL,  816) =   RKI( NCELL,   70 ) 
!  Reaction Label IC09            
             RKI( NCELL,  817) =   RKI( NCELL,   70 ) 
!  Reaction Label IC10            
             RKI( NCELL,  818) =   RKI( NCELL,   70 ) 
!  Reaction Label IC11            
             RKI( NCELL,  819) =   RKI( NCELL,   70 ) 
!  Reaction Label IC12            
             RKI( NCELL,  820) =   RKI( NCELL,   70 ) 
!  Reaction Label IC13            
             RKI( NCELL,  821) =   RKI( NCELL,   70 ) 
!  Reaction Label IC14            
             RKI( NCELL,  822) =   RKI( NCELL,   70 ) 
!  Reaction Label IC15            
             RKI( NCELL,  823) =   RKI( NCELL,   70 ) 
!  Reaction Label IC16            
             RKI( NCELL,  824) =   RKI( NCELL,   70 ) 
!  Reaction Label IC17            
             RKI( NCELL,  825) =   RKI( NCELL,   70 ) 
!  Reaction Label IC18            
             RKI( NCELL,  826) =   RKI( NCELL,   70 ) 
!  Reaction Label IC19            
             RKI( NCELL,  827) =   RKI( NCELL,   70 ) 
!  Reaction Label IC20            
             RKI( NCELL,  828) =   RKI( NCELL,   70 ) 
!  Reaction Label IC21            
             RKI( NCELL,  829) =   RKI( NCELL,   70 ) 
!  Reaction Label IC22            
             RKI( NCELL,  830) =   RKI( NCELL,   70 ) 
!  Reaction Label IC23            
             RKI( NCELL,  831) =   RKI( NCELL,   70 ) 
!  Reaction Label IC24            
             RKI( NCELL,  832) =   RKI( NCELL,   70 ) 
!  Reaction Label IC25            
             RKI( NCELL,  833) =   RKI( NCELL,   70 ) 
!  Reaction Label IC26            
             RKI( NCELL,  834) =   RKI( NCELL,   70 ) 
!  Reaction Label IC27            
             RKI( NCELL,  835) =   RKI( NCELL,   70 ) 
!  Reaction Label IC28            
             RKI( NCELL,  836) =   RKI( NCELL,   70 ) 
!  Reaction Label IC29            
             RKI( NCELL,  837) =   RKI( NCELL,   70 ) 
!  Reaction Label IC30            
             RKI( NCELL,  838) =   RKI( NCELL,   70 ) 
!  Reaction Label IC31            
             RKI( NCELL,  839) =   RKI( NCELL,   70 ) 
!  Reaction Label IC32            
             RKI( NCELL,  840) =   RKI( NCELL,   70 ) 
!  Reaction Label IC33            
             RKI( NCELL,  841) =   RKI( NCELL,   70 ) 
!  Reaction Label IC34            
             RKI( NCELL,  842) =   RKI( NCELL,   70 ) 
!  Reaction Label IC35            
             RKI( NCELL,  843) =   RKI( NCELL,   70 ) 
!  Reaction Label IA108           
             RKI( NCELL,  844) =   3.0000D-11 * CFACT 
!  Reaction Label IA90            
             RKI( NCELL,  845) =   1.0000D-12 * CFACT 
!  Reaction Label IA91            
             RKI( NCELL,  846) =   4.4000D-12 * CFACT 
!  Reaction Label IA92            
             RKI( NCELL,  847) =   1.6600D-11 * CFACT 
!  Reaction Label HET_N2O5        
             RKI( NCELL,  848) =  BLKHET(  NCELL, IK_HETERO_N2O5IJ )
!  Reaction Label HET_N02         
             RKI( NCELL,  849) =  BLKHET(  NCELL, IK_HETERO_NO2 )
!  Reaction Label HET_N2O5IJ      
             RKI( NCELL,  850) =  BLKHET(  NCELL, IK_HETERO_N2O5IJ )
!  Reaction Label HET_N2O5K       
             RKI( NCELL,  851) =  BLKHET(  NCELL, IK_HETERO_N2O5K )
!  Reaction Label HET_H2NO3PIJA   
             RKI( NCELL,  852) =  BLKHET(  NCELL, IK_HETERO_H2NO3PAIJ )
!  Reaction Label HET_H2NO3PKA    
             RKI( NCELL,  853) =  BLKHET(  NCELL, IK_HETERO_H2NO3PAK )
!  Reaction Label HET_H2NO3PIB    
             RKI( NCELL,  854) =  BLKHET(  NCELL, IK_HETERO_H2NO3PBIJ )
!  Reaction Label HET_H2NO3PJB    
             RKI( NCELL,  855) =  BLKHET(  NCELL, IK_HETERO_H2NO3PBIJ )
!  Reaction Label HET_H2NO3PKB    
             RKI( NCELL,  856) =  BLKHET(  NCELL, IK_HETERO_H2NO3PBK )
!  Reaction Label HET_IEPOX       
             RKI( NCELL,  858) =  BLKHET(  NCELL, IK_HETERO_IEPOX )
!  Reaction Label HET_IMAE        
             RKI( NCELL,  859) =  BLKHET(  NCELL, IK_HETERO_IMAE )
!  Reaction Label HET_IHMML       
             RKI( NCELL,  860) =  BLKHET(  NCELL, IK_HETERO_IMAE )
!  Reaction Label HET_TETROL      
             RKI( NCELL,  861) =  BLKHET(  NCELL, IK_HETERO_TETROL )
!  Reaction Label HET_IEPOXOS     
             RKI( NCELL,  862) =  BLKHET(  NCELL, IK_HETERO_IEPOXOS )
!  Reaction Label HET_DIM1        
             RKI( NCELL,  863) =  BLKHET(  NCELL, IK_HETERO_TETROLDIM )
!  Reaction Label HET_DIM2        
             RKI( NCELL,  864) =  BLKHET(  NCELL, IK_HETERO_IEPOXOSDI )
!  Reaction Label HET_2MG1        
             RKI( NCELL,  865) =  BLKHET(  NCELL, IK_HETERO_2MG )
!  Reaction Label HET_IMAEOS1     
             RKI( NCELL,  866) =  BLKHET(  NCELL, IK_HETERO_IMAEOS )
!  Reaction Label HET_2MG2        
             RKI( NCELL,  867) =  BLKHET(  NCELL, IK_HETERO_IMAEOS )
!  Reaction Label HET_IMAEOS2     
             RKI( NCELL,  868) =  BLKHET(  NCELL, IK_HETERO_IMAEOS )
!  Reaction Label OLIG_XYLENE1    
             RKI( NCELL,  869) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_XYLENE2    
             RKI( NCELL,  870) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TOLUENE1   
             RKI( NCELL,  871) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TOLUENE2   
             RKI( NCELL,  872) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_BENZENE1   
             RKI( NCELL,  873) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_BENZENE2   
             RKI( NCELL,  874) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TERPENE1   
             RKI( NCELL,  875) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TERPENE2   
             RKI( NCELL,  876) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ISOPRENE1  
             RKI( NCELL,  877) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ISOPRENE2  
             RKI( NCELL,  878) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_SESQT1     
             RKI( NCELL,  879) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_PAH1       
             RKI( NCELL,  880) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_PAH2       
             RKI( NCELL,  881) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ALK1       
             RKI( NCELL,  882) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ALK2       
             RKI( NCELL,  883) =   9.4882D-06 * SFACT 
!  Reaction Label RPOAGEPI        
             RKI( NCELL,  884) =   2.5000D-12 * CFACT 
!  Reaction Label RPOAGELI        
             RKI( NCELL,  885) =  BLKHET(  NCELL, IK_HETERO_IMAEOS )
!  Reaction Label RPOAGEPJ        
             RKI( NCELL,  886) =   2.5000D-12 * CFACT 
!  Reaction Label RPOAGELJ        
             RKI( NCELL,  887) =  BLKHET(  NCELL, IK_HETERO_IMAEOS )

        END DO  
!  Multiply rate constants by [M], [O2], [N2], [H2O], [H2], or [CH4]
!  where needed and return
       IF ( NWM .GT. 0 ) THEN
          DO NRT = 1, NWM
             IRXN = NRXWM( NRT )
             DO NCELL = 1, NUMCELLS
                RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_AIR
             END DO
          END DO
       END IF
       IF ( NWO2 .GT. 0 ) THEN
          DO NRT = 1, NWO2
             IRXN = NRXWO2( NRT )
             DO NCELL = 1, NUMCELLS
                RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_O2
             END DO
          END DO
       END IF
       IF ( NWN2 .GT. 0 ) THEN
          DO NRT = 1, NWN2
             IRXN = NRXWN2( NRT )
             DO NCELL = 1, NUMCELLS
                RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_N2
             END DO
          END DO
       END IF
       IF ( NWW .GT. 0 ) THEN
          DO NRT = 1, NWW
             IRXN = NRXWW( NRT )
             DO NCELL = 1, NUMCELLS
                RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * BLKH2O( NCELL )
             END DO
          END DO
       END IF
       IF ( NWH2 .GT. 0 ) THEN
          DO NRT = 1, NWH2
             IRXN = NRXWH2( NRT )
             DO NCELL = 1, NUMCELLS
                RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_H2
             END DO
          END DO
       END IF
       IF ( NWCH4 .GT. 0 ) THEN
          DO NRT = 1, NWCH4
             IRXN = NRXWCH4( NRT )
             DO NCELL = 1, NUMCELLS
                RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_CH4
             END DO
          END DO
       END IF
       RETURN
       END SUBROUTINE CALC_RCONST
         FUNCTION MAP_CHEMISTRY_SPECIES() RESULT ( SUCCESS )

! Purpose find or test the CGRID Index, Species Type, and Conversion Factor
! for the Mechanism against the CMAQ namelists

            USE UTILIO_DEFN
            USE CGRID_SPCS
            USE RXNS_DATA

            IMPLICIT NONE

!Parameters:
            CHARACTER(  1 ), PARAMETER :: BL = ' '
            INTEGER,         PARAMETER :: SPC_DIM = 200
!Local:

            LOGICAL SUCCESS
            INTEGER I, IOS, J
            INTEGER I1, I2, I3, I4      ! SURROGATE TYPE 1 COUNTERS
            INTEGER J1, J2              ! SURROGATE TYPE 2 COUNTERS
            INTEGER K1, K2, K3, K4, K5  ! CONTROL TYPE COUNTERS
            INTEGER ICALL
            INTEGER LOGDEV

            LOGICAL :: ORDER = .TRUE.
            LOGICAL :: FOUND = .TRUE.

            CHARACTER( 120 ) :: XMSG

            CHARACTER( 16 ), ALLOCATABLE     :: CGRID_SPC  ( : )
            CHARACTER( 16 ), ALLOCATABLE     :: NML_SPC    ( : )
            CHARACTER(  2 ), ALLOCATABLE     :: NML_TYPE   ( : )
            INTEGER,         ALLOCATABLE     :: NML_INDEX  ( : )
            LOGICAL,         ALLOCATABLE     :: NML_CONVERT( : )
            REAL,            ALLOCATABLE     :: NML_MOLWT  ( : )
            REAL                             :: DELTA            ! fractional difference

            LOGICAL, SAVE :: INITIALIZED = .FALSE.

            IF( INITIALIZED )RETURN

            INITIALIZED = .TRUE.
            LOGDEV      =  INIT3()
            SUCCESS     = .TRUE.


            ALLOCATE ( CGRID_SPC( NSPCSD - 1 ),    &
     &                 NML_SPC  ( NSPCSD - 1 ),    &
     &                 NML_INDEX( NSPCSD - 1 ),    &
     &                 NML_TYPE( NSPCSD - 1 ),     &
     &                 NML_CONVERT( NSPCSD - 1 ),  &
     &                 NML_MOLWT( NSPCSD - 1 ),    &
     &                 STAT = IOS )


            J = 0


            NML_INDEX     = -1
            TYPE_INDEX    = -1
            NML_TYPE      = '??'
            NML_CONVERT   = .FALSE.

            DO I = 1, N_GC_SPC ! load gc names and indices
               J = J + 1
               CGRID_SPC( I )     = GC_SPC( I )
               NML_INDEX( J )     = I + GC_STRT -1
               NML_TYPE( J )      = 'GC'
               NML_MOLWT( J )     = GC_MOLWT( I )
            END DO

            DO I = 1, N_AE_SPC ! load ae names and indices
               J = J + 1
               CGRID_SPC( J )     = AE_SPC( I )
               NML_INDEX( J )     = I + AE_STRT - 1
               NML_TYPE( J )      = 'AE'
               NML_CONVERT( J )   = .TRUE.
               NML_MOLWT( J )     = AE_MOLWT( I )
            END DO

            DO I = 1, N_NR_SPC ! load nr names and indices
               J = J + 1
               CGRID_SPC( J )     = NR_SPC( I )
               NML_INDEX( J )     = I + NR_STRT - 1
               NML_TYPE( J )      = 'NR'
               NML_MOLWT( J )     = NR_MOLWT( I )
            END DO

            DO I = 1, N_TR_SPC ! load tr names and indices
               J = J + 1
               CGRID_SPC( J )     = TR_SPC( I )
               NML_INDEX( J )     = I + TR_STRT - 1
               NML_TYPE( J )      = 'TR'
               NML_MOLWT( J )     = TR_MOLWT( I )
            END DO

            NML_SPC( 1:(NSPCSD-1) ) = CGRID_SPC( 1:(NSPCSD-1) )


! determine if mechanism species are in cgrid species

            DO I = 1, NUMB_MECH_SPC
               I1 = INDEX1R( CHEMISTRY_SPC( I ), (NSPCSD-1), CGRID_SPC )
               IF ( I1 .LT. 1 ) THEN
                  FOUND = .FALSE.
               ELSE
                  FOUND = .TRUE.
                  IF( .NOT. MAPPED_TO_CGRID )THEN
                      CGRID_INDEX( I )   = NML_INDEX( I1 )
                      SPECIES_TYPE( I )  = NML_TYPE ( I1 )
                      SPECIES_MOLWT( I ) = NML_MOLWT( I1 )
                      CONVERT_CONC( I )  = NML_CONVERT( I1 )
                  ELSE
                      IF(CGRID_INDEX( I ) .NE. NML_INDEX( I1 ))THEN
                         SUCCESS = .FALSE.
                         XMSG = '*** For Species ' // TRIM( CHEMISTRY_SPC( I ) ) &
    &                        // ' cgrid index does not match mechanism value.'
                         WRITE( LOGDEV,'( /5X, A )' ) TRIM( XMSG )
                         WRITE( XMSG,'(A,I3,1X,I3)')'CGRID Indices: Mechanism and NML Values are ',    &
    &                    CGRID_INDEX( I ),NML_INDEX( I1 )
                         WRITE( LOGDEV,'( 5X, A )' )XMSG
                      END IF
                      IF(CONVERT_CONC( I ) .NEQV. NML_CONVERT( I1 ))THEN
                         SUCCESS = .FALSE.
                         XMSG = '*** For Species ' // TRIM( CHEMISTRY_SPC( I ) ) &
    &                        // ' species unit conversion flag does not match mechanism value.'
                         WRITE( LOGDEV,'( /5X, A )' ) TRIM( XMSG )
                         WRITE( XMSG,'(A,1X,L21X,L2)')'CONVERSION FLAGS: Mechanism and NML Values are ', &
    &                    CONVERT_CONC( I ),NML_CONVERT( I1 )
                         WRITE( LOGDEV,'( 5X, A )' )XMSG
                         WRITE( XMSG,'(A,1X,A3,1X,A3)')'SPECIES TYPE: Mechanism and NML Values are ',    &
    &                    SPECIES_TYPE( I ),NML_TYPE( I1 )
                         WRITE( LOGDEV,'( 5X, A )' )XMSG
                      END IF
                      DELTA = ( SPECIES_MOLWT( I ) - NML_MOLWT( I1 ) )/MAX(NML_MOLWT( I1 ),1.0E-20)
                      IF( ABS( DELTA ) .GE. 0.05 )THEN
                         IF( CONVERT_CONC( I ) )SUCCESS = .FALSE.
                         XMSG = '*** For Species ' // TRIM( CHEMISTRY_SPC( I ) ) &
    &                        // ' species molecular weight does not match mechanism value.'
                         WRITE( LOGDEV,'( /5X, A )' ) TRIM( XMSG )
                         WRITE( XMSG,'(A,2(ES12.4,1X))')'Molecular Weight: Mechanism and NML Values are ', &
    &                    SPECIES_MOLWT( I ), NML_MOLWT( I1 )
                         WRITE( LOGDEV,'( 5X, A )' )XMSG
                      END IF
                 END IF
              END IF
              IF( INDEX( CHEMISTRY_SPC( I ), 'SRF') .GT. 0 )THEN
                  SUCCESS = .FALSE.
                  XMSG = '*** reactions cannot use modal aerosol surface area as species'
                  WRITE( LOGDEV,'( /5X, A )' ) TRIM( XMSG )
                  XMSG = TRIM( CHEMISTRY_SPC( I ) )
                  WRITE( LOGDEV,'( 2X, A )' ) TRIM( XMSG )
              END IF
              IF( INDEX( CHEMISTRY_SPC( I ), 'NUM') .GT. 0 )THEN
                  SUCCESS = .FALSE.
                  XMSG = '*** reactions cannot use modal aerosol number density as species'
                  WRITE( LOGDEV,'( /5X, A )' ) TRIM( XMSG )
                  XMSG = TRIM( CHEMISTRY_SPC( I ) )
                  WRITE( LOGDEV,'( 2X, A )' ) TRIM( XMSG )
              END IF
              IF ( .NOT. FOUND ) THEN
                 XMSG = 'Fatal error: Mechanism Species found not in species namelist:'
                 WRITE( LOGDEV,'( /5X, A )', ADVANCE = 'NO' ) TRIM( XMSG )
                 XMSG = TRIM( CHEMISTRY_SPC( I ) )
                 WRITE( LOGDEV,'( 2X, A )' ) TRIM( XMSG )
                 SUCCESS = .FALSE.
              END IF
            END DO

         RETURN

         END FUNCTION MAP_CHEMISTRY_SPECIES
!----------------------------------------------------------------------------------------
         INTEGER FUNCTION INDEX1R ( NAME, N, NLIST )
            IMPLICIT NONE
            CHARACTER( * ) NAME        ! character string being searched for
            INTEGER N                  ! length of array to be searched
            CHARACTER( * ) NLIST( : )  ! array to be searched

            INTEGER I

            DO I = 1, N
               IF ( NAME .EQ. NLIST( I ) ) THEN
                  INDEX1R = I
                  RETURN
               END IF
           END DO
           INDEX1R = 0
           RETURN

          END FUNCTION INDEX1R
       END MODULE RXNS_FUNCTION
