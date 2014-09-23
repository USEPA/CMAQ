       MODULE RXNS_FUNCTION


       IMPLICIT NONE



! Name of Mechanism SAPRC99_AE5_AQ

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
 
       SUBROUTINE CALC_RCONST( BLKTEMP, BLKPRES, BLKH2O, RJBLK, BLKHET, LSUNLIGHT, RKI, NUMCELLS )

!**********************************************************************

!  Function: To compute thermal and photolytic reaction rate
!            coefficients for each reaction.

!  Preconditions: Photolysis rates for individual species must have
!                 been calculated and stored in RJPHOT. Expects
!                 temperature in deg K, pressure in atm., water
!                 vapor in ppmV, and J-values in /min.
!  Key Subroutines/Functions Called: POWER_02, ARRHRENUIS_T0*, FALLOFF_T* 
!***********************************************************************




       USE RXNS_DATA

        IMPLICIT NONE  

!  Arguements: None 

        REAL( 8 ), INTENT( IN  ) :: BLKTEMP( : )      ! temperature, deg K 
        REAL( 8 ), INTENT( IN  ) :: BLKPRES( : )      ! pressure, Atm
        REAL( 8 ), INTENT( IN  ) :: BLKH2O ( : )      ! water mixing ratio, ppm 
        REAL( 8 ), INTENT( IN  ) :: RJBLK  ( :, : )   ! photolysis rates, 1/min 
        REAL( 8 ), INTENT( IN  ) :: BLKHET ( :, : )   ! heterogeneous rate constants, ???/min
        INTEGER,   INTENT( IN  ) :: NUMCELLS          ! Number of cells in block 
        LOGICAL,   INTENT( IN  ) :: LSUNLIGHT         ! Is there sunlight? 
        REAL( 8 ), INTENT( OUT ) :: RKI ( :, : )   ! reaction rate constant, ppm/min 
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

        RKI = 0.0 

! All rate constants converted from  molec/cm3 to ppm
! and 1/sec to 1/min

        IF( LSUNLIGHT )THEN 
            DO NCELL = 1, NUMCELLS 

!  Reaction Label 1               
                RKI( NCELL,    1) =  RJBLK( NCELL, IJ_NO2_SAPRC99 )
!  Reaction Label 18              
                RKI( NCELL,   15) =  RJBLK( NCELL, IJ_NO3NO_SAPRC99 )
!  Reaction Label 19              
                RKI( NCELL,   16) =  RJBLK( NCELL, IJ_NO3NO2_SAPRC99 )
!  Reaction Label 20              
                RKI( NCELL,   17) =  RJBLK( NCELL, IJ_O3O3P_SAPRC99 )
!  Reaction Label 21              
                RKI( NCELL,   18) =  RJBLK( NCELL, IJ_O3O1D_SAPRC99 )
!  Reaction Label 25              
                RKI( NCELL,   22) =  RJBLK( NCELL, IJ_HONO_NO_SAPRC99 )
!  Reaction Label 26              
                RKI( NCELL,   23) =  RJBLK( NCELL, IJ_HONO_NO2_SAPRC99 )
!  Reaction Label 31              
                RKI( NCELL,   28) =  RJBLK( NCELL, IJ_HNO3_SAPRC99 )
!  Reaction Label 37              
                RKI( NCELL,   34) =  RJBLK( NCELL, IJ_HO2NO2_SAPRC99 )
!  Reaction Label 43              
                RKI( NCELL,   41) =  RJBLK( NCELL, IJ_H2O2_SAPRC99 )
!  Reaction Label FAHV            
                RKI( NCELL,  123) =  RJBLK( NCELL, IJ_HCHO_R_SAPRC99 )
!  Reaction Label FAVS            
                RKI( NCELL,  124) =  RJBLK( NCELL, IJ_HCHO_M_SAPRC99 )
!  Reaction Label AAHV            
                RKI( NCELL,  131) =  RJBLK( NCELL, IJ_CCHO_R_SAPRC99 )
!  Reaction Label PAHV            
                RKI( NCELL,  134) =  RJBLK( NCELL, IJ_C2CHO_SAPRC99 )
!  Reaction Label K3HV            
                RKI( NCELL,  137) =  RJBLK( NCELL, IJ_ACETONE_SAPRC99 )
!  Reaction Label K4HV            
                RKI( NCELL,  139) =   1.5000D-01 * RJBLK( NCELL, IJ_KETONE_SAPRC99 )
!  Reaction Label MERA            
                RKI( NCELL,  142) =  RJBLK( NCELL, IJ_COOH_SAPRC99 )
!  Reaction Label LPRA            
                RKI( NCELL,  144) =  RJBLK( NCELL, IJ_COOH_SAPRC99 )
!  Reaction Label GLHV            
                RKI( NCELL,  145) =  RJBLK( NCELL, IJ_GLY_R_SAPRC99 )
!  Reaction Label GLVM            
                RKI( NCELL,  146) =   6.0000D-03 * RJBLK( NCELL, IJ_GLY_ABS_SAPRC99 )
!  Reaction Label MGHV            
                RKI( NCELL,  149) =  RJBLK( NCELL, IJ_MGLY_ADJ_SAPRC99 )
!  Reaction Label BAHV            
                RKI( NCELL,  152) =  RJBLK( NCELL, IJ_BACL_ADJ_SAPRC99 )
!  Reaction Label BZHV            
                RKI( NCELL,  159) =   5.0000D-02 * RJBLK( NCELL, IJ_BZCHO_SAPRC99 )
!  Reaction Label MAHV            
                RKI( NCELL,  165) =   4.1000D-03 * RJBLK( NCELL, IJ_ACROLEIN_SAPRC99 )
!  Reaction Label MVHV            
                RKI( NCELL,  169) =   2.1000D-03 * RJBLK( NCELL, IJ_ACROLEIN_SAPRC99 )
!  Reaction Label IPHV            
                RKI( NCELL,  173) =   4.1000D-03 * RJBLK( NCELL, IJ_ACROLEIN_SAPRC99 )
!  Reaction Label K6HV            
                RKI( NCELL,  175) =   2.0000D-02 * RJBLK( NCELL, IJ_KETONE_SAPRC99 )
!  Reaction Label RNHV            
                RKI( NCELL,  177) =  RJBLK( NCELL, IJ_IC3ONO2_SAPRC99 )
!  Reaction Label D2HV            
                RKI( NCELL,  181) =   3.6500D-01 * RJBLK( NCELL, IJ_MGLY_ABS_SAPRC99 )
!  Reaction Label D3HV            
                RKI( NCELL,  183) =   7.2800D+00 * RJBLK( NCELL, IJ_ACROLEIN_SAPRC99 )
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
             RKI( NCELL,    2) =  CFACT_SQU * POWER_T02( TEMPOT300,   5.6800D-34,  -2.8000D+00 )
!  Reaction Label 3               
             RKI( NCELL,    3) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.0000D-12,  -2.0600D+03 )
!  Reaction Label 4               
             RKI( NCELL,    4) =  CFACT_SQU * POWER_T02( TEMPOT300,   1.0000D-31,  -1.6000D+00 )
!  Reaction Label 5               
             RKI( NCELL,    5) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.5000D-12,   1.2000D+02 )
!  Reaction Label 6               
             RKI( NCELL,    6) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 9.0000D-32,   0.0000D+00,  -2.0000D+00,  & 
     &                                                 2.2000D-11,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   8.0000D-01 )
!  Reaction Label 8               
             RKI( NCELL,    7) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-12,  -1.3700D+03 )
!  Reaction Label 9               
             RKI( NCELL,    8) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-13,  -2.4700D+03 )
!  Reaction Label 10              
             RKI( NCELL,    9) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-11,   1.1000D+02 )
!  Reaction Label 11              
             RKI( NCELL,   10) =  CFACT_SQU * ARRHENUIS_T03( INV_TEMP,  3.3000D-39,   5.3000D+02 )
!  Reaction Label 12              
             RKI( NCELL,   11) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.8000D-30,   0.0000D+00,  -3.5000D+00,  & 
     &                                                 2.0000D-12,   0.0000D+00,   2.0000D-01,  & 
     &                                                 1.0000D+00,   4.5000D-01 )
!  Reaction Label 13              
             RKI( NCELL,   12) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.0000D-03,  -1.1000D+04,  -3.5000D+00,  & 
     &                                                 9.7000D+14,  -1.1080D+04,   1.0000D-01,  & 
     &                                                 1.0000D+00,   4.5000D-01 )
!  Reaction Label 14              
             RKI( NCELL,   13) =   2.6000D-22 * CFACT 
!  Reaction Label 17              
             RKI( NCELL,   14) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.5000D-14,  -1.2600D+03 )
!  Reaction Label 22              
             RKI( NCELL,   19) =   2.2000D-10 * CFACT 
!  Reaction Label 23              
             RKI( NCELL,   20) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0900D-11,   9.5000D+01 )
!  Reaction Label 24              
             RKI( NCELL,   21) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 7.0000D-31,   0.0000D+00,  -2.6000D+00,  & 
     &                                                 3.6000D-11,   0.0000D+00,  -1.0000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 27              
             RKI( NCELL,   24) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   2.6000D+02 )
!  Reaction Label 28              
             RKI( NCELL,   25) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.4300D-30,   0.0000D+00,  -3.1000D+00,  & 
     &                                                 1.6700D-11,   0.0000D+00,  -2.1000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 29              
             RKI( NCELL,   26) =   2.0000D-11 * CFACT 
!  Reaction Label 30              
             RKI( NCELL,   27) =  CFACT * FALLOFF_T08( INV_TEMP,  CAIR, & 
     &                                                 7.2000D-15,   7.8500D+02,   4.1000D-16,  & 
     &                                                 1.4400D+03,   1.9000D-33,   7.2500D+02 )
!  Reaction Label 32              
             RKI( NCELL,   29) =  CFACT * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 1.3000D-13,   0.0000D+00,   3.1900D-33,  & 
     &                                                 0.0000D+00 )
!  Reaction Label 33              
             RKI( NCELL,   30) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-12,  -1.0000D+03 )
!  Reaction Label 34              
             RKI( NCELL,   31) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-12,   2.7000D+02 )
!  Reaction Label 35              
             RKI( NCELL,   32) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.8000D-31,   0.0000D+00,  -3.2000D+00,  & 
     &                                                 4.7000D-12,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 36              
             RKI( NCELL,   33) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.1000D-05,  -1.0650D+04,   0.0000D+00,  & 
     &                                                 5.7000D+15,  -1.1170D+04,   0.0000D+00,  & 
     &                                                 1.0000D+00,   5.0000D-01 )
!  Reaction Label 38              
             RKI( NCELL,   35) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5000D-12,   3.6000D+02 )
!  Reaction Label 39              
             RKI( NCELL,   36) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-14,  -6.0000D+02 )
!  Reaction Label 40A             
             RKI( NCELL,   37) =  CFACT * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 2.2000D-13,   6.0000D+02,   1.8500D-33,  & 
     &                                                 9.8000D+02 )
!  Reaction Label 40B             
             RKI( NCELL,   38) =  CFACT_SQU * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 3.0800D-34,   2.8000D+03,   2.5900D-54,  & 
     &                                                 3.1800D+03 )
!  Reaction Label 41              
             RKI( NCELL,   39) =   4.0000D-12 * CFACT 
!  Reaction Label 42              
             RKI( NCELL,   40) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5000D-13,  -2.4500D+03 )
!  Reaction Label 44              
             RKI( NCELL,   42) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D-12,  -1.6000D+02 )
!  Reaction Label 45              
             RKI( NCELL,   43) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.8000D-11,   2.5000D+02 )
!  Reaction Label S2OH            
             RKI( NCELL,   44) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.0000D-31,   0.0000D+00,  -3.3000D+00,  & 
     &                                                 2.0000D-12,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   4.5000D-01 )
!  Reaction Label H2OH            
             RKI( NCELL,   45) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.7000D-12,  -2.1000D+03 )
!  Reaction Label MER1            
             RKI( NCELL,   46) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-12,   2.8500D+02 )
!  Reaction Label MER4            
             RKI( NCELL,   47) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.8000D-13,   7.8000D+02 )
!  Reaction Label MEN3            
             RKI( NCELL,   48) =   1.3000D-12 * CFACT 
!  Reaction Label MER5            
             RKI( NCELL,   49) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.4500D-14,   7.1000D+02 )
!  Reaction Label MER6            
             RKI( NCELL,   50) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.9000D-13,  -5.0900D+02 )
!  Reaction Label RRNO            
             RKI( NCELL,   51) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label RRH2            
             RKI( NCELL,   52) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label RRN3            
             RKI( NCELL,   53) =   2.3000D-12 * CFACT 
!  Reaction Label RRME            
             RKI( NCELL,   54) =   2.0000D-13 * CFACT 
!  Reaction Label RRR2            
             RKI( NCELL,   55) =   3.5000D-14 * CFACT 
!  Reaction Label R2NO            
             RKI( NCELL,   56) =   RKI( NCELL,   51 ) 
!  Reaction Label R2H2            
             RKI( NCELL,   57) =   RKI( NCELL,   52 ) 
!  Reaction Label R2N3            
             RKI( NCELL,   58) =   RKI( NCELL,   53 ) 
!  Reaction Label R2ME            
             RKI( NCELL,   59) =   RKI( NCELL,   54 ) 
!  Reaction Label R2RR            
             RKI( NCELL,   60) =   RKI( NCELL,   55 ) 
!  Reaction Label R2R3            
             RKI( NCELL,   61) =   RKI( NCELL,   55 ) 
!  Reaction Label RNNO            
             RKI( NCELL,   62) =   RKI( NCELL,   51 ) 
!  Reaction Label RNH2            
             RKI( NCELL,   63) =   RKI( NCELL,   52 ) 
!  Reaction Label RNME            
             RKI( NCELL,   64) =   RKI( NCELL,   54 ) 
!  Reaction Label RNN3            
             RKI( NCELL,   65) =   RKI( NCELL,   53 ) 
!  Reaction Label RNRR            
             RKI( NCELL,   66) =   RKI( NCELL,   55 ) 
!  Reaction Label RNR2            
             RKI( NCELL,   67) =   RKI( NCELL,   55 ) 
!  Reaction Label RNRN            
             RKI( NCELL,   68) =   RKI( NCELL,   55 ) 
!  Reaction Label APN2            
             RKI( NCELL,   69) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.7000D-28,   0.0000D+00,  -7.1000D+00,  & 
     &                                                 1.2000D-11,   0.0000D+00,  -9.0000D-01,  & 
     &                                                 1.0000D+00,   3.0000D-01 )
!  Reaction Label DPAN            
             RKI( NCELL,   70) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.9000D-03,  -1.2100D+04,   0.0000D+00,  & 
     &                                                 4.0000D+16,  -1.3600D+04,   0.0000D+00,  & 
     &                                                 1.0000D+00,   3.0000D-01 )
!  Reaction Label APNO            
             RKI( NCELL,   71) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.8000D-12,   3.0000D+02 )
!  Reaction Label APH2            
             RKI( NCELL,   72) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.3000D-13,   1.0400D+03 )
!  Reaction Label APN3            
             RKI( NCELL,   73) =   4.0000D-12 * CFACT 
!  Reaction Label APME            
             RKI( NCELL,   74) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-12,   5.0000D+02 )
!  Reaction Label APRR            
             RKI( NCELL,   75) =   7.5000D-12 * CFACT 
!  Reaction Label APR2            
             RKI( NCELL,   76) =   RKI( NCELL,   75 ) 
!  Reaction Label APRN            
             RKI( NCELL,   77) =   RKI( NCELL,   75 ) 
!  Reaction Label APAP            
             RKI( NCELL,   78) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D-12,   5.0000D+02 )
!  Reaction Label PPN2            
             RKI( NCELL,   79) =  CFACT * POWER_T02( TEMPOT300,   1.2000D-11,  -9.0000D-01 )
!  Reaction Label PAN2            
             RKI( NCELL,   80) =  SFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D+15,  -1.2800D+04 )
!  Reaction Label PPNO            
             RKI( NCELL,   81) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2500D-11,   2.4000D+02 )
!  Reaction Label PPH2            
             RKI( NCELL,   82) =   RKI( NCELL,   72 ) 
!  Reaction Label PPN3            
             RKI( NCELL,   83) =   RKI( NCELL,   73 ) 
!  Reaction Label PPME            
             RKI( NCELL,   84) =   RKI( NCELL,   74 ) 
!  Reaction Label PPRR            
             RKI( NCELL,   85) =   RKI( NCELL,   75 ) 
!  Reaction Label PPR2            
             RKI( NCELL,   86) =   RKI( NCELL,   75 ) 
!  Reaction Label PPRN            
             RKI( NCELL,   87) =   RKI( NCELL,   75 ) 
!  Reaction Label PPAP            
             RKI( NCELL,   88) =   RKI( NCELL,   78 ) 
!  Reaction Label PPPP            
             RKI( NCELL,   89) =   RKI( NCELL,   78 ) 
!  Reaction Label BPN2            
             RKI( NCELL,   90) =   1.3700D-11 * CFACT 
!  Reaction Label BPAN            
             RKI( NCELL,   91) =  SFACT * ARRHENUIS_T03( INV_TEMP,  7.9000D+16,  -1.4000D+04 )
!  Reaction Label BPNO            
             RKI( NCELL,   92) =   RKI( NCELL,   81 ) 
!  Reaction Label BPH2            
             RKI( NCELL,   93) =   RKI( NCELL,   72 ) 
!  Reaction Label BPN3            
             RKI( NCELL,   94) =   RKI( NCELL,   73 ) 
!  Reaction Label BPME            
             RKI( NCELL,   95) =   RKI( NCELL,   74 ) 
!  Reaction Label BPRR            
             RKI( NCELL,   96) =   RKI( NCELL,   75 ) 
!  Reaction Label BPR2            
             RKI( NCELL,   97) =   RKI( NCELL,   75 ) 
!  Reaction Label BPRN            
             RKI( NCELL,   98) =   RKI( NCELL,   75 ) 
!  Reaction Label BPAP            
             RKI( NCELL,   99) =   RKI( NCELL,   78 ) 
!  Reaction Label BPPP            
             RKI( NCELL,  100) =   RKI( NCELL,   78 ) 
!  Reaction Label BPBP            
             RKI( NCELL,  101) =   RKI( NCELL,   78 ) 
!  Reaction Label MPN2            
             RKI( NCELL,  102) =   RKI( NCELL,   79 ) 
!  Reaction Label MPPN            
             RKI( NCELL,  103) =  SFACT * ARRHENUIS_T03( INV_TEMP,  1.6000D+16,  -1.3486D+04 )
!  Reaction Label MPNO            
             RKI( NCELL,  104) =   RKI( NCELL,   81 ) 
!  Reaction Label MPH2            
             RKI( NCELL,  105) =   RKI( NCELL,   72 ) 
!  Reaction Label MPN3            
             RKI( NCELL,  106) =   RKI( NCELL,   73 ) 
!  Reaction Label MPME            
             RKI( NCELL,  107) =   RKI( NCELL,   74 ) 
!  Reaction Label MPRR            
             RKI( NCELL,  108) =   RKI( NCELL,   75 ) 
!  Reaction Label MPR2            
             RKI( NCELL,  109) =   RKI( NCELL,   75 ) 
!  Reaction Label MPRN            
             RKI( NCELL,  110) =   RKI( NCELL,   75 ) 
!  Reaction Label MPAP            
             RKI( NCELL,  111) =   RKI( NCELL,   78 ) 
!  Reaction Label MPPP            
             RKI( NCELL,  112) =   RKI( NCELL,   78 ) 
!  Reaction Label MPBP            
             RKI( NCELL,  113) =   RKI( NCELL,   78 ) 
!  Reaction Label MPMP            
             RKI( NCELL,  114) =   RKI( NCELL,   78 ) 
!  Reaction Label TBON            
             RKI( NCELL,  115) =   2.4000D-11 * CFACT 
!  Reaction Label TBOD            
             RKI( NCELL,  116) =  SFACT * ARRHENUIS_T03( INV_TEMP,  7.5000D+14,  -8.1520D+03 )
!  Reaction Label BRN2            
             RKI( NCELL,  117) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3000D-11,   1.5000D+02 )
!  Reaction Label BRH2            
             RKI( NCELL,  118) =   RKI( NCELL,   52 ) 
!  Reaction Label BRXX            
             RKI( NCELL,  119) =   1.0000D-03 * SFACT 
!  Reaction Label BNN2            
             RKI( NCELL,  120) =   RKI( NCELL,  117 ) 
!  Reaction Label BNH2            
             RKI( NCELL,  121) =   RKI( NCELL,   52 ) 
!  Reaction Label BNXX            
             RKI( NCELL,  122) =   RKI( NCELL,  119 ) 
!  Reaction Label FAOH            
             RKI( NCELL,  125) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.6000D-12,   2.0000D+01 )
!  Reaction Label FAH2            
             RKI( NCELL,  126) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.7000D-15,   6.2500D+02 )
!  Reaction Label FAHR            
             RKI( NCELL,  127) =  SFACT * ARRHENUIS_T03( INV_TEMP,  2.4000D+12,  -7.0000D+03 )
!  Reaction Label FAHN            
             RKI( NCELL,  128) =   RKI( NCELL,   46 ) 
!  Reaction Label FAN3            
             RKI( NCELL,  129) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-12,  -2.4310D+03 )
!  Reaction Label AAOH            
             RKI( NCELL,  130) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.6000D-12,   3.1000D+02 )
!  Reaction Label AAN3            
             RKI( NCELL,  132) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label PAOH            
             RKI( NCELL,  133) =   2.0000D-11 * CFACT 
!  Reaction Label PAN3            
             RKI( NCELL,  135) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.7710D+03 )
!  Reaction Label K3OH            
             RKI( NCELL,  136) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1000D-12,  -5.2000D+02 )
!  Reaction Label K4OH            
             RKI( NCELL,  138) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.3000D-12,  -2.5000D+01,   2.0000D+00 )
!  Reaction Label MeOH            
             RKI( NCELL,  140) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   3.1000D-12,  -3.6000D+02,   2.0000D+00 )
!  Reaction Label MER9            
             RKI( NCELL,  141) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D-12,   1.9000D+02 )
!  Reaction Label LPR9            
             RKI( NCELL,  143) =   1.1000D-11 * CFACT 
!  Reaction Label GLOH            
             RKI( NCELL,  147) =   1.1000D-11 * CFACT 
!  Reaction Label GLN3            
             RKI( NCELL,  148) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-12,  -2.3760D+03 )
!  Reaction Label MGOH            
             RKI( NCELL,  150) =   1.5000D-11 * CFACT 
!  Reaction Label MGN3            
             RKI( NCELL,  151) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8950D+03 )
!  Reaction Label PHOH            
             RKI( NCELL,  153) =   2.6300D-11 * CFACT 
!  Reaction Label PHN3            
             RKI( NCELL,  154) =   3.7800D-12 * CFACT 
!  Reaction Label CROH            
             RKI( NCELL,  155) =   4.2000D-11 * CFACT 
!  Reaction Label CRN3            
             RKI( NCELL,  156) =   1.3700D-11 * CFACT 
!  Reaction Label NPN3            
             RKI( NCELL,  157) =   RKI( NCELL,  154 ) 
!  Reaction Label BZOH            
             RKI( NCELL,  158) =   1.2900D-11 * CFACT 
!  Reaction Label BZNT            
             RKI( NCELL,  160) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8720D+03 )
!  Reaction Label MAOH            
             RKI( NCELL,  161) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8600D-11,   1.7600D+02 )
!  Reaction Label MAO3            
             RKI( NCELL,  162) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3600D-15,  -2.1140D+03 )
!  Reaction Label MAN3            
             RKI( NCELL,  163) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5000D-12,  -1.7260D+03 )
!  Reaction Label MAOP            
             RKI( NCELL,  164) =   6.3400D-12 * CFACT 
!  Reaction Label MVOH            
             RKI( NCELL,  166) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.1400D-12,   4.5300D+02 )
!  Reaction Label MVO3            
             RKI( NCELL,  167) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.5100D-16,  -1.5200D+03 )
!  Reaction Label MVOP            
             RKI( NCELL,  168) =   4.3200D-12 * CFACT 
!  Reaction Label IPOH            
             RKI( NCELL,  170) =   6.1900D-11 * CFACT 
!  Reaction Label IPO3            
             RKI( NCELL,  171) =   4.1800D-18 * CFACT 
!  Reaction Label IPN3            
             RKI( NCELL,  172) =   1.0000D-13 * CFACT 
!  Reaction Label K6OH            
             RKI( NCELL,  174) =   1.5000D-11 * CFACT 
!  Reaction Label RNOH            
             RKI( NCELL,  176) =   7.8000D-12 * CFACT 
!  Reaction Label D1OH            
             RKI( NCELL,  178) =   5.0000D-11 * CFACT 
!  Reaction Label D1O3            
             RKI( NCELL,  179) =   2.0000D-18 * CFACT 
!  Reaction Label D2OH            
             RKI( NCELL,  180) =   5.0000D-11 * CFACT 
!  Reaction Label D3OH            
             RKI( NCELL,  182) =   5.0000D-11 * CFACT 
!  Reaction Label c1OH            
             RKI( NCELL,  184) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.1500D-12,  -1.7350D+03 )
!  Reaction Label etOH            
             RKI( NCELL,  185) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9600D-12,   4.3800D+02 )
!  Reaction Label etO3            
             RKI( NCELL,  186) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.1400D-15,  -2.5800D+03 )
!  Reaction Label etN3            
             RKI( NCELL,  187) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   4.3900D-13,  -2.2820D+03,   2.0000D+00 )
!  Reaction Label etOA            
             RKI( NCELL,  188) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0400D-11,  -7.9200D+02 )
!  Reaction Label isOH            
             RKI( NCELL,  189) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5000D-11,   4.0800D+02 )
!  Reaction Label isO3            
             RKI( NCELL,  190) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.8600D-15,  -1.9120D+03 )
!  Reaction Label isN3            
             RKI( NCELL,  191) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0300D-12,  -4.4800D+02 )
!  Reaction Label isOP            
             RKI( NCELL,  192) =   3.6000D-11 * CFACT 
!  Reaction Label t1OH            
             RKI( NCELL,  193) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8300D-11,   4.4900D+02 )
!  Reaction Label t1O3            
             RKI( NCELL,  194) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0800D-15,  -8.2100D+02 )
!  Reaction Label t1N3            
             RKI( NCELL,  195) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.6600D-12,   1.7500D+02 )
!  Reaction Label t1OP            
             RKI( NCELL,  196) =   3.2700D-11 * CFACT 
!  Reaction Label a1OH            
             RKI( NCELL,  197) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.3700D-12,  -4.9800D+02,   2.0000D+00 )
!  Reaction Label a2OH            
             RKI( NCELL,  198) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.8700D-12,  -6.7100D+02 )
!  Reaction Label a3OH            
             RKI( NCELL,  199) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0200D-11,  -4.3400D+02 )
!  Reaction Label a4OH            
             RKI( NCELL,  200) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.9500D-12,  -9.1000D+01 )
!  Reaction Label a5OH            
             RKI( NCELL,  201) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1100D-11,  -5.2000D+01 )
!  Reaction Label b1OH            
             RKI( NCELL,  202) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8100D-12,   3.5500D+02 )
!  Reaction Label AR1N            
             RKI( NCELL,  203) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label AR1H            
             RKI( NCELL,  204) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label b2OH            
             RKI( NCELL,  205) =   2.6400D-11 * CFACT 
!  Reaction Label AR2N            
             RKI( NCELL,  206) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label AR2H            
             RKI( NCELL,  207) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label BENZ            
             RKI( NCELL,  208) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.4700D-12,  -2.0600D+02 )
!  Reaction Label BNZN            
             RKI( NCELL,  209) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label BNZH            
             RKI( NCELL,  210) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label o1OH            
             RKI( NCELL,  211) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.1000D-12,   4.5100D+02 )
!  Reaction Label o1O3            
             RKI( NCELL,  212) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6200D-15,  -1.6400D+03 )
!  Reaction Label o1N3            
             RKI( NCELL,  213) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4500D-14,  -3.7600D+02 )
!  Reaction Label o1OP            
             RKI( NCELL,  214) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0700D-11,  -2.3400D+02 )
!  Reaction Label o2OH            
             RKI( NCELL,  215) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7400D-11,   3.8400D+02 )
!  Reaction Label o2O3            
             RKI( NCELL,  216) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.0200D-16,  -4.6100D+02 )
!  Reaction Label o2N3            
             RKI( NCELL,  217) =   7.2600D-13 * CFACT 
!  Reaction Label o2OP            
             RKI( NCELL,  218) =   2.0900D-11 * CFACT 
!  Reaction Label I1OH            
             RKI( NCELL,  219) =   4.5000D-13 * CFACT 
!  Reaction Label I2OH            
             RKI( NCELL,  220) =   8.0000D-13 * CFACT 
!  Reaction Label I3OH            
             RKI( NCELL,  221) =   1.1600D-12 * CFACT 
!  Reaction Label SSO3            
             RKI( NCELL,  222) =   1.1600D-14 * CFACT 
!  Reaction Label SSOH            
             RKI( NCELL,  223) =   1.9700D-10 * CFACT 
!  Reaction Label SSN3            
             RKI( NCELL,  224) =   1.9000D-11 * CFACT 
!  Reaction Label HET_N2O5        
             RKI( NCELL,  225) =  BLKHET(  NCELL, IK_HETERO_N2O5IJ )
!  Reaction Label HET_N02         
             RKI( NCELL,  226) =  BLKHET(  NCELL, IK_HETERO_NO2 )
!  Reaction Label OLIG_ALKENE     
             RKI( NCELL,  227) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_XYLENE1    
             RKI( NCELL,  228) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_XYLENE2    
             RKI( NCELL,  229) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TOLUENE1   
             RKI( NCELL,  230) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TOLUENE2   
             RKI( NCELL,  231) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_BENZENE1   
             RKI( NCELL,  232) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_BENZENE2   
             RKI( NCELL,  233) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TERPENE1   
             RKI( NCELL,  234) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TERPENE2   
             RKI( NCELL,  235) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ISOPRENE1  
             RKI( NCELL,  236) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ISOPRENE2  
             RKI( NCELL,  237) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_SESQT1     
             RKI( NCELL,  238) =   9.4882D-06 * SFACT 

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
