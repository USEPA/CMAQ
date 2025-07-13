       MODULE RXNS_FUNCTION


       IMPLICIT NONE



! Name of Mechanism SAPRC22_AE65_AQ

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
       REAL( 8 ) FUNCTION HALOGEN_FALLOFF(PRESS,A1,B1,A2,B2,A3)
         IMPLICIT NONE
         REAL( 8 ), INTENT( IN ) :: PRESS
         REAL( 8 ), INTENT( IN ) :: A1
         REAL( 8 ), INTENT( IN ) :: B1
         REAL( 8 ), INTENT( IN ) :: A2
         REAL( 8 ), INTENT( IN ) :: B2
         REAL( 8 ), INTENT( IN ) :: A3 ! Maximum loss rate (1/sec)
         INTRINSIC DEXP
         HALOGEN_FALLOFF = A1 * DEXP( B1 * PRESS ) + A2 * DEXP( B2 * PRESS )
         HALOGEN_FALLOFF = DMIN1 (A3, HALOGEN_FALLOFF )
         RETURN
       END FUNCTION HALOGEN_FALLOFF

       SUBROUTINE SPECIAL_RATES( NUMCELLS, Y, TEMP, DENS, RKI )
! Purpose: calculate special rate operators and update
!         appropriate rate constants

       USE RXNS_DATA
       IMPLICIT NONE

! Arguments:
       INTEGER,      INTENT( IN  )   :: NUMCELLS        ! Number of cells in block 
       REAL( 8 ),    INTENT( IN )    :: Y( :, : )       ! species concs
       REAL( 8 ),    INTENT( IN )    :: TEMP( : )       ! air temperature, K 
       REAL( 8 ),    INTENT( IN )    :: DENS( : )       ! air density, Kg/m3
       REAL( 8 ),    INTENT( INOUT ) :: RKI( :, : )     ! reaction rate constant, ppm/min 
! Local:
       REAL( 8 ), PARAMETER :: DENSITY_TO_NUMBER = 2.07930D+19 ! Kg/m3 to molecules/cm3

       INTEGER   :: NCELL
       REAL( 8 ) :: INV_TEMP
       REAL( 8 ) :: CAIR
       REAL( 8 ) :: CFACT         ! scales operator if not multiplied by RKI, cm^3/(molecule) to 1/(ppm)
       REAL( 8 ) :: CFACT_SQU     ! scales operator if not multiplied by RKI, cm^6/(molec^2) to 1/(ppm^2)
! special rate operators listed below



       DO NCELL = 1, NUMCELLS
          INV_TEMP  = 1.0D0 / TEMP( NCELL )
          CAIR      = DENSITY_TO_NUMBER * DENS( NCELL )
          CFACT     = 1.0D-06 * CAIR
          CFACT_SQU = 1.0D-12 * CAIR * CAIR


! define special rate operators


! define rate constants in terms of special rate operators 

       END DO

       RETURN
       END SUBROUTINE SPECIAL_RATES
 
       SUBROUTINE CALC_RCONST( BLKTEMP, BLKPRES, BLKH2O, RJBLK, BLKHET, LSUNLIGHT, SEAWATER, RKI, NUMCELLS )

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
        REAL( 8 ),           INTENT( IN  ) :: SEAWATER( : )     ! fractional area of OPEN+SURF 
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
        REAL( 8 ) :: H2O           ! concentration, [molec/cm^3] 

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
!  Reaction Label 35              
                RKI( NCELL,   35) =  RJBLK( NCELL, IJ_HNO4_06 )
!  Reaction Label 42              
                RKI( NCELL,   42) =  RJBLK( NCELL, IJ_H2O2 )
!  Reaction Label 117             
                RKI( NCELL,  117) =  RJBLK( NCELL, IJ_HCHOR_13 )
!  Reaction Label 118             
                RKI( NCELL,  118) =  RJBLK( NCELL, IJ_HCHOM_13 )
!  Reaction Label P1HV            
                RKI( NCELL,  122) =  RJBLK( NCELL, IJ_PAN_11 )
!  Reaction Label 123             
                RKI( NCELL,  123) =  RJBLK( NCELL, IJ_GLY_I13R )
!  Reaction Label 124             
                RKI( NCELL,  124) =  RJBLK( NCELL, IJ_GLY_I13M )
!  Reaction Label BLHV            
                RKI( NCELL,  128) =   9.0000D-02 * RJBLK( NCELL, IJ_BALD_11 )
!  Reaction Label PBHV            
                RKI( NCELL,  131) =  RJBLK( NCELL, IJ_PPN_11 )
!  Reaction Label NPHV            
                RKI( NCELL,  133) =   1.5000D-03 * RJBLK( NCELL, IJ_NO2_06 )
!  Reaction Label 139             
                RKI( NCELL,  139) =  RJBLK( NCELL, IJ_BACL_11 )
!  Reaction Label H1HV            
                RKI( NCELL,  528) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label A2HV            
                RKI( NCELL,  533) =  RJBLK( NCELL, IJ_CCHOR_13 )
!  Reaction Label GAHV            
                RKI( NCELL,  537) =  RJBLK( NCELL, IJ_GLALD_14 )
!  Reaction Label A3HV            
                RKI( NCELL,  542) =  RJBLK( NCELL, IJ_C2CHOabs )
!  Reaction Label ARHV            
                RKI( NCELL,  548) =  RJBLK( NCELL, IJ_ACROL_16 )
!  Reaction Label K3HV            
                RKI( NCELL,  552) =  RJBLK( NCELL, IJ_ACET_06 )
!  Reaction Label K4HV            
                RKI( NCELL,  554) =   1.7500D-01 * RJBLK( NCELL, IJ_MEK_06 )
!  Reaction Label MAHV            
                RKI( NCELL,  562) =  RJBLK( NCELL, IJ_MACR_06 )
!  Reaction Label MVHV            
                RKI( NCELL,  565) =  RJBLK( NCELL, IJ_MVK_16 )
!  Reaction Label F1HV            
                RKI( NCELL,  569) =   2.5000D-01 * RJBLK( NCELL, IJ_AFGS )
!  Reaction Label A4HV            
                RKI( NCELL,  637) =  RJBLK( NCELL, IJ_C2CHOabs )
!  Reaction Label A5HV            
                RKI( NCELL,  645) =  RJBLK( NCELL, IJ_MACR_06 )
!  Reaction Label A6HV            
                RKI( NCELL,  655) =  RJBLK( NCELL, IJ_C2CHOabs )
!  Reaction Label K5HV            
                RKI( NCELL,  659) =   7.5300D-02 * RJBLK( NCELL, IJ_MEK_06 )
!  Reaction Label K6HV            
                RKI( NCELL,  666) =  RJBLK( NCELL, IJ_MVK_16 )
!  Reaction Label PAHV            
                RKI( NCELL,  676) =  RJBLK( NCELL, IJ_PAA )
!  Reaction Label MGHV            
                RKI( NCELL,  679) =  RJBLK( NCELL, IJ_MGLY_13 )
!  Reaction Label BAHV            
                RKI( NCELL,  680) =  RJBLK( NCELL, IJ_BACL_11 )
!  Reaction Label N4HV            
                RKI( NCELL,  690) =  RJBLK( NCELL, IJ_CRBNIT )
!  Reaction Label N3HV            
                RKI( NCELL,  694) =  RJBLK( NCELL, IJ_IC3ONO2 )
!  Reaction Label N5HV            
                RKI( NCELL,  696) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label N6HV            
                RKI( NCELL,  700) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label NDHV            
                RKI( NCELL,  704) =  RJBLK( NCELL, IJ_DIONO2 )
!  Reaction Label N1HV            
                RKI( NCELL,  708) =  RJBLK( NCELL, IJ_IC3ONO2 )
!  Reaction Label N2HV            
                RKI( NCELL,  710) =  RJBLK( NCELL, IJ_IC3ONO2 )
!  Reaction Label H4HV            
                RKI( NCELL,  714) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label H3HV            
                RKI( NCELL,  718) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label H5HV            
                RKI( NCELL,  722) =   1.0000D-01 * RJBLK( NCELL, IJ_HPALDS )
!  Reaction Label H2HV            
                RKI( NCELL,  726) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label F2HV            
                RKI( NCELL,  730) =   2.2000D-01 * RJBLK( NCELL, IJ_AFGS )
!  Reaction Label F3HV            
                RKI( NCELL,  736) =   2.5000D-01 * RJBLK( NCELL, IJ_AFGS )
!  Reaction Label F4HV            
                RKI( NCELL,  740) =   2.2000D-01 * RJBLK( NCELL, IJ_AFGS )
!  Reaction Label P2HV            
                RKI( NCELL,  746) =  RJBLK( NCELL, IJ_PPN_11 )
!  Reaction Label P4HV            
                RKI( NCELL,  751) =  RJBLK( NCELL, IJ_PPN_11 )
!  Reaction Label TR01            
                RKI( NCELL,  791) =  RJBLK( NCELL, IJ_HCHOR_13 )
!  Reaction Label TR02            
                RKI( NCELL,  792) =  RJBLK( NCELL, IJ_HCHOM_13 )
!  Reaction Label TR08            
                RKI( NCELL,  796) =  RJBLK( NCELL, IJ_CCHOR_13 )
!  Reaction Label TR15            
                RKI( NCELL,  801) =  RJBLK( NCELL, IJ_ACROL_16 )

                IF ( SEAWATER (NCELL) .GT. 0.001D0 ) THEN
!  Reaction Label HAL_Ozone       
                   RKI( NCELL,  766) = SEAWATER (NCELL) *  SFACT * HALOGEN_FALLOFF( BLKPRES( NCELL ),   6.7006D-11,   1.0743D+01,  & 
     &                                                           3.4153D-08,  -6.7130D-01,         2.0000D-06 )
                ELSE
                   RKI( NCELL,  766) = 0.0D0 
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
             RKI( NCELL,    2) =  CFACT_SQU * POWER_T02( TEMPOT300,   6.0000D-34,   2.4000D+00 )
!  Reaction Label 3               
             RKI( NCELL,    3) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.0000D-12,  -2.0600D+03 )
!  Reaction Label 4               
             RKI( NCELL,    4) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 9.0000D-32,   0.0000D+00,  -1.5000D+00,  & 
     &                                                 3.0000D-11,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 5               
             RKI( NCELL,    5) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.1000D-12,   2.1000D+02 )
!  Reaction Label 6               
             RKI( NCELL,    6) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.5000D-31,   0.0000D+00,  -1.8000D+00,  & 
     &                                                 2.2000D-11,   0.0000D+00,  -7.0000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 7               
             RKI( NCELL,    7) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0000D-12,  -1.5000D+03 )
!  Reaction Label 8               
             RKI( NCELL,    8) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2000D-13,  -2.4500D+03 )
!  Reaction Label 9               
             RKI( NCELL,    9) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5000D-11,   1.7000D+02 )
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
             RKI( NCELL,   13) =   0.0000D+00 * CFACT 
!  Reaction Label 14              
             RKI( NCELL,   14) =   0.0000D+00 * CFACT_SQU 
!  Reaction Label 15              
             RKI( NCELL,   15) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.5000D-14,  -1.2600D+03 )
!  Reaction Label 20              
             RKI( NCELL,   20) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6300D-10,   6.0000D+01 )
!  Reaction Label 21              
             RKI( NCELL,   21) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6500D-11,   9.8000D+01 )
!  Reaction Label 22              
             RKI( NCELL,   22) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 7.0000D-31,   0.0000D+00,  -2.6000D+00,  & 
     &                                                 3.6000D-11,   0.0000D+00,  -1.0000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 24              
             RKI( NCELL,   24) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-11,  -3.9000D+02 )
!  Reaction Label 25              
             RKI( NCELL,   25) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 3.2000D-30,   0.0000D+00,  -4.5000D+00,  & 
     &                                                 3.0000D-11,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.2400D+00,   4.1000D-01 )
!  Reaction Label 26              
             RKI( NCELL,   26) =   2.2000D-11 * CFACT 
!  Reaction Label 27              
             RKI( NCELL,   27) =  CFACT * FALLOFF_T08( INV_TEMP,  CAIR, & 
     &                                                 2.4000D-14,   4.6000D+02,   2.7000D-17,  & 
     &                                                 2.1990D+03,   6.5000D-34,   1.3350D+03 )
!  Reaction Label 29              
             RKI( NCELL,   29) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7000D-12,  -9.4000D+02 )
!  Reaction Label 30              
             RKI( NCELL,   30) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.3000D-12,   2.7000D+02 )
!  Reaction Label 31              
             RKI( NCELL,   31) =  CFACT * FALLOFF_T11( INV_TEMP,TEMPOT300,CAIR, & 
     &                                                 2.3900D-12,  -1.3770D+01,  -1.7110D+03, &
     &                                                 1.8300D-32,  -4.8500D+00,  -7.7200D+02,  & 
     &                                                 0.0000D+00,   0.0000D+00 )
!  Reaction Label 32              
             RKI( NCELL,   32) =  CFACT_SQU * ARRHENUIS_T03( INV_TEMP,  1.2000D-35,   2.9440D+03 )
!  Reaction Label 33              
             RKI( NCELL,   33) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.4000D-31,   0.0000D+00,  -3.1000D+00,  & 
     &                                                 4.0000D-12,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.2600D+00,   4.0000D-01 )
!  Reaction Label 34              
             RKI( NCELL,   34) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.1000D-05,  -1.0650D+04,   0.0000D+00,  & 
     &                                                 6.0000D+15,  -1.1170D+04,   0.0000D+00,  & 
     &                                                 1.2600D+00,   4.0000D-01 )
!  Reaction Label 36              
             RKI( NCELL,   36) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3000D-12,   3.8000D+02 )
!  Reaction Label 37              
             RKI( NCELL,   37) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0000D-14,  -4.9000D+02 )
!  Reaction Label 38              
             RKI( NCELL,   38) =  CFACT * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 3.0000D-13,   4.6000D+02,   2.1000D-33,  & 
     &                                                 9.2000D+02 )
!  Reaction Label 39              
             RKI( NCELL,   39) =  CFACT_SQU * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 4.2000D-34,   2.6600D+03,   2.9400D-54,  & 
     &                                                 3.1200D+03 )
!  Reaction Label 40              
             RKI( NCELL,   40) =   3.5000D-12 * CFACT 
!  Reaction Label 41              
             RKI( NCELL,   41) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5000D-13,  -2.4500D+03 )
!  Reaction Label 43              
             RKI( NCELL,   43) =   1.8000D-12 * CFACT 
!  Reaction Label 44              
             RKI( NCELL,   44) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.8000D-11,   2.5000D+02 )
!  Reaction Label 45              
             RKI( NCELL,   45) =  CFACT * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 1.4400D-13,   0.0000D+00,   3.4300D-33,  & 
     &                                                 0.0000D+00 )
!  Reaction Label 46              
             RKI( NCELL,   46) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-12,  -1.8000D+03 )
!  Reaction Label R2NO            
             RKI( NCELL,   47) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label R2H2            
             RKI( NCELL,   48) =   1.4900D-11 * CFACT 
!  Reaction Label R2N3            
             RKI( NCELL,   49) =   2.3000D-12 * CFACT 
!  Reaction Label R2R2            
             RKI( NCELL,   50) =   1.6000D-14 * CFACT 
!  Reaction Label R3N2            
             RKI( NCELL,   51) =  CFACT * POWER_T02( TEMPOT300,   7.7000D-12,  -2.0000D-01 )
!  Reaction Label R3NO            
             RKI( NCELL,   52) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.7000D-12,   3.4000D+02 )
!  Reaction Label R3H2            
             RKI( NCELL,   53) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.1400D-12,   5.8000D+02 )
!  Reaction Label R3N3            
             RKI( NCELL,   54) =   4.0000D-12 * CFACT 
!  Reaction Label R3R2            
             RKI( NCELL,   55) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label R3R3            
             RKI( NCELL,   56) =   1.7000D-11 * CFACT 
!  Reaction Label 57              
             RKI( NCELL,   57) =   RKI( NCELL,   47 ) 
!  Reaction Label 58              
             RKI( NCELL,   58) =   RKI( NCELL,   48 ) 
!  Reaction Label 59              
             RKI( NCELL,   59) =   RKI( NCELL,   49 ) 
!  Reaction Label 60              
             RKI( NCELL,   60) =   RKI( NCELL,   50 ) 
!  Reaction Label 61              
             RKI( NCELL,   61) =   RKI( NCELL,   55 ) 
!  Reaction Label 62              
             RKI( NCELL,   62) =   RKI( NCELL,   47 ) 
!  Reaction Label 63              
             RKI( NCELL,   63) =   RKI( NCELL,   48 ) 
!  Reaction Label 64              
             RKI( NCELL,   64) =   RKI( NCELL,   49 ) 
!  Reaction Label 65              
             RKI( NCELL,   65) =   RKI( NCELL,   50 ) 
!  Reaction Label 66              
             RKI( NCELL,   66) =   RKI( NCELL,   55 ) 
!  Reaction Label 67              
             RKI( NCELL,   67) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-12,   3.0000D+02 )
!  Reaction Label 68              
             RKI( NCELL,   68) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.8000D-13,   7.8000D+02 )
!  Reaction Label 69              
             RKI( NCELL,   69) =   1.2000D-12 * CFACT 
!  Reaction Label 70              
             RKI( NCELL,   70) =   2.1600D-13 * CFACT 
!  Reaction Label 71              
             RKI( NCELL,   71) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-12,   5.0000D+02 )
!  Reaction Label 72              
             RKI( NCELL,   72) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label 73              
             RKI( NCELL,   73) =   7.4400D-12 * CFACT 
!  Reaction Label 74              
             RKI( NCELL,   74) =   2.3000D-12 * CFACT 
!  Reaction Label 75              
             RKI( NCELL,   75) =   2.9000D-14 * CFACT 
!  Reaction Label 76              
             RKI( NCELL,   76) =   1.6000D-11 * CFACT 
!  Reaction Label 77              
             RKI( NCELL,   77) =   RKI( NCELL,   47 ) 
!  Reaction Label 78              
             RKI( NCELL,   78) =   RKI( NCELL,   48 ) 
!  Reaction Label 79              
             RKI( NCELL,   79) =   RKI( NCELL,   49 ) 
!  Reaction Label 80              
             RKI( NCELL,   80) =   RKI( NCELL,   50 ) 
!  Reaction Label 81              
             RKI( NCELL,   81) =   RKI( NCELL,   55 ) 
!  Reaction Label Q1NO            
             RKI( NCELL,   82) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.1000D-12,   2.7000D+02 )
!  Reaction Label Q1N2            
             RKI( NCELL,   83) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 9.7000D-29,   0.0000D+00,  -5.6000D+00,  & 
     &                                                 9.3000D-12,   0.0000D+00,  -1.5000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label Q1N3            
             RKI( NCELL,   84) =   4.0000D-12 * CFACT 
!  Reaction Label Q1H2            
             RKI( NCELL,   85) =   2.2000D-11 * CFACT 
!  Reaction Label Q1R2            
             RKI( NCELL,   86) =   1.6000D-11 * CFACT 
!  Reaction Label Q1R3            
             RKI( NCELL,   87) =   1.4000D-11 * CFACT 
!  Reaction Label Q6N2            
             RKI( NCELL,   88) =   1.1100D-11 * CFACT 
!  Reaction Label Q6NO            
             RKI( NCELL,   89) =   1.6000D-11 * CFACT 
!  Reaction Label Q6H2            
             RKI( NCELL,   90) =   RKI( NCELL,   53 ) 
!  Reaction Label Q6N3            
             RKI( NCELL,   91) =   RKI( NCELL,   54 ) 
!  Reaction Label Q6R2            
             RKI( NCELL,   92) =   RKI( NCELL,   55 ) 
!  Reaction Label Q6R3            
             RKI( NCELL,   93) =   RKI( NCELL,   56 ) 
!  Reaction Label 94              
             RKI( NCELL,   94) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5000D-12,   5.5300D+02 )
!  Reaction Label 95              
             RKI( NCELL,   95) =  SFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D+13,  -6.8560D+03 )
!  Reaction Label 96              
             RKI( NCELL,   96) =   2.0800D-12 * CFACT 
!  Reaction Label 97              
             RKI( NCELL,   97) =   RKI( NCELL,   48 ) 
!  Reaction Label 98              
             RKI( NCELL,   98) =   2.8600D-13 * CFACT 
!  Reaction Label 99              
             RKI( NCELL,   99) =   1.4900D-11 * CFACT 
!  Reaction Label 100             
             RKI( NCELL,  100) =   RKI( NCELL,   51 ) 
!  Reaction Label 101             
             RKI( NCELL,  101) =   RKI( NCELL,   53 ) 
!  Reaction Label 102             
             RKI( NCELL,  102) =   1.0000D-03 * SFACT 
!  Reaction Label 103             
             RKI( NCELL,  103) =   RKI( NCELL,   51 ) 
!  Reaction Label 104             
             RKI( NCELL,  104) =   RKI( NCELL,   53 ) 
!  Reaction Label 105             
             RKI( NCELL,  105) =   1.0000D-03 * SFACT 
!  Reaction Label G1N2            
             RKI( NCELL,  106) =   7.0000D-12 * CFACT 
!  Reaction Label G1WA            
             RKI( NCELL,  107) =   2.4000D-15 * CFACT 
!  Reaction Label G1S2            
             RKI( NCELL,  108) =   3.8000D-11 * CFACT 
!  Reaction Label G2N2            
             RKI( NCELL,  109) =   7.0000D-12 * CFACT 
!  Reaction Label G2WA            
             RKI( NCELL,  110) =   2.4000D-15 * CFACT 
!  Reaction Label G2S2            
             RKI( NCELL,  111) =   3.8000D-11 * CFACT 
!  Reaction Label G3N2            
             RKI( NCELL,  112) =   7.0000D-12 * CFACT 
!  Reaction Label G3WA            
             RKI( NCELL,  113) =   2.4000D-15 * CFACT 
!  Reaction Label G3S2            
             RKI( NCELL,  114) =   3.8000D-11 * CFACT 
!  Reaction Label S2OH            
             RKI( NCELL,  115) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 3.3000D-31,   0.0000D+00,  -4.3000D+00,  & 
     &                                                 1.6000D-12,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label C1OH            
             RKI( NCELL,  116) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.4500D-12,  -1.7750D+03 )
!  Reaction Label 119             
             RKI( NCELL,  119) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.5000D-12,   1.2500D+02 )
!  Reaction Label 120             
             RKI( NCELL,  120) =   5.8000D-16 * CFACT 
!  Reaction Label P1UI            
             RKI( NCELL,  121) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.0800D+00,  -1.4000D+04,  -5.6000D+00,  & 
     &                                                 1.0300D+17,  -1.4000D+04,  -1.5000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label 125             
             RKI( NCELL,  125) =   1.1500D-11 * CFACT 
!  Reaction Label 126             
             RKI( NCELL,  126) =   4.0000D-16 * CFACT 
!  Reaction Label BLOH            
             RKI( NCELL,  127) =   1.2000D-11 * CFACT 
!  Reaction Label BLN3            
             RKI( NCELL,  129) =   4.0000D-15 * CFACT 
!  Reaction Label PBUI            
             RKI( NCELL,  130) =  SFACT * ARRHENUIS_T03( INV_TEMP,  2.1000D+16,  -1.3600D+04 )
!  Reaction Label NPOH            
             RKI( NCELL,  132) =   3.5000D-12 * CFACT 
!  Reaction Label NAOH            
             RKI( NCELL,  134) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5500D-11,   1.1700D+02 )
!  Reaction Label CTOH            
             RKI( NCELL,  135) =   5.9700D-10 * CFACT 
!  Reaction Label CTN3            
             RKI( NCELL,  136) =   4.8600D-10 * CFACT 
!  Reaction Label PNOH            
             RKI( NCELL,  137) =   2.0000D-10 * CFACT 
!  Reaction Label PNN3            
             RKI( NCELL,  138) =   1.7000D-10 * CFACT 
!  Reaction Label 140             
             RKI( NCELL,  140) =   2.7800D-04 * SFACT 
!  Reaction Label 141             
             RKI( NCELL,  141) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5400D-12,   3.2500D+02 )
!  Reaction Label 142             
             RKI( NCELL,  142) =   4.1300D-12 * CFACT 
!  Reaction Label 143             
             RKI( NCELL,  143) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5000D-12,  -9.2000D+02 )
!  Reaction Label 144             
             RKI( NCELL,  144) =   4.0300D-13 * CFACT 
!  Reaction Label 145             
             RKI( NCELL,  145) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-12,  -8.6000D+02 )
!  Reaction Label 146             
             RKI( NCELL,  146) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.6900D-12,  -1.0560D+03 )
!  Reaction Label 147             
             RKI( NCELL,  147) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.6900D-12,  -1.0700D+03 )
!  Reaction Label 148             
             RKI( NCELL,  148) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6300D-12,  -8.5600D+02 )
!  Reaction Label 149             
             RKI( NCELL,  149) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-12,  -8.5000D+02 )
!  Reaction Label 150             
             RKI( NCELL,  150) =   RKI( NCELL,   47 ) 
!  Reaction Label 151             
             RKI( NCELL,  151) =   RKI( NCELL,   48 ) 
!  Reaction Label 152             
             RKI( NCELL,  152) =   RKI( NCELL,   49 ) 
!  Reaction Label 153             
             RKI( NCELL,  153) =   RKI( NCELL,   50 ) 
!  Reaction Label 154             
             RKI( NCELL,  154) =   RKI( NCELL,   55 ) 
!  Reaction Label 155             
             RKI( NCELL,  155) =   RKI( NCELL,   47 ) 
!  Reaction Label 156             
             RKI( NCELL,  156) =   RKI( NCELL,   48 ) 
!  Reaction Label 157             
             RKI( NCELL,  157) =   RKI( NCELL,   49 ) 
!  Reaction Label 158             
             RKI( NCELL,  158) =   RKI( NCELL,   50 ) 
!  Reaction Label 159             
             RKI( NCELL,  159) =   RKI( NCELL,   55 ) 
!  Reaction Label 160             
             RKI( NCELL,  160) =   RKI( NCELL,   47 ) 
!  Reaction Label 161             
             RKI( NCELL,  161) =   RKI( NCELL,   48 ) 
!  Reaction Label 162             
             RKI( NCELL,  162) =   RKI( NCELL,   49 ) 
!  Reaction Label 163             
             RKI( NCELL,  163) =   RKI( NCELL,   50 ) 
!  Reaction Label 164             
             RKI( NCELL,  164) =   RKI( NCELL,   55 ) 
!  Reaction Label 165             
             RKI( NCELL,  165) =   RKI( NCELL,   47 ) 
!  Reaction Label 166             
             RKI( NCELL,  166) =   RKI( NCELL,   48 ) 
!  Reaction Label 167             
             RKI( NCELL,  167) =   RKI( NCELL,   49 ) 
!  Reaction Label 168             
             RKI( NCELL,  168) =   RKI( NCELL,   50 ) 
!  Reaction Label 169             
             RKI( NCELL,  169) =   RKI( NCELL,   55 ) 
!  Reaction Label 170             
             RKI( NCELL,  170) =   RKI( NCELL,   47 ) 
!  Reaction Label 171             
             RKI( NCELL,  171) =   RKI( NCELL,   48 ) 
!  Reaction Label 172             
             RKI( NCELL,  172) =   RKI( NCELL,   49 ) 
!  Reaction Label 173             
             RKI( NCELL,  173) =   RKI( NCELL,   50 ) 
!  Reaction Label 174             
             RKI( NCELL,  174) =   RKI( NCELL,   55 ) 
!  Reaction Label 175             
             RKI( NCELL,  175) =   RKI( NCELL,   47 ) 
!  Reaction Label 176             
             RKI( NCELL,  176) =   RKI( NCELL,   48 ) 
!  Reaction Label 177             
             RKI( NCELL,  177) =   RKI( NCELL,   49 ) 
!  Reaction Label 178             
             RKI( NCELL,  178) =   RKI( NCELL,   50 ) 
!  Reaction Label 179             
             RKI( NCELL,  179) =   RKI( NCELL,   55 ) 
!  Reaction Label 180             
             RKI( NCELL,  180) =   RKI( NCELL,   47 ) 
!  Reaction Label 181             
             RKI( NCELL,  181) =   RKI( NCELL,   48 ) 
!  Reaction Label 182             
             RKI( NCELL,  182) =   RKI( NCELL,   49 ) 
!  Reaction Label 183             
             RKI( NCELL,  183) =   RKI( NCELL,   50 ) 
!  Reaction Label 184             
             RKI( NCELL,  184) =   RKI( NCELL,   55 ) 
!  Reaction Label 185             
             RKI( NCELL,  185) =   RKI( NCELL,   47 ) 
!  Reaction Label 186             
             RKI( NCELL,  186) =   RKI( NCELL,   48 ) 
!  Reaction Label 187             
             RKI( NCELL,  187) =   RKI( NCELL,   49 ) 
!  Reaction Label 188             
             RKI( NCELL,  188) =   RKI( NCELL,   50 ) 
!  Reaction Label 189             
             RKI( NCELL,  189) =   RKI( NCELL,   55 ) 
!  Reaction Label 190             
             RKI( NCELL,  190) =   RKI( NCELL,   47 ) 
!  Reaction Label 191             
             RKI( NCELL,  191) =   RKI( NCELL,   48 ) 
!  Reaction Label 192             
             RKI( NCELL,  192) =   RKI( NCELL,   49 ) 
!  Reaction Label 193             
             RKI( NCELL,  193) =   RKI( NCELL,   50 ) 
!  Reaction Label 194             
             RKI( NCELL,  194) =   RKI( NCELL,   55 ) 
!  Reaction Label 195             
             RKI( NCELL,  195) =   RKI( NCELL,   47 ) 
!  Reaction Label 196             
             RKI( NCELL,  196) =   RKI( NCELL,   48 ) 
!  Reaction Label 197             
             RKI( NCELL,  197) =   RKI( NCELL,   49 ) 
!  Reaction Label 198             
             RKI( NCELL,  198) =   RKI( NCELL,   50 ) 
!  Reaction Label 199             
             RKI( NCELL,  199) =   RKI( NCELL,   55 ) 
!  Reaction Label 200             
             RKI( NCELL,  200) =   RKI( NCELL,   47 ) 
!  Reaction Label 201             
             RKI( NCELL,  201) =   RKI( NCELL,   48 ) 
!  Reaction Label 202             
             RKI( NCELL,  202) =   RKI( NCELL,   49 ) 
!  Reaction Label 203             
             RKI( NCELL,  203) =   RKI( NCELL,   50 ) 
!  Reaction Label 204             
             RKI( NCELL,  204) =   RKI( NCELL,   55 ) 
!  Reaction Label 205             
             RKI( NCELL,  205) =   RKI( NCELL,   47 ) 
!  Reaction Label 206             
             RKI( NCELL,  206) =   RKI( NCELL,   48 ) 
!  Reaction Label 207             
             RKI( NCELL,  207) =   RKI( NCELL,   49 ) 
!  Reaction Label 208             
             RKI( NCELL,  208) =   RKI( NCELL,   50 ) 
!  Reaction Label 209             
             RKI( NCELL,  209) =   RKI( NCELL,   55 ) 
!  Reaction Label 210             
             RKI( NCELL,  210) =   RKI( NCELL,   47 ) 
!  Reaction Label 211             
             RKI( NCELL,  211) =   RKI( NCELL,   48 ) 
!  Reaction Label 212             
             RKI( NCELL,  212) =   RKI( NCELL,   49 ) 
!  Reaction Label 213             
             RKI( NCELL,  213) =   RKI( NCELL,   50 ) 
!  Reaction Label 214             
             RKI( NCELL,  214) =   RKI( NCELL,   55 ) 
!  Reaction Label 215             
             RKI( NCELL,  215) =   RKI( NCELL,   47 ) 
!  Reaction Label 216             
             RKI( NCELL,  216) =   RKI( NCELL,   48 ) 
!  Reaction Label 217             
             RKI( NCELL,  217) =   RKI( NCELL,   49 ) 
!  Reaction Label 218             
             RKI( NCELL,  218) =   RKI( NCELL,   50 ) 
!  Reaction Label 219             
             RKI( NCELL,  219) =   RKI( NCELL,   55 ) 
!  Reaction Label 220             
             RKI( NCELL,  220) =   RKI( NCELL,   47 ) 
!  Reaction Label 221             
             RKI( NCELL,  221) =   RKI( NCELL,   48 ) 
!  Reaction Label 222             
             RKI( NCELL,  222) =   RKI( NCELL,   49 ) 
!  Reaction Label 223             
             RKI( NCELL,  223) =   RKI( NCELL,   50 ) 
!  Reaction Label 224             
             RKI( NCELL,  224) =   RKI( NCELL,   55 ) 
!  Reaction Label 225             
             RKI( NCELL,  225) =   RKI( NCELL,   47 ) 
!  Reaction Label 226             
             RKI( NCELL,  226) =   RKI( NCELL,   48 ) 
!  Reaction Label 227             
             RKI( NCELL,  227) =   RKI( NCELL,   49 ) 
!  Reaction Label 228             
             RKI( NCELL,  228) =   RKI( NCELL,   50 ) 
!  Reaction Label 229             
             RKI( NCELL,  229) =   RKI( NCELL,   55 ) 
!  Reaction Label 230             
             RKI( NCELL,  230) =   RKI( NCELL,   47 ) 
!  Reaction Label 231             
             RKI( NCELL,  231) =   RKI( NCELL,   48 ) 
!  Reaction Label 232             
             RKI( NCELL,  232) =   RKI( NCELL,   49 ) 
!  Reaction Label 233             
             RKI( NCELL,  233) =   RKI( NCELL,   50 ) 
!  Reaction Label 234             
             RKI( NCELL,  234) =   RKI( NCELL,   55 ) 
!  Reaction Label 235             
             RKI( NCELL,  235) =   RKI( NCELL,   47 ) 
!  Reaction Label 236             
             RKI( NCELL,  236) =   RKI( NCELL,   48 ) 
!  Reaction Label 237             
             RKI( NCELL,  237) =   RKI( NCELL,   49 ) 
!  Reaction Label 238             
             RKI( NCELL,  238) =   RKI( NCELL,   50 ) 
!  Reaction Label 239             
             RKI( NCELL,  239) =   RKI( NCELL,   55 ) 
!  Reaction Label 240             
             RKI( NCELL,  240) =   RKI( NCELL,   47 ) 
!  Reaction Label 241             
             RKI( NCELL,  241) =   RKI( NCELL,   48 ) 
!  Reaction Label 242             
             RKI( NCELL,  242) =   RKI( NCELL,   49 ) 
!  Reaction Label 243             
             RKI( NCELL,  243) =   RKI( NCELL,   50 ) 
!  Reaction Label 244             
             RKI( NCELL,  244) =   RKI( NCELL,   55 ) 
!  Reaction Label 245             
             RKI( NCELL,  245) =   RKI( NCELL,   47 ) 
!  Reaction Label 246             
             RKI( NCELL,  246) =   RKI( NCELL,   48 ) 
!  Reaction Label 247             
             RKI( NCELL,  247) =   RKI( NCELL,   49 ) 
!  Reaction Label 248             
             RKI( NCELL,  248) =   RKI( NCELL,   50 ) 
!  Reaction Label 249             
             RKI( NCELL,  249) =   RKI( NCELL,   55 ) 
!  Reaction Label 250             
             RKI( NCELL,  250) =   RKI( NCELL,   47 ) 
!  Reaction Label 251             
             RKI( NCELL,  251) =   RKI( NCELL,   48 ) 
!  Reaction Label 252             
             RKI( NCELL,  252) =   RKI( NCELL,   49 ) 
!  Reaction Label 253             
             RKI( NCELL,  253) =   RKI( NCELL,   50 ) 
!  Reaction Label 254             
             RKI( NCELL,  254) =   RKI( NCELL,   55 ) 
!  Reaction Label 255             
             RKI( NCELL,  255) =   RKI( NCELL,   47 ) 
!  Reaction Label 256             
             RKI( NCELL,  256) =   RKI( NCELL,   48 ) 
!  Reaction Label 257             
             RKI( NCELL,  257) =   RKI( NCELL,   49 ) 
!  Reaction Label 258             
             RKI( NCELL,  258) =   RKI( NCELL,   50 ) 
!  Reaction Label 259             
             RKI( NCELL,  259) =   RKI( NCELL,   55 ) 
!  Reaction Label 260             
             RKI( NCELL,  260) =   RKI( NCELL,   47 ) 
!  Reaction Label 261             
             RKI( NCELL,  261) =   RKI( NCELL,   48 ) 
!  Reaction Label 262             
             RKI( NCELL,  262) =   RKI( NCELL,   49 ) 
!  Reaction Label 263             
             RKI( NCELL,  263) =   RKI( NCELL,   50 ) 
!  Reaction Label 264             
             RKI( NCELL,  264) =   RKI( NCELL,   55 ) 
!  Reaction Label 265             
             RKI( NCELL,  265) =   RKI( NCELL,   47 ) 
!  Reaction Label 266             
             RKI( NCELL,  266) =   RKI( NCELL,   48 ) 
!  Reaction Label 267             
             RKI( NCELL,  267) =   RKI( NCELL,   49 ) 
!  Reaction Label 268             
             RKI( NCELL,  268) =   RKI( NCELL,   50 ) 
!  Reaction Label 269             
             RKI( NCELL,  269) =   RKI( NCELL,   55 ) 
!  Reaction Label 270             
             RKI( NCELL,  270) =   RKI( NCELL,   47 ) 
!  Reaction Label 271             
             RKI( NCELL,  271) =   RKI( NCELL,   48 ) 
!  Reaction Label 272             
             RKI( NCELL,  272) =   RKI( NCELL,   49 ) 
!  Reaction Label 273             
             RKI( NCELL,  273) =   RKI( NCELL,   50 ) 
!  Reaction Label 274             
             RKI( NCELL,  274) =   RKI( NCELL,   55 ) 
!  Reaction Label 275             
             RKI( NCELL,  275) =   RKI( NCELL,   47 ) 
!  Reaction Label 276             
             RKI( NCELL,  276) =   RKI( NCELL,   48 ) 
!  Reaction Label 277             
             RKI( NCELL,  277) =   RKI( NCELL,   49 ) 
!  Reaction Label 278             
             RKI( NCELL,  278) =   RKI( NCELL,   50 ) 
!  Reaction Label 279             
             RKI( NCELL,  279) =   RKI( NCELL,   55 ) 
!  Reaction Label 280             
             RKI( NCELL,  280) =   RKI( NCELL,   47 ) 
!  Reaction Label 281             
             RKI( NCELL,  281) =   RKI( NCELL,   48 ) 
!  Reaction Label 282             
             RKI( NCELL,  282) =   RKI( NCELL,   49 ) 
!  Reaction Label 283             
             RKI( NCELL,  283) =   RKI( NCELL,   50 ) 
!  Reaction Label 284             
             RKI( NCELL,  284) =   RKI( NCELL,   55 ) 
!  Reaction Label 285             
             RKI( NCELL,  285) =   RKI( NCELL,   47 ) 
!  Reaction Label 286             
             RKI( NCELL,  286) =   RKI( NCELL,   48 ) 
!  Reaction Label 287             
             RKI( NCELL,  287) =   RKI( NCELL,   49 ) 
!  Reaction Label 288             
             RKI( NCELL,  288) =   RKI( NCELL,   50 ) 
!  Reaction Label 289             
             RKI( NCELL,  289) =   RKI( NCELL,   55 ) 
!  Reaction Label 290             
             RKI( NCELL,  290) =   RKI( NCELL,   47 ) 
!  Reaction Label 291             
             RKI( NCELL,  291) =   RKI( NCELL,   48 ) 
!  Reaction Label 292             
             RKI( NCELL,  292) =   RKI( NCELL,   49 ) 
!  Reaction Label 293             
             RKI( NCELL,  293) =   RKI( NCELL,   50 ) 
!  Reaction Label 294             
             RKI( NCELL,  294) =   RKI( NCELL,   55 ) 
!  Reaction Label 295             
             RKI( NCELL,  295) =   RKI( NCELL,   47 ) 
!  Reaction Label 296             
             RKI( NCELL,  296) =   RKI( NCELL,   48 ) 
!  Reaction Label 297             
             RKI( NCELL,  297) =   RKI( NCELL,   49 ) 
!  Reaction Label 298             
             RKI( NCELL,  298) =   RKI( NCELL,   50 ) 
!  Reaction Label 299             
             RKI( NCELL,  299) =   RKI( NCELL,   55 ) 
!  Reaction Label 305             
             RKI( NCELL,  300) =   RKI( NCELL,   47 ) 
!  Reaction Label 306             
             RKI( NCELL,  301) =   RKI( NCELL,   48 ) 
!  Reaction Label 307             
             RKI( NCELL,  302) =   RKI( NCELL,   49 ) 
!  Reaction Label 308             
             RKI( NCELL,  303) =   RKI( NCELL,   50 ) 
!  Reaction Label 309             
             RKI( NCELL,  304) =   RKI( NCELL,   55 ) 
!  Reaction Label 310             
             RKI( NCELL,  305) =   RKI( NCELL,   47 ) 
!  Reaction Label 311             
             RKI( NCELL,  306) =   RKI( NCELL,   48 ) 
!  Reaction Label 312             
             RKI( NCELL,  307) =   RKI( NCELL,   49 ) 
!  Reaction Label 313             
             RKI( NCELL,  308) =   RKI( NCELL,   50 ) 
!  Reaction Label 314             
             RKI( NCELL,  309) =   RKI( NCELL,   55 ) 
!  Reaction Label 315             
             RKI( NCELL,  310) =   RKI( NCELL,   47 ) 
!  Reaction Label 316             
             RKI( NCELL,  311) =   RKI( NCELL,   48 ) 
!  Reaction Label 317             
             RKI( NCELL,  312) =   RKI( NCELL,   49 ) 
!  Reaction Label 318             
             RKI( NCELL,  313) =   RKI( NCELL,   50 ) 
!  Reaction Label 319             
             RKI( NCELL,  314) =   RKI( NCELL,   55 ) 
!  Reaction Label 320             
             RKI( NCELL,  315) =   RKI( NCELL,   47 ) 
!  Reaction Label 321             
             RKI( NCELL,  316) =   RKI( NCELL,   48 ) 
!  Reaction Label 322             
             RKI( NCELL,  317) =   RKI( NCELL,   49 ) 
!  Reaction Label 323             
             RKI( NCELL,  318) =   RKI( NCELL,   50 ) 
!  Reaction Label 324             
             RKI( NCELL,  319) =   RKI( NCELL,   55 ) 
!  Reaction Label 330             
             RKI( NCELL,  320) =   RKI( NCELL,   47 ) 
!  Reaction Label 331             
             RKI( NCELL,  321) =   RKI( NCELL,   48 ) 
!  Reaction Label 332             
             RKI( NCELL,  322) =   RKI( NCELL,   49 ) 
!  Reaction Label 333             
             RKI( NCELL,  323) =   RKI( NCELL,   50 ) 
!  Reaction Label 334             
             RKI( NCELL,  324) =   RKI( NCELL,   55 ) 
!  Reaction Label 335             
             RKI( NCELL,  325) =   RKI( NCELL,   47 ) 
!  Reaction Label 336             
             RKI( NCELL,  326) =   RKI( NCELL,   48 ) 
!  Reaction Label 337             
             RKI( NCELL,  327) =   RKI( NCELL,   49 ) 
!  Reaction Label 338             
             RKI( NCELL,  328) =   RKI( NCELL,   50 ) 
!  Reaction Label 339             
             RKI( NCELL,  329) =   RKI( NCELL,   55 ) 
!  Reaction Label 340             
             RKI( NCELL,  330) =   RKI( NCELL,   47 ) 
!  Reaction Label 341             
             RKI( NCELL,  331) =   RKI( NCELL,   48 ) 
!  Reaction Label 342             
             RKI( NCELL,  332) =   RKI( NCELL,   49 ) 
!  Reaction Label 343             
             RKI( NCELL,  333) =   RKI( NCELL,   50 ) 
!  Reaction Label 344             
             RKI( NCELL,  334) =   RKI( NCELL,   55 ) 
!  Reaction Label 345             
             RKI( NCELL,  335) =   RKI( NCELL,   47 ) 
!  Reaction Label 346             
             RKI( NCELL,  336) =   RKI( NCELL,   48 ) 
!  Reaction Label 347             
             RKI( NCELL,  337) =   RKI( NCELL,   49 ) 
!  Reaction Label 348             
             RKI( NCELL,  338) =   RKI( NCELL,   50 ) 
!  Reaction Label 349             
             RKI( NCELL,  339) =   RKI( NCELL,   55 ) 
!  Reaction Label 350             
             RKI( NCELL,  340) =   RKI( NCELL,   47 ) 
!  Reaction Label 351             
             RKI( NCELL,  341) =   RKI( NCELL,   48 ) 
!  Reaction Label 352             
             RKI( NCELL,  342) =   RKI( NCELL,   49 ) 
!  Reaction Label 353             
             RKI( NCELL,  343) =   RKI( NCELL,   50 ) 
!  Reaction Label 354             
             RKI( NCELL,  344) =   RKI( NCELL,   55 ) 
!  Reaction Label 355             
             RKI( NCELL,  345) =   RKI( NCELL,   47 ) 
!  Reaction Label 356             
             RKI( NCELL,  346) =   RKI( NCELL,   48 ) 
!  Reaction Label 357             
             RKI( NCELL,  347) =   RKI( NCELL,   49 ) 
!  Reaction Label 358             
             RKI( NCELL,  348) =   RKI( NCELL,   50 ) 
!  Reaction Label 359             
             RKI( NCELL,  349) =   RKI( NCELL,   55 ) 
!  Reaction Label 365             
             RKI( NCELL,  350) =   RKI( NCELL,   47 ) 
!  Reaction Label 366             
             RKI( NCELL,  351) =   RKI( NCELL,   48 ) 
!  Reaction Label 367             
             RKI( NCELL,  352) =   RKI( NCELL,   49 ) 
!  Reaction Label 368             
             RKI( NCELL,  353) =   RKI( NCELL,   50 ) 
!  Reaction Label 369             
             RKI( NCELL,  354) =   RKI( NCELL,   55 ) 
!  Reaction Label 370             
             RKI( NCELL,  355) =   RKI( NCELL,   47 ) 
!  Reaction Label 371             
             RKI( NCELL,  356) =   RKI( NCELL,   48 ) 
!  Reaction Label 372             
             RKI( NCELL,  357) =   RKI( NCELL,   49 ) 
!  Reaction Label 373             
             RKI( NCELL,  358) =   RKI( NCELL,   50 ) 
!  Reaction Label 374             
             RKI( NCELL,  359) =   RKI( NCELL,   55 ) 
!  Reaction Label 375             
             RKI( NCELL,  360) =   RKI( NCELL,   47 ) 
!  Reaction Label 376             
             RKI( NCELL,  361) =   RKI( NCELL,   48 ) 
!  Reaction Label 377             
             RKI( NCELL,  362) =   RKI( NCELL,   49 ) 
!  Reaction Label 378             
             RKI( NCELL,  363) =   RKI( NCELL,   50 ) 
!  Reaction Label 379             
             RKI( NCELL,  364) =   RKI( NCELL,   55 ) 
!  Reaction Label 380             
             RKI( NCELL,  365) =   RKI( NCELL,   47 ) 
!  Reaction Label 381             
             RKI( NCELL,  366) =   RKI( NCELL,   48 ) 
!  Reaction Label 382             
             RKI( NCELL,  367) =   RKI( NCELL,   49 ) 
!  Reaction Label 383             
             RKI( NCELL,  368) =   RKI( NCELL,   50 ) 
!  Reaction Label 384             
             RKI( NCELL,  369) =   RKI( NCELL,   55 ) 
!  Reaction Label 385             
             RKI( NCELL,  370) =   RKI( NCELL,   47 ) 
!  Reaction Label 386             
             RKI( NCELL,  371) =   RKI( NCELL,   48 ) 
!  Reaction Label 387             
             RKI( NCELL,  372) =   RKI( NCELL,   49 ) 
!  Reaction Label 388             
             RKI( NCELL,  373) =   RKI( NCELL,   50 ) 
!  Reaction Label 389             
             RKI( NCELL,  374) =   RKI( NCELL,   55 ) 
!  Reaction Label 390             
             RKI( NCELL,  375) =   RKI( NCELL,   47 ) 
!  Reaction Label 391             
             RKI( NCELL,  376) =   RKI( NCELL,   48 ) 
!  Reaction Label 392             
             RKI( NCELL,  377) =   RKI( NCELL,   49 ) 
!  Reaction Label 393             
             RKI( NCELL,  378) =   RKI( NCELL,   50 ) 
!  Reaction Label 394             
             RKI( NCELL,  379) =   RKI( NCELL,   55 ) 
!  Reaction Label 395             
             RKI( NCELL,  380) =   RKI( NCELL,   47 ) 
!  Reaction Label 396             
             RKI( NCELL,  381) =   RKI( NCELL,   48 ) 
!  Reaction Label 397             
             RKI( NCELL,  382) =   RKI( NCELL,   49 ) 
!  Reaction Label 398             
             RKI( NCELL,  383) =   RKI( NCELL,   50 ) 
!  Reaction Label 399             
             RKI( NCELL,  384) =   RKI( NCELL,   55 ) 
!  Reaction Label 400             
             RKI( NCELL,  385) =   RKI( NCELL,   47 ) 
!  Reaction Label 401             
             RKI( NCELL,  386) =   RKI( NCELL,   48 ) 
!  Reaction Label 402             
             RKI( NCELL,  387) =   RKI( NCELL,   49 ) 
!  Reaction Label 403             
             RKI( NCELL,  388) =   RKI( NCELL,   50 ) 
!  Reaction Label 404             
             RKI( NCELL,  389) =   RKI( NCELL,   55 ) 
!  Reaction Label 405             
             RKI( NCELL,  390) =   RKI( NCELL,   47 ) 
!  Reaction Label 406             
             RKI( NCELL,  391) =   RKI( NCELL,   48 ) 
!  Reaction Label 407             
             RKI( NCELL,  392) =   RKI( NCELL,   49 ) 
!  Reaction Label 408             
             RKI( NCELL,  393) =   RKI( NCELL,   50 ) 
!  Reaction Label 409             
             RKI( NCELL,  394) =   RKI( NCELL,   55 ) 
!  Reaction Label 410             
             RKI( NCELL,  395) =   RKI( NCELL,   47 ) 
!  Reaction Label 411             
             RKI( NCELL,  396) =   RKI( NCELL,   48 ) 
!  Reaction Label 412             
             RKI( NCELL,  397) =   RKI( NCELL,   49 ) 
!  Reaction Label 413             
             RKI( NCELL,  398) =   RKI( NCELL,   50 ) 
!  Reaction Label 414             
             RKI( NCELL,  399) =   RKI( NCELL,   55 ) 
!  Reaction Label 415             
             RKI( NCELL,  400) =   RKI( NCELL,   47 ) 
!  Reaction Label 416             
             RKI( NCELL,  401) =   RKI( NCELL,   48 ) 
!  Reaction Label 417             
             RKI( NCELL,  402) =   RKI( NCELL,   49 ) 
!  Reaction Label 418             
             RKI( NCELL,  403) =   RKI( NCELL,   50 ) 
!  Reaction Label 419             
             RKI( NCELL,  404) =   RKI( NCELL,   55 ) 
!  Reaction Label 420             
             RKI( NCELL,  405) =   RKI( NCELL,   47 ) 
!  Reaction Label 421             
             RKI( NCELL,  406) =   RKI( NCELL,   48 ) 
!  Reaction Label 422             
             RKI( NCELL,  407) =   RKI( NCELL,   49 ) 
!  Reaction Label 423             
             RKI( NCELL,  408) =   RKI( NCELL,   50 ) 
!  Reaction Label 424             
             RKI( NCELL,  409) =   RKI( NCELL,   55 ) 
!  Reaction Label 425             
             RKI( NCELL,  410) =   RKI( NCELL,   47 ) 
!  Reaction Label 426             
             RKI( NCELL,  411) =   RKI( NCELL,   48 ) 
!  Reaction Label 427             
             RKI( NCELL,  412) =   RKI( NCELL,   49 ) 
!  Reaction Label 428             
             RKI( NCELL,  413) =   RKI( NCELL,   50 ) 
!  Reaction Label 429             
             RKI( NCELL,  414) =   RKI( NCELL,   55 ) 
!  Reaction Label 430             
             RKI( NCELL,  415) =   RKI( NCELL,   47 ) 
!  Reaction Label 431             
             RKI( NCELL,  416) =   RKI( NCELL,   48 ) 
!  Reaction Label 432             
             RKI( NCELL,  417) =   RKI( NCELL,   49 ) 
!  Reaction Label 433             
             RKI( NCELL,  418) =   RKI( NCELL,   50 ) 
!  Reaction Label 434             
             RKI( NCELL,  419) =   RKI( NCELL,   55 ) 
!  Reaction Label 435             
             RKI( NCELL,  420) =   RKI( NCELL,   47 ) 
!  Reaction Label 436             
             RKI( NCELL,  421) =   RKI( NCELL,   48 ) 
!  Reaction Label 437             
             RKI( NCELL,  422) =   RKI( NCELL,   49 ) 
!  Reaction Label 438             
             RKI( NCELL,  423) =   RKI( NCELL,   50 ) 
!  Reaction Label 439             
             RKI( NCELL,  424) =   RKI( NCELL,   55 ) 
!  Reaction Label 440             
             RKI( NCELL,  425) =   RKI( NCELL,   47 ) 
!  Reaction Label 441             
             RKI( NCELL,  426) =   RKI( NCELL,   48 ) 
!  Reaction Label 442             
             RKI( NCELL,  427) =   RKI( NCELL,   49 ) 
!  Reaction Label 443             
             RKI( NCELL,  428) =   RKI( NCELL,   50 ) 
!  Reaction Label 444             
             RKI( NCELL,  429) =   RKI( NCELL,   55 ) 
!  Reaction Label 445             
             RKI( NCELL,  430) =   RKI( NCELL,   47 ) 
!  Reaction Label 446             
             RKI( NCELL,  431) =   RKI( NCELL,   48 ) 
!  Reaction Label 447             
             RKI( NCELL,  432) =   RKI( NCELL,   49 ) 
!  Reaction Label 448             
             RKI( NCELL,  433) =   RKI( NCELL,   50 ) 
!  Reaction Label 449             
             RKI( NCELL,  434) =   RKI( NCELL,   55 ) 
!  Reaction Label 450             
             RKI( NCELL,  435) =   RKI( NCELL,   47 ) 
!  Reaction Label 451             
             RKI( NCELL,  436) =   RKI( NCELL,   48 ) 
!  Reaction Label 452             
             RKI( NCELL,  437) =   RKI( NCELL,   49 ) 
!  Reaction Label 453             
             RKI( NCELL,  438) =   RKI( NCELL,   50 ) 
!  Reaction Label 454             
             RKI( NCELL,  439) =   RKI( NCELL,   55 ) 
!  Reaction Label 455             
             RKI( NCELL,  440) =   RKI( NCELL,   47 ) 
!  Reaction Label 456             
             RKI( NCELL,  441) =   RKI( NCELL,   48 ) 
!  Reaction Label 457             
             RKI( NCELL,  442) =   RKI( NCELL,   49 ) 
!  Reaction Label 458             
             RKI( NCELL,  443) =   RKI( NCELL,   50 ) 
!  Reaction Label 459             
             RKI( NCELL,  444) =   RKI( NCELL,   55 ) 
!  Reaction Label 460             
             RKI( NCELL,  445) =   RKI( NCELL,   47 ) 
!  Reaction Label 461             
             RKI( NCELL,  446) =   RKI( NCELL,   48 ) 
!  Reaction Label 462             
             RKI( NCELL,  447) =   RKI( NCELL,   49 ) 
!  Reaction Label 463             
             RKI( NCELL,  448) =   RKI( NCELL,   50 ) 
!  Reaction Label 464             
             RKI( NCELL,  449) =   RKI( NCELL,   55 ) 
!  Reaction Label 465             
             RKI( NCELL,  450) =   RKI( NCELL,   47 ) 
!  Reaction Label 466             
             RKI( NCELL,  451) =   RKI( NCELL,   48 ) 
!  Reaction Label 467             
             RKI( NCELL,  452) =   RKI( NCELL,   49 ) 
!  Reaction Label 468             
             RKI( NCELL,  453) =   RKI( NCELL,   50 ) 
!  Reaction Label 469             
             RKI( NCELL,  454) =   RKI( NCELL,   55 ) 
!  Reaction Label Q2NO            
             RKI( NCELL,  455) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.7000D-12,   3.4000D+02 )
!  Reaction Label Q2N2            
             RKI( NCELL,  456) =   7.7000D-12 * CFACT 
!  Reaction Label Q2N3            
             RKI( NCELL,  457) =   4.0000D-12 * CFACT 
!  Reaction Label Q2H2            
             RKI( NCELL,  458) =   2.2000D-11 * CFACT 
!  Reaction Label Q2R2            
             RKI( NCELL,  459) =   1.6000D-11 * CFACT 
!  Reaction Label Q2R3            
             RKI( NCELL,  460) =   1.4000D-11 * CFACT 
!  Reaction Label Q5UI            
             RKI( NCELL,  461) =  SFACT * ARRHENUIS_T03( INV_TEMP,  7.7900D+08,  -5.0030D+03 )
!  Reaction Label Q5NO            
             RKI( NCELL,  462) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.7000D-12,   3.4000D+02 )
!  Reaction Label Q5N2            
             RKI( NCELL,  463) =   7.7000D-12 * CFACT 
!  Reaction Label Q5N3            
             RKI( NCELL,  464) =   4.0000D-12 * CFACT 
!  Reaction Label Q5H2            
             RKI( NCELL,  465) =   2.2000D-11 * CFACT 
!  Reaction Label Q5R2            
             RKI( NCELL,  466) =   1.6000D-11 * CFACT 
!  Reaction Label Q5R3            
             RKI( NCELL,  467) =   1.4000D-11 * CFACT 
!  Reaction Label C2OH            
             RKI( NCELL,  468) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.5100D-12,  -5.3300D+02,   1.9200D+00 )
!  Reaction Label C3OH            
             RKI( NCELL,  469) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   2.0000D-12,  -1.7200D+02,   1.7600D+00 )
!  Reaction Label C4OH            
             RKI( NCELL,  470) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   2.0900D-12,   4.2000D+01,   1.8200D+00 )
!  Reaction Label E1OH            
             RKI( NCELL,  471) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.1000D-28,   0.0000D+00,  -3.5000D+00,  & 
     &                                                 8.4000D-12,   0.0000D+00,  -1.7500D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label E1O3            
             RKI( NCELL,  472) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.8200D-15,  -2.5000D+03 )
!  Reaction Label E1N3            
             RKI( NCELL,  473) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.3000D-12,  -2.8800D+03 )
!  Reaction Label E1OP            
             RKI( NCELL,  474) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0700D-11,  -8.0000D+02 )
!  Reaction Label 495             
             RKI( NCELL,  475) =   2.5700D+00 * SFACT 
!  Reaction Label 496             
             RKI( NCELL,  476) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label E2OH            
             RKI( NCELL,  477) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.2000D-11,   2.1000D+02,  -6.2000D-01 )
!  Reaction Label E2O3            
             RKI( NCELL,  478) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.7700D-15,  -1.8800D+03 )
!  Reaction Label 499             
             RKI( NCELL,  479) =   2.4500D+00 * SFACT 
!  Reaction Label 500             
             RKI( NCELL,  480) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label E2N3            
             RKI( NCELL,  481) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.6000D-13,  -1.1550D+03 )
!  Reaction Label E2OP            
             RKI( NCELL,  482) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0200D-11,  -2.8000D+02 )
!  Reaction Label IPOH            
             RKI( NCELL,  483) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-11,   3.9000D+02 )
!  Reaction Label 504             
             RKI( NCELL,  484) =   1.2900D+00 * SFACT 
!  Reaction Label 505             
             RKI( NCELL,  485) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label IPO3            
             RKI( NCELL,  486) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0500D-14,  -2.0000D+03 )
!  Reaction Label 507             
             RKI( NCELL,  487) =   2.8100D+00 * SFACT 
!  Reaction Label 508             
             RKI( NCELL,  488) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label IPN3            
             RKI( NCELL,  489) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9500D-12,  -4.5000D+02 )
!  Reaction Label 510             
             RKI( NCELL,  490) =   1.0300D+00 * SFACT 
!  Reaction Label 511             
             RKI( NCELL,  491) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label IPOP            
             RKI( NCELL,  492) =   3.5000D-11 * CFACT 
!  Reaction Label E3OH            
             RKI( NCELL,  493) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1200D-11,   5.3000D+02 )
!  Reaction Label 514             
             RKI( NCELL,  494) =   1.0700D+00 * SFACT 
!  Reaction Label 515             
             RKI( NCELL,  495) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label E3O3            
             RKI( NCELL,  496) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3400D-14,  -2.2830D+03 )
!  Reaction Label 517             
             RKI( NCELL,  497) =   2.5500D+00 * SFACT 
!  Reaction Label 518             
             RKI( NCELL,  498) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label E3N3            
             RKI( NCELL,  499) =   1.1000D-13 * CFACT 
!  Reaction Label E3OP            
             RKI( NCELL,  500) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2600D-11,  -4.0000D+01 )
!  Reaction Label APOH            
             RKI( NCELL,  501) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3400D-11,   4.1000D+02 )
!  Reaction Label 522             
             RKI( NCELL,  502) =   7.4900D+00 * SFACT 
!  Reaction Label 523             
             RKI( NCELL,  503) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label APO3            
             RKI( NCELL,  504) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.2200D-16,  -6.4000D+02 )
!  Reaction Label APN3            
             RKI( NCELL,  505) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2000D-12,   4.9000D+02 )
!  Reaction Label APOP            
             RKI( NCELL,  506) =   3.2000D-11 * CFACT 
!  Reaction Label BPOH            
             RKI( NCELL,  507) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6200D-11,   4.6000D+02 )
!  Reaction Label 528             
             RKI( NCELL,  508) =   3.4500D+00 * SFACT 
!  Reaction Label 529             
             RKI( NCELL,  509) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label BPO3            
             RKI( NCELL,  510) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3900D-15,  -1.2800D+03 )
!  Reaction Label BPN3            
             RKI( NCELL,  511) =   2.5000D-12 * CFACT 
!  Reaction Label BPOP            
             RKI( NCELL,  512) =   2.7000D-11 * CFACT 
!  Reaction Label ACOH            
             RKI( NCELL,  513) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 5.5000D-30,   0.0000D+00,   0.0000D+00,  & 
     &                                                 8.3000D-13,   0.0000D+00,   2.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label ACO3            
             RKI( NCELL,  514) =   1.0000D-20 * CFACT 
!  Reaction Label BZOH            
             RKI( NCELL,  515) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3000D-12,  -1.9000D+02 )
!  Reaction Label TLOH            
             RKI( NCELL,  516) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-12,   3.4000D+02 )
!  Reaction Label OXOH            
             RKI( NCELL,  517) =   1.3600D-11 * CFACT 
!  Reaction Label MXOH            
             RKI( NCELL,  518) =   2.3100D-11 * CFACT 
!  Reaction Label PXOH            
             RKI( NCELL,  519) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.1400D-12,   3.1900D+02 )
!  Reaction Label X1OH            
             RKI( NCELL,  520) =   3.2700D-11 * CFACT 
!  Reaction Label X2OH            
             RKI( NCELL,  521) =   3.2500D-11 * CFACT 
!  Reaction Label X3OH            
             RKI( NCELL,  522) =   5.8600D-11 * CFACT 
!  Reaction Label EBOH            
             RKI( NCELL,  523) =   7.0000D-12 * CFACT 
!  Reaction Label MTOH            
             RKI( NCELL,  524) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.8700D-13,   8.4300D+02,   3.3400D+00 )
!  Reaction Label MLOH            
             RKI( NCELL,  525) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   2.3200D-13,   4.0200D+02,   2.7200D+00 )
!  Reaction Label FAOH            
             RKI( NCELL,  526) =   4.5000D-13 * CFACT 
!  Reaction Label H1OH            
             RKI( NCELL,  527) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.3000D-12,   1.9000D+02 )
!  Reaction Label A2OH            
             RKI( NCELL,  529) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   2.4000D-12,   5.4600D+02,   7.7000D-01 )
!  Reaction Label 550             
             RKI( NCELL,  530) =   3.7500D+00 * SFACT 
!  Reaction Label 551             
             RKI( NCELL,  531) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label A2N3            
             RKI( NCELL,  532) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label EAOH            
             RKI( NCELL,  534) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   4.4200D-13,   6.0600D+02,   2.2900D+00 )
!  Reaction Label GAOH            
             RKI( NCELL,  535) =   1.1000D-11 * CFACT 
!  Reaction Label GAN3            
             RKI( NCELL,  536) =   1.8400D-14 * CFACT 
!  Reaction Label 558             
             RKI( NCELL,  538) =   2.6200D+00 * SFACT 
!  Reaction Label 559             
             RKI( NCELL,  539) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label A3OH            
             RKI( NCELL,  540) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   6.6300D-13,   1.0180D+03,   1.9900D+00 )
!  Reaction Label A3N3            
             RKI( NCELL,  541) =   6.3000D-15 * CFACT 
!  Reaction Label AROH            
             RKI( NCELL,  543) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.1000D-12,   3.3300D+02 )
!  Reaction Label 564             
             RKI( NCELL,  544) =   1.6900D+00 * SFACT 
!  Reaction Label 565             
             RKI( NCELL,  545) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label ARO3            
             RKI( NCELL,  546) =   2.8000D-19 * CFACT 
!  Reaction Label ARN3            
             RKI( NCELL,  547) =   1.1000D-15 * CFACT 
!  Reaction Label 569             
             RKI( NCELL,  549) =   2.4000D+00 * SFACT 
!  Reaction Label 570             
             RKI( NCELL,  550) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label K3OH            
             RKI( NCELL,  551) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.9700D-14,   6.7800D+02,   3.8800D+00 )
!  Reaction Label K4OH            
             RKI( NCELL,  553) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   5.4200D-14,   8.8900D+02,   3.5700D+00 )
!  Reaction Label MAOH            
             RKI( NCELL,  555) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.0000D-12,   3.8000D+02 )
!  Reaction Label 576             
             RKI( NCELL,  556) =   5.2600D-01 * SFACT 
!  Reaction Label 577             
             RKI( NCELL,  557) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label MAO3            
             RKI( NCELL,  558) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-15,  -2.1000D+03 )
!  Reaction Label MAN3            
             RKI( NCELL,  559) =   3.4000D-15 * CFACT 
!  Reaction Label 580             
             RKI( NCELL,  560) =   5.2400D-01 * SFACT 
!  Reaction Label 581             
             RKI( NCELL,  561) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label MVOH            
             RKI( NCELL,  563) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   6.1000D+02 )
!  Reaction Label MVO3            
             RKI( NCELL,  564) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5000D-16,  -1.5200D+03 )
!  Reaction Label F1OH            
             RKI( NCELL,  566) =   5.2900D-11 * CFACT 
!  Reaction Label 587             
             RKI( NCELL,  567) =   2.0200D+01 * SFACT 
!  Reaction Label 588             
             RKI( NCELL,  568) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label PHOH            
             RKI( NCELL,  570) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.7000D-13,   1.2200D+03 )
!  Reaction Label PHN3            
             RKI( NCELL,  571) =   4.5000D-12 * CFACT 
!  Reaction Label L1OH            
             RKI( NCELL,  572) =   3.3500D-13 * CFACT 
!  Reaction Label L2OH            
             RKI( NCELL,  573) =   1.6700D-12 * CFACT 
!  Reaction Label L3OH            
             RKI( NCELL,  574) =   2.8500D-12 * CFACT 
!  Reaction Label L4OH            
             RKI( NCELL,  575) =   4.5400D-12 * CFACT 
!  Reaction Label L5OH            
             RKI( NCELL,  576) =   1.1400D-11 * CFACT 
!  Reaction Label 597             
             RKI( NCELL,  577) =   2.8700D-01 * SFACT 
!  Reaction Label 598             
             RKI( NCELL,  578) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label L6OH            
             RKI( NCELL,  579) =   1.6300D-11 * CFACT 
!  Reaction Label 600             
             RKI( NCELL,  580) =   2.7000D-01 * SFACT 
!  Reaction Label 601             
             RKI( NCELL,  581) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label O1OH            
             RKI( NCELL,  582) =   3.1800D-11 * CFACT 
!  Reaction Label O1O3            
             RKI( NCELL,  583) =   8.7000D-18 * CFACT 
!  Reaction Label O1N3            
             RKI( NCELL,  584) =   1.4400D-14 * CFACT 
!  Reaction Label O1OP            
             RKI( NCELL,  585) =   4.4300D-12 * CFACT 
!  Reaction Label O2OH            
             RKI( NCELL,  586) =   6.2900D-11 * CFACT 
!  Reaction Label O2O3            
             RKI( NCELL,  587) =   1.9000D-16 * CFACT 
!  Reaction Label 608             
             RKI( NCELL,  588) =   2.4500D+00 * SFACT 
!  Reaction Label 609             
             RKI( NCELL,  589) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label O2N3            
             RKI( NCELL,  590) =   4.3400D-13 * CFACT 
!  Reaction Label O2OP            
             RKI( NCELL,  591) =   1.9500D-11 * CFACT 
!  Reaction Label O3OH            
             RKI( NCELL,  592) =   5.2600D-11 * CFACT 
!  Reaction Label O3O3            
             RKI( NCELL,  593) =   1.1800D-17 * CFACT 
!  Reaction Label O3N3            
             RKI( NCELL,  594) =   3.6200D-13 * CFACT 
!  Reaction Label O3OP            
             RKI( NCELL,  595) =   1.7000D-11 * CFACT 
!  Reaction Label O4OH            
             RKI( NCELL,  596) =   8.7100D-11 * CFACT 
!  Reaction Label O4O3            
             RKI( NCELL,  597) =   4.0500D-16 * CFACT 
!  Reaction Label 618             
             RKI( NCELL,  598) =   2.4500D+00 * SFACT 
!  Reaction Label 619             
             RKI( NCELL,  599) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label O4N3            
             RKI( NCELL,  600) =   9.3100D-12 * CFACT 
!  Reaction Label O4OP            
             RKI( NCELL,  601) =   5.1100D-11 * CFACT 
!  Reaction Label TPOH            
             RKI( NCELL,  602) =   1.1000D-10 * CFACT 
!  Reaction Label 623             
             RKI( NCELL,  603) =   1.4800D+00 * SFACT 
!  Reaction Label 624             
             RKI( NCELL,  604) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label TPO3            
             RKI( NCELL,  605) =   1.1700D-16 * CFACT 
!  Reaction Label 626             
             RKI( NCELL,  606) =   1.0700D+00 * SFACT 
!  Reaction Label 627             
             RKI( NCELL,  607) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label TPN3            
             RKI( NCELL,  608) =   1.1100D-11 * CFACT 
!  Reaction Label 629             
             RKI( NCELL,  609) =   1.2700D+00 * SFACT 
!  Reaction Label 630             
             RKI( NCELL,  610) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label TPOP            
             RKI( NCELL,  611) =   4.2400D-11 * CFACT 
!  Reaction Label SQOH            
             RKI( NCELL,  612) =   2.0000D-10 * CFACT 
!  Reaction Label 633             
             RKI( NCELL,  613) =   5.5800D+00 * SFACT 
!  Reaction Label 634             
             RKI( NCELL,  614) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label SQO3            
             RKI( NCELL,  615) =   3.1400D-16 * CFACT 
!  Reaction Label 636             
             RKI( NCELL,  616) =   4.3200D+00 * SFACT 
!  Reaction Label 637             
             RKI( NCELL,  617) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label SQN3            
             RKI( NCELL,  618) =   1.9000D-11 * CFACT 
!  Reaction Label 639             
             RKI( NCELL,  619) =   2.5300D+00 * SFACT 
!  Reaction Label 640             
             RKI( NCELL,  620) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label SQOP            
             RKI( NCELL,  621) =   6.8500D-11 * CFACT 
!  Reaction Label BXOH            
             RKI( NCELL,  622) =   1.2100D-12 * CFACT 
!  Reaction Label B1OH            
             RKI( NCELL,  623) =   7.6900D-12 * CFACT 
!  Reaction Label B2OH            
             RKI( NCELL,  624) =   2.1300D-11 * CFACT 
!  Reaction Label 645             
             RKI( NCELL,  625) =   1.7600D-01 * SFACT 
!  Reaction Label 646             
             RKI( NCELL,  626) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label FUOH            
             RKI( NCELL,  627) =   3.8400D-11 * CFACT 
!  Reaction Label FUO3            
             RKI( NCELL,  628) =   2.4000D-18 * CFACT 
!  Reaction Label FUN3            
             RKI( NCELL,  629) =   1.2000D-12 * CFACT 
!  Reaction Label STOH            
             RKI( NCELL,  630) =   5.8000D-11 * CFACT 
!  Reaction Label STO3            
             RKI( NCELL,  631) =   1.6000D-17 * CFACT 
!  Reaction Label AMOH            
             RKI( NCELL,  632) =   4.3500D-11 * CFACT 
!  Reaction Label AMO3            
             RKI( NCELL,  633) =   3.0900D-18 * CFACT 
!  Reaction Label TAOH            
             RKI( NCELL,  634) =   1.0100D-11 * CFACT 
!  Reaction Label A4OH            
             RKI( NCELL,  635) =   3.2900D-11 * CFACT 
!  Reaction Label A4N3            
             RKI( NCELL,  636) =   2.2300D-14 * CFACT 
!  Reaction Label A5OH            
             RKI( NCELL,  638) =   5.0600D-11 * CFACT 
!  Reaction Label 659             
             RKI( NCELL,  639) =   7.5200D-01 * SFACT 
!  Reaction Label 660             
             RKI( NCELL,  640) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label A5O3            
             RKI( NCELL,  641) =   3.5000D-18 * CFACT 
!  Reaction Label A5N3            
             RKI( NCELL,  642) =   9.6400D-14 * CFACT 
!  Reaction Label 663             
             RKI( NCELL,  643) =   5.5100D+01 * SFACT 
!  Reaction Label 664             
             RKI( NCELL,  644) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label A6OH            
             RKI( NCELL,  646) =   8.7800D-11 * CFACT 
!  Reaction Label 667             
             RKI( NCELL,  647) =   4.1400D+00 * SFACT 
!  Reaction Label 668             
             RKI( NCELL,  648) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label A6O3            
             RKI( NCELL,  649) =   1.6500D-17 * CFACT 
!  Reaction Label 670             
             RKI( NCELL,  650) =   1.5300D+00 * SFACT 
!  Reaction Label 671             
             RKI( NCELL,  651) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label A6N3            
             RKI( NCELL,  652) =   1.1900D-12 * CFACT 
!  Reaction Label 673             
             RKI( NCELL,  653) =   3.1700D+01 * SFACT 
!  Reaction Label 674             
             RKI( NCELL,  654) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label 676             
             RKI( NCELL,  656) =   2.2200D+00 * SFACT 
!  Reaction Label 677             
             RKI( NCELL,  657) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label K5OH            
             RKI( NCELL,  658) =   9.5600D-12 * CFACT 
!  Reaction Label K6OH            
             RKI( NCELL,  660) =   6.0900D-11 * CFACT 
!  Reaction Label 681             
             RKI( NCELL,  661) =   4.6800D-01 * SFACT 
!  Reaction Label 682             
             RKI( NCELL,  662) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label K6O3            
             RKI( NCELL,  663) =   3.0400D-17 * CFACT 
!  Reaction Label 684             
             RKI( NCELL,  664) =   5.1300D+00 * SFACT 
!  Reaction Label 685             
             RKI( NCELL,  665) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label O5OH            
             RKI( NCELL,  667) =   8.3400D-11 * CFACT 
!  Reaction Label 688             
             RKI( NCELL,  668) =   2.3000D+00 * SFACT 
!  Reaction Label 689             
             RKI( NCELL,  669) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label O5O3            
             RKI( NCELL,  670) =   1.5000D-16 * CFACT 
!  Reaction Label 691             
             RKI( NCELL,  671) =   9.3600D-01 * SFACT 
!  Reaction Label 692             
             RKI( NCELL,  672) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label O5N3            
             RKI( NCELL,  673) =   8.5000D-12 * CFACT 
!  Reaction Label OAOH            
             RKI( NCELL,  674) =   7.4700D-13 * CFACT 
!  Reaction Label PAOH            
             RKI( NCELL,  675) =   3.0000D-14 * CFACT 
!  Reaction Label MGOH            
             RKI( NCELL,  677) =   1.1900D-11 * CFACT 
!  Reaction Label MGN3            
             RKI( NCELL,  678) =   5.0000D-16 * CFACT 
!  Reaction Label CROH            
             RKI( NCELL,  681) =   4.6500D-11 * CFACT 
!  Reaction Label CRN3            
             RKI( NCELL,  682) =   1.2700D-11 * CFACT 
!  Reaction Label XLOH            
             RKI( NCELL,  683) =   6.7300D-11 * CFACT 
!  Reaction Label XLN3            
             RKI( NCELL,  684) =   3.0900D-11 * CFACT 
!  Reaction Label CAOH            
             RKI( NCELL,  685) =   1.5600D-10 * CFACT 
!  Reaction Label CAN3            
             RKI( NCELL,  686) =   4.0400D-11 * CFACT 
!  Reaction Label N4OH            
             RKI( NCELL,  687) =   2.0600D-11 * CFACT 
!  Reaction Label 708             
             RKI( NCELL,  688) =   2.7600D+00 * SFACT 
!  Reaction Label 709             
             RKI( NCELL,  689) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label 711             
             RKI( NCELL,  691) =   3.1500D-01 * SFACT 
!  Reaction Label 712             
             RKI( NCELL,  692) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label N3OH            
             RKI( NCELL,  693) =   3.8000D-11 * CFACT 
!  Reaction Label N5OH            
             RKI( NCELL,  695) =   4.4900D-11 * CFACT 
!  Reaction Label N6OH            
             RKI( NCELL,  697) =   5.1900D-11 * CFACT 
!  Reaction Label 718             
             RKI( NCELL,  698) =   4.7700D-01 * SFACT 
!  Reaction Label 719             
             RKI( NCELL,  699) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label 721             
             RKI( NCELL,  701) =   1.1100D+00 * SFACT 
!  Reaction Label 722             
             RKI( NCELL,  702) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label NDOH            
             RKI( NCELL,  703) =   3.6000D-11 * CFACT 
!  Reaction Label 725             
             RKI( NCELL,  705) =   1.0900D+00 * SFACT 
!  Reaction Label 726             
             RKI( NCELL,  706) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label N1OH            
             RKI( NCELL,  707) =   1.4700D-12 * CFACT 
!  Reaction Label N2OH            
             RKI( NCELL,  709) =   2.5100D-11 * CFACT 
!  Reaction Label 731             
             RKI( NCELL,  711) =   9.6900D-01 * SFACT 
!  Reaction Label 732             
             RKI( NCELL,  712) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label H4OH            
             RKI( NCELL,  713) =   8.2700D-11 * CFACT 
!  Reaction Label H3OH            
             RKI( NCELL,  715) =   5.9700D-11 * CFACT 
!  Reaction Label 736             
             RKI( NCELL,  716) =   1.2800D-02 * SFACT 
!  Reaction Label 737             
             RKI( NCELL,  717) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label H5OH            
             RKI( NCELL,  719) =   5.4000D-11 * CFACT 
!  Reaction Label 740             
             RKI( NCELL,  720) =   6.3700D-01 * SFACT 
!  Reaction Label 741             
             RKI( NCELL,  721) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label H2OH            
             RKI( NCELL,  723) =   1.1600D-11 * CFACT 
!  Reaction Label 744             
             RKI( NCELL,  724) =   6.7800D-02 * SFACT 
!  Reaction Label 745             
             RKI( NCELL,  725) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label F2OH            
             RKI( NCELL,  727) =   3.3900D-11 * CFACT 
!  Reaction Label 748             
             RKI( NCELL,  728) =   1.3100D+01 * SFACT 
!  Reaction Label 749             
             RKI( NCELL,  729) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label 751             
             RKI( NCELL,  731) =   1.7500D+01 * SFACT 
!  Reaction Label 752             
             RKI( NCELL,  732) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label F3OH            
             RKI( NCELL,  733) =   5.9900D-11 * CFACT 
!  Reaction Label 754             
             RKI( NCELL,  734) =   2.3400D+01 * SFACT 
!  Reaction Label 755             
             RKI( NCELL,  735) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label F4OH            
             RKI( NCELL,  737) =   4.5800D-11 * CFACT 
!  Reaction Label 758             
             RKI( NCELL,  738) =   1.1600D+01 * SFACT 
!  Reaction Label 759             
             RKI( NCELL,  739) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label 761             
             RKI( NCELL,  741) =   8.6200D+00 * SFACT 
!  Reaction Label 762             
             RKI( NCELL,  742) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5500D-12,   3.8000D+02 )
!  Reaction Label F5OH            
             RKI( NCELL,  743) =   7.2000D-11 * CFACT 
!  Reaction Label P2UI            
             RKI( NCELL,  744) =   3.3900D-04 * SFACT 
!  Reaction Label P2OH            
             RKI( NCELL,  745) =   3.4200D-12 * CFACT 
!  Reaction Label P4UI            
             RKI( NCELL,  747) =   3.3900D-04 * SFACT 
!  Reaction Label P4OH            
             RKI( NCELL,  748) =   2.9000D-11 * CFACT 
!  Reaction Label P4O3            
             RKI( NCELL,  749) =   8.2000D-18 * CFACT 
!  Reaction Label P4N3            
             RKI( NCELL,  750) =   1.6000D-16 * CFACT 
!  Reaction Label AALK            
             RKI( NCELL,  752) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.7400D+02 )
!  Reaction Label AE51            
             RKI( NCELL,  753) =   RKI( NCELL,   47 ) 
!  Reaction Label AE52            
             RKI( NCELL,  754) =   RKI( NCELL,   48 ) 
!  Reaction Label AE53            
             RKI( NCELL,  755) =   RKI( NCELL,   47 ) 
!  Reaction Label AE54            
             RKI( NCELL,  756) =   RKI( NCELL,   48 ) 
!  Reaction Label AE55            
             RKI( NCELL,  757) =   RKI( NCELL,   47 ) 
!  Reaction Label AE56            
             RKI( NCELL,  758) =   RKI( NCELL,   48 ) 
!  Reaction Label AE57            
             RKI( NCELL,  759) =   RKI( NCELL,   47 ) 
!  Reaction Label AE58            
             RKI( NCELL,  760) =   RKI( NCELL,   48 ) 
!  Reaction Label HET_NO2         
             RKI( NCELL,  761) =  BLKHET(  NCELL, IK_HETERO_NO2 )
!  Reaction Label HET_N2O5IJ      
             RKI( NCELL,  762) =  BLKHET(  NCELL, IK_HETERO_N2O5IJ )
!  Reaction Label HET_N2O5K       
             RKI( NCELL,  763) =  BLKHET(  NCELL, IK_HETERO_N2O5K )
!  Reaction Label HET_H2NO3PIJA   
             RKI( NCELL,  764) =  BLKHET(  NCELL, IK_HETERO_H2NO3PAIJ )
!  Reaction Label HET_H2NO3PKA    
             RKI( NCELL,  765) =  BLKHET(  NCELL, IK_HETERO_H2NO3PAK )
!  Reaction Label HET_NO3         
             RKI( NCELL,  767) =  BLKHET(  NCELL, IK_HETERO_NO3 )
!  Reaction Label OLIG_ISOPRENE1  
             RKI( NCELL,  768) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ISOPRENE2  
             RKI( NCELL,  769) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_SESQT1     
             RKI( NCELL,  770) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_AROMATIC1  
             RKI( NCELL,  771) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_AROMATIC2  
             RKI( NCELL,  772) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_AROMATIC3  
             RKI( NCELL,  773) =   9.4882D-06 * SFACT 
!  Reaction Label RPOAGEPI        
             RKI( NCELL,  774) =   2.5000D-12 * CFACT 
!  Reaction Label RPOAGELI        
             RKI( NCELL,  775) =  BLKHET(  NCELL, IK_HETERO_PNCOMLI )
!  Reaction Label RPOAGEPJ        
             RKI( NCELL,  776) =   2.5000D-12 * CFACT 
!  Reaction Label RPOAGELJ        
             RKI( NCELL,  777) =  BLKHET(  NCELL, IK_HETERO_PNCOMLJ )
!  Reaction Label PCSOA           
             RKI( NCELL,  778) =   1.2500D-11 * CFACT 
!  Reaction Label POA_AGE1        
             RKI( NCELL,  779) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE2        
             RKI( NCELL,  780) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE3        
             RKI( NCELL,  781) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE4        
             RKI( NCELL,  782) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE5        
             RKI( NCELL,  783) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE6        
             RKI( NCELL,  784) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE7        
             RKI( NCELL,  785) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE8        
             RKI( NCELL,  786) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE9        
             RKI( NCELL,  787) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE10       
             RKI( NCELL,  788) =   4.0000D-11 * CFACT 
!  Reaction Label HET_GLY         
             RKI( NCELL,  789) =  BLKHET(  NCELL, IK_HETERO_GLY )
!  Reaction Label HET_MGLY        
             RKI( NCELL,  790) =  BLKHET(  NCELL, IK_HETERO_MGLY )
!  Reaction Label TR03            
             RKI( NCELL,  793) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.5000D-12,   1.2500D+02 )
!  Reaction Label TR05            
             RKI( NCELL,  794) =   5.8000D-16 * CFACT 
!  Reaction Label TR07            
             RKI( NCELL,  795) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   2.4000D-12,   5.4600D+02,   7.7000D-01 )
!  Reaction Label TR09            
             RKI( NCELL,  797) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label TR11            
             RKI( NCELL,  798) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.1000D-12,   3.3300D+02 )
!  Reaction Label TR12            
             RKI( NCELL,  799) =   2.8000D-19 * CFACT 
!  Reaction Label TR13            
             RKI( NCELL,  800) =   1.1000D-15 * CFACT 

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
! set species informations arrays using SPECIES_LIST array before mapping
               CHEMISTRY_SPC( I ) = SPECIES_LIST( I )%CHEMISTRY_SPC
               CGRID_INDEX  ( I ) = SPECIES_LIST( I )%CGRID_INDEX
               SPECIES_TYPE ( I ) = SPECIES_LIST( I )%SPECIES_TYPE
               CONVERT_CONC ( I ) = SPECIES_LIST( I )%CONVERT_CONC
               SPECIES_MOLWT( I ) = SPECIES_LIST( I )%SPECIES_MOLWT

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

            IF( SUCCESS )RETURN

            WRITE(LOGDEV,99901)TRIM( MECHNAME )
            XMSG = 'The FATAL errors found in namelist used. Check ' &
      &          //  'the log of exiting processor if more details are needed.'
            CALL M3WARN('MAP_CHEMISTRY_SPECIES',0,0,XMSG)


99901       FORMAT( / 'FATAL error(s) found in the namelists used. Check that ' &
     &     /  'these namelists contain the above data as the respective files ' &
     &     /  'in the respository version of the mechanism: ' , A )

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
          SUBROUTINE RESET_SPECIES_POINTERS( IOLD2NEW )

             USE RXNS_DATA
             IMPLICIT NONE
             INTEGER, INTENT( IN ) :: IOLD2NEW( :,: ) 


             INDEX_NO2          = IOLD2NEW( INDEX_NO2         , 1 )
             INDEX_NO           = IOLD2NEW( INDEX_NO          , 1 )
             INDEX_O3P          = IOLD2NEW( INDEX_O3P         , 1 )
             INDEX_O3           = IOLD2NEW( INDEX_O3          , 1 )
             INDEX_NO3          = IOLD2NEW( INDEX_NO3         , 1 )
             INDEX_N2O5         = IOLD2NEW( INDEX_N2O5        , 1 )
             INDEX_HNO3         = IOLD2NEW( INDEX_HNO3        , 1 )
             INDEX_O1D          = IOLD2NEW( INDEX_O1D         , 1 )
             INDEX_OH           = IOLD2NEW( INDEX_OH          , 1 )
             INDEX_HONO         = IOLD2NEW( INDEX_HONO        , 1 )
             INDEX_HO2          = IOLD2NEW( INDEX_HO2         , 1 )
             INDEX_HNO4         = IOLD2NEW( INDEX_HNO4        , 1 )
             INDEX_HO2H         = IOLD2NEW( INDEX_HO2H        , 1 )
             INDEX_CO           = IOLD2NEW( INDEX_CO          , 1 )
             INDEX_CO2          = IOLD2NEW( INDEX_CO2         , 1 )
             INDEX_SumRO2       = IOLD2NEW( INDEX_SumRO2      , 1 )
             INDEX_SumRCO3      = IOLD2NEW( INDEX_SumRCO3     , 1 )
             INDEX_RO2C         = IOLD2NEW( INDEX_RO2C        , 1 )
             INDEX_RO2XC        = IOLD2NEW( INDEX_RO2XC       , 1 )
             INDEX_MEO2         = IOLD2NEW( INDEX_MEO2        , 1 )
             INDEX_HCHO         = IOLD2NEW( INDEX_HCHO        , 1 )
             INDEX_MEOOH        = IOLD2NEW( INDEX_MEOOH       , 1 )
             INDEX_MEOH         = IOLD2NEW( INDEX_MEOH        , 1 )
             INDEX_ETO2         = IOLD2NEW( INDEX_ETO2        , 1 )
             INDEX_MECHO        = IOLD2NEW( INDEX_MECHO       , 1 )
             INDEX_ROOH         = IOLD2NEW( INDEX_ROOH        , 1 )
             INDEX_ETOH         = IOLD2NEW( INDEX_ETOH        , 1 )
             INDEX_BZO2         = IOLD2NEW( INDEX_BZO2        , 1 )
             INDEX_BZO          = IOLD2NEW( INDEX_BZO         , 1 )
             INDEX_MECO3        = IOLD2NEW( INDEX_MECO3       , 1 )
             INDEX_PAN          = IOLD2NEW( INDEX_PAN         , 1 )
             INDEX_OACID        = IOLD2NEW( INDEX_OACID       , 1 )
             INDEX_PACID        = IOLD2NEW( INDEX_PACID       , 1 )
             INDEX_BZCO3        = IOLD2NEW( INDEX_BZCO3       , 1 )
             INDEX_PBZN         = IOLD2NEW( INDEX_PBZN        , 1 )
             INDEX_ALK5         = IOLD2NEW( INDEX_ALK5        , 1 )
             INDEX_TBUO         = IOLD2NEW( INDEX_TBUO        , 1 )
             INDEX_R1NO3        = IOLD2NEW( INDEX_R1NO3       , 1 )
             INDEX_ACET         = IOLD2NEW( INDEX_ACET        , 1 )
             INDEX_NPHE         = IOLD2NEW( INDEX_NPHE        , 1 )
             INDEX_CRES         = IOLD2NEW( INDEX_CRES        , 1 )
             INDEX_NPRAD        = IOLD2NEW( INDEX_NPRAD       , 1 )
             INDEX_NAPPRD       = IOLD2NEW( INDEX_NAPPRD      , 1 )
             INDEX_PNAMIN       = IOLD2NEW( INDEX_PNAMIN      , 1 )
             INDEX_NAMIN        = IOLD2NEW( INDEX_NAMIN       , 1 )
             INDEX_AMINS        = IOLD2NEW( INDEX_AMINS       , 1 )
             INDEX_HCHO2        = IOLD2NEW( INDEX_HCHO2       , 1 )
             INDEX_HCOOH        = IOLD2NEW( INDEX_HCOOH       , 1 )
             INDEX_SO2          = IOLD2NEW( INDEX_SO2         , 1 )
             INDEX_SULF         = IOLD2NEW( INDEX_SULF        , 1 )
             INDEX_SULRXN       = IOLD2NEW( INDEX_SULRXN      , 1 )
             INDEX_MECHO2       = IOLD2NEW( INDEX_MECHO2      , 1 )
             INDEX_RCHO2        = IOLD2NEW( INDEX_RCHO2       , 1 )
             INDEX_RCHO         = IOLD2NEW( INDEX_RCHO        , 1 )
             INDEX_GLY          = IOLD2NEW( INDEX_GLY         , 1 )
             INDEX_BALD         = IOLD2NEW( INDEX_BALD        , 1 )
             INDEX_PHEN         = IOLD2NEW( INDEX_PHEN        , 1 )
             INDEX_NAPS         = IOLD2NEW( INDEX_NAPS        , 1 )
             INDEX_CATL         = IOLD2NEW( INDEX_CATL        , 1 )
             INDEX_AFG2A        = IOLD2NEW( INDEX_AFG2A       , 1 )
             INDEX_AFG2B        = IOLD2NEW( INDEX_AFG2B       , 1 )
             INDEX_MACO3        = IOLD2NEW( INDEX_MACO3       , 1 )
             INDEX_PAHRO2       = IOLD2NEW( INDEX_PAHRO2      , 1 )
             INDEX_CATL3        = IOLD2NEW( INDEX_CATL3       , 1 )
             INDEX_OTHN         = IOLD2NEW( INDEX_OTHN        , 1 )
             INDEX_PHOT         = IOLD2NEW( INDEX_PHOT        , 1 )
             INDEX_ALK3         = IOLD2NEW( INDEX_ALK3        , 1 )
             INDEX_IMINE        = IOLD2NEW( INDEX_IMINE       , 1 )
             INDEX_CLETHE       = IOLD2NEW( INDEX_CLETHE      , 1 )
             INDEX_xHO2         = IOLD2NEW( INDEX_xHO2        , 1 )
             INDEX_xHCHO        = IOLD2NEW( INDEX_xHCHO       , 1 )
             INDEX_yROOH        = IOLD2NEW( INDEX_yROOH       , 1 )
             INDEX_ACRLNT       = IOLD2NEW( INDEX_ACRLNT      , 1 )
             INDEX_PCE          = IOLD2NEW( INDEX_PCE         , 1 )
             INDEX_PCLBEN       = IOLD2NEW( INDEX_PCLBEN      , 1 )
             INDEX_MECL2        = IOLD2NEW( INDEX_MECL2       , 1 )
             INDEX_ETBR2        = IOLD2NEW( INDEX_ETBR2       , 1 )
             INDEX_ETCL2        = IOLD2NEW( INDEX_ETCL2       , 1 )
             INDEX_ETOX         = IOLD2NEW( INDEX_ETOX        , 1 )
             INDEX_CHCL3        = IOLD2NEW( INDEX_CHCL3       , 1 )
             INDEX_xOH          = IOLD2NEW( INDEX_xOH         , 1 )
             INDEX_xNO2         = IOLD2NEW( INDEX_xNO2        , 1 )
             INDEX_xNO3         = IOLD2NEW( INDEX_xNO3        , 1 )
             INDEX_xGLY         = IOLD2NEW( INDEX_xGLY        , 1 )
             INDEX_xHCOOH       = IOLD2NEW( INDEX_xHCOOH      , 1 )
             INDEX_xMECHO       = IOLD2NEW( INDEX_xMECHO      , 1 )
             INDEX_xETCHO       = IOLD2NEW( INDEX_xETCHO      , 1 )
             INDEX_ETCHO        = IOLD2NEW( INDEX_ETCHO       , 1 )
             INDEX_xGLCHO       = IOLD2NEW( INDEX_xGLCHO      , 1 )
             INDEX_GLCHO        = IOLD2NEW( INDEX_GLCHO       , 1 )
             INDEX_xMEK         = IOLD2NEW( INDEX_xMEK        , 1 )
             INDEX_MEK          = IOLD2NEW( INDEX_MEK         , 1 )
             INDEX_xACRO        = IOLD2NEW( INDEX_xACRO       , 1 )
             INDEX_ACRO         = IOLD2NEW( INDEX_ACRO        , 1 )
             INDEX_xACET        = IOLD2NEW( INDEX_xACET       , 1 )
             INDEX_xMACR        = IOLD2NEW( INDEX_xMACR       , 1 )
             INDEX_MACR         = IOLD2NEW( INDEX_MACR        , 1 )
             INDEX_xMVK         = IOLD2NEW( INDEX_xMVK        , 1 )
             INDEX_MVK          = IOLD2NEW( INDEX_MVK         , 1 )
             INDEX_xBACL        = IOLD2NEW( INDEX_xBACL       , 1 )
             INDEX_BACL         = IOLD2NEW( INDEX_BACL        , 1 )
             INDEX_xMGLY        = IOLD2NEW( INDEX_xMGLY       , 1 )
             INDEX_MGLY         = IOLD2NEW( INDEX_MGLY        , 1 )
             INDEX_xBUDAL       = IOLD2NEW( INDEX_xBUDAL      , 1 )
             INDEX_BUDAL        = IOLD2NEW( INDEX_BUDAL       , 1 )
             INDEX_xFURNS       = IOLD2NEW( INDEX_xFURNS      , 1 )
             INDEX_FURNS        = IOLD2NEW( INDEX_FURNS       , 1 )
             INDEX_xBALD        = IOLD2NEW( INDEX_xBALD       , 1 )
             INDEX_xBENX        = IOLD2NEW( INDEX_xBENX       , 1 )
             INDEX_BENX         = IOLD2NEW( INDEX_BENX        , 1 )
             INDEX_xRCHO        = IOLD2NEW( INDEX_xRCHO       , 1 )
             INDEX_xKET2        = IOLD2NEW( INDEX_xKET2       , 1 )
             INDEX_KET2         = IOLD2NEW( INDEX_KET2        , 1 )
             INDEX_xLVKS        = IOLD2NEW( INDEX_xLVKS       , 1 )
             INDEX_LVKS         = IOLD2NEW( INDEX_LVKS        , 1 )
             INDEX_xOLEA1       = IOLD2NEW( INDEX_xOLEA1      , 1 )
             INDEX_OLEA1        = IOLD2NEW( INDEX_OLEA1       , 1 )
             INDEX_xOLEA2       = IOLD2NEW( INDEX_xOLEA2      , 1 )
             INDEX_OLEA2        = IOLD2NEW( INDEX_OLEA2       , 1 )
             INDEX_xOLEP        = IOLD2NEW( INDEX_xOLEP       , 1 )
             INDEX_OLEP         = IOLD2NEW( INDEX_OLEP        , 1 )
             INDEX_xOACID       = IOLD2NEW( INDEX_xOACID      , 1 )
             INDEX_xPACID       = IOLD2NEW( INDEX_xPACID      , 1 )
             INDEX_xAMINS       = IOLD2NEW( INDEX_xAMINS      , 1 )
             INDEX_xRPNO3       = IOLD2NEW( INDEX_xRPNO3      , 1 )
             INDEX_RPNO3        = IOLD2NEW( INDEX_RPNO3       , 1 )
             INDEX_xRCNO3       = IOLD2NEW( INDEX_xRCNO3      , 1 )
             INDEX_RCNO3        = IOLD2NEW( INDEX_RCNO3       , 1 )
             INDEX_xRHNO3       = IOLD2NEW( INDEX_xRHNO3      , 1 )
             INDEX_RHNO3        = IOLD2NEW( INDEX_RHNO3       , 1 )
             INDEX_xRDNO3       = IOLD2NEW( INDEX_xRDNO3      , 1 )
             INDEX_RDNO3        = IOLD2NEW( INDEX_RDNO3       , 1 )
             INDEX_xHPCRB       = IOLD2NEW( INDEX_xHPCRB      , 1 )
             INDEX_HPCRB        = IOLD2NEW( INDEX_HPCRB       , 1 )
             INDEX_xAFG1        = IOLD2NEW( INDEX_xAFG1       , 1 )
             INDEX_AFG1         = IOLD2NEW( INDEX_AFG1        , 1 )
             INDEX_xAFG2A       = IOLD2NEW( INDEX_xAFG2A      , 1 )
             INDEX_xAFG2B       = IOLD2NEW( INDEX_xAFG2B      , 1 )
             INDEX_xAFG3        = IOLD2NEW( INDEX_xAFG3       , 1 )
             INDEX_AFG3         = IOLD2NEW( INDEX_AFG3        , 1 )
             INDEX_xPAN2        = IOLD2NEW( INDEX_xPAN2       , 1 )
             INDEX_PAN2         = IOLD2NEW( INDEX_PAN2        , 1 )
             INDEX_xMEO2        = IOLD2NEW( INDEX_xMEO2       , 1 )
             INDEX_xETO2        = IOLD2NEW( INDEX_xETO2       , 1 )
             INDEX_xMECO3       = IOLD2NEW( INDEX_xMECO3      , 1 )
             INDEX_xR2CO3       = IOLD2NEW( INDEX_xR2CO3      , 1 )
             INDEX_R2CO3        = IOLD2NEW( INDEX_R2CO3       , 1 )
             INDEX_xMACO3       = IOLD2NEW( INDEX_xMACO3      , 1 )
             INDEX_xTBUO        = IOLD2NEW( INDEX_xTBUO       , 1 )
             INDEX_xBZO         = IOLD2NEW( INDEX_xBZO        , 1 )
             INDEX_yRUOOH       = IOLD2NEW( INDEX_yRUOOH      , 1 )
             INDEX_RUOOH        = IOLD2NEW( INDEX_RUOOH       , 1 )
             INDEX_yRAOOH       = IOLD2NEW( INDEX_yRAOOH      , 1 )
             INDEX_RAOOH        = IOLD2NEW( INDEX_RAOOH       , 1 )
             INDEX_yHPCRB       = IOLD2NEW( INDEX_yHPCRB      , 1 )
             INDEX_yRPNO3       = IOLD2NEW( INDEX_yRPNO3      , 1 )
             INDEX_zR1NO3       = IOLD2NEW( INDEX_zR1NO3      , 1 )
             INDEX_zR2NO3       = IOLD2NEW( INDEX_zR2NO3      , 1 )
             INDEX_R2NO3        = IOLD2NEW( INDEX_R2NO3       , 1 )
             INDEX_zRHNO3       = IOLD2NEW( INDEX_zRHNO3      , 1 )
             INDEX_zRCNO3       = IOLD2NEW( INDEX_zRCNO3      , 1 )
             INDEX_zRANO3       = IOLD2NEW( INDEX_zRANO3      , 1 )
             INDEX_RANO3        = IOLD2NEW( INDEX_RANO3       , 1 )
             INDEX_zRPNO3       = IOLD2NEW( INDEX_zRPNO3      , 1 )
             INDEX_zRDNO3       = IOLD2NEW( INDEX_zRDNO3      , 1 )
             INDEX_zRNNO3       = IOLD2NEW( INDEX_zRNNO3      , 1 )
             INDEX_RNNO3        = IOLD2NEW( INDEX_RNNO3       , 1 )
             INDEX_zPAN2        = IOLD2NEW( INDEX_zPAN2       , 1 )
             INDEX_APANS        = IOLD2NEW( INDEX_APANS       , 1 )
             INDEX_ETHAN        = IOLD2NEW( INDEX_ETHAN       , 1 )
             INDEX_PROP         = IOLD2NEW( INDEX_PROP        , 1 )
             INDEX_NC4          = IOLD2NEW( INDEX_NC4         , 1 )
             INDEX_ETHEN        = IOLD2NEW( INDEX_ETHEN       , 1 )
             INDEX_ETHEN_OP     = IOLD2NEW( INDEX_ETHEN_OP    , 1 )
             INDEX_NROG         = IOLD2NEW( INDEX_NROG        , 1 )
             INDEX_PROPE        = IOLD2NEW( INDEX_PROPE       , 1 )
             INDEX_PROPE_O3     = IOLD2NEW( INDEX_PROPE_O3    , 1 )
             INDEX_ALK2         = IOLD2NEW( INDEX_ALK2        , 1 )
             INDEX_ISOP         = IOLD2NEW( INDEX_ISOP        , 1 )
             INDEX_ISOP_OH      = IOLD2NEW( INDEX_ISOP_OH     , 1 )
             INDEX_ISOP_O3      = IOLD2NEW( INDEX_ISOP_O3     , 1 )
             INDEX_ISOPRXN      = IOLD2NEW( INDEX_ISOPRXN     , 1 )
             INDEX_ISOP_N3      = IOLD2NEW( INDEX_ISOP_N3     , 1 )
             INDEX_BUT13        = IOLD2NEW( INDEX_BUT13       , 1 )
             INDEX_BUT13_OH     = IOLD2NEW( INDEX_BUT13_OH    , 1 )
             INDEX_BUT13_O3     = IOLD2NEW( INDEX_BUT13_O3    , 1 )
             INDEX_APINE        = IOLD2NEW( INDEX_APINE       , 1 )
             INDEX_APINE_OH     = IOLD2NEW( INDEX_APINE_OH    , 1 )
             INDEX_TRPRXN       = IOLD2NEW( INDEX_TRPRXN      , 1 )
             INDEX_ALK4         = IOLD2NEW( INDEX_ALK4        , 1 )
             INDEX_BPINE        = IOLD2NEW( INDEX_BPINE       , 1 )
             INDEX_BPINE_OH     = IOLD2NEW( INDEX_BPINE_OH    , 1 )
             INDEX_ACETL        = IOLD2NEW( INDEX_ACETL       , 1 )
             INDEX_BENZ         = IOLD2NEW( INDEX_BENZ        , 1 )
             INDEX_BENZRO2      = IOLD2NEW( INDEX_BENZRO2     , 1 )
             INDEX_TOLU         = IOLD2NEW( INDEX_TOLU        , 1 )
             INDEX_TOLRO2       = IOLD2NEW( INDEX_TOLRO2      , 1 )
             INDEX_OXYL         = IOLD2NEW( INDEX_OXYL        , 1 )
             INDEX_XYNL         = IOLD2NEW( INDEX_XYNL        , 1 )
             INDEX_XYLRO2       = IOLD2NEW( INDEX_XYLRO2      , 1 )
             INDEX_MXYL         = IOLD2NEW( INDEX_MXYL        , 1 )
             INDEX_PXYL         = IOLD2NEW( INDEX_PXYL        , 1 )
             INDEX_BZ123        = IOLD2NEW( INDEX_BZ123       , 1 )
             INDEX_BZ124        = IOLD2NEW( INDEX_BZ124       , 1 )
             INDEX_BZ135        = IOLD2NEW( INDEX_BZ135       , 1 )
             INDEX_C2BEN        = IOLD2NEW( INDEX_C2BEN       , 1 )
             INDEX_MTBE         = IOLD2NEW( INDEX_MTBE        , 1 )
             INDEX_ALK1         = IOLD2NEW( INDEX_ALK1        , 1 )
             INDEX_MECHO_OH     = IOLD2NEW( INDEX_MECHO_OH    , 1 )
             INDEX_GLCHO_HV     = IOLD2NEW( INDEX_GLCHO_HV    , 1 )
             INDEX_ACRO_OH      = IOLD2NEW( INDEX_ACRO_OH     , 1 )
             INDEX_ACRO_HV      = IOLD2NEW( INDEX_ACRO_HV     , 1 )
             INDEX_MACR_OH      = IOLD2NEW( INDEX_MACR_OH     , 1 )
             INDEX_MACR_N3      = IOLD2NEW( INDEX_MACR_N3     , 1 )
             INDEX_BUDAL_OH     = IOLD2NEW( INDEX_BUDAL_OH    , 1 )
             INDEX_MALAH        = IOLD2NEW( INDEX_MALAH       , 1 )
             INDEX_ALK5_OH      = IOLD2NEW( INDEX_ALK5_OH     , 1 )
             INDEX_ALK6         = IOLD2NEW( INDEX_ALK6        , 1 )
             INDEX_ALK6_OH      = IOLD2NEW( INDEX_ALK6_OH     , 1 )
             INDEX_OLE1         = IOLD2NEW( INDEX_OLE1        , 1 )
             INDEX_OLE2         = IOLD2NEW( INDEX_OLE2        , 1 )
             INDEX_OLE2_O3      = IOLD2NEW( INDEX_OLE2_O3     , 1 )
             INDEX_OLE3         = IOLD2NEW( INDEX_OLE3        , 1 )
             INDEX_OLE4         = IOLD2NEW( INDEX_OLE4        , 1 )
             INDEX_OLE4_O3      = IOLD2NEW( INDEX_OLE4_O3     , 1 )
             INDEX_TERP         = IOLD2NEW( INDEX_TERP        , 1 )
             INDEX_TERP_OH      = IOLD2NEW( INDEX_TERP_OH     , 1 )
             INDEX_TERP_O3      = IOLD2NEW( INDEX_TERP_O3     , 1 )
             INDEX_TERP_N3      = IOLD2NEW( INDEX_TERP_N3     , 1 )
             INDEX_SESQ         = IOLD2NEW( INDEX_SESQ        , 1 )
             INDEX_SESQ_OH      = IOLD2NEW( INDEX_SESQ_OH     , 1 )
             INDEX_SESQRXN      = IOLD2NEW( INDEX_SESQRXN     , 1 )
             INDEX_SESQ_O3      = IOLD2NEW( INDEX_SESQ_O3     , 1 )
             INDEX_SESQ_N3      = IOLD2NEW( INDEX_SESQ_N3     , 1 )
             INDEX_ARO1         = IOLD2NEW( INDEX_ARO1        , 1 )
             INDEX_ARO2         = IOLD2NEW( INDEX_ARO2        , 1 )
             INDEX_ARO2_OH      = IOLD2NEW( INDEX_ARO2_OH     , 1 )
             INDEX_STYRS        = IOLD2NEW( INDEX_STYRS       , 1 )
             INDEX_TAMNS        = IOLD2NEW( INDEX_TAMNS       , 1 )
             INDEX_OLEA1_OH     = IOLD2NEW( INDEX_OLEA1_OH    , 1 )
             INDEX_OLEA1_N3     = IOLD2NEW( INDEX_OLEA1_N3    , 1 )
             INDEX_OLEA2_OH     = IOLD2NEW( INDEX_OLEA2_OH    , 1 )
             INDEX_OLEA2_O3     = IOLD2NEW( INDEX_OLEA2_O3    , 1 )
             INDEX_OLEA2_N3     = IOLD2NEW( INDEX_OLEA2_N3    , 1 )
             INDEX_OLEA2_HV     = IOLD2NEW( INDEX_OLEA2_HV    , 1 )
             INDEX_LVKS_OH      = IOLD2NEW( INDEX_LVKS_OH     , 1 )
             INDEX_LVKS_O3      = IOLD2NEW( INDEX_LVKS_O3     , 1 )
             INDEX_OLEP_OH      = IOLD2NEW( INDEX_OLEP_OH     , 1 )
             INDEX_OLEP_O3      = IOLD2NEW( INDEX_OLEP_O3     , 1 )
             INDEX_RCNO3_OH     = IOLD2NEW( INDEX_RCNO3_OH    , 1 )
             INDEX_RCNO3_HV     = IOLD2NEW( INDEX_RCNO3_HV    , 1 )
             INDEX_RPNO3_OH     = IOLD2NEW( INDEX_RPNO3_OH    , 1 )
             INDEX_RPNO3_HV     = IOLD2NEW( INDEX_RPNO3_HV    , 1 )
             INDEX_RDNO3_HV     = IOLD2NEW( INDEX_RDNO3_HV    , 1 )
             INDEX_R2NO3_HV     = IOLD2NEW( INDEX_R2NO3_HV    , 1 )
             INDEX_RUOOH_OH     = IOLD2NEW( INDEX_RUOOH_OH    , 1 )
             INDEX_HPCRB_OH     = IOLD2NEW( INDEX_HPCRB_OH    , 1 )
             INDEX_ROOH_OH      = IOLD2NEW( INDEX_ROOH_OH     , 1 )
             INDEX_AFG1_OH      = IOLD2NEW( INDEX_AFG1_OH     , 1 )
             INDEX_AFG1_HV      = IOLD2NEW( INDEX_AFG1_HV     , 1 )
             INDEX_AFG2A_OH     = IOLD2NEW( INDEX_AFG2A_OH    , 1 )
             INDEX_AFG2B_OH     = IOLD2NEW( INDEX_AFG2B_OH    , 1 )
             INDEX_AFG2B_HV     = IOLD2NEW( INDEX_AFG2B_HV    , 1 )
             INDEX_SOAALK       = IOLD2NEW( INDEX_SOAALK      , 1 )
             INDEX_SVAVB2       = IOLD2NEW( INDEX_SVAVB2      , 1 )
             INDEX_SVAVB3       = IOLD2NEW( INDEX_SVAVB3      , 1 )
             INDEX_SVAVB4       = IOLD2NEW( INDEX_SVAVB4      , 1 )
             INDEX_SVAVB1       = IOLD2NEW( INDEX_SVAVB1      , 1 )
             INDEX_H2NO3PIJ     = IOLD2NEW( INDEX_H2NO3PIJ    , 1 )
             INDEX_H2NO3PK      = IOLD2NEW( INDEX_H2NO3PK     , 1 )
             INDEX_AISO1J       = IOLD2NEW( INDEX_AISO1J      , 1 )
             INDEX_AOLGBJ       = IOLD2NEW( INDEX_AOLGBJ      , 1 )
             INDEX_AISO2J       = IOLD2NEW( INDEX_AISO2J      , 1 )
             INDEX_ASQTJ        = IOLD2NEW( INDEX_ASQTJ       , 1 )
             INDEX_AAVB2J       = IOLD2NEW( INDEX_AAVB2J      , 1 )
             INDEX_AOLGAJ       = IOLD2NEW( INDEX_AOLGAJ      , 1 )
             INDEX_AAVB3J       = IOLD2NEW( INDEX_AAVB3J      , 1 )
             INDEX_AAVB4J       = IOLD2NEW( INDEX_AAVB4J      , 1 )
             INDEX_APOCI        = IOLD2NEW( INDEX_APOCI       , 1 )
             INDEX_APNCOMI      = IOLD2NEW( INDEX_APNCOMI     , 1 )
             INDEX_APOCJ        = IOLD2NEW( INDEX_APOCJ       , 1 )
             INDEX_APNCOMJ      = IOLD2NEW( INDEX_APNCOMJ     , 1 )
             INDEX_PCVOC        = IOLD2NEW( INDEX_PCVOC       , 1 )
             INDEX_PCSOARXN     = IOLD2NEW( INDEX_PCSOARXN    , 1 )
             INDEX_VLVPO1       = IOLD2NEW( INDEX_VLVPO1      , 1 )
             INDEX_VSVPO1       = IOLD2NEW( INDEX_VSVPO1      , 1 )
             INDEX_VSVPO2       = IOLD2NEW( INDEX_VSVPO2      , 1 )
             INDEX_VSVPO3       = IOLD2NEW( INDEX_VSVPO3      , 1 )
             INDEX_VIVPO1       = IOLD2NEW( INDEX_VIVPO1      , 1 )
             INDEX_VLVOO1       = IOLD2NEW( INDEX_VLVOO1      , 1 )
             INDEX_VLVOO2       = IOLD2NEW( INDEX_VLVOO2      , 1 )
             INDEX_VSVOO2       = IOLD2NEW( INDEX_VSVOO2      , 1 )
             INDEX_VSVOO3       = IOLD2NEW( INDEX_VSVOO3      , 1 )
             INDEX_VSVOO1       = IOLD2NEW( INDEX_VSVOO1      , 1 )
             INDEX_AGLYJ        = IOLD2NEW( INDEX_AGLYJ       , 1 )
             INDEX_HCHO_PRIMARY = IOLD2NEW( INDEX_HCHO_PRIMARY, 1 )
             INDEX_CCHO_PRIMARY = IOLD2NEW( INDEX_CCHO_PRIMARY, 1 )
             INDEX_ACRO_PRIMARY = IOLD2NEW( INDEX_ACRO_PRIMARY, 1 )
          END SUBROUTINE RESET_SPECIES_POINTERS
       END MODULE RXNS_FUNCTION
