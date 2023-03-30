       MODULE RXNS_FUNCTION


       IMPLICIT NONE



! Name of Mechanism CB6R3_AE7_AQ

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

!  Reaction Label R1              
                RKI( NCELL,    1) =  RJBLK( NCELL, IJ_NO2_IUPAC10 )
!  Reaction Label R8              
                RKI( NCELL,    8) =  RJBLK( NCELL, IJ_O3_O3P_IUPAC10 )
!  Reaction Label R9              
                RKI( NCELL,    9) =  RJBLK( NCELL, IJ_O3_O1D_IUPAC10 )
!  Reaction Label R21             
                RKI( NCELL,   21) =  RJBLK( NCELL, IJ_H2O2_IUPAC10 )
!  Reaction Label R27             
                RKI( NCELL,   27) =  RJBLK( NCELL, IJ_NO3NO2_06 )
!  Reaction Label R28             
                RKI( NCELL,   28) =  RJBLK( NCELL, IJ_NO3NO_06 )
!  Reaction Label R38             
                RKI( NCELL,   38) =  RJBLK( NCELL, IJ_N2O5_IUPAC10 )
!  Reaction Label R43             
                RKI( NCELL,   43) =  RJBLK( NCELL, IJ_HONO_IUPAC10 )
!  Reaction Label R47             
                RKI( NCELL,   47) =  RJBLK( NCELL, IJ_HNO3_IUPAC10 )
!  Reaction Label R50             
                RKI( NCELL,   50) =  RJBLK( NCELL, IJ_PNA_IUPAC10 )
!  Reaction Label R56             
                RKI( NCELL,   56) =  RJBLK( NCELL, IJ_PAN_IUPAC10 )
!  Reaction Label R64             
                RKI( NCELL,   64) =  RJBLK( NCELL, IJ_PAN_IUPAC10 )
!  Reaction Label R88             
                RKI( NCELL,   88) =  RJBLK( NCELL, IJ_MEPX_IUPAC10 )
!  Reaction Label R90             
                RKI( NCELL,   90) =  RJBLK( NCELL, IJ_MEPX_IUPAC10 )
!  Reaction Label R92             
                RKI( NCELL,   92) =  RJBLK( NCELL, IJ_NTR_IUPAC10 )
!  Reaction Label R97             
                RKI( NCELL,   97) =  RJBLK( NCELL, IJ_FORM_R_IUPAC10 )
!  Reaction Label R98             
                RKI( NCELL,   98) =  RJBLK( NCELL, IJ_FORM_M_IUPAC10 )
!  Reaction Label R108            
                RKI( NCELL,  108) =  RJBLK( NCELL, IJ_ALD2_R_IUPAC10 )
!  Reaction Label R112            
                RKI( NCELL,  112) =  RJBLK( NCELL, IJ_ALDX_R_IUPAC10 )
!  Reaction Label R114            
                RKI( NCELL,  114) =  RJBLK( NCELL, IJ_GLYD_IUPAC10 )
!  Reaction Label R117            
                RKI( NCELL,  117) =  RJBLK( NCELL, IJ_GLY_R_IUPAC10 )
!  Reaction Label R119            
                RKI( NCELL,  119) =  RJBLK( NCELL, IJ_MGLY_IUPAC10 )
!  Reaction Label R128            
                RKI( NCELL,  128) =  RJBLK( NCELL, IJ_KET_IUPAC10 )
!  Reaction Label R129            
                RKI( NCELL,  129) =  RJBLK( NCELL, IJ_ACET_IUPAC10 )
!  Reaction Label R161            
                RKI( NCELL,  161) =  RJBLK( NCELL, IJ_ISPD )
!  Reaction Label R163            
                RKI( NCELL,  163) =  RJBLK( NCELL, IJ_HPALD )
!  Reaction Label R196            
                RKI( NCELL,  201) =  RJBLK( NCELL, IJ_NTR_IUPAC10 )
!  Reaction Label R197            
                RKI( NCELL,  202) =   5.0000D-02 * RJBLK( NCELL, IJ_NO2_IUPAC10 )
!  Reaction Label R201            
                RKI( NCELL,  206) =   2.8000D-02 * RJBLK( NCELL, IJ_NO2_IUPAC10 )
!  Reaction Label CL1             
                RKI( NCELL,  225) =  RJBLK( NCELL, IJ_CL2_IUPAC04 )
!  Reaction Label CL2             
                RKI( NCELL,  226) =  RJBLK( NCELL, IJ_HOCL_IUPAC04 )
!  Reaction Label CL9             
                RKI( NCELL,  233) =  RJBLK( NCELL, IJ_FMCL_IUPAC04 )
!  Reaction Label CL27            
                RKI( NCELL,  251) =  RJBLK( NCELL, IJ_CLNO2_IUPAC13 )
!  Reaction Label CL30            
                RKI( NCELL,  253) =  RJBLK( NCELL, IJ_CLONO2_1 )
!  Reaction Label CL31            
                RKI( NCELL,  254) =  RJBLK( NCELL, IJ_CLONO2_2 )
!  Reaction Label BP71mtp         
                RKI( NCELL,  289) =  RJBLK( NCELL, IJ_IC3ONO2 )
!  Reaction Label T04             
                RKI( NCELL,  315) =  RJBLK( NCELL, IJ_FORM_R_IUPAC10 )
!  Reaction Label T05             
                RKI( NCELL,  316) =  RJBLK( NCELL, IJ_FORM_M_IUPAC10 )
!  Reaction Label T09             
                RKI( NCELL,  321) =  RJBLK( NCELL, IJ_ALD2_R_IUPAC10 )
!  Reaction Label T16             
                RKI( NCELL,  330) =  RJBLK( NCELL, IJ_ACRO_09 )
!  Reaction Label T20             
                RKI( NCELL,  335) =  RJBLK( NCELL, IJ_ACRO_09 )

                IF ( SEAWATER (NCELL) .GT. 0.001D0 ) THEN
!  Reaction Label HAL_Ozone       
                   RKI( NCELL,  277) = SEAWATER (NCELL) *  SFACT * HALOGEN_FALLOFF( BLKPRES( NCELL ),   6.7006D-11,   1.0743D+01,  & 
     &                                                           3.4153D-08,  -6.7130D-01,         2.0000D-06 )
                ELSE
                   RKI( NCELL,  277) = 0.0D0 
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

!  Reaction Label R2              
             RKI( NCELL,    2) =  CFACT_SQU * POWER_T02( TEMPOT300,   5.6800D-34,  -2.6000D+00 )
!  Reaction Label R3              
             RKI( NCELL,    3) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.3100D+03 )
!  Reaction Label R4              
             RKI( NCELL,    4) =  CFACT_SQU * POWER_T02( TEMPOT300,   1.0000D-31,  -1.6000D+00 )
!  Reaction Label R5              
             RKI( NCELL,    5) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.5000D-12,   1.8800D+02 )
!  Reaction Label R6              
             RKI( NCELL,    6) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.3000D-31,   0.0000D+00,  -1.5000D+00,  & 
     &                                                 2.3000D-11,   0.0000D+00,   2.4000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R7              
             RKI( NCELL,    7) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.0000D-12,  -2.0600D+03 )
!  Reaction Label R10             
             RKI( NCELL,   10) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2300D-11,   1.1500D+02 )
!  Reaction Label R11             
             RKI( NCELL,   11) =   2.1400D-10 * CFACT 
!  Reaction Label R12             
             RKI( NCELL,   12) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7000D-12,  -9.4000D+02 )
!  Reaction Label R13             
             RKI( NCELL,   13) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   2.0300D-16,   6.9300D+02,   4.5700D+00 )
!  Reaction Label R14             
             RKI( NCELL,   14) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.4000D-11,   1.1000D+02 )
!  Reaction Label R15             
             RKI( NCELL,   15) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-11,   2.2400D+02 )
!  Reaction Label R16             
             RKI( NCELL,   16) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   6.2000D-14,   9.4500D+02,   2.6000D+00 )
!  Reaction Label R17             
             RKI( NCELL,   17) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 6.9000D-31,   0.0000D+00,  -8.0000D-01,  & 
     &                                                 2.6000D-11,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.1300D+00,   5.0000D-01 )
!  Reaction Label R18             
             RKI( NCELL,   18) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.8000D-11,   2.5000D+02 )
!  Reaction Label R19             
             RKI( NCELL,   19) =  CFACT * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 2.2000D-13,   6.0000D+02,   1.9000D-33,  & 
     &                                                 9.8000D+02 )
!  Reaction Label R20             
             RKI( NCELL,   20) =  CFACT_SQU * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 3.0800D-34,   2.8000D+03,   2.6600D-54,  & 
     &                                                 3.1800D+03 )
!  Reaction Label R22             
             RKI( NCELL,   22) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D-12,  -1.6000D+02 )
!  Reaction Label R23             
             RKI( NCELL,   23) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -2.0000D+03 )
!  Reaction Label R24             
             RKI( NCELL,   24) =  CFACT_SQU * ARRHENUIS_T03( INV_TEMP,  3.3000D-39,   5.3000D+02 )
!  Reaction Label R25             
             RKI( NCELL,   25) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4500D-12,   2.7000D+02 )
!  Reaction Label R26             
             RKI( NCELL,   26) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-13,  -2.4700D+03 )
!  Reaction Label R29             
             RKI( NCELL,   29) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-11,   1.1000D+02 )
!  Reaction Label R30             
             RKI( NCELL,   30) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.5000D-14,  -1.2600D+03 )
!  Reaction Label R31             
             RKI( NCELL,   31) =   1.7000D-11 * CFACT 
!  Reaction Label R32             
             RKI( NCELL,   32) =   2.0000D-11 * CFACT 
!  Reaction Label R33             
             RKI( NCELL,   33) =   4.0000D-12 * CFACT 
!  Reaction Label R34             
             RKI( NCELL,   34) =   1.0000D-17 * CFACT 
!  Reaction Label R35             
             RKI( NCELL,   35) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5000D-13,  -2.4500D+03 )
!  Reaction Label R36             
             RKI( NCELL,   36) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 3.6000D-30,   0.0000D+00,  -4.1000D+00,  & 
     &                                                 1.9000D-12,   0.0000D+00,   2.0000D-01,  & 
     &                                                 1.3300D+00,   3.5000D-01 )
!  Reaction Label R37             
             RKI( NCELL,   37) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.3000D-03,  -1.1000D+04,  -3.5000D+00,  & 
     &                                                 9.7000D+14,  -1.1080D+04,   1.0000D-01,  & 
     &                                                 1.3300D+00,   3.5000D-01 )
!  Reaction Label R39             
             RKI( NCELL,   39) =   1.0000D-22 * CFACT 
!  Reaction Label R40             
             RKI( NCELL,   40) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 7.4000D-31,   0.0000D+00,  -2.4000D+00,  & 
     &                                                 3.3000D-11,   0.0000D+00,  -3.0000D-01,  & 
     &                                                 8.7000D-01,   8.1000D-01 )
!  Reaction Label R41             
             RKI( NCELL,   41) =   5.0000D-40 * CFACT_SQU 
!  Reaction Label R42             
             RKI( NCELL,   42) =   1.0000D-20 * CFACT 
!  Reaction Label R44             
             RKI( NCELL,   44) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5000D-12,   2.6000D+02 )
!  Reaction Label R45             
             RKI( NCELL,   45) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.8000D-30,   0.0000D+00,  -3.0000D+00,  & 
     &                                                 2.8000D-11,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R46             
             RKI( NCELL,   46) =  CFACT * FALLOFF_T08( INV_TEMP,  CAIR, & 
     &                                                 2.4000D-14,   4.6000D+02,   2.7000D-17,  & 
     &                                                 2.1990D+03,   6.5000D-34,   1.3350D+03 )
!  Reaction Label R48             
             RKI( NCELL,   48) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.8000D-31,   0.0000D+00,  -3.2000D+00,  & 
     &                                                 4.7000D-12,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R49             
             RKI( NCELL,   49) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.1000D-05,  -1.0650D+04,   0.0000D+00,  & 
     &                                                 4.8000D+15,  -1.1170D+04,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R51             
             RKI( NCELL,   51) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.2000D-13,   6.9000D+02 )
!  Reaction Label R52             
             RKI( NCELL,   52) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.5000D-31,   0.0000D+00,  -3.9000D+00,  & 
     &                                                 1.3000D-12,   0.0000D+00,  -7.0000D-01,  & 
     &                                                 1.1000D+00,   5.3000D-01 )
!  Reaction Label R53             
             RKI( NCELL,   53) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.5000D-12,   2.9000D+02 )
!  Reaction Label R54             
             RKI( NCELL,   54) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.7000D-28,   0.0000D+00,  -7.1000D+00,  & 
     &                                                 1.2000D-11,   0.0000D+00,  -9.0000D-01,  & 
     &                                                 1.4100D+00,   3.0000D-01 )
!  Reaction Label R55             
             RKI( NCELL,   55) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.9000D-03,  -1.2100D+04,   0.0000D+00,  & 
     &                                                 5.4000D+16,  -1.3830D+04,   0.0000D+00,  & 
     &                                                 1.4100D+00,   3.0000D-01 )
!  Reaction Label R57             
             RKI( NCELL,   57) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.2000D-13,   9.8000D+02 )
!  Reaction Label R58             
             RKI( NCELL,   58) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.9000D-13,   8.0000D+02 )
!  Reaction Label R59             
             RKI( NCELL,   59) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D-12,   5.0000D+02 )
!  Reaction Label R60             
             RKI( NCELL,   60) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D-12,   5.0000D+02 )
!  Reaction Label R61             
             RKI( NCELL,   61) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.7000D-12,   3.4000D+02 )
!  Reaction Label R62             
             RKI( NCELL,   62) =   RKI( NCELL,   54 ) 
!  Reaction Label R63             
             RKI( NCELL,   63) =   RKI( NCELL,   55 ) 
!  Reaction Label R65             
             RKI( NCELL,   65) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.2000D-13,   9.8000D+02 )
!  Reaction Label R66             
             RKI( NCELL,   66) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.9000D-13,   8.0000D+02 )
!  Reaction Label R67             
             RKI( NCELL,   67) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.2000D-12,   5.0000D+02 )
!  Reaction Label R68             
             RKI( NCELL,   68) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.4000D-12,   3.6000D+02 )
!  Reaction Label R69             
             RKI( NCELL,   69) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.8000D-13,   8.0000D+02 )
!  Reaction Label R70             
             RKI( NCELL,   70) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.5000D-14,   5.0000D+02 )
!  Reaction Label R71             
             RKI( NCELL,   71) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3000D-12,   3.6000D+02 )
!  Reaction Label R72             
             RKI( NCELL,   72) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.8000D-13,   7.8000D+02 )
!  Reaction Label R73             
             RKI( NCELL,   73) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-12,   5.0000D+02 )
!  Reaction Label R74             
             RKI( NCELL,   74) =   RKI( NCELL,   70 ) 
!  Reaction Label R75             
             RKI( NCELL,   75) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R76             
             RKI( NCELL,   76) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.8000D-13,   8.0000D+02 )
!  Reaction Label R77             
             RKI( NCELL,   77) =   RKI( NCELL,   58 ) 
!  Reaction Label R78             
             RKI( NCELL,   78) =   RKI( NCELL,   70 ) 
!  Reaction Label R79             
             RKI( NCELL,   79) =   RKI( NCELL,   75 ) 
!  Reaction Label R80             
             RKI( NCELL,   80) =   RKI( NCELL,   76 ) 
!  Reaction Label R81             
             RKI( NCELL,   81) =   RKI( NCELL,   58 ) 
!  Reaction Label R82             
             RKI( NCELL,   82) =   RKI( NCELL,   70 ) 
!  Reaction Label R83             
             RKI( NCELL,   83) =   RKI( NCELL,   75 ) 
!  Reaction Label R84             
             RKI( NCELL,   84) =   RKI( NCELL,   76 ) 
!  Reaction Label R85             
             RKI( NCELL,   85) =   RKI( NCELL,   58 ) 
!  Reaction Label R86             
             RKI( NCELL,   86) =   RKI( NCELL,   70 ) 
!  Reaction Label R87             
             RKI( NCELL,   87) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.3000D-12,   1.9000D+02 )
!  Reaction Label R89             
             RKI( NCELL,   89) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.3000D-12,   1.9000D+02 )
!  Reaction Label R91             
             RKI( NCELL,   91) =   2.0000D-12 * CFACT 
!  Reaction Label R93             
             RKI( NCELL,   93) =   4.5000D-13 * CFACT 
!  Reaction Label R94             
             RKI( NCELL,   94) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.0000D-14,   8.5000D+02 )
!  Reaction Label R95             
             RKI( NCELL,   95) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.3000D-12,   1.9000D+02 )
!  Reaction Label R96             
             RKI( NCELL,   96) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.4000D-12,   1.3500D+02 )
!  Reaction Label R99             
             RKI( NCELL,   99) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-11,  -1.6000D+03 )
!  Reaction Label R100            
             RKI( NCELL,  100) =   5.5000D-16 * CFACT 
!  Reaction Label R101            
             RKI( NCELL,  101) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.7000D-15,   6.2500D+02 )
!  Reaction Label R102            
             RKI( NCELL,  102) =  SFACT * ARRHENUIS_T03( INV_TEMP,  2.4000D+12,  -7.0000D+03 )
!  Reaction Label R103            
             RKI( NCELL,  103) =   5.6000D-12 * CFACT 
!  Reaction Label R104            
             RKI( NCELL,  104) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.6000D-15,   2.3000D+03 )
!  Reaction Label R105            
             RKI( NCELL,  105) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-11,  -1.1000D+03 )
!  Reaction Label R106            
             RKI( NCELL,  106) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.7000D-12,   3.4500D+02 )
!  Reaction Label R107            
             RKI( NCELL,  107) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label R109            
             RKI( NCELL,  109) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3000D-11,  -8.7000D+02 )
!  Reaction Label R110            
             RKI( NCELL,  110) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.9000D-12,   4.0500D+02 )
!  Reaction Label R111            
             RKI( NCELL,  111) =   6.3000D-15 * CFACT 
!  Reaction Label R113            
             RKI( NCELL,  113) =   8.0000D-12 * CFACT 
!  Reaction Label R115            
             RKI( NCELL,  115) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label R116            
             RKI( NCELL,  116) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.1000D-12,   3.4000D+02 )
!  Reaction Label R118            
             RKI( NCELL,  118) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label R120            
             RKI( NCELL,  120) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label R121            
             RKI( NCELL,  121) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-12,   5.7500D+02 )
!  Reaction Label R122            
             RKI( NCELL,  122) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.7000D-12,  -2.1000D+03 )
!  Reaction Label R123            
             RKI( NCELL,  123) =  CFACT * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 1.4400D-13,   0.0000D+00,   3.4300D-33,  & 
     &                                                 0.0000D+00 )
!  Reaction Label R124            
             RKI( NCELL,  124) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8500D-12,  -1.6900D+03 )
!  Reaction Label R125            
             RKI( NCELL,  125) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.9000D-12,  -1.0000D+03 )
!  Reaction Label R126            
             RKI( NCELL,  126) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8500D-12,  -3.4500D+02 )
!  Reaction Label R127            
             RKI( NCELL,  127) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0000D-12,   2.0000D+01 )
!  Reaction Label R130            
             RKI( NCELL,  130) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4100D-12,  -6.2060D+02 )
!  Reaction Label R131            
             RKI( NCELL,  131) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.6000D-12,  -5.8500D+02 )
!  Reaction Label R132            
             RKI( NCELL,  132) =   8.1000D-13 * CFACT 
!  Reaction Label R133            
             RKI( NCELL,  133) =  SFACT * ARRHENUIS_T03( INV_TEMP,  5.7000D+12,  -5.7800D+03 )
!  Reaction Label R134            
             RKI( NCELL,  134) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5000D-14,  -2.0000D+02 )
!  Reaction Label R135            
             RKI( NCELL,  135) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.6000D-12,   4.0000D+02 )
!  Reaction Label R136            
             RKI( NCELL,  136) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 5.0000D-30,   0.0000D+00,  -1.5000D+00,  & 
     &                                                 1.0000D-12,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.3000D+00,   3.7000D-01 )
!  Reaction Label R137            
             RKI( NCELL,  137) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0400D-11,  -7.9200D+02 )
!  Reaction Label R138            
             RKI( NCELL,  138) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 8.6000D-29,   0.0000D+00,  -3.1000D+00,  & 
     &                                                 9.0000D-12,   0.0000D+00,  -8.5000D-01,  & 
     &                                                 1.1500D+00,   4.8000D-01 )
!  Reaction Label R139            
             RKI( NCELL,  139) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.1000D-15,  -2.5800D+03 )
!  Reaction Label R140            
             RKI( NCELL,  140) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.3000D-12,  -2.8800D+03 )
!  Reaction Label R141            
             RKI( NCELL,  141) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0000D-11,  -2.8000D+02 )
!  Reaction Label R142            
             RKI( NCELL,  142) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 8.0000D-27,   0.0000D+00,  -3.5000D+00,  & 
     &                                                 3.0000D-11,   0.0000D+00,  -1.0000D+00,  & 
     &                                                 1.1300D+00,   5.0000D-01 )
!  Reaction Label R143            
             RKI( NCELL,  143) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.5000D-15,  -1.8800D+03 )
!  Reaction Label R144            
             RKI( NCELL,  144) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.6000D-13,  -1.1550D+03 )
!  Reaction Label R145            
             RKI( NCELL,  145) =   2.3000D-11 * CFACT 
!  Reaction Label R146            
             RKI( NCELL,  146) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0500D-11,   5.1900D+02 )
!  Reaction Label R147            
             RKI( NCELL,  147) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.7000D-15,  -1.0130D+03 )
!  Reaction Label R148            
             RKI( NCELL,  148) =   3.7000D-13 * CFACT 
!  Reaction Label R149            
             RKI( NCELL,  149) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-11,   3.9000D+02 )
!  Reaction Label R150            
             RKI( NCELL,  150) =   3.0000D-11 * CFACT 
!  Reaction Label R151            
             RKI( NCELL,  151) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3900D-12,   3.6500D+02 )
!  Reaction Label R152            
             RKI( NCELL,  152) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4300D-13,   7.0000D+02 )
!  Reaction Label R153            
             RKI( NCELL,  153) =   RKI( NCELL,   58 ) 
!  Reaction Label R154            
             RKI( NCELL,  154) =   RKI( NCELL,   70 ) 
!  Reaction Label R155            
             RKI( NCELL,  155) =  SFACT * ARRHENUIS_T03( INV_TEMP,  3.3000D+09,  -8.3000D+03 )
!  Reaction Label R156            
             RKI( NCELL,  156) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0300D-14,  -1.9950D+03 )
!  Reaction Label R157            
             RKI( NCELL,  157) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0300D-12,  -4.4800D+02 )
!  Reaction Label R158            
             RKI( NCELL,  158) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.5800D-12,   5.1100D+02 )
!  Reaction Label R159            
             RKI( NCELL,  159) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.8800D-15,  -1.7700D+03 )
!  Reaction Label R160            
             RKI( NCELL,  160) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.1000D-12,  -1.8600D+03 )
!  Reaction Label R162            
             RKI( NCELL,  162) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2300D-11,   3.7200D+02 )
!  Reaction Label R164            
             RKI( NCELL,  164) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.0000D-12,  -1.8600D+03 )
!  Reaction Label R165            
             RKI( NCELL,  165) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.7800D-11,  -4.0000D+02 )
!  Reaction Label R166            
             RKI( NCELL,  166) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4300D-13,   7.0000D+02 )
!  Reaction Label R167            
             RKI( NCELL,  167) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3900D-12,   3.6500D+02 )
!  Reaction Label R168            
             RKI( NCELL,  168) =   RKI( NCELL,   58 ) 
!  Reaction Label R169            
             RKI( NCELL,  169) =   RKI( NCELL,   70 ) 
!  Reaction Label R170            
             RKI( NCELL,  170) =   3.1000D-11 * CFACT 
!  Reaction Label R171            
             RKI( NCELL,  171) =   3.6000D-11 * CFACT 
!  Reaction Label R172            
             RKI( NCELL,  172) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5000D-11,   4.4900D+02 )
!  Reaction Label R173            
             RKI( NCELL,  173) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2000D-15,  -8.2100D+02 )
!  Reaction Label R174            
             RKI( NCELL,  174) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.7000D-12,   1.7500D+02 )
!  Reaction Label R171a           
             RKI( NCELL,  175) =   3.6000D-11 * CFACT 
!  Reaction Label R172a           
             RKI( NCELL,  176) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5000D-11,   4.4900D+02 )
!  Reaction Label R173a           
             RKI( NCELL,  177) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2000D-15,  -8.2100D+02 )
!  Reaction Label R174a           
             RKI( NCELL,  178) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.7000D-12,   1.7500D+02 )
!  Reaction Label R175            
             RKI( NCELL,  179) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3000D-12,  -1.9000D+02 )
!  Reaction Label R176            
             RKI( NCELL,  180) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R177            
             RKI( NCELL,  181) =   RKI( NCELL,   58 ) 
!  Reaction Label R178            
             RKI( NCELL,  182) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label R179            
             RKI( NCELL,  183) =   RKI( NCELL,   70 ) 
!  Reaction Label R180            
             RKI( NCELL,  184) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-12,   3.4000D+02 )
!  Reaction Label R181            
             RKI( NCELL,  185) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R182            
             RKI( NCELL,  186) =   RKI( NCELL,   58 ) 
!  Reaction Label R183            
             RKI( NCELL,  187) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label R184            
             RKI( NCELL,  188) =   RKI( NCELL,   70 ) 
!  Reaction Label R185            
             RKI( NCELL,  189) =   1.8500D-11 * CFACT 
!  Reaction Label R185a           
             RKI( NCELL,  190) =   1.8500D-11 * CFACT 
!  Reaction Label R186            
             RKI( NCELL,  191) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R187            
             RKI( NCELL,  192) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label R188            
             RKI( NCELL,  193) =   RKI( NCELL,   58 ) 
!  Reaction Label R189            
             RKI( NCELL,  194) =   RKI( NCELL,   70 ) 
!  Reaction Label R190            
             RKI( NCELL,  195) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7000D-12,   9.5000D+02 )
!  Reaction Label R191            
             RKI( NCELL,  196) =   1.4000D-11 * CFACT 
!  Reaction Label R192            
             RKI( NCELL,  197) =   2.1000D-12 * CFACT 
!  Reaction Label R193            
             RKI( NCELL,  198) =   5.5000D-12 * CFACT 
!  Reaction Label R194            
             RKI( NCELL,  199) =   1.5300D-12 * CFACT 
!  Reaction Label R195            
             RKI( NCELL,  200) =   3.8000D-12 * CFACT 
!  Reaction Label R198            
             RKI( NCELL,  203) =   9.0000D-11 * CFACT 
!  Reaction Label R199            
             RKI( NCELL,  204) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0800D-16,  -5.0000D+02 )
!  Reaction Label R200            
             RKI( NCELL,  205) =   3.0000D-12 * CFACT 
!  Reaction Label R202            
             RKI( NCELL,  207) =   4.4000D-11 * CFACT 
!  Reaction Label R203            
             RKI( NCELL,  208) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.4000D-17,  -5.0000D+02 )
!  Reaction Label R204            
             RKI( NCELL,  209) =   3.8000D-12 * CFACT 
!  Reaction Label R205            
             RKI( NCELL,  210) =   5.0000D-11 * CFACT 
!  Reaction Label R206            
             RKI( NCELL,  211) =   1.7000D-10 * CFACT 
!  Reaction Label R207            
             RKI( NCELL,  212) =   1.0000D-11 * CFACT 
!  Reaction Label R208            
             RKI( NCELL,  213) =   RKI( NCELL,   54 ) 
!  Reaction Label R209            
             RKI( NCELL,  214) =   RKI( NCELL,   55 ) 
!  Reaction Label R210            
             RKI( NCELL,  215) =   RKI( NCELL,   57 ) 
!  Reaction Label R211            
             RKI( NCELL,  216) =   RKI( NCELL,   59 ) 
!  Reaction Label R212            
             RKI( NCELL,  217) =   RKI( NCELL,   58 ) 
!  Reaction Label R213            
             RKI( NCELL,  218) =   3.6000D-11 * CFACT 
!  Reaction Label R214            
             RKI( NCELL,  219) =   3.0000D-12 * CFACT 
!  Reaction Label R216            
             RKI( NCELL,  220) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8500D-12,  -1.6900D+03 )
!  Reaction Label R217            
             RKI( NCELL,  221) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.3700D-21,   0.0000D+00,   0.0000D+00,  & 
     &                                                 4.3000D-01,   0.0000D+00,  -8.0000D+00,  & 
     &                                                 1.0000D+00,   4.1000D-01 )
!  Reaction Label R218            
             RKI( NCELL,  222) =   1.0000D+00 * SFACT 
!  Reaction Label R219            
             RKI( NCELL,  223) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.8100D-20,   0.0000D+00,   0.0000D+00,  & 
     &                                                 4.3000D-01,   0.0000D+00,  -8.0000D+00,  & 
     &                                                 1.0000D+00,   4.1000D-01 )
!  Reaction Label R220            
             RKI( NCELL,  224) =   1.0000D+00 * SFACT 
!  Reaction Label CL3             
             RKI( NCELL,  227) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3000D-11,  -2.0000D+02 )
!  Reaction Label CL4             
             RKI( NCELL,  228) =   1.6300D-14 * CFACT 
!  Reaction Label CL5             
             RKI( NCELL,  229) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.4000D-12,   2.9000D+02 )
!  Reaction Label CL6             
             RKI( NCELL,  230) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2000D-12,   3.4000D+02 )
!  Reaction Label CL7             
             RKI( NCELL,  231) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.2000D-12,  -1.1000D+02 )
!  Reaction Label CL8             
             RKI( NCELL,  232) =   5.0000D-13 * CFACT 
!  Reaction Label CL10            
             RKI( NCELL,  234) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.6000D-12,  -1.2400D+03 )
!  Reaction Label CL11            
             RKI( NCELL,  235) =   5.0000D-11 * CFACT 
!  Reaction Label CL12            
             RKI( NCELL,  236) =   1.4000D-10 * CFACT 
!  Reaction Label CL13            
             RKI( NCELL,  237) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.3000D-11,  -1.0000D+02 )
!  Reaction Label CL14            
             RKI( NCELL,  238) =   1.0700D-10 * CFACT 
!  Reaction Label CL15            
             RKI( NCELL,  239) =   2.5000D-10 * CFACT 
!  Reaction Label CL16            
             RKI( NCELL,  240) =   3.5000D-10 * CFACT 
!  Reaction Label CL17            
             RKI( NCELL,  241) =   4.3000D-10 * CFACT 
!  Reaction Label CL18            
             RKI( NCELL,  242) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.2000D-11,  -3.4000D+01 )
!  Reaction Label CL19            
             RKI( NCELL,  243) =   7.9000D-11 * CFACT 
!  Reaction Label CL20            
             RKI( NCELL,  244) =   1.3000D-10 * CFACT 
!  Reaction Label CL21            
             RKI( NCELL,  245) =   5.5000D-11 * CFACT 
!  Reaction Label CL22            
             RKI( NCELL,  246) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.2000D-11,   4.5000D+01 )
!  Reaction Label CL23            
             RKI( NCELL,  247) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   6.5800D-13,   5.8000D+01,   1.1600D+00 )
!  Reaction Label CL24            
             RKI( NCELL,  248) =   6.1000D-11 * CFACT 
!  Reaction Label CL25            
             RKI( NCELL,  249) =   1.2000D-10 * CFACT 
!  Reaction Label CL26            
             RKI( NCELL,  250) =   1.2000D-10 * CFACT 
!  Reaction Label CL28            
             RKI( NCELL,  252) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.8000D-31,   0.0000D+00,  -3.4000D+00,  & 
     &                                                 1.5000D-11,   0.0000D+00,  -1.9000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label HET_CLNO3_WAJ   
             RKI( NCELL,  255) =  BLKHET(  NCELL, IK_HETERO_CLNO3_WAJ )
!  Reaction Label SA01            
             RKI( NCELL,  256) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label SA02            
             RKI( NCELL,  257) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label SA03            
             RKI( NCELL,  258) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label SA04            
             RKI( NCELL,  259) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label SA06            
             RKI( NCELL,  260) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label SA07            
             RKI( NCELL,  261) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label SA08            
             RKI( NCELL,  262) =   1.1600D-14 * CFACT 
!  Reaction Label SA09            
             RKI( NCELL,  263) =   1.9700D-10 * CFACT 
!  Reaction Label SA10            
             RKI( NCELL,  264) =   1.9000D-11 * CFACT 
!  Reaction Label SA11            
             RKI( NCELL,  265) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label SA12            
             RKI( NCELL,  266) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label SA13            
             RKI( NCELL,  267) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.7400D+02 )
!  Reaction Label HET_NTR2        
             RKI( NCELL,  268) =   1.4000D+00 * BLKHET( NCELL, IK_HETERO_NTR2 )
!  Reaction Label HET_N2O5IJ      
             RKI( NCELL,  269) =  BLKHET(  NCELL, IK_HETERO_N2O5IJ )
!  Reaction Label HET_N2O5K       
             RKI( NCELL,  270) =  BLKHET(  NCELL, IK_HETERO_N2O5K )
!  Reaction Label HET_H2NO3PIJA   
             RKI( NCELL,  271) =  BLKHET(  NCELL, IK_HETERO_H2NO3PAIJ )
!  Reaction Label HET_H2NO3PKA    
             RKI( NCELL,  272) =  BLKHET(  NCELL, IK_HETERO_H2NO3PAK )
!  Reaction Label HET_H2NO3PIB    
             RKI( NCELL,  273) =  BLKHET(  NCELL, IK_HETERO_H2NO3PBIJ )
!  Reaction Label HET_H2NO3PJB    
             RKI( NCELL,  274) =  BLKHET(  NCELL, IK_HETERO_H2NO3PBIJ )
!  Reaction Label HET_H2NO3PKB    
             RKI( NCELL,  275) =  BLKHET(  NCELL, IK_HETERO_H2NO3PBK )
!  Reaction Label HET_N02         
             RKI( NCELL,  276) =  BLKHET(  NCELL, IK_HETERO_NO2 )
!  Reaction Label HET_IEPOX       
             RKI( NCELL,  278) =  BLKHET(  NCELL, IK_HETERO_IEPOX )
!  Reaction Label HET_IEPOXOS     
             RKI( NCELL,  279) =  BLKHET(  NCELL, IK_HETERO_IEPOXOS )
!  Reaction Label HET_TETROL      
             RKI( NCELL,  280) =  BLKHET(  NCELL, IK_HETERO_TETROL )
!  Reaction Label HET_GLY         
             RKI( NCELL,  281) =  BLKHET(  NCELL, IK_HETERO_GLY )
!  Reaction Label HET_MGLY        
             RKI( NCELL,  282) =  BLKHET(  NCELL, IK_HETERO_MGLY )
!  Reaction Label BL18a           
             RKI( NCELL,  283) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label BL18b           
             RKI( NCELL,  284) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6500D-13,   1.3000D+03 )
!  Reaction Label BL18c           
             RKI( NCELL,  285) =   2.3000D-12 * CFACT 
!  Reaction Label BL18d           
             RKI( NCELL,  286) =   3.5000D-14 * CFACT 
!  Reaction Label CP07mtp         
             RKI( NCELL,  287) =   1.9200D-10 * CFACT 
!  Reaction Label BP70mtp         
             RKI( NCELL,  288) =   7.2000D-12 * CFACT 
!  Reaction Label HYD_MT          
             RKI( NCELL,  290) =   9.2590D-05 * SFACT 
!  Reaction Label OLIG_AROMATIC1  
             RKI( NCELL,  291) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_AROMATIC2  
             RKI( NCELL,  292) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_AROMATIC3  
             RKI( NCELL,  293) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ISOPRENE1  
             RKI( NCELL,  294) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ISOPRENE2  
             RKI( NCELL,  295) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_SESQT1     
             RKI( NCELL,  296) =   9.4882D-06 * SFACT 
!  Reaction Label RPOAGEPI        
             RKI( NCELL,  297) =   2.5000D-12 * CFACT 
!  Reaction Label RPOAGELI        
             RKI( NCELL,  298) =  BLKHET(  NCELL, IK_HETERO_PNCOMLI )
!  Reaction Label RPOAGEPJ        
             RKI( NCELL,  299) =   2.5000D-12 * CFACT 
!  Reaction Label RPOAGELJ        
             RKI( NCELL,  300) =  BLKHET(  NCELL, IK_HETERO_PNCOMLJ )
!  Reaction Label PCSOA           
             RKI( NCELL,  301) =   1.2500D-11 * CFACT 
!  Reaction Label POA_AGE1        
             RKI( NCELL,  302) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE2        
             RKI( NCELL,  303) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE3        
             RKI( NCELL,  304) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE4        
             RKI( NCELL,  305) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE5        
             RKI( NCELL,  306) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE6        
             RKI( NCELL,  307) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE7        
             RKI( NCELL,  308) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE8        
             RKI( NCELL,  309) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE9        
             RKI( NCELL,  310) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE10       
             RKI( NCELL,  311) =   4.0000D-11 * CFACT 
!  Reaction Label T01             
             RKI( NCELL,  312) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.4000D-12,   1.3500D+02 )
!  Reaction Label T02             
             RKI( NCELL,  313) =   5.5000D-16 * CFACT 
!  Reaction Label T03             
             RKI( NCELL,  314) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-11,  -1.6000D+03 )
!  Reaction Label TCL1            
             RKI( NCELL,  317) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.2000D-11,  -3.4000D+01 )
!  Reaction Label T06             
             RKI( NCELL,  318) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.7000D-12,   3.4500D+02 )
!  Reaction Label T07             
             RKI( NCELL,  319) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label T08             
             RKI( NCELL,  320) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-11,  -1.1000D+03 )
!  Reaction Label TCL2            
             RKI( NCELL,  322) =   7.9000D-11 * CFACT 
!  Reaction Label T10             
             RKI( NCELL,  323) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4800D-11,   4.4800D+02 )
!  Reaction Label T11             
             RKI( NCELL,  324) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3400D-14,  -2.2830D+03 )
!  Reaction Label T12             
             RKI( NCELL,  325) =   1.7900D-13 * CFACT 
!  Reaction Label TCL3            
             RKI( NCELL,  326) =   2.5100D-10 * CFACT 
!  Reaction Label T13             
             RKI( NCELL,  327) =   2.0000D-11 * CFACT 
!  Reaction Label T14             
             RKI( NCELL,  328) =   2.6100D-19 * CFACT 
!  Reaction Label T15             
             RKI( NCELL,  329) =   1.1500D-15 * CFACT 
!  Reaction Label TCL4            
             RKI( NCELL,  331) =   2.3700D-10 * CFACT 
!  Reaction Label T17             
             RKI( NCELL,  332) =   2.0000D-11 * CFACT 
!  Reaction Label T18             
             RKI( NCELL,  333) =   2.6100D-19 * CFACT 
!  Reaction Label T19             
             RKI( NCELL,  334) =   1.1500D-15 * CFACT 
!  Reaction Label TCL5            
             RKI( NCELL,  336) =   2.3700D-10 * CFACT 
!  Reaction Label T21             
             RKI( NCELL,  337) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-12,   3.4000D+02 )
!  Reaction Label TCL6            
             RKI( NCELL,  338) =   6.1000D-11 * CFACT 
!  Reaction Label HG1             
             RKI( NCELL,  339) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.1100D-18,  -1.2565D+03 )
!  Reaction Label HG2             
             RKI( NCELL,  340) =   2.6000D-18 * CFACT 
!  Reaction Label HG3             
             RKI( NCELL,  341) =   8.5000D-19 * CFACT 
!  Reaction Label HG4             
             RKI( NCELL,  342) =   7.7000D-14 * CFACT 
!  Reaction Label HG5             
             RKI( NCELL,  343) =  CFACT_SQU * ARRHENUIS_T03( INV_TEMP,  2.2500D-33,   6.8000D+02 )

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
             INDEX_O            = IOLD2NEW( INDEX_O           , 1 )
             INDEX_O3           = IOLD2NEW( INDEX_O3          , 1 )
             INDEX_NO3          = IOLD2NEW( INDEX_NO3         , 1 )
             INDEX_O1D          = IOLD2NEW( INDEX_O1D         , 1 )
             INDEX_OH           = IOLD2NEW( INDEX_OH          , 1 )
             INDEX_HO2          = IOLD2NEW( INDEX_HO2         , 1 )
             INDEX_H2O2         = IOLD2NEW( INDEX_H2O2        , 1 )
             INDEX_N2O5         = IOLD2NEW( INDEX_N2O5        , 1 )
             INDEX_HNO3         = IOLD2NEW( INDEX_HNO3        , 1 )
             INDEX_HONO         = IOLD2NEW( INDEX_HONO        , 1 )
             INDEX_PNA          = IOLD2NEW( INDEX_PNA         , 1 )
             INDEX_SO2          = IOLD2NEW( INDEX_SO2         , 1 )
             INDEX_SULF         = IOLD2NEW( INDEX_SULF        , 1 )
             INDEX_SULRXN       = IOLD2NEW( INDEX_SULRXN      , 1 )
             INDEX_C2O3         = IOLD2NEW( INDEX_C2O3        , 1 )
             INDEX_MEO2         = IOLD2NEW( INDEX_MEO2        , 1 )
             INDEX_RO2          = IOLD2NEW( INDEX_RO2         , 1 )
             INDEX_PAN          = IOLD2NEW( INDEX_PAN         , 1 )
             INDEX_PACD         = IOLD2NEW( INDEX_PACD        , 1 )
             INDEX_AACD         = IOLD2NEW( INDEX_AACD        , 1 )
             INDEX_CXO3         = IOLD2NEW( INDEX_CXO3        , 1 )
             INDEX_ALD2         = IOLD2NEW( INDEX_ALD2        , 1 )
             INDEX_XO2H         = IOLD2NEW( INDEX_XO2H        , 1 )
             INDEX_PANX         = IOLD2NEW( INDEX_PANX        , 1 )
             INDEX_FORM         = IOLD2NEW( INDEX_FORM        , 1 )
             INDEX_MEPX         = IOLD2NEW( INDEX_MEPX        , 1 )
             INDEX_MEOH         = IOLD2NEW( INDEX_MEOH        , 1 )
             INDEX_ROOH         = IOLD2NEW( INDEX_ROOH        , 1 )
             INDEX_XO2          = IOLD2NEW( INDEX_XO2         , 1 )
             INDEX_XO2N         = IOLD2NEW( INDEX_XO2N        , 1 )
             INDEX_NTR1         = IOLD2NEW( INDEX_NTR1        , 1 )
             INDEX_NTR2         = IOLD2NEW( INDEX_NTR2        , 1 )
             INDEX_FACD         = IOLD2NEW( INDEX_FACD        , 1 )
             INDEX_CO           = IOLD2NEW( INDEX_CO          , 1 )
             INDEX_HCO3         = IOLD2NEW( INDEX_HCO3        , 1 )
             INDEX_ALDX         = IOLD2NEW( INDEX_ALDX        , 1 )
             INDEX_GLYD         = IOLD2NEW( INDEX_GLYD        , 1 )
             INDEX_GLY          = IOLD2NEW( INDEX_GLY         , 1 )
             INDEX_MGLY         = IOLD2NEW( INDEX_MGLY        , 1 )
             INDEX_ETHA         = IOLD2NEW( INDEX_ETHA        , 1 )
             INDEX_ETOH         = IOLD2NEW( INDEX_ETOH        , 1 )
             INDEX_KET          = IOLD2NEW( INDEX_KET         , 1 )
             INDEX_PAR          = IOLD2NEW( INDEX_PAR         , 1 )
             INDEX_ACET         = IOLD2NEW( INDEX_ACET        , 1 )
             INDEX_PRPA         = IOLD2NEW( INDEX_PRPA        , 1 )
             INDEX_XPRP         = IOLD2NEW( INDEX_XPRP        , 1 )
             INDEX_XPAR         = IOLD2NEW( INDEX_XPAR        , 1 )
             INDEX_ROR          = IOLD2NEW( INDEX_ROR         , 1 )
             INDEX_ETHY         = IOLD2NEW( INDEX_ETHY        , 1 )
             INDEX_ETH          = IOLD2NEW( INDEX_ETH         , 1 )
             INDEX_OLE          = IOLD2NEW( INDEX_OLE         , 1 )
             INDEX_IOLE         = IOLD2NEW( INDEX_IOLE        , 1 )
             INDEX_ISOP         = IOLD2NEW( INDEX_ISOP        , 1 )
             INDEX_ISO2         = IOLD2NEW( INDEX_ISO2        , 1 )
             INDEX_ISOPRXN      = IOLD2NEW( INDEX_ISOPRXN     , 1 )
             INDEX_ISPD         = IOLD2NEW( INDEX_ISPD        , 1 )
             INDEX_INTR         = IOLD2NEW( INDEX_INTR        , 1 )
             INDEX_ISPX         = IOLD2NEW( INDEX_ISPX        , 1 )
             INDEX_HPLD         = IOLD2NEW( INDEX_HPLD        , 1 )
             INDEX_OPO3         = IOLD2NEW( INDEX_OPO3        , 1 )
             INDEX_EPOX         = IOLD2NEW( INDEX_EPOX        , 1 )
             INDEX_EPX2         = IOLD2NEW( INDEX_EPX2        , 1 )
             INDEX_TERP         = IOLD2NEW( INDEX_TERP        , 1 )
             INDEX_TRPRXN       = IOLD2NEW( INDEX_TRPRXN      , 1 )
             INDEX_TERPNRO2     = IOLD2NEW( INDEX_TERPNRO2    , 1 )
             INDEX_APIN         = IOLD2NEW( INDEX_APIN        , 1 )
             INDEX_BENZENE      = IOLD2NEW( INDEX_BENZENE     , 1 )
             INDEX_CRES         = IOLD2NEW( INDEX_CRES        , 1 )
             INDEX_BZO2         = IOLD2NEW( INDEX_BZO2        , 1 )
             INDEX_OPEN         = IOLD2NEW( INDEX_OPEN        , 1 )
             INDEX_BENZRO2      = IOLD2NEW( INDEX_BENZRO2     , 1 )
             INDEX_TOL          = IOLD2NEW( INDEX_TOL         , 1 )
             INDEX_TO2          = IOLD2NEW( INDEX_TO2         , 1 )
             INDEX_TOLRO2       = IOLD2NEW( INDEX_TOLRO2      , 1 )
             INDEX_XOPN         = IOLD2NEW( INDEX_XOPN        , 1 )
             INDEX_XYLMN        = IOLD2NEW( INDEX_XYLMN       , 1 )
             INDEX_XLO2         = IOLD2NEW( INDEX_XLO2        , 1 )
             INDEX_XYLRO2       = IOLD2NEW( INDEX_XYLRO2      , 1 )
             INDEX_NAPH         = IOLD2NEW( INDEX_NAPH        , 1 )
             INDEX_PAHRO2       = IOLD2NEW( INDEX_PAHRO2      , 1 )
             INDEX_CRO          = IOLD2NEW( INDEX_CRO         , 1 )
             INDEX_CAT1         = IOLD2NEW( INDEX_CAT1        , 1 )
             INDEX_CRON         = IOLD2NEW( INDEX_CRON        , 1 )
             INDEX_OPAN         = IOLD2NEW( INDEX_OPAN        , 1 )
             INDEX_ECH4         = IOLD2NEW( INDEX_ECH4        , 1 )
             INDEX_CL2          = IOLD2NEW( INDEX_CL2         , 1 )
             INDEX_CL           = IOLD2NEW( INDEX_CL          , 1 )
             INDEX_HOCL         = IOLD2NEW( INDEX_HOCL        , 1 )
             INDEX_CLO          = IOLD2NEW( INDEX_CLO         , 1 )
             INDEX_FMCL         = IOLD2NEW( INDEX_FMCL        , 1 )
             INDEX_HCL          = IOLD2NEW( INDEX_HCL         , 1 )
             INDEX_CLNO2        = IOLD2NEW( INDEX_CLNO2       , 1 )
             INDEX_CLNO3        = IOLD2NEW( INDEX_CLNO3       , 1 )
             INDEX_SVAVB2       = IOLD2NEW( INDEX_SVAVB2      , 1 )
             INDEX_SVAVB3       = IOLD2NEW( INDEX_SVAVB3      , 1 )
             INDEX_SVAVB4       = IOLD2NEW( INDEX_SVAVB4      , 1 )
             INDEX_SVAVB1       = IOLD2NEW( INDEX_SVAVB1      , 1 )
             INDEX_SESQ         = IOLD2NEW( INDEX_SESQ        , 1 )
             INDEX_SESQRXN      = IOLD2NEW( INDEX_SESQRXN     , 1 )
             INDEX_SOAALK       = IOLD2NEW( INDEX_SOAALK      , 1 )
             INDEX_H2NO3PIJ     = IOLD2NEW( INDEX_H2NO3PIJ    , 1 )
             INDEX_H2NO3PK      = IOLD2NEW( INDEX_H2NO3PK     , 1 )
             INDEX_ACLI         = IOLD2NEW( INDEX_ACLI        , 1 )
             INDEX_ACLJ         = IOLD2NEW( INDEX_ACLJ        , 1 )
             INDEX_ACLK         = IOLD2NEW( INDEX_ACLK        , 1 )
             INDEX_IEPOXP       = IOLD2NEW( INDEX_IEPOXP      , 1 )
             INDEX_ASO4J        = IOLD2NEW( INDEX_ASO4J       , 1 )
             INDEX_AISO3J       = IOLD2NEW( INDEX_AISO3J      , 1 )
             INDEX_AGLYJ        = IOLD2NEW( INDEX_AGLYJ       , 1 )
             INDEX_MTNO3        = IOLD2NEW( INDEX_MTNO3       , 1 )
             INDEX_AMTNO3J      = IOLD2NEW( INDEX_AMTNO3J     , 1 )
             INDEX_AMTHYDJ      = IOLD2NEW( INDEX_AMTHYDJ     , 1 )
             INDEX_AAVB2J       = IOLD2NEW( INDEX_AAVB2J      , 1 )
             INDEX_AOLGAJ       = IOLD2NEW( INDEX_AOLGAJ      , 1 )
             INDEX_AAVB3J       = IOLD2NEW( INDEX_AAVB3J      , 1 )
             INDEX_AAVB4J       = IOLD2NEW( INDEX_AAVB4J      , 1 )
             INDEX_AISO1J       = IOLD2NEW( INDEX_AISO1J      , 1 )
             INDEX_AOLGBJ       = IOLD2NEW( INDEX_AOLGBJ      , 1 )
             INDEX_AISO2J       = IOLD2NEW( INDEX_AISO2J      , 1 )
             INDEX_ASQTJ        = IOLD2NEW( INDEX_ASQTJ       , 1 )
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
             INDEX_FORM_PRIMARY = IOLD2NEW( INDEX_FORM_PRIMARY, 1 )
             INDEX_ALD2_PRIMARY = IOLD2NEW( INDEX_ALD2_PRIMARY, 1 )
             INDEX_BUTADIENE13  = IOLD2NEW( INDEX_BUTADIENE13 , 1 )
             INDEX_ACROLEIN     = IOLD2NEW( INDEX_ACROLEIN    , 1 )
             INDEX_ACRO_PRIMARY = IOLD2NEW( INDEX_ACRO_PRIMARY, 1 )
             INDEX_TOLU         = IOLD2NEW( INDEX_TOLU        , 1 )
             INDEX_HG           = IOLD2NEW( INDEX_HG          , 1 )
             INDEX_HGIIAER      = IOLD2NEW( INDEX_HGIIAER     , 1 )
             INDEX_HGIIGAS      = IOLD2NEW( INDEX_HGIIGAS     , 1 )
          END SUBROUTINE RESET_SPECIES_POINTERS
       END MODULE RXNS_FUNCTION
