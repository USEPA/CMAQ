       MODULE RXNS_FUNCTION


       IMPLICIT NONE



! Name of Mechanism CRACMM1_AQ

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

!  Reaction Label R001            
                RKI( NCELL,    1) =  RJBLK( NCELL, IJ_O3O3P_NASA06 )
!  Reaction Label R002            
                RKI( NCELL,    2) =  RJBLK( NCELL, IJ_O3O1D_NASA06 )
!  Reaction Label R003            
                RKI( NCELL,    3) =  RJBLK( NCELL, IJ_H2O2_RACM2 )
!  Reaction Label R004            
                RKI( NCELL,    4) =  RJBLK( NCELL, IJ_NO2_RACM2 )
!  Reaction Label R005            
                RKI( NCELL,    5) =  RJBLK( NCELL, IJ_NO3NO_RACM2 )
!  Reaction Label R006            
                RKI( NCELL,    6) =  RJBLK( NCELL, IJ_NO3NO2_RACM2 )
!  Reaction Label R007            
                RKI( NCELL,    7) =  RJBLK( NCELL, IJ_HONO_RACM2 )
!  Reaction Label R008            
                RKI( NCELL,    8) =  RJBLK( NCELL, IJ_HNO3_RACM2 )
!  Reaction Label R009            
                RKI( NCELL,    9) =  RJBLK( NCELL, IJ_HNO4_RACM2 )
!  Reaction Label R010            
                RKI( NCELL,   10) =  RJBLK( NCELL, IJ_HCHO_MOL_JPL19 )
!  Reaction Label R011            
                RKI( NCELL,   11) =  RJBLK( NCELL, IJ_HCHO_RAD_JPL19 )
!  Reaction Label R012            
                RKI( NCELL,   12) =  RJBLK( NCELL, IJ_CH3CHO_RACM2 )
!  Reaction Label R013            
                RKI( NCELL,   13) =  RJBLK( NCELL, IJ_ALD_JPL19 )
!  Reaction Label R014            
                RKI( NCELL,   14) =  RJBLK( NCELL, IJ_CH3COCH3A_JPL19 )
!  Reaction Label R014a           
                RKI( NCELL,   15) =  RJBLK( NCELL, IJ_CH3COCH3B_JPL19 )
!  Reaction Label R015            
                RKI( NCELL,   16) =  RJBLK( NCELL, IJ_UALD_RACM2 )
!  Reaction Label TRP01           
                RKI( NCELL,   17) =  RJBLK( NCELL, IJ_ALD_JPL19 )
!  Reaction Label TRP02           
                RKI( NCELL,   18) =  RJBLK( NCELL, IJ_ALD_JPL19 )
!  Reaction Label R016            
                RKI( NCELL,   19) =  RJBLK( NCELL, IJ_MEK_JGR19 )
!  Reaction Label R017            
                RKI( NCELL,   20) =  RJBLK( NCELL, IJ_KET_JGR19 )
!  Reaction Label R018            
                RKI( NCELL,   21) =  RJBLK( NCELL, IJ_HKET_RACM2 )
!  Reaction Label R019            
                RKI( NCELL,   22) =  RJBLK( NCELL, IJ_MACR_RACM2 )
!  Reaction Label R020            
                RKI( NCELL,   23) =  RJBLK( NCELL, IJ_MVK_RACM2 )
!  Reaction Label R021            
                RKI( NCELL,   24) =  RJBLK( NCELL, IJ_GLYH2_RACM2 )
!  Reaction Label R022            
                RKI( NCELL,   25) =  RJBLK( NCELL, IJ_GLYF_RACM2 )
!  Reaction Label R023            
                RKI( NCELL,   26) =  RJBLK( NCELL, IJ_GLYHX_RACM2 )
!  Reaction Label R024            
                RKI( NCELL,   27) =  RJBLK( NCELL, IJ_MGLY_RACM2 )
!  Reaction Label R025            
                RKI( NCELL,   28) =  RJBLK( NCELL, IJ_MGLY_RACM2 )
!  Reaction Label R026            
                RKI( NCELL,   29) =  RJBLK( NCELL, IJ_MGLY_RACM2 )
!  Reaction Label R027            
                RKI( NCELL,   30) =  RJBLK( NCELL, IJ_BALD_RACM2 )
!  Reaction Label R028            
                RKI( NCELL,   31) =  RJBLK( NCELL, IJ_OP1_RACM2 )
!  Reaction Label R029            
                RKI( NCELL,   32) =  RJBLK( NCELL, IJ_OP1_RACM2 )
!  Reaction Label TRP03           
                RKI( NCELL,   33) =  RJBLK( NCELL, IJ_OP1_RACM2 )
!  Reaction Label R029a           
                RKI( NCELL,   34) =  RJBLK( NCELL, IJ_OP1_RACM2 )
!  Reaction Label R030            
                RKI( NCELL,   35) =  RJBLK( NCELL, IJ_PAA_RACM2 )
!  Reaction Label R031            
                RKI( NCELL,   36) =  RJBLK( NCELL, IJ_ONIT_RACM2 )
!  Reaction Label R032            
                RKI( NCELL,   37) =  RJBLK( NCELL, IJ_PAN1_RACM2 )
!  Reaction Label R033            
                RKI( NCELL,   38) =  RJBLK( NCELL, IJ_PAN2_RACM2 )
!  Reaction Label T20             
                RKI( NCELL,  386) =  RJBLK( NCELL, IJ_ACRO_09 )

                IF ( SEAWATER (NCELL) .GT. 0.001D0 ) THEN
!  Reaction Label HAL_Ozone       
                   RKI( NCELL,  415) = SEAWATER (NCELL) *  SFACT * HALOGEN_FALLOFF( BLKPRES( NCELL ),   6.7006D-11,   1.0743D+01,  & 
     &                                                           3.4153D-08,  -6.7130D-01,         2.0000D-06 )
                ELSE
                   RKI( NCELL,  415) = 0.0D0 
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

!  Reaction Label R034            
             RKI( NCELL,   39) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7000D-12,  -9.4000D+02 )
!  Reaction Label R035            
             RKI( NCELL,   40) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0000D-14,  -4.9000D+02 )
!  Reaction Label R036            
             RKI( NCELL,   41) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0000D-12,  -1.5000D+03 )
!  Reaction Label R037            
             RKI( NCELL,   42) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2000D-13,  -2.4500D+03 )
!  Reaction Label R038            
             RKI( NCELL,   43) =  CFACT_SQU * POWER_T02( TEMPOT300,   6.1000D-34,  -2.4000D+00 )
!  Reaction Label R039            
             RKI( NCELL,   44) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.0000D-12,  -2.0600D+03 )
!  Reaction Label R040            
             RKI( NCELL,   45) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.3000D-11,   5.5000D+01 )
!  Reaction Label R041            
             RKI( NCELL,   46) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.1500D-11,   1.1000D+02 )
!  Reaction Label R042            
             RKI( NCELL,   47) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6300D-10,   6.0000D+01 )
!  Reaction Label R043            
             RKI( NCELL,   48) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-12,  -1.8000D+03 )
!  Reaction Label R044            
             RKI( NCELL,   49) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.8000D-11,   2.5000D+02 )
!  Reaction Label R045            
             RKI( NCELL,   50) =  CFACT * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 3.0000D-13,   4.6000D+02,   2.1000D-33,  & 
     &                                                 9.2000D+02 )
!  Reaction Label R046            
             RKI( NCELL,   51) =  CFACT_SQU * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 4.2000D-34,   2.6600D+03,   2.9400D-54,  & 
     &                                                 3.1200D+03 )
!  Reaction Label R047            
             RKI( NCELL,   52) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8000D-12,   0.0000D+00 )
!  Reaction Label R048            
             RKI( NCELL,   53) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 9.1000D-32,   0.0000D+00,  -1.5000D+00,  & 
     &                                                 3.0000D-11,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R049            
             RKI( NCELL,   54) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 7.1000D-31,   0.0000D+00,  -2.6000D+00,  & 
     &                                                 3.6000D-11,   0.0000D+00,  -1.0000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R050            
             RKI( NCELL,   55) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4400D-12,   2.6000D+02 )
!  Reaction Label R051            
             RKI( NCELL,   56) =  CFACT * FALLOFF_T11( INV_TEMP,TEMPOT300,CAIR, & 
     &                                                 6.0950D-14,  -1.0000D+00,   2.7000D+02, &
     &                                                 6.8570D-34,   1.0000D+00,   2.7000D+02,  & 
     &                                                -5.9680D-14,   2.7000D+02 )
!  Reaction Label R052            
             RKI( NCELL,   57) =  CFACT_SQU * ARRHENUIS_T03( INV_TEMP,  4.2500D-39,   6.6350D+02 )
!  Reaction Label R053            
             RKI( NCELL,   58) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0000D-12,   2.5000D+02 )
!  Reaction Label R054            
             RKI( NCELL,   59) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.3000D-12,   2.0000D+02 )
!  Reaction Label R055            
             RKI( NCELL,   60) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 3.4000D-31,   0.0000D+00,  -1.6000D+00,  & 
     &                                                 2.3000D-11,   0.0000D+00,  -2.0000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R056            
             RKI( NCELL,   61) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.8000D-30,   0.0000D+00,  -3.0000D+00,  & 
     &                                                 2.8000D-11,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R057            
             RKI( NCELL,   62) =  CFACT * FALLOFF_T08( INV_TEMP,  CAIR, & 
     &                                                 2.4000D-14,   4.6000D+02,   2.7000D-17,  & 
     &                                                 2.1990D+03,   6.5000D-34,   1.3350D+03 )
!  Reaction Label R058            
             RKI( NCELL,   63) =   2.0000D-11 * CFACT 
!  Reaction Label R059            
             RKI( NCELL,   64) =   3.5000D-12 * CFACT 
!  Reaction Label R060            
             RKI( NCELL,   65) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7000D-11,   1.2500D+02 )
!  Reaction Label R061            
             RKI( NCELL,   66) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.3500D-14,  -1.3350D+03 )
!  Reaction Label R062            
             RKI( NCELL,   67) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5000D-13,  -2.4500D+03 )
!  Reaction Label R063            
             RKI( NCELL,   68) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.4000D-30,   0.0000D+00,  -3.0000D+00,  & 
     &                                                 1.6000D-12,   0.0000D+00,   1.0000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R064            
             RKI( NCELL,   69) =  RFACT * RKI( NCELL,   68 ) & 
     &                         * (  1.7241D+26 * DEXP( -1.0840D+04 * INV_TEMP) ) 
!  Reaction Label R065            
             RKI( NCELL,   70) =   1.0000D-22 * CFACT 
!  Reaction Label R066            
             RKI( NCELL,   71) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.9000D-31,   0.0000D+00,  -3.4000D+00,  & 
     &                                                 4.0000D-12,   0.0000D+00,  -3.0000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R067            
             RKI( NCELL,   72) =  RFACT * RKI( NCELL,   71 ) & 
     &                         * (  4.7619D+26 * DEXP( -1.0900D+04 * INV_TEMP) ) 
!  Reaction Label R068            
             RKI( NCELL,   73) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.5000D-13,   6.1000D+02 )
!  Reaction Label R069            
             RKI( NCELL,   74) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 2.9000D-31,   0.0000D+00,  -4.1000D+00,  & 
     &                                                 1.7000D-12,   0.0000D+00,   2.0000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R070            
             RKI( NCELL,   75) =  CFACT * FALLOFF_T09( INV_TEMP,  CAIR, & 
     &                                                 1.4400D-13,   0.0000D+00,   2.7400D-33,  & 
     &                                                 0.0000D+00 )
!  Reaction Label R071            
             RKI( NCELL,   76) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.4500D-12,  -1.7750D+03 )
!  Reaction Label R072            
             RKI( NCELL,   77) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.6600D-12,  -1.0200D+03 )
!  Reaction Label R073            
             RKI( NCELL,   78) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.6800D-12,  -3.7000D+02 )
!  Reaction Label R074            
             RKI( NCELL,   79) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0100D-11,  -2.4500D+02 )
!  Reaction Label R076            
             RKI( NCELL,   80) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.0000D-28,   0.0000D+00,  -4.5000D+00,  & 
     &                                                 8.8000D-12,   0.0000D+00,  -8.5000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R077            
             RKI( NCELL,   81) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.7200D-12,   5.0000D+02 )
!  Reaction Label R078            
             RKI( NCELL,   82) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3300D-11,   5.0000D+02 )
!  Reaction Label R080            
             RKI( NCELL,   83) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 5.5000D-30,   0.0000D+00,   0.0000D+00,  & 
     &                                                 8.3000D-13,   0.0000D+00,   2.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label ROCARO31        
             RKI( NCELL,   84) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3300D-12,  -1.9300D+02 )
!  Reaction Label ROCARO41        
             RKI( NCELL,   85) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8100D-12,   3.5400D+02 )
!  Reaction Label ROCARO51        
             RKI( NCELL,   86) =   2.3300D-11 * CFACT 
!  Reaction Label ROCARO61        
             RKI( NCELL,   87) =   7.1600D-12 * CFACT 
!  Reaction Label R086            
             RKI( NCELL,   88) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-11,   3.9000D+02 )
!  Reaction Label R087            
             RKI( NCELL,   89) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2100D-11,   4.4000D+02 )
!  Reaction Label R088            
             RKI( NCELL,   90) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.2000D-11,   4.0100D+02 )
!  Reaction Label TRP04           
             RKI( NCELL,   91) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.2000D-12,   6.0000D+02 )
!  Reaction Label TRP05           
             RKI( NCELL,   92) =   1.0000D-10 * CFACT 
!  Reaction Label R089            
             RKI( NCELL,   93) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.5000D-12,   1.2500D+02 )
!  Reaction Label R090            
             RKI( NCELL,   94) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.7000D-12,   3.4500D+02 )
!  Reaction Label R091            
             RKI( NCELL,   95) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.9000D-12,   4.0500D+02 )
!  Reaction Label R092            
             RKI( NCELL,   96) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   4.5600D-14,  -4.2700D+02,   3.6500D+00 )
!  Reaction Label R093            
             RKI( NCELL,   97) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5000D-12,  -9.0000D+01 )
!  Reaction Label R094            
             RKI( NCELL,   98) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-12,   1.0000D+01 )
!  Reaction Label R095            
             RKI( NCELL,   99) =   3.0000D-12 * CFACT 
!  Reaction Label R096            
             RKI( NCELL,  100) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.0000D-12,   3.8000D+02 )
!  Reaction Label R097            
             RKI( NCELL,  101) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   6.1000D+02 )
!  Reaction Label R098            
             RKI( NCELL,  102) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.7700D-12,   5.3300D+02 )
!  Reaction Label R099            
             RKI( NCELL,  103) =   1.1000D-11 * CFACT 
!  Reaction Label R100            
             RKI( NCELL,  104) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.2600D-13,   8.3000D+02 )
!  Reaction Label R101            
             RKI( NCELL,  105) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-11,   1.7500D+02 )
!  Reaction Label R102            
             RKI( NCELL,  106) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-11,   1.7500D+02 )
!  Reaction Label R103            
             RKI( NCELL,  107) =   1.0000D-11 * CFACT 
!  Reaction Label R104            
             RKI( NCELL,  108) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.3200D-12,   2.4300D+02 )
!  Reaction Label R105            
             RKI( NCELL,  109) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.7500D-12,   4.0500D+02 )
!  Reaction Label R106            
             RKI( NCELL,  110) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.6500D-11,   0.0000D+00 )
!  Reaction Label R108            
             RKI( NCELL,  111) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0500D-10,   0.0000D+00 )
!  Reaction Label R109            
             RKI( NCELL,  112) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8500D-12,  -3.4500D+02 )
!  Reaction Label R110            
             RKI( NCELL,  113) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0000D-12,   2.0000D+01 )
!  Reaction Label R111            
             RKI( NCELL,  114) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   2.0000D+02 )
!  Reaction Label R112            
             RKI( NCELL,  115) =   1.4700D-11 * CFACT 
!  Reaction Label R113            
             RKI( NCELL,  116) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D-12,   1.9000D+02 )
!  Reaction Label R114            
             RKI( NCELL,  117) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-12,   1.9000D+02 )
!  Reaction Label TRP06           
             RKI( NCELL,  118) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-12,   1.9000D+02 )
!  Reaction Label R114a           
             RKI( NCELL,  119) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-12,   1.9000D+02 )
!  Reaction Label R115            
             RKI( NCELL,  120) =   1.0000D-10 * CFACT 
!  Reaction Label R116            
             RKI( NCELL,  121) =   3.0000D-11 * CFACT 
!  Reaction Label R117            
             RKI( NCELL,  122) =   4.5000D-13 * CFACT 
!  Reaction Label R118            
             RKI( NCELL,  123) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.0000D-14,   8.5000D+02 )
!  Reaction Label R119            
             RKI( NCELL,  124) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9300D-12,   1.9000D+02 )
!  Reaction Label R120            
             RKI( NCELL,  125) =   4.0000D-14 * CFACT 
!  Reaction Label R121            
             RKI( NCELL,  126) =   4.0000D-14 * CFACT 
!  Reaction Label R122            
             RKI( NCELL,  127) =   3.2000D-11 * CFACT 
!  Reaction Label R123            
             RKI( NCELL,  128) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.3100D-12,  -2.6000D+02 )
!  Reaction Label TRP07           
             RKI( NCELL,  129) =   4.8000D-12 * CFACT 
!  Reaction Label R124            
             RKI( NCELL,  130) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.6000D-12,   2.7000D+02 )
!  Reaction Label R125            
             RKI( NCELL,  131) =   1.3000D-11 * CFACT 
!  Reaction Label R126            
             RKI( NCELL,  132) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.1400D-15,  -2.5800D+03 )
!  Reaction Label R127            
             RKI( NCELL,  133) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.3300D-15,  -1.8000D+03 )
!  Reaction Label R128            
             RKI( NCELL,  134) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-15,  -8.4500D+02 )
!  Reaction Label R130            
             RKI( NCELL,  135) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.8600D-15,  -1.9130D+03 )
!  Reaction Label R131            
             RKI( NCELL,  136) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.0000D-16,  -5.3000D+02 )
!  Reaction Label R132            
             RKI( NCELL,  137) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9500D-15,  -7.8300D+02 )
!  Reaction Label TRP08           
             RKI( NCELL,  138) =   8.3000D-18 * CFACT 
!  Reaction Label TRP09           
             RKI( NCELL,  139) =   1.6700D-16 * CFACT 
!  Reaction Label R133            
             RKI( NCELL,  140) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3600D-15,  -2.1120D+03 )
!  Reaction Label R134            
             RKI( NCELL,  141) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5000D-16,  -1.5200D+03 )
!  Reaction Label R135            
             RKI( NCELL,  142) =   1.6600D-18 * CFACT 
!  Reaction Label R136            
             RKI( NCELL,  143) =   2.0000D-16 * CFACT 
!  Reaction Label R137            
             RKI( NCELL,  144) =   2.0000D-16 * CFACT 
!  Reaction Label R138            
             RKI( NCELL,  145) =   9.0000D-17 * CFACT 
!  Reaction Label R140            
             RKI( NCELL,  146) =   2.8600D-13 * CFACT 
!  Reaction Label R141            
             RKI( NCELL,  147) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   4.3920D-13,  -2.2820D+03,   2.0000D+00 )
!  Reaction Label R142            
             RKI( NCELL,  148) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7900D-13,  -4.5000D+02 )
!  Reaction Label R143            
             RKI( NCELL,  149) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.6400D-13,   4.5000D+02 )
!  Reaction Label R145            
             RKI( NCELL,  150) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0300D-12,  -4.4600D+02 )
!  Reaction Label R146            
             RKI( NCELL,  151) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1900D-12,   4.9000D+02 )
!  Reaction Label R147            
             RKI( NCELL,  152) =   1.2200D-11 * CFACT 
!  Reaction Label TRP10           
             RKI( NCELL,  153) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.1500D-14,  -4.4800D+02 )
!  Reaction Label R148            
             RKI( NCELL,  154) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-12,  -2.4400D+03 )
!  Reaction Label R149            
             RKI( NCELL,  155) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.9000D+03 )
!  Reaction Label R150            
             RKI( NCELL,  156) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.7600D-12,  -1.9000D+03 )
!  Reaction Label R151            
             RKI( NCELL,  157) =   3.4000D-15 * CFACT 
!  Reaction Label R152            
             RKI( NCELL,  158) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.0200D-13,  -1.0760D+03 )
!  Reaction Label R153            
             RKI( NCELL,  159) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D-12,  -1.9000D+03 )
!  Reaction Label R154            
             RKI( NCELL,  160) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.7600D-12,  -1.9000D+03 )
!  Reaction Label R155            
             RKI( NCELL,  161) =   3.7800D-12 * CFACT 
!  Reaction Label R156            
             RKI( NCELL,  162) =   1.0600D-12 * CFACT 
!  Reaction Label R158            
             RKI( NCELL,  163) =   2.0100D-10 * CFACT 
!  Reaction Label R159            
             RKI( NCELL,  164) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2000D-14,  -5.0000D+02 )
!  Reaction Label TRP11           
             RKI( NCELL,  165) =   1.0000D+00 * SFACT 
!  Reaction Label TRP12           
             RKI( NCELL,  166) =   1.0000D+00 * SFACT 
!  Reaction Label R166            
             RKI( NCELL,  167) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 9.7000D-29,   0.0000D+00,  -5.6000D+00,  & 
     &                                                 9.3000D-12,   0.0000D+00,  -1.5000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R167            
             RKI( NCELL,  168) =  RFACT * RKI( NCELL,  167 ) & 
     &                         * (  1.1111D+28 * DEXP( -1.4000D+04 * INV_TEMP) ) 
!  Reaction Label R168            
             RKI( NCELL,  169) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 9.7000D-29,   0.0000D+00,  -5.6000D+00,  & 
     &                                                 9.3000D-12,   0.0000D+00,  -1.5000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label R169            
             RKI( NCELL,  170) =  RFACT * RKI( NCELL,  169 ) & 
     &                         * (  1.1111D+28 * DEXP( -1.4000D+04 * INV_TEMP) ) 
!  Reaction Label R170            
             RKI( NCELL,  171) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-12,   1.8100D+02 )
!  Reaction Label R171            
             RKI( NCELL,  172) =  SFACT * ARRHENUIS_T03( INV_TEMP,  1.6000D+16,  -1.3486D+04 )
!  Reaction Label R172            
             RKI( NCELL,  173) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-12,   3.0000D+02 )
!  Reaction Label R173            
             RKI( NCELL,  174) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.6500D+02 )
!  Reaction Label R174            
             RKI( NCELL,  175) =   4.0000D-12 * CFACT 
!  Reaction Label R175            
             RKI( NCELL,  176) =   4.0000D-12 * CFACT 
!  Reaction Label R177            
             RKI( NCELL,  177) =   9.0000D-12 * CFACT 
!  Reaction Label R178            
             RKI( NCELL,  178) =   4.0000D-12 * CFACT 
!  Reaction Label R179            
             RKI( NCELL,  179) =   4.0000D-12 * CFACT 
!  Reaction Label ROCARO33        
             RKI( NCELL,  180) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCARO43        
             RKI( NCELL,  181) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCARO53        
             RKI( NCELL,  182) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCARO63        
             RKI( NCELL,  183) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R188            
             RKI( NCELL,  184) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.4300D-12,   3.6000D+02 )
!  Reaction Label R189            
             RKI( NCELL,  185) =   4.0000D-12 * CFACT 
!  Reaction Label TRP13           
             RKI( NCELL,  186) =   4.0000D-12 * CFACT 
!  Reaction Label TRP14           
             RKI( NCELL,  187) =   4.0000D-12 * CFACT 
!  Reaction Label TRP15           
             RKI( NCELL,  188) =   4.0000D-12 * CFACT 
!  Reaction Label R190            
             RKI( NCELL,  189) =   4.0000D-12 * CFACT 
!  Reaction Label TRP16           
             RKI( NCELL,  190) =   4.0000D-12 * CFACT 
!  Reaction Label TRP17           
             RKI( NCELL,  191) =   4.0000D-12 * CFACT 
!  Reaction Label TRP18           
             RKI( NCELL,  192) =   4.0000D-12 * CFACT 
!  Reaction Label TRP19           
             RKI( NCELL,  193) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label TRP20           
             RKI( NCELL,  194) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R191            
             RKI( NCELL,  195) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.1000D-12,   2.7000D+02 )
!  Reaction Label R192            
             RKI( NCELL,  196) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.1000D-12,   2.7000D+02 )
!  Reaction Label R193            
             RKI( NCELL,  197) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9000D-12,   3.0000D+02 )
!  Reaction Label R194            
             RKI( NCELL,  198) =   4.0000D-12 * CFACT 
!  Reaction Label R195            
             RKI( NCELL,  199) =   4.0000D-12 * CFACT 
!  Reaction Label R196            
             RKI( NCELL,  200) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5400D-12,   3.6000D+02 )
!  Reaction Label R197            
             RKI( NCELL,  201) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5400D-12,   3.6000D+02 )
!  Reaction Label R198            
             RKI( NCELL,  202) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5400D-12,   3.6000D+02 )
!  Reaction Label R199            
             RKI( NCELL,  203) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5400D-12,   3.6000D+02 )
!  Reaction Label R200            
             RKI( NCELL,  204) =   4.0000D-12 * CFACT 
!  Reaction Label R201            
             RKI( NCELL,  205) =   4.0000D-12 * CFACT 
!  Reaction Label R202            
             RKI( NCELL,  206) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R203            
             RKI( NCELL,  207) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R204            
             RKI( NCELL,  208) =   4.0000D-12 * CFACT 
!  Reaction Label R205            
             RKI( NCELL,  209) =   4.0000D-12 * CFACT 
!  Reaction Label R206            
             RKI( NCELL,  210) =   4.0000D-12 * CFACT 
!  Reaction Label R207            
             RKI( NCELL,  211) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R208            
             RKI( NCELL,  212) =   4.0000D-12 * CFACT 
!  Reaction Label R209            
             RKI( NCELL,  213) =   2.0000D-11 * CFACT 
!  Reaction Label R210            
             RKI( NCELL,  214) =   2.0000D-11 * CFACT 
!  Reaction Label R211            
             RKI( NCELL,  215) =   2.0800D-12 * CFACT 
!  Reaction Label R212            
             RKI( NCELL,  216) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.1000D-13,   7.5000D+02 )
!  Reaction Label R213            
             RKI( NCELL,  217) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.5000D-13,   7.0000D+02 )
!  Reaction Label R214            
             RKI( NCELL,  218) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6600D-13,   1.3000D+03 )
!  Reaction Label R215            
             RKI( NCELL,  219) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6600D-13,   1.3000D+03 )
!  Reaction Label R217            
             RKI( NCELL,  220) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-13,   1.3000D+03 )
!  Reaction Label R218            
             RKI( NCELL,  221) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6600D-13,   1.3000D+03 )
!  Reaction Label R219            
             RKI( NCELL,  222) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6600D-13,   1.3000D+03 )
!  Reaction Label ROCARO32        
             RKI( NCELL,  223) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label ROCARO42        
             RKI( NCELL,  224) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label ROCARO52        
             RKI( NCELL,  225) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label ROCARO62        
             RKI( NCELL,  226) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label R228            
             RKI( NCELL,  227) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0500D-13,   1.3000D+03 )
!  Reaction Label R229            
             RKI( NCELL,  228) =   1.5000D-11 * CFACT 
!  Reaction Label TRP21           
             RKI( NCELL,  229) =   1.5000D-11 * CFACT 
!  Reaction Label TRP22           
             RKI( NCELL,  230) =   1.5000D-11 * CFACT 
!  Reaction Label TRP23           
             RKI( NCELL,  231) =   1.5000D-11 * CFACT 
!  Reaction Label R230            
             RKI( NCELL,  232) =   1.5000D-11 * CFACT 
!  Reaction Label TRP24           
             RKI( NCELL,  233) =   1.5000D-11 * CFACT 
!  Reaction Label TRP25           
             RKI( NCELL,  234) =   1.5000D-11 * CFACT 
!  Reaction Label TRP26           
             RKI( NCELL,  235) =   1.5000D-11 * CFACT 
!  Reaction Label TRP27           
             RKI( NCELL,  236) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label TRP28           
             RKI( NCELL,  237) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label R231            
             RKI( NCELL,  238) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.3000D-13,   1.0400D+03 )
!  Reaction Label R232            
             RKI( NCELL,  239) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.3000D-13,   1.0400D+03 )
!  Reaction Label R233            
             RKI( NCELL,  240) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1500D-13,   1.3000D+03 )
!  Reaction Label R234            
             RKI( NCELL,  241) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1500D-13,   1.3000D+03 )
!  Reaction Label R235            
             RKI( NCELL,  242) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1500D-13,   1.3000D+03 )
!  Reaction Label R236            
             RKI( NCELL,  243) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8200D-13,   1.3000D+03 )
!  Reaction Label R237            
             RKI( NCELL,  244) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8200D-13,   1.3000D+03 )
!  Reaction Label R238            
             RKI( NCELL,  245) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label R239            
             RKI( NCELL,  246) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label R240            
             RKI( NCELL,  247) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.7500D-13,   9.8000D+02 )
!  Reaction Label R241            
             RKI( NCELL,  248) =   1.0000D-11 * CFACT 
!  Reaction Label R242            
             RKI( NCELL,  249) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.7500D-13,   9.8000D+02 )
!  Reaction Label R243            
             RKI( NCELL,  250) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1500D-13,   1.3000D+03 )
!  Reaction Label R244            
             RKI( NCELL,  251) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6600D-13,   1.3000D+03 )
!  Reaction Label R245            
             RKI( NCELL,  252) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6600D-13,   1.3000D+03 )
!  Reaction Label R246            
             RKI( NCELL,  253) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.7500D-13,   9.8000D+02 )
!  Reaction Label R247            
             RKI( NCELL,  254) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6600D-13,   1.3000D+03 )
!  Reaction Label R248            
             RKI( NCELL,  255) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.5000D-14,   3.9000D+02 )
!  Reaction Label R249            
             RKI( NCELL,  256) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1800D-13,   1.5800D+02 )
!  Reaction Label R250            
             RKI( NCELL,  257) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.4600D-14,   4.3100D+02 )
!  Reaction Label R251            
             RKI( NCELL,  258) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0000D-13,   4.6700D+02 )
!  Reaction Label R253            
             RKI( NCELL,  259) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7100D-13,   7.0800D+02 )
!  Reaction Label R254            
             RKI( NCELL,  260) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4600D-13,   7.0800D+02 )
!  Reaction Label R255            
             RKI( NCELL,  261) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.1800D-14,   7.0800D+02 )
!  Reaction Label ROCARO35        
             RKI( NCELL,  262) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label ROCARO45        
             RKI( NCELL,  263) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label ROCARO55        
             RKI( NCELL,  264) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label ROCARO65        
             RKI( NCELL,  265) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label R264            
             RKI( NCELL,  266) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-14,   2.2100D+02 )
!  Reaction Label R265            
             RKI( NCELL,  267) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label TRP29           
             RKI( NCELL,  268) =   1.0000D-10 * CFACT 
!  Reaction Label TRP30           
             RKI( NCELL,  269) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label TRP31           
             RKI( NCELL,  270) =   1.0000D-10 * CFACT 
!  Reaction Label R266            
             RKI( NCELL,  271) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label TRP32           
             RKI( NCELL,  272) =   1.0000D-10 * CFACT 
!  Reaction Label TRP33           
             RKI( NCELL,  273) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label TRP34           
             RKI( NCELL,  274) =   1.0000D-10 * CFACT 
!  Reaction Label R267            
             RKI( NCELL,  275) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-11,   5.0000D+02 )
!  Reaction Label R268            
             RKI( NCELL,  276) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-11,   5.0000D+02 )
!  Reaction Label R269            
             RKI( NCELL,  277) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.5000D-13,   5.0000D+02 )
!  Reaction Label R270            
             RKI( NCELL,  278) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.9100D-13,   5.0800D+02 )
!  Reaction Label R271            
             RKI( NCELL,  279) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.9100D-13,   5.0800D+02 )
!  Reaction Label R272            
             RKI( NCELL,  280) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-14,   2.2100D+02 )
!  Reaction Label R273            
             RKI( NCELL,  281) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-14,   2.2100D+02 )
!  Reaction Label R274            
             RKI( NCELL,  282) =   8.3700D-14 * CFACT 
!  Reaction Label R275            
             RKI( NCELL,  283) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-14,   2.2100D+02 )
!  Reaction Label R276            
             RKI( NCELL,  284) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label R277            
             RKI( NCELL,  285) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label R278            
             RKI( NCELL,  286) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label R279            
             RKI( NCELL,  287) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label R280            
             RKI( NCELL,  288) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.5000D-13,   5.0000D+02 )
!  Reaction Label R281            
             RKI( NCELL,  289) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6000D-13,   7.0800D+02 )
!  Reaction Label R282            
             RKI( NCELL,  290) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.6800D-14,   7.0800D+02 )
!  Reaction Label R283            
             RKI( NCELL,  291) =   3.5600D-14 * CFACT 
!  Reaction Label R284            
             RKI( NCELL,  292) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.9900D-15,   1.5100D+03 )
!  Reaction Label R285            
             RKI( NCELL,  293) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0300D-12,   2.1100D+02 )
!  Reaction Label R286            
             RKI( NCELL,  294) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.9000D-13,   4.6000D+02 )
!  Reaction Label R287            
             RKI( NCELL,  295) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.5900D-13,   5.2200D+02 )
!  Reaction Label R289            
             RKI( NCELL,  296) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.4800D-13,   7.6500D+02 )
!  Reaction Label R290            
             RKI( NCELL,  297) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.1100D-13,   7.6500D+02 )
!  Reaction Label R291            
             RKI( NCELL,  298) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.0900D-13,   7.6500D+02 )
!  Reaction Label ROCARO36        
             RKI( NCELL,  299) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label ROCARO46        
             RKI( NCELL,  300) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label ROCARO56        
             RKI( NCELL,  301) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label ROCARO66        
             RKI( NCELL,  302) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label R300            
             RKI( NCELL,  303) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.4000D-14,   2.2100D+02 )
!  Reaction Label R301            
             RKI( NCELL,  304) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label TRP35           
             RKI( NCELL,  305) =   1.0000D-10 * CFACT 
!  Reaction Label TRP36           
             RKI( NCELL,  306) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label TRP37           
             RKI( NCELL,  307) =   1.0000D-10 * CFACT 
!  Reaction Label R302            
             RKI( NCELL,  308) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label TRP38           
             RKI( NCELL,  309) =   1.0000D-10 * CFACT 
!  Reaction Label TRP39           
             RKI( NCELL,  310) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label TRP40           
             RKI( NCELL,  311) =   1.0000D-10 * CFACT 
!  Reaction Label R303            
             RKI( NCELL,  312) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5000D-12,   5.0000D+02 )
!  Reaction Label R304            
             RKI( NCELL,  313) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5000D-12,   5.0000D+02 )
!  Reaction Label R305            
             RKI( NCELL,  314) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.5100D-13,   5.6500D+02 )
!  Reaction Label R306            
             RKI( NCELL,  315) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.5100D-13,   5.6500D+02 )
!  Reaction Label R307            
             RKI( NCELL,  316) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.5100D-13,   5.6500D+02 )
!  Reaction Label R308            
             RKI( NCELL,  317) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.4000D-14,   2.2100D+02 )
!  Reaction Label R309            
             RKI( NCELL,  318) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.4000D-14,   2.2100D+02 )
!  Reaction Label R310            
             RKI( NCELL,  319) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6800D-12,   5.0000D+02 )
!  Reaction Label R311            
             RKI( NCELL,  320) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6800D-12,   5.0000D+02 )
!  Reaction Label R312            
             RKI( NCELL,  321) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label R313            
             RKI( NCELL,  322) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label R314            
             RKI( NCELL,  323) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.0800D+02 )
!  Reaction Label R315            
             RKI( NCELL,  324) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.0800D+02 )
!  Reaction Label R316            
             RKI( NCELL,  325) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.5100D-13,   5.6500D+02 )
!  Reaction Label R317            
             RKI( NCELL,  326) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.8500D-13,   7.6500D+02 )
!  Reaction Label R318            
             RKI( NCELL,  327) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.3700D-13,   7.6500D+02 )
!  Reaction Label R319            
             RKI( NCELL,  328) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.0800D+02 )
!  Reaction Label R320            
             RKI( NCELL,  329) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.4000D-14,   1.5600D+03 )
!  Reaction Label R321            
             RKI( NCELL,  330) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5000D-12,   5.0000D+02 )
!  Reaction Label R322            
             RKI( NCELL,  331) =   1.2000D-12 * CFACT 
!  Reaction Label R323            
             RKI( NCELL,  332) =   1.2000D-12 * CFACT 
!  Reaction Label R324            
             RKI( NCELL,  333) =   1.2000D-12 * CFACT 
!  Reaction Label R325            
             RKI( NCELL,  334) =   1.2000D-12 * CFACT 
!  Reaction Label R327            
             RKI( NCELL,  335) =   1.2000D-12 * CFACT 
!  Reaction Label R328            
             RKI( NCELL,  336) =   1.2000D-12 * CFACT 
!  Reaction Label R329            
             RKI( NCELL,  337) =   1.2000D-12 * CFACT 
!  Reaction Label ROCARO34        
             RKI( NCELL,  338) =   2.3000D-12 * CFACT 
!  Reaction Label ROCARO44        
             RKI( NCELL,  339) =   2.3000D-12 * CFACT 
!  Reaction Label ROCARO54        
             RKI( NCELL,  340) =   2.3000D-12 * CFACT 
!  Reaction Label ROCARO64        
             RKI( NCELL,  341) =   2.3000D-12 * CFACT 
!  Reaction Label R338            
             RKI( NCELL,  342) =   1.2000D-12 * CFACT 
!  Reaction Label R339            
             RKI( NCELL,  343) =   1.2000D-12 * CFACT 
!  Reaction Label R340            
             RKI( NCELL,  344) =   1.2000D-12 * CFACT 
!  Reaction Label R341            
             RKI( NCELL,  345) =   4.0000D-12 * CFACT 
!  Reaction Label R342            
             RKI( NCELL,  346) =   4.0000D-12 * CFACT 
!  Reaction Label R343            
             RKI( NCELL,  347) =   1.2000D-12 * CFACT 
!  Reaction Label R344            
             RKI( NCELL,  348) =   1.2000D-12 * CFACT 
!  Reaction Label R345            
             RKI( NCELL,  349) =   1.2000D-12 * CFACT 
!  Reaction Label R346            
             RKI( NCELL,  350) =   1.2000D-12 * CFACT 
!  Reaction Label R347            
             RKI( NCELL,  351) =   1.2000D-12 * CFACT 
!  Reaction Label R348            
             RKI( NCELL,  352) =   2.5000D-12 * CFACT 
!  Reaction Label R349            
             RKI( NCELL,  353) =   2.5000D-12 * CFACT 
!  Reaction Label R350            
             RKI( NCELL,  354) =   2.5000D-12 * CFACT 
!  Reaction Label R351            
             RKI( NCELL,  355) =   2.5000D-12 * CFACT 
!  Reaction Label R352            
             RKI( NCELL,  356) =   1.2000D-12 * CFACT 
!  Reaction Label R353            
             RKI( NCELL,  357) =   1.2000D-12 * CFACT 
!  Reaction Label R354            
             RKI( NCELL,  358) =   1.2000D-12 * CFACT 
!  Reaction Label R355            
             RKI( NCELL,  359) =   1.2000D-12 * CFACT 
!  Reaction Label R356            
             RKI( NCELL,  360) =   1.2000D-12 * CFACT 
!  Reaction Label R357            
             RKI( NCELL,  361) =   1.2000D-12 * CFACT 
!  Reaction Label R358            
             RKI( NCELL,  362) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.0000D-14,   1.0000D+03 )
!  Reaction Label R359            
             RKI( NCELL,  363) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.2500D-14,   1.0000D+03 )
!  Reaction Label R360            
             RKI( NCELL,  364) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9600D-14,   1.0000D+03 )
!  Reaction Label R361            
             RKI( NCELL,  365) =   1.2000D-12 * CFACT 
!  Reaction Label R362            
             RKI( NCELL,  366) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5000D-12,   5.0000D+02 )
!  Reaction Label R363            
             RKI( NCELL,  367) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.1300D-17,   2.9500D+03 )
!  Reaction Label TRP41           
             RKI( NCELL,  368) =   1.0000D-10 * CFACT 
!  Reaction Label TRP42           
             RKI( NCELL,  369) =   1.0000D-10 * CFACT 
!  Reaction Label TRP43           
             RKI( NCELL,  370) =   1.0000D-10 * CFACT 
!  Reaction Label TRP44           
             RKI( NCELL,  371) =   1.0000D-10 * CFACT 
!  Reaction Label TRP45           
             RKI( NCELL,  372) =   1.0000D-10 * CFACT 
!  Reaction Label TRP46           
             RKI( NCELL,  373) =   1.0000D-10 * CFACT 
!  Reaction Label TRP47           
             RKI( NCELL,  374) =   1.0000D-10 * CFACT 
!  Reaction Label TRP48           
             RKI( NCELL,  375) =   1.0000D-10 * CFACT 
!  Reaction Label TRP49           
             RKI( NCELL,  376) =   1.0000D-10 * CFACT 
!  Reaction Label TRP50           
             RKI( NCELL,  377) =   1.0000D-10 * CFACT 
!  Reaction Label TRP51           
             RKI( NCELL,  378) =   1.0000D-10 * CFACT 
!  Reaction Label TRP52           
             RKI( NCELL,  379) =   1.0000D-10 * CFACT 
!  Reaction Label SA14            
             RKI( NCELL,  380) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.7800D-11,  -4.0000D+02 )
!  Reaction Label R001c           
             RKI( NCELL,  381) =   6.8900D-12 * CFACT 
!  Reaction Label R002c           
             RKI( NCELL,  382) =   6.5500D-14 * CFACT 
!  Reaction Label T17             
             RKI( NCELL,  383) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.0000D-12,   3.8000D+02 )
!  Reaction Label T18             
             RKI( NCELL,  384) =   2.9000D-19 * CFACT 
!  Reaction Label T19             
             RKI( NCELL,  385) =   3.4000D-15 * CFACT 
!  Reaction Label T10             
             RKI( NCELL,  387) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4800D-11,   4.4800D+02 )
!  Reaction Label T10a            
             RKI( NCELL,  388) =   9.0500D-12 * CFACT 
!  Reaction Label T10b            
             RKI( NCELL,  389) =   2.3000D-12 * CFACT 
!  Reaction Label T10c            
             RKI( NCELL,  390) =   1.6100D-11 * CFACT 
!  Reaction Label T10d            
             RKI( NCELL,  391) =   2.3900D-12 * CFACT 
!  Reaction Label T10e            
             RKI( NCELL,  392) =   1.3700D-11 * CFACT 
!  Reaction Label T11             
             RKI( NCELL,  393) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3400D-14,  -2.2830D+03 )
!  Reaction Label T12             
             RKI( NCELL,  394) =   1.0000D-13 * CFACT 
!  Reaction Label R003c           
             RKI( NCELL,  395) =   5.0100D-11 * CFACT 
!  Reaction Label R004c           
             RKI( NCELL,  396) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R005c           
             RKI( NCELL,  397) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.7500D-13,   9.8000D+02 )
!  Reaction Label R006c           
             RKI( NCELL,  398) =   4.4000D-11 * CFACT 
!  Reaction Label R007c           
             RKI( NCELL,  399) =   3.4300D-17 * CFACT 
!  Reaction Label R008c           
             RKI( NCELL,  400) =   8.9900D-12 * CFACT 
!  Reaction Label R010c           
             RKI( NCELL,  401) =   1.2000D-11 * CFACT 
!  Reaction Label R011c           
             RKI( NCELL,  402) =   1.9000D-11 * CFACT 
!  Reaction Label R012c           
             RKI( NCELL,  403) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8400D-13,   1.3000D+03 )
!  Reaction Label R013c           
             RKI( NCELL,  404) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label R014c           
             RKI( NCELL,  405) =   2.3000D-12 * CFACT 
!  Reaction Label R015c           
             RKI( NCELL,  406) =   1.2000D-14 * CFACT 
!  Reaction Label R016c           
             RKI( NCELL,  407) =   1.9700D-10 * CFACT 
!  Reaction Label R017c           
             RKI( NCELL,  408) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8400D-13,   1.3000D+03 )
!  Reaction Label R019c           
             RKI( NCELL,  409) =   2.3000D-12 * CFACT 
!  Reaction Label R020c           
             RKI( NCELL,  410) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label HET_GLY         
             RKI( NCELL,  411) =  BLKHET(  NCELL, IK_HETERO_GLY )
!  Reaction Label HET_MGLY        
             RKI( NCELL,  412) =  BLKHET(  NCELL, IK_HETERO_MGLY )
!  Reaction Label HET_N2O5        
             RKI( NCELL,  413) =  BLKHET(  NCELL, IK_HETERO_N2O5IJ )
!  Reaction Label HET_N02         
             RKI( NCELL,  414) =  BLKHET(  NCELL, IK_HETERO_NO2 )
!  Reaction Label HET_IEPOX       
             RKI( NCELL,  416) =  BLKHET(  NCELL, IK_HETERO_IEPOX )
!  Reaction Label HET_ISO3TET     
             RKI( NCELL,  417) =  BLKHET(  NCELL, IK_HETERO_ISO3NOSJ )
!  Reaction Label HET_IEPOXOS     
             RKI( NCELL,  418) =  BLKHET(  NCELL, IK_HETERO_ISO3OSJ )
!  Reaction Label ROCALK1c        
             RKI( NCELL,  419) =   1.5300D-11 * CFACT 
!  Reaction Label ROCALK2c        
             RKI( NCELL,  420) =   1.6800D-11 * CFACT 
!  Reaction Label ROCALK3c        
             RKI( NCELL,  421) =   2.2400D-11 * CFACT 
!  Reaction Label ROCALK4c        
             RKI( NCELL,  422) =   2.6700D-11 * CFACT 
!  Reaction Label ROCALK5c        
             RKI( NCELL,  423) =   3.0900D-11 * CFACT 
!  Reaction Label ROCALK6c        
             RKI( NCELL,  424) =   3.3800D-11 * CFACT 
!  Reaction Label HC1001          
             RKI( NCELL,  425) =   1.1000D-11 * CFACT 
!  Reaction Label ROCALK7c        
             RKI( NCELL,  426) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK8c        
             RKI( NCELL,  427) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK9c        
             RKI( NCELL,  428) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK10c       
             RKI( NCELL,  429) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK11c       
             RKI( NCELL,  430) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK12c       
             RKI( NCELL,  431) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label HC1002          
             RKI( NCELL,  432) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK13c       
             RKI( NCELL,  433) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK14c       
             RKI( NCELL,  434) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK15c       
             RKI( NCELL,  435) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK16c       
             RKI( NCELL,  436) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK17c       
             RKI( NCELL,  437) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK18c       
             RKI( NCELL,  438) =   2.3000D-12 * CFACT 
!  Reaction Label HC1003          
             RKI( NCELL,  439) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK19c       
             RKI( NCELL,  440) =   2.1700D-11 * CFACT 
!  Reaction Label ROCALK20c       
             RKI( NCELL,  441) =   2.2000D-11 * CFACT 
!  Reaction Label ROCALK21c       
             RKI( NCELL,  442) =   2.2500D-11 * CFACT 
!  Reaction Label ROCALK22c       
             RKI( NCELL,  443) =   2.2600D-11 * CFACT 
!  Reaction Label ROCALK23c       
             RKI( NCELL,  444) =   2.2700D-11 * CFACT 
!  Reaction Label ROCALK24c       
             RKI( NCELL,  445) =   2.2700D-11 * CFACT 
!  Reaction Label HC1004          
             RKI( NCELL,  446) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6600D-13,   1.3000D+03 )
!  Reaction Label ROCALK25c       
             RKI( NCELL,  447) =   1.8800D-01 * SFACT 
!  Reaction Label ROCALK26c       
             RKI( NCELL,  448) =   1.8800D-01 * SFACT 
!  Reaction Label ROCALK27c       
             RKI( NCELL,  449) =   1.8800D-01 * SFACT 
!  Reaction Label ROCALK28c       
             RKI( NCELL,  450) =   1.8800D-01 * SFACT 
!  Reaction Label ROCALK29c       
             RKI( NCELL,  451) =   1.8800D-01 * SFACT 
!  Reaction Label ROCALK30c       
             RKI( NCELL,  452) =   1.8800D-01 * SFACT 
!  Reaction Label HC1005          
             RKI( NCELL,  453) =   1.8800D-01 * SFACT 
!  Reaction Label ROCALK31c       
             RKI( NCELL,  454) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK32c       
             RKI( NCELL,  455) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK33c       
             RKI( NCELL,  456) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK34c       
             RKI( NCELL,  457) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK35c       
             RKI( NCELL,  458) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK36c       
             RKI( NCELL,  459) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label HC1006          
             RKI( NCELL,  460) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCALK37c       
             RKI( NCELL,  461) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK38c       
             RKI( NCELL,  462) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK39c       
             RKI( NCELL,  463) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK40c       
             RKI( NCELL,  464) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK41c       
             RKI( NCELL,  465) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK42c       
             RKI( NCELL,  466) =   2.3000D-12 * CFACT 
!  Reaction Label HC1007          
             RKI( NCELL,  467) =   2.3000D-12 * CFACT 
!  Reaction Label ROCALK43c       
             RKI( NCELL,  468) =   2.1700D-11 * CFACT 
!  Reaction Label ROCALK44c       
             RKI( NCELL,  469) =   2.2000D-11 * CFACT 
!  Reaction Label ROCALK45c       
             RKI( NCELL,  470) =   2.2500D-11 * CFACT 
!  Reaction Label ROCALK46c       
             RKI( NCELL,  471) =   2.2600D-11 * CFACT 
!  Reaction Label ROCALK47c       
             RKI( NCELL,  472) =   2.2700D-11 * CFACT 
!  Reaction Label ROCALK48c       
             RKI( NCELL,  473) =   2.2700D-11 * CFACT 
!  Reaction Label HC1008          
             RKI( NCELL,  474) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6600D-13,   1.3000D+03 )
!  Reaction Label ROCARO01        
             RKI( NCELL,  475) =   1.8100D-11 * CFACT 
!  Reaction Label ROCARO02        
             RKI( NCELL,  476) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label ROCARO03        
             RKI( NCELL,  477) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCARO04        
             RKI( NCELL,  478) =   2.3000D-12 * CFACT 
!  Reaction Label ROCARO05        
             RKI( NCELL,  479) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label ROCARO06        
             RKI( NCELL,  480) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label ROCARO11        
             RKI( NCELL,  481) =   1.8100D-11 * CFACT 
!  Reaction Label ROCARO12        
             RKI( NCELL,  482) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label ROCARO13        
             RKI( NCELL,  483) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCARO14        
             RKI( NCELL,  484) =   2.3000D-12 * CFACT 
!  Reaction Label ROCARO15        
             RKI( NCELL,  485) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label ROCARO16        
             RKI( NCELL,  486) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label ROCARO21        
             RKI( NCELL,  487) =   2.3100D-11 * CFACT 
!  Reaction Label ROCARO22        
             RKI( NCELL,  488) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.9100D-13,   1.3000D+03 )
!  Reaction Label ROCARO23        
             RKI( NCELL,  489) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.6000D+02 )
!  Reaction Label ROCARO24        
             RKI( NCELL,  490) =   2.3000D-12 * CFACT 
!  Reaction Label ROCARO25        
             RKI( NCELL,  491) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.5600D-14,   7.0800D+02 )
!  Reaction Label ROCARO26        
             RKI( NCELL,  492) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.4000D-13,   7.6500D+02 )
!  Reaction Label ROCOXY1c        
             RKI( NCELL,  493) =   5.9000D-11 * CFACT 
!  Reaction Label ROCOXY2c        
             RKI( NCELL,  494) =   6.0700D-11 * CFACT 
!  Reaction Label ROCOXY3c        
             RKI( NCELL,  495) =   5.5400D-11 * CFACT 
!  Reaction Label ROCOXY4c        
             RKI( NCELL,  496) =   5.6300D-11 * CFACT 
!  Reaction Label ROCOXY5c        
             RKI( NCELL,  497) =   5.4600D-11 * CFACT 
!  Reaction Label ROCOXY6c        
             RKI( NCELL,  498) =   4.5000D-11 * CFACT 
!  Reaction Label ROCOXY7c        
             RKI( NCELL,  499) =   5.1700D-11 * CFACT 
!  Reaction Label ROCOXY8c        
             RKI( NCELL,  500) =   4.7300D-11 * CFACT 
!  Reaction Label ROCOXY9c        
             RKI( NCELL,  501) =   4.6000D-11 * CFACT 
!  Reaction Label ROCOXY10c       
             RKI( NCELL,  502) =   3.8000D-11 * CFACT 
!  Reaction Label ROCOXY11c       
             RKI( NCELL,  503) =   3.9300D-11 * CFACT 
!  Reaction Label ROCOXY12c       
             RKI( NCELL,  504) =   3.5200D-11 * CFACT 
!  Reaction Label ROCOXY13c       
             RKI( NCELL,  505) =   3.1200D-11 * CFACT 
!  Reaction Label ROCOXY14c       
             RKI( NCELL,  506) =   2.4000D-11 * CFACT 
!  Reaction Label ROCOXY15c       
             RKI( NCELL,  507) =   2.0500D-11 * CFACT 
!  Reaction Label ROCOXY16c       
             RKI( NCELL,  508) =   4.6900D-11 * CFACT 

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


             INDEX_O3          = IOLD2NEW( INDEX_O3         , 1 )
             INDEX_O3P         = IOLD2NEW( INDEX_O3P        , 1 )
             INDEX_O1D         = IOLD2NEW( INDEX_O1D        , 1 )
             INDEX_H2O2        = IOLD2NEW( INDEX_H2O2       , 1 )
             INDEX_HO          = IOLD2NEW( INDEX_HO         , 1 )
             INDEX_NO2         = IOLD2NEW( INDEX_NO2        , 1 )
             INDEX_NO          = IOLD2NEW( INDEX_NO         , 1 )
             INDEX_NO3         = IOLD2NEW( INDEX_NO3        , 1 )
             INDEX_HONO        = IOLD2NEW( INDEX_HONO       , 1 )
             INDEX_HNO3        = IOLD2NEW( INDEX_HNO3       , 1 )
             INDEX_HNO4        = IOLD2NEW( INDEX_HNO4       , 1 )
             INDEX_HO2         = IOLD2NEW( INDEX_HO2        , 1 )
             INDEX_HCHO        = IOLD2NEW( INDEX_HCHO       , 1 )
             INDEX_CO          = IOLD2NEW( INDEX_CO         , 1 )
             INDEX_ACD         = IOLD2NEW( INDEX_ACD        , 1 )
             INDEX_MO2         = IOLD2NEW( INDEX_MO2        , 1 )
             INDEX_ALD         = IOLD2NEW( INDEX_ALD        , 1 )
             INDEX_ETHP        = IOLD2NEW( INDEX_ETHP       , 1 )
             INDEX_ACT         = IOLD2NEW( INDEX_ACT        , 1 )
             INDEX_ACO3        = IOLD2NEW( INDEX_ACO3       , 1 )
             INDEX_UALD        = IOLD2NEW( INDEX_UALD       , 1 )
             INDEX_KET         = IOLD2NEW( INDEX_KET        , 1 )
             INDEX_PINAL       = IOLD2NEW( INDEX_PINAL      , 1 )
             INDEX_HC10P       = IOLD2NEW( INDEX_HC10P      , 1 )
             INDEX_LIMAL       = IOLD2NEW( INDEX_LIMAL      , 1 )
             INDEX_MEK         = IOLD2NEW( INDEX_MEK        , 1 )
             INDEX_HKET        = IOLD2NEW( INDEX_HKET       , 1 )
             INDEX_MACR        = IOLD2NEW( INDEX_MACR       , 1 )
             INDEX_MACP        = IOLD2NEW( INDEX_MACP       , 1 )
             INDEX_XO2         = IOLD2NEW( INDEX_XO2        , 1 )
             INDEX_MVK         = IOLD2NEW( INDEX_MVK        , 1 )
             INDEX_GLY         = IOLD2NEW( INDEX_GLY        , 1 )
             INDEX_MGLY        = IOLD2NEW( INDEX_MGLY       , 1 )
             INDEX_DCB1        = IOLD2NEW( INDEX_DCB1       , 1 )
             INDEX_DCB2        = IOLD2NEW( INDEX_DCB2       , 1 )
             INDEX_BALD        = IOLD2NEW( INDEX_BALD       , 1 )
             INDEX_CHO         = IOLD2NEW( INDEX_CHO        , 1 )
             INDEX_OP1         = IOLD2NEW( INDEX_OP1        , 1 )
             INDEX_OP2         = IOLD2NEW( INDEX_OP2        , 1 )
             INDEX_OPB         = IOLD2NEW( INDEX_OPB        , 1 )
             INDEX_OP3         = IOLD2NEW( INDEX_OP3        , 1 )
             INDEX_PAA         = IOLD2NEW( INDEX_PAA        , 1 )
             INDEX_ONIT        = IOLD2NEW( INDEX_ONIT       , 1 )
             INDEX_PAN         = IOLD2NEW( INDEX_PAN        , 1 )
             INDEX_N2O5        = IOLD2NEW( INDEX_N2O5       , 1 )
             INDEX_SO2         = IOLD2NEW( INDEX_SO2        , 1 )
             INDEX_SULF        = IOLD2NEW( INDEX_SULF       , 1 )
             INDEX_SULRXN      = IOLD2NEW( INDEX_SULRXN     , 1 )
             INDEX_ETH         = IOLD2NEW( INDEX_ETH        , 1 )
             INDEX_HC3         = IOLD2NEW( INDEX_HC3        , 1 )
             INDEX_HC3P        = IOLD2NEW( INDEX_HC3P       , 1 )
             INDEX_ASOATJ      = IOLD2NEW( INDEX_ASOATJ     , 1 )
             INDEX_HC5         = IOLD2NEW( INDEX_HC5        , 1 )
             INDEX_HC5P        = IOLD2NEW( INDEX_HC5P       , 1 )
             INDEX_ETE         = IOLD2NEW( INDEX_ETE        , 1 )
             INDEX_ETEP        = IOLD2NEW( INDEX_ETEP       , 1 )
             INDEX_OLT         = IOLD2NEW( INDEX_OLT        , 1 )
             INDEX_OLTP        = IOLD2NEW( INDEX_OLTP       , 1 )
             INDEX_OLI         = IOLD2NEW( INDEX_OLI        , 1 )
             INDEX_OLIP        = IOLD2NEW( INDEX_OLIP       , 1 )
             INDEX_ACE         = IOLD2NEW( INDEX_ACE        , 1 )
             INDEX_ORA1        = IOLD2NEW( INDEX_ORA1       , 1 )
             INDEX_BEN         = IOLD2NEW( INDEX_BEN        , 1 )
             INDEX_BENP        = IOLD2NEW( INDEX_BENP       , 1 )
             INDEX_PHEN        = IOLD2NEW( INDEX_PHEN       , 1 )
             INDEX_TOL         = IOLD2NEW( INDEX_TOL        , 1 )
             INDEX_TOLP        = IOLD2NEW( INDEX_TOLP       , 1 )
             INDEX_CSL         = IOLD2NEW( INDEX_CSL        , 1 )
             INDEX_XYM         = IOLD2NEW( INDEX_XYM        , 1 )
             INDEX_XYMP        = IOLD2NEW( INDEX_XYMP       , 1 )
             INDEX_XYE         = IOLD2NEW( INDEX_XYE        , 1 )
             INDEX_XYEP        = IOLD2NEW( INDEX_XYEP       , 1 )
             INDEX_ISO         = IOLD2NEW( INDEX_ISO        , 1 )
             INDEX_ISOP        = IOLD2NEW( INDEX_ISOP       , 1 )
             INDEX_API         = IOLD2NEW( INDEX_API        , 1 )
             INDEX_APIP1       = IOLD2NEW( INDEX_APIP1      , 1 )
             INDEX_APIP2       = IOLD2NEW( INDEX_APIP2      , 1 )
             INDEX_LIM         = IOLD2NEW( INDEX_LIM        , 1 )
             INDEX_LIMP1       = IOLD2NEW( INDEX_LIMP1      , 1 )
             INDEX_LIMP2       = IOLD2NEW( INDEX_LIMP2      , 1 )
             INDEX_PINALP      = IOLD2NEW( INDEX_PINALP     , 1 )
             INDEX_RCO3        = IOLD2NEW( INDEX_RCO3       , 1 )
             INDEX_LIMALP      = IOLD2NEW( INDEX_LIMALP     , 1 )
             INDEX_ACTP        = IOLD2NEW( INDEX_ACTP       , 1 )
             INDEX_MEKP        = IOLD2NEW( INDEX_MEKP       , 1 )
             INDEX_KETP        = IOLD2NEW( INDEX_KETP       , 1 )
             INDEX_MCP         = IOLD2NEW( INDEX_MCP        , 1 )
             INDEX_MVKP        = IOLD2NEW( INDEX_MVKP       , 1 )
             INDEX_UALP        = IOLD2NEW( INDEX_UALP       , 1 )
             INDEX_DCB3        = IOLD2NEW( INDEX_DCB3       , 1 )
             INDEX_BALP        = IOLD2NEW( INDEX_BALP       , 1 )
             INDEX_ADDC        = IOLD2NEW( INDEX_ADDC       , 1 )
             INDEX_MCT         = IOLD2NEW( INDEX_MCT        , 1 )
             INDEX_MCTO        = IOLD2NEW( INDEX_MCTO       , 1 )
             INDEX_MOH         = IOLD2NEW( INDEX_MOH        , 1 )
             INDEX_EOH         = IOLD2NEW( INDEX_EOH        , 1 )
             INDEX_ROH         = IOLD2NEW( INDEX_ROH        , 1 )
             INDEX_ETEG        = IOLD2NEW( INDEX_ETEG       , 1 )
             INDEX_ISHP        = IOLD2NEW( INDEX_ISHP       , 1 )
             INDEX_IEPOX       = IOLD2NEW( INDEX_IEPOX      , 1 )
             INDEX_MAHP        = IOLD2NEW( INDEX_MAHP       , 1 )
             INDEX_ORA2        = IOLD2NEW( INDEX_ORA2       , 1 )
             INDEX_ORAP        = IOLD2NEW( INDEX_ORAP       , 1 )
             INDEX_PPN         = IOLD2NEW( INDEX_PPN        , 1 )
             INDEX_MPAN        = IOLD2NEW( INDEX_MPAN       , 1 )
             INDEX_TRPN        = IOLD2NEW( INDEX_TRPN       , 1 )
             INDEX_HOM         = IOLD2NEW( INDEX_HOM        , 1 )
             INDEX_NALD        = IOLD2NEW( INDEX_NALD       , 1 )
             INDEX_ISON        = IOLD2NEW( INDEX_ISON       , 1 )
             INDEX_MCTP        = IOLD2NEW( INDEX_MCTP       , 1 )
             INDEX_OLNN        = IOLD2NEW( INDEX_OLNN       , 1 )
             INDEX_OLND        = IOLD2NEW( INDEX_OLND       , 1 )
             INDEX_APINP1      = IOLD2NEW( INDEX_APINP1     , 1 )
             INDEX_APINP2      = IOLD2NEW( INDEX_APINP2     , 1 )
             INDEX_LIMNP1      = IOLD2NEW( INDEX_LIMNP1     , 1 )
             INDEX_LIMNP2      = IOLD2NEW( INDEX_LIMNP2     , 1 )
             INDEX_ADCN        = IOLD2NEW( INDEX_ADCN       , 1 )
             INDEX_VROCP4OXY2  = IOLD2NEW( INDEX_VROCP4OXY2 , 1 )
             INDEX_VROCN1OXY6  = IOLD2NEW( INDEX_VROCN1OXY6 , 1 )
             INDEX_FURANONE    = IOLD2NEW( INDEX_FURANONE   , 1 )
             INDEX_VROCP3OXY2  = IOLD2NEW( INDEX_VROCP3OXY2 , 1 )
             INDEX_VROCP0OXY4  = IOLD2NEW( INDEX_VROCP0OXY4 , 1 )
             INDEX_BAL1        = IOLD2NEW( INDEX_BAL1       , 1 )
             INDEX_BAL2        = IOLD2NEW( INDEX_BAL2       , 1 )
             INDEX_ELHOM       = IOLD2NEW( INDEX_ELHOM      , 1 )
             INDEX_VROCIOXY    = IOLD2NEW( INDEX_VROCIOXY   , 1 )
             INDEX_SLOWROC     = IOLD2NEW( INDEX_SLOWROC    , 1 )
             INDEX_ACRO        = IOLD2NEW( INDEX_ACRO       , 1 )
             INDEX_BDE13       = IOLD2NEW( INDEX_BDE13      , 1 )
             INDEX_BDE13P      = IOLD2NEW( INDEX_BDE13P     , 1 )
             INDEX_FURAN       = IOLD2NEW( INDEX_FURAN      , 1 )
             INDEX_FURANO2     = IOLD2NEW( INDEX_FURANO2    , 1 )
             INDEX_PROG        = IOLD2NEW( INDEX_PROG       , 1 )
             INDEX_SESQ        = IOLD2NEW( INDEX_SESQ       , 1 )
             INDEX_SESQNRO2    = IOLD2NEW( INDEX_SESQNRO2   , 1 )
             INDEX_VROCN2OXY2  = IOLD2NEW( INDEX_VROCN2OXY2 , 1 )
             INDEX_SESQRO2     = IOLD2NEW( INDEX_SESQRO2    , 1 )
             INDEX_VROCP0OXY2  = IOLD2NEW( INDEX_VROCP0OXY2 , 1 )
             INDEX_VROCP1OXY3  = IOLD2NEW( INDEX_VROCP1OXY3 , 1 )
             INDEX_AGLYJ       = IOLD2NEW( INDEX_AGLYJ      , 1 )
             INDEX_IEPOXP      = IOLD2NEW( INDEX_IEPOXP     , 1 )
             INDEX_AISO3NOSJ   = IOLD2NEW( INDEX_AISO3NOSJ  , 1 )
             INDEX_ASO4J       = IOLD2NEW( INDEX_ASO4J      , 1 )
             INDEX_AISO3OSJ    = IOLD2NEW( INDEX_AISO3OSJ   , 1 )
             INDEX_VROCP6ALK   = IOLD2NEW( INDEX_VROCP6ALK  , 1 )
             INDEX_VROCP6ALKP  = IOLD2NEW( INDEX_VROCP6ALKP , 1 )
             INDEX_VROCP5ALK   = IOLD2NEW( INDEX_VROCP5ALK  , 1 )
             INDEX_VROCP5ALKP  = IOLD2NEW( INDEX_VROCP5ALKP , 1 )
             INDEX_VROCP4ALK   = IOLD2NEW( INDEX_VROCP4ALK  , 1 )
             INDEX_VROCP4ALKP  = IOLD2NEW( INDEX_VROCP4ALKP , 1 )
             INDEX_VROCP3ALK   = IOLD2NEW( INDEX_VROCP3ALK  , 1 )
             INDEX_VROCP3ALKP  = IOLD2NEW( INDEX_VROCP3ALKP , 1 )
             INDEX_VROCP2ALK   = IOLD2NEW( INDEX_VROCP2ALK  , 1 )
             INDEX_VROCP2ALKP  = IOLD2NEW( INDEX_VROCP2ALKP , 1 )
             INDEX_VROCP1ALK   = IOLD2NEW( INDEX_VROCP1ALK  , 1 )
             INDEX_VROCP1ALKP  = IOLD2NEW( INDEX_VROCP1ALKP , 1 )
             INDEX_HC10        = IOLD2NEW( INDEX_HC10       , 1 )
             INDEX_VROCP6ALKP2 = IOLD2NEW( INDEX_VROCP6ALKP2, 1 )
             INDEX_VROCP5ALKP2 = IOLD2NEW( INDEX_VROCP5ALKP2, 1 )
             INDEX_VROCP4ALKP2 = IOLD2NEW( INDEX_VROCP4ALKP2, 1 )
             INDEX_VROCP2OXY2  = IOLD2NEW( INDEX_VROCP2OXY2 , 1 )
             INDEX_VROCP3ALKP2 = IOLD2NEW( INDEX_VROCP3ALKP2, 1 )
             INDEX_VROCP1OXY1  = IOLD2NEW( INDEX_VROCP1OXY1 , 1 )
             INDEX_VROCP2ALKP2 = IOLD2NEW( INDEX_VROCP2ALKP2, 1 )
             INDEX_VROCP1ALKP2 = IOLD2NEW( INDEX_VROCP1ALKP2, 1 )
             INDEX_VROCN1OXY1  = IOLD2NEW( INDEX_VROCN1OXY1 , 1 )
             INDEX_HC10P2      = IOLD2NEW( INDEX_HC10P2     , 1 )
             INDEX_VROCP6ARO   = IOLD2NEW( INDEX_VROCP6ARO  , 1 )
             INDEX_VROCP6AROP  = IOLD2NEW( INDEX_VROCP6AROP , 1 )
             INDEX_VROCN2OXY4  = IOLD2NEW( INDEX_VROCN2OXY4 , 1 )
             INDEX_VROCN1OXY3  = IOLD2NEW( INDEX_VROCN1OXY3 , 1 )
             INDEX_VROCP5ARO   = IOLD2NEW( INDEX_VROCP5ARO  , 1 )
             INDEX_VROCP5AROP  = IOLD2NEW( INDEX_VROCP5AROP , 1 )
             INDEX_NAPH        = IOLD2NEW( INDEX_NAPH       , 1 )
             INDEX_NAPHP       = IOLD2NEW( INDEX_NAPHP      , 1 )
             INDEX_VROCN2OXY8  = IOLD2NEW( INDEX_VROCN2OXY8 , 1 )
             INDEX_VROCP5OXY1  = IOLD2NEW( INDEX_VROCP5OXY1 , 1 )
             INDEX_VROCP6OXY1  = IOLD2NEW( INDEX_VROCP6OXY1 , 1 )
          END SUBROUTINE RESET_SPECIES_POINTERS
       END MODULE RXNS_FUNCTION
