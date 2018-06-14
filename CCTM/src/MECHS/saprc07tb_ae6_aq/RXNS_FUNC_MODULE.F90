       MODULE RXNS_FUNCTION


       IMPLICIT NONE



! Name of Mechanism SAPRC07TB_AE6_AQ

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
         REAL( 8 ), PARAMETER    :: MAX_RATE = 2.4D-06  ! Maximum loss rate (1/sec)
         REAL( 8 ), INTENT( IN ) :: PRESS
         REAL( 8 ), INTENT( IN ) :: A1
         REAL( 8 ), INTENT( IN ) :: B1
         REAL( 8 ), INTENT( IN ) :: A2
         REAL( 8 ), INTENT( IN ) :: B2
         INTRINSIC DEXP
         HALOGEN_FALLOFF = A1 * DEXP( B1 * PRESS ) + A2 * DEXP( B2 * PRESS )
         HALOGEN_FALLOFF = DMIN1 (MAX_RATE, HALOGEN_FALLOFF )
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


       REAL( 8 )  :: RO2NO           
       REAL( 8 )  :: RO2HO2          
       REAL( 8 )  :: RO2NO3          
       REAL( 8 )  :: RO2RO2          
       REAL( 8 )  :: RO2RO3          
       REAL( 8 )  :: RO2RO           
       REAL( 8 )  :: RO2XRO          
       REAL( 8 )  :: RO2RO2M         
       REAL( 8 )  :: RO22NN          

       DO NCELL = 1, NUMCELLS

! define special rate operators

           RO2NO            =  RKI( NCELL,   52 ) * Y( NCELL, IOLD2NEW(    2, NCS) )

           RO2HO2           =  RKI( NCELL,   53 ) * Y( NCELL, IOLD2NEW(   11, NCS) )

           RO2NO3           =  RKI( NCELL,   54 ) * Y( NCELL, IOLD2NEW(    5, NCS) )

           RO2RO2           =  RKI( NCELL,   55 ) * Y( NCELL, IOLD2NEW(   19, NCS) )  &
     &                      +  RKI( NCELL,   56 ) * Y( NCELL, IOLD2NEW(   23, NCS) )  &
     &                      +  RKI( NCELL,   56 ) * Y( NCELL, IOLD2NEW(   24, NCS) )

           RO2RO3           =  RKI( NCELL,   70 ) * Y( NCELL, IOLD2NEW(   25, NCS) )  &
     &                      +  RKI( NCELL,   70 ) * Y( NCELL, IOLD2NEW(   29, NCS) )  &
     &                      +  RKI( NCELL,   70 ) * Y( NCELL, IOLD2NEW(   36, NCS) )  &
     &                      +  RKI( NCELL,   70 ) * Y( NCELL, IOLD2NEW(   39, NCS) )

           RO2RO            =  RO2NO  &
     &                      +  RO2NO3  &
     &                      +  RO2RO3  &
     &                      +  5.0000D-01 * RO2RO2

           RO2XRO           =  RO2HO2  &
     &                      +  5.0000D-01 * RO2RO2

           RO2RO2M          =   5.0000D-01 * RO2RO2

           RO22NN           =  RO2NO3  &
     &                      +  RO2RO3  &
     &                      +  5.0000D-01 * RO2RO2


! define rate constants in terms of special rate operators 

           RKI( NCELL, 114 ) = RO2RO                         ! reaction: RO01
           RKI( NCELL, 115 ) = RO2XRO                        ! reaction: RO02
           RKI( NCELL, 116 ) = RO2RO                         ! reaction: RO03
           RKI( NCELL, 117 ) = RO2XRO                        ! reaction: RO04
           RKI( NCELL, 118 ) = RO2RO                         ! reaction: RO05
           RKI( NCELL, 119 ) = RO2XRO                        ! reaction: RO06
           RKI( NCELL, 120 ) = RO2RO                         ! reaction: RO07
           RKI( NCELL, 121 ) = RO2XRO                        ! reaction: RO08
           RKI( NCELL, 122 ) = RO2RO                         ! reaction: RO09
           RKI( NCELL, 123 ) = RO2XRO                        ! reaction: RO10
           RKI( NCELL, 124 ) = RO2RO                         ! reaction: RO11
           RKI( NCELL, 125 ) = RO2XRO                        ! reaction: RO12
           RKI( NCELL, 126 ) = RO2RO                         ! reaction: RO13
           RKI( NCELL, 127 ) = RO2XRO                        ! reaction: RO14
           RKI( NCELL, 128 ) = RO2RO                         ! reaction: RO15
           RKI( NCELL, 129 ) = RO2XRO                        ! reaction: RO16
           RKI( NCELL, 130 ) = RO2RO                         ! reaction: RO17
           RKI( NCELL, 131 ) = RO2XRO                        ! reaction: RO18
           RKI( NCELL, 213 ) = RO2RO                         ! reaction: PO01
           RKI( NCELL, 214 ) = RO2XRO                        ! reaction: PO02
           RKI( NCELL, 215 ) = RO2RO                         ! reaction: PO03
           RKI( NCELL, 216 ) = RO2XRO                        ! reaction: PO04
           RKI( NCELL, 217 ) = RO2RO                         ! reaction: PO05
           RKI( NCELL, 218 ) = RO2XRO                        ! reaction: PO06
           RKI( NCELL, 219 ) = RO2RO                         ! reaction: PO07
           RKI( NCELL, 220 ) = RO2XRO                        ! reaction: PO08
           RKI( NCELL, 221 ) = RO2RO                         ! reaction: PO09
           RKI( NCELL, 222 ) = RO2XRO                        ! reaction: PO10
           RKI( NCELL, 223 ) = RO2RO                         ! reaction: PO11
           RKI( NCELL, 224 ) = RO2XRO                        ! reaction: PO12
           RKI( NCELL, 225 ) = RO2RO                         ! reaction: PO13
           RKI( NCELL, 226 ) = RO2XRO                        ! reaction: PO14
           RKI( NCELL, 227 ) = RO2RO                         ! reaction: PO15
           RKI( NCELL, 228 ) = RO2XRO                        ! reaction: PO16
           RKI( NCELL, 229 ) = RO2RO                         ! reaction: PO17
           RKI( NCELL, 230 ) = RO2XRO                        ! reaction: PO18
           RKI( NCELL, 231 ) = RO2RO                         ! reaction: PO19
           RKI( NCELL, 232 ) = RO2XRO                        ! reaction: PO20
           RKI( NCELL, 233 ) = RO2RO                         ! reaction: PO21
           RKI( NCELL, 234 ) = RO2XRO                        ! reaction: PO22
           RKI( NCELL, 235 ) = RO2RO                         ! reaction: PO23
           RKI( NCELL, 236 ) = RO2XRO                        ! reaction: PO24
           RKI( NCELL, 237 ) = RO2RO                         ! reaction: PO25
           RKI( NCELL, 238 ) = RO2XRO                        ! reaction: PO26
           RKI( NCELL, 239 ) = RO2RO                         ! reaction: PO27
           RKI( NCELL, 240 ) = RO2XRO                        ! reaction: PO28
           RKI( NCELL, 241 ) = RO2RO                         ! reaction: PO29
           RKI( NCELL, 242 ) = RO2XRO                        ! reaction: PO30
           RKI( NCELL, 243 ) = RO2RO                         ! reaction: PO31
           RKI( NCELL, 244 ) = RO2XRO                        ! reaction: PO32
           RKI( NCELL, 245 ) = RO2RO                         ! reaction: PO33
           RKI( NCELL, 246 ) = RO2XRO                        ! reaction: PO34
           RKI( NCELL, 247 ) = RO2NO                         ! reaction: PO35
           RKI( NCELL, 248 ) = RO22NN                        ! reaction: PO36
           RKI( NCELL, 249 ) = RO2XRO                        ! reaction: PO37
           RKI( NCELL, 250 ) = RO2HO2                        ! reaction: PO38
           RKI( NCELL, 251 ) = RO2RO2M                       ! reaction: PO39
           RKI( NCELL, 252 ) = RO2RO                         ! reaction: PO40
           RKI( NCELL, 253 ) = RO2HO2                        ! reaction: PO41
           RKI( NCELL, 254 ) = RO2RO2M                       ! reaction: PO42
           RKI( NCELL, 255 ) = RO2RO                         ! reaction: PO43
           RKI( NCELL, 256 ) = RO2HO2                        ! reaction: PO41a
           RKI( NCELL, 257 ) = RO2RO2M                       ! reaction: PO42b
           RKI( NCELL, 258 ) = RO2RO                         ! reaction: PO43c
           RKI( NCELL, 259 ) = RO2HO2                        ! reaction: PO44
           RKI( NCELL, 260 ) = RO2RO2M                       ! reaction: PO45
           RKI( NCELL, 261 ) = RO2RO                         ! reaction: PO46
           RKI( NCELL, 262 ) = RO2RO                         ! reaction: PO47
           RKI( NCELL, 263 ) = RO2XRO                        ! reaction: PO48
           RKI( NCELL, 264 ) = RO2RO                         ! reaction: PO49
           RKI( NCELL, 265 ) = RO2XRO                        ! reaction: PO50
           RKI( NCELL, 375 ) = RO2RO                         ! reaction: CP23
           RKI( NCELL, 376 ) = RO2XRO                        ! reaction: CP24
           RKI( NCELL, 377 ) = RO2RO                         ! reaction: CP25
           RKI( NCELL, 378 ) = RO2XRO                        ! reaction: CP26
           RKI( NCELL, 379 ) = RO2RO                         ! reaction: CP27
           RKI( NCELL, 380 ) = RO2XRO                        ! reaction: CP28
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
                RKI( NCELL,  132) =  RJBLK( NCELL, IJ_HCHOR_06 )
!  Reaction Label BP02            
                RKI( NCELL,  133) =  RJBLK( NCELL, IJ_HCHOM_06 )
!  Reaction Label BP09            
                RKI( NCELL,  137) =  RJBLK( NCELL, IJ_CCHO_R )
!  Reaction Label BP12            
                RKI( NCELL,  140) =  RJBLK( NCELL, IJ_C2CHO )
!  Reaction Label BP15            
                RKI( NCELL,  143) =   5.0000D-01 * RJBLK( NCELL, IJ_ACET_06 )
!  Reaction Label BP17            
                RKI( NCELL,  145) =   1.7500D-01 * RJBLK( NCELL, IJ_MEK_06 )
!  Reaction Label BP23            
                RKI( NCELL,  151) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label BP25            
                RKI( NCELL,  153) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label BP27            
                RKI( NCELL,  155) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label BP29            
                RKI( NCELL,  157) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label BP30            
                RKI( NCELL,  158) =  RJBLK( NCELL, IJ_GLY_07R )
!  Reaction Label BP31            
                RKI( NCELL,  159) =  RJBLK( NCELL, IJ_GLY_07M )
!  Reaction Label BP34            
                RKI( NCELL,  162) =  RJBLK( NCELL, IJ_MGLY_06 )
!  Reaction Label BP37            
                RKI( NCELL,  165) =  RJBLK( NCELL, IJ_BACL_07 )
!  Reaction Label BP41            
                RKI( NCELL,  169) =   1.5000D-03 * RJBLK( NCELL, IJ_NO2_06 )
!  Reaction Label BP42            
                RKI( NCELL,  170) =   1.5000D-02 * RJBLK( NCELL, IJ_NO2_06 )
!  Reaction Label BP44            
                RKI( NCELL,  172) =   6.0000D-02 * RJBLK( NCELL, IJ_BALD_06 )
!  Reaction Label BP48            
                RKI( NCELL,  176) =  RJBLK( NCELL, IJ_AFG1 )
!  Reaction Label BP51            
                RKI( NCELL,  179) =  RJBLK( NCELL, IJ_AFG1 )
!  Reaction Label BP58            
                RKI( NCELL,  186) =  RJBLK( NCELL, IJ_MACR_06 )
!  Reaction Label BP63            
                RKI( NCELL,  190) =  RJBLK( NCELL, IJ_MVK_06 )
!  Reaction Label BP67            
                RKI( NCELL,  194) =  RJBLK( NCELL, IJ_MACR_06 )
!  Reaction Label BP69            
                RKI( NCELL,  196) =   4.8600D-03 * RJBLK( NCELL, IJ_MEK_06 )
!  Reaction Label BP71            
                RKI( NCELL,  198) =  RJBLK( NCELL, IJ_IC3ONO2 )
!  Reaction Label BP73            
                RKI( NCELL,  200) =  RJBLK( NCELL, IJ_HOCCHO_IUPAC )
!  Reaction Label BP79            
                RKI( NCELL,  206) =  RJBLK( NCELL, IJ_ACRO_09 )
!  Reaction Label BP81            
                RKI( NCELL,  208) =  RJBLK( NCELL, IJ_PAA )
!  Reaction Label IS92            
                RKI( NCELL,  285) =  RJBLK( NCELL, IJ_COOH )
!  Reaction Label CI01            
                RKI( NCELL,  330) =  RJBLK( NCELL, IJ_CL2 )
!  Reaction Label CI03            
                RKI( NCELL,  332) =  RJBLK( NCELL, IJ_CLNO_06 )
!  Reaction Label CI06            
                RKI( NCELL,  335) =  RJBLK( NCELL, IJ_CLONO )
!  Reaction Label CI07            
                RKI( NCELL,  336) =  RJBLK( NCELL, IJ_CLNO2 )
!  Reaction Label CI14            
                RKI( NCELL,  343) =  RJBLK( NCELL, IJ_CLONO2_1 )
!  Reaction Label CI15            
                RKI( NCELL,  344) =  RJBLK( NCELL, IJ_CLONO2_2 )
!  Reaction Label CI19            
                RKI( NCELL,  348) =  RJBLK( NCELL, IJ_HOCL_06 )
!  Reaction Label CP19            
                RKI( NCELL,  371) =  RJBLK( NCELL, IJ_CLCCHO )
!  Reaction Label CP22            
                RKI( NCELL,  374) =   5.0000D-01 * RJBLK( NCELL, IJ_CLACET )
!  Reaction Label TR01            
                RKI( NCELL,  414) =  RJBLK( NCELL, IJ_HCHOR_06 )
!  Reaction Label TR02            
                RKI( NCELL,  415) =  RJBLK( NCELL, IJ_HCHOM_06 )
!  Reaction Label TR08            
                RKI( NCELL,  420) =  RJBLK( NCELL, IJ_CCHO_R )
!  Reaction Label TR15            
                RKI( NCELL,  427) =  RJBLK( NCELL, IJ_ACRO_09 )

                IF( .NOT. LAND( NCELL ) )THEN
!  Reaction Label HAL_Ozone       
                   RKI( NCELL,  437) =  SFACT * HALOGEN_FALLOFF( BLKPRES( NCELL ),   1.0000D-40,   7.8426D+01,  & 
     &                                                           4.0582D-09,         5.8212D+00 )
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
!  Reaction Label BR54            
             RKI( NCELL,   99) =   RKI( NCELL,   76 ) 
!  Reaction Label BR55            
             RKI( NCELL,  100) =   RKI( NCELL,   67 ) 
!  Reaction Label BR56            
             RKI( NCELL,  101) =   RKI( NCELL,   54 ) 
!  Reaction Label BR57            
             RKI( NCELL,  102) =   RKI( NCELL,   69 ) 
!  Reaction Label BR58            
             RKI( NCELL,  103) =   RKI( NCELL,   70 ) 
!  Reaction Label BR59            
             RKI( NCELL,  104) =   RKI( NCELL,   70 ) 
!  Reaction Label BR60            
             RKI( NCELL,  105) =   RKI( NCELL,   72 ) 
!  Reaction Label BR61            
             RKI( NCELL,  106) =   RKI( NCELL,   72 ) 
!  Reaction Label BR62            
             RKI( NCELL,  107) =   RKI( NCELL,   72 ) 
!  Reaction Label BR63            
             RKI( NCELL,  108) =   RKI( NCELL,   72 ) 
!  Reaction Label BR64            
             RKI( NCELL,  109) =   2.4000D-11 * CFACT 
!  Reaction Label BR65            
             RKI( NCELL,  110) =  SFACT * ARRHENUIS_T03( INV_TEMP,  7.5000D+14,  -8.1520D+03 )
!  Reaction Label BR66            
             RKI( NCELL,  111) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3000D-11,   1.5000D+02 )
!  Reaction Label BR67            
             RKI( NCELL,  112) =   RKI( NCELL,   53 ) 
!  Reaction Label BR68            
             RKI( NCELL,  113) =   1.0000D-03 * SFACT 
! RKI for Reaction RO01 set in SPECIAL_RATES Routine

! RKI for Reaction RO02 set in SPECIAL_RATES Routine

! RKI for Reaction RO03 set in SPECIAL_RATES Routine

! RKI for Reaction RO04 set in SPECIAL_RATES Routine

! RKI for Reaction RO05 set in SPECIAL_RATES Routine

! RKI for Reaction RO06 set in SPECIAL_RATES Routine

! RKI for Reaction RO07 set in SPECIAL_RATES Routine

! RKI for Reaction RO08 set in SPECIAL_RATES Routine

! RKI for Reaction RO09 set in SPECIAL_RATES Routine

! RKI for Reaction RO10 set in SPECIAL_RATES Routine

! RKI for Reaction RO11 set in SPECIAL_RATES Routine

! RKI for Reaction RO12 set in SPECIAL_RATES Routine

! RKI for Reaction RO13 set in SPECIAL_RATES Routine

! RKI for Reaction RO14 set in SPECIAL_RATES Routine

! RKI for Reaction RO15 set in SPECIAL_RATES Routine

! RKI for Reaction RO16 set in SPECIAL_RATES Routine

! RKI for Reaction RO17 set in SPECIAL_RATES Routine

! RKI for Reaction RO18 set in SPECIAL_RATES Routine

!  Reaction Label BP03            
             RKI( NCELL,  134) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.4000D-12,   1.3500D+02 )
!  Reaction Label BP07            
             RKI( NCELL,  135) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-12,  -2.4310D+03 )
!  Reaction Label BP08            
             RKI( NCELL,  136) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-12,   3.6500D+02 )
!  Reaction Label BP10            
             RKI( NCELL,  138) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label BP11            
             RKI( NCELL,  139) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.1000D-12,   4.0500D+02 )
!  Reaction Label BP13            
             RKI( NCELL,  141) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.6010D+03 )
!  Reaction Label BP14            
             RKI( NCELL,  142) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   4.5600D-14,   4.2900D+02,   3.6500D+00 )
!  Reaction Label BP16            
             RKI( NCELL,  144) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.3000D-12,  -2.5000D+01,   2.0000D+00 )
!  Reaction Label BP18            
             RKI( NCELL,  146) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8500D-12,  -3.4500D+02 )
!  Reaction Label BP19            
             RKI( NCELL,  147) =   4.5000D-13 * CFACT 
!  Reaction Label BP20            
             RKI( NCELL,  148) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.2000D-14,   8.5500D+02 )
!  Reaction Label BP21            
             RKI( NCELL,  149) =   1.2000D-12 * CFACT 
!  Reaction Label BP22            
             RKI( NCELL,  150) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.8000D-12,   2.0000D+02 )
!  Reaction Label BP24            
             RKI( NCELL,  152) =   2.5000D-11 * CFACT 
!  Reaction Label BP26            
             RKI( NCELL,  154) =   5.6000D-11 * CFACT 
!  Reaction Label BP28            
             RKI( NCELL,  156) =   1.4100D-10 * CFACT 
!  Reaction Label BP32            
             RKI( NCELL,  160) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.1000D-12,   3.4220D+02 )
!  Reaction Label BP33            
             RKI( NCELL,  161) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-12,  -2.3900D+03 )
!  Reaction Label BP35            
             RKI( NCELL,  163) =   1.5000D-11 * CFACT 
!  Reaction Label BP36            
             RKI( NCELL,  164) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8950D+03 )
!  Reaction Label BP38            
             RKI( NCELL,  166) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7000D-12,   9.5000D+02 )
!  Reaction Label BP39            
             RKI( NCELL,  167) =   1.4000D-11 * CFACT 
!  Reaction Label BP40            
             RKI( NCELL,  168) =   3.5000D-12 * CFACT 
!  Reaction Label BP43            
             RKI( NCELL,  171) =   1.2000D-11 * CFACT 
!  Reaction Label BP45            
             RKI( NCELL,  173) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3400D-12,  -1.8600D+03 )
!  Reaction Label BP46            
             RKI( NCELL,  174) =   7.4000D-11 * CFACT 
!  Reaction Label BP47            
             RKI( NCELL,  175) =   9.6600D-18 * CFACT 
!  Reaction Label BP49            
             RKI( NCELL,  177) =   7.4000D-11 * CFACT 
!  Reaction Label BP50            
             RKI( NCELL,  178) =   9.6600D-18 * CFACT 
!  Reaction Label BP52            
             RKI( NCELL,  180) =   9.3500D-11 * CFACT 
!  Reaction Label BP53            
             RKI( NCELL,  181) =   1.4300D-17 * CFACT 
!  Reaction Label BP54            
             RKI( NCELL,  182) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.0000D-12,   3.8000D+02 )
!  Reaction Label BP55            
             RKI( NCELL,  183) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-15,  -2.1000D+03 )
!  Reaction Label BP56            
             RKI( NCELL,  184) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5000D-12,  -1.8150D+03 )
!  Reaction Label BP57            
             RKI( NCELL,  185) =   6.3400D-12 * CFACT 
!  Reaction Label BP59            
             RKI( NCELL,  187) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   6.1000D+02 )
!  Reaction Label BP60            
             RKI( NCELL,  188) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5000D-16,  -1.5200D+03 )
!  Reaction Label BP62            
             RKI( NCELL,  189) =   4.3200D-12 * CFACT 
!  Reaction Label BP64            
             RKI( NCELL,  191) =   6.1900D-11 * CFACT 
!  Reaction Label BP65            
             RKI( NCELL,  192) =   4.1800D-18 * CFACT 
!  Reaction Label BP66            
             RKI( NCELL,  193) =   1.0000D-13 * CFACT 
!  Reaction Label BP68            
             RKI( NCELL,  195) =   1.5500D-11 * CFACT 
!  Reaction Label BP70            
             RKI( NCELL,  197) =   7.2000D-12 * CFACT 
!  Reaction Label BP72            
             RKI( NCELL,  199) =   RKI( NCELL,  136 ) 
!  Reaction Label BP74            
             RKI( NCELL,  201) =   RKI( NCELL,  138 ) 
!  Reaction Label BP75            
             RKI( NCELL,  202) =   1.9900D-11 * CFACT 
!  Reaction Label BP76            
             RKI( NCELL,  203) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-15,  -2.5280D+03 )
!  Reaction Label BP77            
             RKI( NCELL,  204) =   1.1800D-15 * CFACT 
!  Reaction Label BP78            
             RKI( NCELL,  205) =   2.3700D-12 * CFACT 
!  Reaction Label BP80            
             RKI( NCELL,  207) =   5.2800D-12 * CFACT 
!  Reaction Label BP82            
             RKI( NCELL,  209) =   6.4200D-12 * CFACT 
!  Reaction Label BP84            
             RKI( NCELL,  210) =   RKI( NCELL,   76 ) 
!  Reaction Label BP85            
             RKI( NCELL,  211) =   RKI( NCELL,   73 ) 
!  Reaction Label BP86            
             RKI( NCELL,  212) =   RKI( NCELL,   67 ) 
! RKI for Reaction PO01 set in SPECIAL_RATES Routine

! RKI for Reaction PO02 set in SPECIAL_RATES Routine

! RKI for Reaction PO03 set in SPECIAL_RATES Routine

! RKI for Reaction PO04 set in SPECIAL_RATES Routine

! RKI for Reaction PO05 set in SPECIAL_RATES Routine

! RKI for Reaction PO06 set in SPECIAL_RATES Routine

! RKI for Reaction PO07 set in SPECIAL_RATES Routine

! RKI for Reaction PO08 set in SPECIAL_RATES Routine

! RKI for Reaction PO09 set in SPECIAL_RATES Routine

! RKI for Reaction PO10 set in SPECIAL_RATES Routine

! RKI for Reaction PO11 set in SPECIAL_RATES Routine

! RKI for Reaction PO12 set in SPECIAL_RATES Routine

! RKI for Reaction PO13 set in SPECIAL_RATES Routine

! RKI for Reaction PO14 set in SPECIAL_RATES Routine

! RKI for Reaction PO15 set in SPECIAL_RATES Routine

! RKI for Reaction PO16 set in SPECIAL_RATES Routine

! RKI for Reaction PO17 set in SPECIAL_RATES Routine

! RKI for Reaction PO18 set in SPECIAL_RATES Routine

! RKI for Reaction PO19 set in SPECIAL_RATES Routine

! RKI for Reaction PO20 set in SPECIAL_RATES Routine

! RKI for Reaction PO21 set in SPECIAL_RATES Routine

! RKI for Reaction PO22 set in SPECIAL_RATES Routine

! RKI for Reaction PO23 set in SPECIAL_RATES Routine

! RKI for Reaction PO24 set in SPECIAL_RATES Routine

! RKI for Reaction PO25 set in SPECIAL_RATES Routine

! RKI for Reaction PO26 set in SPECIAL_RATES Routine

! RKI for Reaction PO27 set in SPECIAL_RATES Routine

! RKI for Reaction PO28 set in SPECIAL_RATES Routine

! RKI for Reaction PO29 set in SPECIAL_RATES Routine

! RKI for Reaction PO30 set in SPECIAL_RATES Routine

! RKI for Reaction PO31 set in SPECIAL_RATES Routine

! RKI for Reaction PO32 set in SPECIAL_RATES Routine

! RKI for Reaction PO33 set in SPECIAL_RATES Routine

! RKI for Reaction PO34 set in SPECIAL_RATES Routine

! RKI for Reaction PO35 set in SPECIAL_RATES Routine

! RKI for Reaction PO36 set in SPECIAL_RATES Routine

! RKI for Reaction PO37 set in SPECIAL_RATES Routine

! RKI for Reaction PO38 set in SPECIAL_RATES Routine

! RKI for Reaction PO39 set in SPECIAL_RATES Routine

! RKI for Reaction PO40 set in SPECIAL_RATES Routine

! RKI for Reaction PO41 set in SPECIAL_RATES Routine

! RKI for Reaction PO42 set in SPECIAL_RATES Routine

! RKI for Reaction PO43 set in SPECIAL_RATES Routine

! RKI for Reaction PO41a set in SPECIAL_RATES Routine

! RKI for Reaction PO42b set in SPECIAL_RATES Routine

! RKI for Reaction PO43c set in SPECIAL_RATES Routine

! RKI for Reaction PO44 set in SPECIAL_RATES Routine

! RKI for Reaction PO45 set in SPECIAL_RATES Routine

! RKI for Reaction PO46 set in SPECIAL_RATES Routine

! RKI for Reaction PO47 set in SPECIAL_RATES Routine

! RKI for Reaction PO48 set in SPECIAL_RATES Routine

! RKI for Reaction PO49 set in SPECIAL_RATES Routine

! RKI for Reaction PO50 set in SPECIAL_RATES Routine

!  Reaction Label BE01            
             RKI( NCELL,  266) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8500D-12,  -1.6900D+03 )
!  Reaction Label BE02            
             RKI( NCELL,  267) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.0000D-28,   0.0000D+00,  -4.5000D+00,  & 
     &                                                 8.8000D-12,   0.0000D+00,  -8.5000D-01,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label BE03            
             RKI( NCELL,  268) =  CFACT * ARRHENUIS_T03( INV_TEMP,  9.1400D-15,  -2.5800D+03 )
!  Reaction Label BE04            
             RKI( NCELL,  269) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.3000D-12,  -2.8800D+03 )
!  Reaction Label BE05            
             RKI( NCELL,  270) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0700D-11,  -8.0000D+02 )
!  Reaction Label BT01            
             RKI( NCELL,  271) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.8500D-12,   5.0400D+02 )
!  Reaction Label BT02            
             RKI( NCELL,  272) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.5100D-15,  -1.8780D+03 )
!  Reaction Label BT03            
             RKI( NCELL,  273) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.5900D-13,  -1.1560D+03 )
!  Reaction Label BT04            
             RKI( NCELL,  274) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0200D-11,  -2.8000D+02 )
!  Reaction Label BT05            
             RKI( NCELL,  275) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4800D-11,   4.4800D+02 )
!  Reaction Label BT06            
             RKI( NCELL,  276) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3400D-14,  -2.2830D+03 )
!  Reaction Label BT07            
             RKI( NCELL,  277) =   1.0000D-13 * CFACT 
!  Reaction Label BT08            
             RKI( NCELL,  278) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2600D-11,  -4.0000D+01 )
!  Reaction Label BE06            
             RKI( NCELL,  279) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.5400D-11,   4.1000D+02 )
!  Reaction Label BE07            
             RKI( NCELL,  280) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.8600D-15,  -1.9120D+03 )
!  Reaction Label BE08            
             RKI( NCELL,  281) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.0300D-12,  -4.4800D+02 )
!  Reaction Label BE09            
             RKI( NCELL,  282) =   3.5000D-11 * CFACT 
!  Reaction Label IS88            
             RKI( NCELL,  283) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.9000D-11,   3.9000D+02 )
!  Reaction Label IS89            
             RKI( NCELL,  284) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.7500D-12,   2.0000D+02 )
!  Reaction Label IS90            
             RKI( NCELL,  286) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.7800D-11,  -4.0000D+02 )
!  Reaction Label IS91            
             RKI( NCELL,  287) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0600D-13,   1.3000D+03 )
!  Reaction Label IS96            
             RKI( NCELL,  288) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.6000D-12,   3.8000D+02 )
!  Reaction Label IS112           
             RKI( NCELL,  289) =   2.0000D-13 * CFACT 
!  Reaction Label IS113           
             RKI( NCELL,  290) =   3.5000D-14 * CFACT 
!  Reaction Label IS114           
             RKI( NCELL,  291) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-13,   1.0700D+03 )
!  Reaction Label BT09            
             RKI( NCELL,  292) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2100D-11,   4.3600D+02 )
!  Reaction Label BT10            
             RKI( NCELL,  293) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.0000D-16,  -5.3000D+02 )
!  Reaction Label BT11            
             RKI( NCELL,  294) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.1900D-12,   4.9000D+02 )
!  Reaction Label BT12            
             RKI( NCELL,  295) =   3.2000D-11 * CFACT 
!  Reaction Label BE13            
             RKI( NCELL,  296) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 5.5000D-30,   0.0000D+00,   0.0000D+00,  & 
     &                                                 8.3000D-13,   0.0000D+00,  -2.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label BE14            
             RKI( NCELL,  297) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.0000D-14,  -4.1000D+03 )
!  Reaction Label BE15            
             RKI( NCELL,  298) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3300D-12,  -1.9300D+02 )
!  Reaction Label BT13            
             RKI( NCELL,  299) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.8100D-12,   3.3800D+02 )
!  Reaction Label BT14            
             RKI( NCELL,  300) =   2.3100D-11 * CFACT 
!  Reaction Label BT15            
             RKI( NCELL,  301) =   1.3600D-11 * CFACT 
!  Reaction Label BT16            
             RKI( NCELL,  302) =   1.4300D-11 * CFACT 
!  Reaction Label BT17            
             RKI( NCELL,  303) =   3.2500D-11 * CFACT 
!  Reaction Label BT18            
             RKI( NCELL,  304) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   5.4900D-13,   5.3000D+02,   2.0000D+00 )
!  Reaction Label BL01            
             RKI( NCELL,  305) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.3400D-12,  -4.9900D+02,   2.0000D+00 )
!  Reaction Label BL02            
             RKI( NCELL,  306) =  CFACT * ARRHENUIS_T04( INV_TEMP,  TEMPOT300, & 
     &                                                   1.4900D-12,  -8.7000D+01,   2.0000D+00 )
!  Reaction Label BL03            
             RKI( NCELL,  307) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.5100D-12,   1.2600D+02 )
!  Reaction Label BL04            
             RKI( NCELL,  308) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.7500D-12,   4.4000D+01 )
!  Reaction Label BL05            
             RKI( NCELL,  309) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.7400D+02 )
!  Reaction Label AALK            
             RKI( NCELL,  310) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.7000D-12,   3.7400D+02 )
!  Reaction Label BL06            
             RKI( NCELL,  311) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.7200D-12,   5.0100D+02 )
!  Reaction Label BL07            
             RKI( NCELL,  312) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.1900D-15,  -1.7010D+03 )
!  Reaction Label BL08            
             RKI( NCELL,  313) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.3700D-13,  -1.0470D+03 )
!  Reaction Label BL09            
             RKI( NCELL,  314) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.6100D-11,  -3.2600D+02 )
!  Reaction Label BL10            
             RKI( NCELL,  315) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2600D-11,   4.8800D+02 )
!  Reaction Label BL11            
             RKI( NCELL,  316) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.5900D-15,  -1.2550D+03 )
!  Reaction Label BL12            
             RKI( NCELL,  317) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.3100D-13,   3.8200D+02 )
!  Reaction Label BL13            
             RKI( NCELL,  318) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4300D-11,   1.1100D+02 )
!  Reaction Label BL14            
             RKI( NCELL,  319) =   7.8400D-12 * CFACT 
!  Reaction Label BL15            
             RKI( NCELL,  320) =   3.0900D-11 * CFACT 
!  Reaction Label BL15b           
             RKI( NCELL,  321) =   3.0900D-11 * CFACT 
!  Reaction Label BL16            
             RKI( NCELL,  322) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2700D-11,   4.3500D+02 )
!  Reaction Label BL17            
             RKI( NCELL,  323) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.2800D-16,  -7.8500D+02 )
!  Reaction Label BL18            
             RKI( NCELL,  324) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.3300D-12,   4.9000D+02 )
!  Reaction Label BL19            
             RKI( NCELL,  325) =   4.0200D-11 * CFACT 
!  Reaction Label BT19            
             RKI( NCELL,  326) =   RKI( NCELL,  322 ) 
!  Reaction Label BT20            
             RKI( NCELL,  327) =   RKI( NCELL,  323 ) 
!  Reaction Label BT21            
             RKI( NCELL,  328) =   RKI( NCELL,  324 ) 
!  Reaction Label BT22            
             RKI( NCELL,  329) =   RKI( NCELL,  325 ) 
!  Reaction Label CI02            
             RKI( NCELL,  331) =  CFACT_SQU * POWER_T02( TEMPOT300,   7.6000D-32,  -1.8000D+00 )
!  Reaction Label CI04            
             RKI( NCELL,  333) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.3000D-30,   0.0000D+00,  -2.0000D+00,  & 
     &                                                 1.0000D-10,   0.0000D+00,  -1.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label CI05            
             RKI( NCELL,  334) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.8000D-31,   0.0000D+00,  -2.0000D+00,  & 
     &                                                 1.0000D-10,   0.0000D+00,  -1.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label CI08            
             RKI( NCELL,  337) =  CFACT * POWER_T02( TEMPOT300,   3.4400D-11,  -5.6000D-01 )
!  Reaction Label CI09            
             RKI( NCELL,  338) =  CFACT * POWER_T02( TEMPOT300,   9.4100D-12,   2.1000D+00 )
!  Reaction Label CI10            
             RKI( NCELL,  339) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.8000D-11,  -2.5000D+02 )
!  Reaction Label CI11            
             RKI( NCELL,  340) =   2.4000D-11 * CFACT 
!  Reaction Label CI12            
             RKI( NCELL,  341) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.2000D-12,   2.9500D+02 )
!  Reaction Label CI13            
             RKI( NCELL,  342) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.8000D-31,   0.0000D+00,  -3.4000D+00,  & 
     &                                                 1.5000D-11,   0.0000D+00,  -1.9000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label CI16            
             RKI( NCELL,  345) =  SFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 4.4800D-05,  -1.2530D+04,  -1.0000D+00,  & 
     &                                                 3.7100D+15,  -1.2530D+04,   3.5000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label CI17            
             RKI( NCELL,  346) =  CFACT * ARRHENUIS_T03( INV_TEMP,  6.2000D-12,   1.4500D+02 )
!  Reaction Label CI18            
             RKI( NCELL,  347) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.2000D-12,   3.4000D+02 )
!  Reaction Label CI20            
             RKI( NCELL,  349) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2500D-11,  -1.9600D+03 )
!  Reaction Label CI21            
             RKI( NCELL,  350) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.7000D-12,  -2.3000D+02 )
!  Reaction Label CI22            
             RKI( NCELL,  351) =  CFACT * ARRHENUIS_T03( INV_TEMP,  3.9000D-11,  -2.3100D+03 )
!  Reaction Label CP01            
             RKI( NCELL,  352) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.1000D-11,  -3.0000D+01 )
!  Reaction Label CP02            
             RKI( NCELL,  353) =   8.0000D-11 * CFACT 
!  Reaction Label CP03            
             RKI( NCELL,  354) =   5.5000D-11 * CFACT 
!  Reaction Label CP04            
             RKI( NCELL,  355) =   1.2300D-10 * CFACT 
!  Reaction Label CP05            
             RKI( NCELL,  356) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.7000D-11,  -1.0000D+03 )
!  Reaction Label CP06            
             RKI( NCELL,  357) =   3.6000D-11 * CFACT 
!  Reaction Label CP07            
             RKI( NCELL,  358) =   1.9200D-10 * CFACT 
!  Reaction Label CP08            
             RKI( NCELL,  359) =   2.0000D-10 * CFACT 
!  Reaction Label CP09            
             RKI( NCELL,  360) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.1000D-11,  -3.0000D+01 )
!  Reaction Label CP10            
             RKI( NCELL,  361) =   8.0000D-11 * CFACT 
!  Reaction Label CP11            
             RKI( NCELL,  362) =   6.2000D-11 * CFACT 
!  Reaction Label CP12            
             RKI( NCELL,  363) =   8.0000D-11 * CFACT 
!  Reaction Label CP13            
             RKI( NCELL,  364) =   1.6600D-10 * CFACT 
!  Reaction Label CP14            
             RKI( NCELL,  365) =   3.0000D-10 * CFACT 
!  Reaction Label CP15            
             RKI( NCELL,  366) =   4.2900D-10 * CFACT 
!  Reaction Label TP01            
             RKI( NCELL,  367) =   2.9400D-10 * CFACT 
!  Reaction Label CP16            
             RKI( NCELL,  368) =   3.8500D-10 * CFACT 
!  Reaction Label CP17            
             RKI( NCELL,  369) =   2.3200D-10 * CFACT 
!  Reaction Label CP18            
             RKI( NCELL,  370) =   4.1200D-10 * CFACT 
!  Reaction Label CP20            
             RKI( NCELL,  372) =   3.1000D-12 * CFACT 
!  Reaction Label CP21            
             RKI( NCELL,  373) =   1.2900D-11 * CFACT 
! RKI for Reaction CP23 set in SPECIAL_RATES Routine

! RKI for Reaction CP24 set in SPECIAL_RATES Routine

! RKI for Reaction CP25 set in SPECIAL_RATES Routine

! RKI for Reaction CP26 set in SPECIAL_RATES Routine

! RKI for Reaction CP27 set in SPECIAL_RATES Routine

! RKI for Reaction CP28 set in SPECIAL_RATES Routine

!  Reaction Label CE01            
             RKI( NCELL,  381) =  CFACT * ARRHENUIS_T03( INV_TEMP,  7.3000D-12,  -1.2800D+03 )
!  Reaction Label CE02            
             RKI( NCELL,  382) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 1.6000D-29,   0.0000D+00,  -3.3000D+00,  & 
     &                                                 3.1000D-10,   0.0000D+00,  -1.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label TE01            
             RKI( NCELL,  383) =   2.6700D-10 * CFACT 
!  Reaction Label TE02            
             RKI( NCELL,  384) =   4.9000D-10 * CFACT 
!  Reaction Label CE03            
             RKI( NCELL,  385) =   4.8000D-10 * CFACT 
!  Reaction Label TE03            
             RKI( NCELL,  386) =   5.4600D-10 * CFACT 
!  Reaction Label CE04            
             RKI( NCELL,  387) =  CFACT * FALLOFF_T10( INV_TEMP,  TEMPOT300,  CAIR, & 
     &                                                 5.2000D-30,   0.0000D+00,  -2.4000D+00,  & 
     &                                                 2.2000D-10,   0.0000D+00,   0.0000D+00,  & 
     &                                                 1.0000D+00,   6.0000D-01 )
!  Reaction Label TE04            
             RKI( NCELL,  388) =   6.2000D-11 * CFACT 
!  Reaction Label TE05            
             RKI( NCELL,  389) =   1.3500D-10 * CFACT 
!  Reaction Label TE06            
             RKI( NCELL,  390) =   1.4000D-10 * CFACT 
!  Reaction Label TE07            
             RKI( NCELL,  391) =   1.4400D-10 * CFACT 
!  Reaction Label TE08            
             RKI( NCELL,  392) =   2.4200D-10 * CFACT 
!  Reaction Label TE09            
             RKI( NCELL,  393) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.6000D-11,   4.5000D+01 )
!  Reaction Label BC01            
             RKI( NCELL,  394) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.3000D-11,  -1.0000D+02 )
!  Reaction Label BC02            
             RKI( NCELL,  395) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.2000D-10,   4.0000D+01 )
!  Reaction Label BC03            
             RKI( NCELL,  396) =   1.8600D-10 * CFACT 
!  Reaction Label BC04            
             RKI( NCELL,  397) =   2.6300D-10 * CFACT 
!  Reaction Label BC05            
             RKI( NCELL,  398) =   4.2100D-10 * CFACT 
!  Reaction Label BC06            
             RKI( NCELL,  399) =   3.9200D-10 * CFACT 
!  Reaction Label BC07            
             RKI( NCELL,  400) =   3.7700D-10 * CFACT 
!  Reaction Label BC08            
             RKI( NCELL,  401) =   2.1600D-10 * CFACT 
!  Reaction Label BC09            
             RKI( NCELL,  402) =   2.6600D-10 * CFACT 
!  Reaction Label BC09b           
             RKI( NCELL,  403) =   2.6600D-10 * CFACT 
!  Reaction Label BC10            
             RKI( NCELL,  404) =   5.4600D-10 * CFACT 
!  Reaction Label BC11            
             RKI( NCELL,  405) =   RKI( NCELL,  404 ) 
!  Reaction Label AE51            
             RKI( NCELL,  406) =   RKI( NCELL,   52 ) 
!  Reaction Label AE52            
             RKI( NCELL,  407) =   RKI( NCELL,   53 ) 
!  Reaction Label AE53            
             RKI( NCELL,  408) =   RKI( NCELL,   52 ) 
!  Reaction Label AE54            
             RKI( NCELL,  409) =   RKI( NCELL,   53 ) 
!  Reaction Label AE55            
             RKI( NCELL,  410) =   RKI( NCELL,   52 ) 
!  Reaction Label AE56            
             RKI( NCELL,  411) =   RKI( NCELL,   53 ) 
!  Reaction Label AE55b           
             RKI( NCELL,  412) =   RKI( NCELL,   52 ) 
!  Reaction Label AE56b           
             RKI( NCELL,  413) =   RKI( NCELL,   53 ) 
!  Reaction Label TR03            
             RKI( NCELL,  416) =  CFACT * ARRHENUIS_T03( INV_TEMP,  5.4000D-12,   1.3500D+02 )
!  Reaction Label TR05            
             RKI( NCELL,  417) =  CFACT * ARRHENUIS_T03( INV_TEMP,  2.0000D-12,  -2.4310D+03 )
!  Reaction Label TR06            
             RKI( NCELL,  418) =  CFACT * ARRHENUIS_T03( INV_TEMP,  8.1000D-11,  -3.0000D+01 )
!  Reaction Label TR07            
             RKI( NCELL,  419) =  CFACT * ARRHENUIS_T03( INV_TEMP,  4.4000D-12,   3.6500D+02 )
!  Reaction Label TR09            
             RKI( NCELL,  421) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-12,  -1.8600D+03 )
!  Reaction Label TR10            
             RKI( NCELL,  422) =   8.0000D-11 * CFACT 
!  Reaction Label TR11            
             RKI( NCELL,  423) =   1.9900D-11 * CFACT 
!  Reaction Label TR12            
             RKI( NCELL,  424) =  CFACT * ARRHENUIS_T03( INV_TEMP,  1.4000D-15,  -2.5280D+03 )
!  Reaction Label TR13            
             RKI( NCELL,  425) =   1.1800D-15 * CFACT 
!  Reaction Label TR14            
             RKI( NCELL,  426) =   2.3700D-12 * CFACT 
!  Reaction Label TR16            
             RKI( NCELL,  428) =   2.9400D-10 * CFACT 
!  Reaction Label HET_N02         
             RKI( NCELL,  429) =  BLKHET(  NCELL, IK_HETERO_NO2 )
!  Reaction Label HET_N2O5IJ      
             RKI( NCELL,  430) =  BLKHET(  NCELL, IK_HETERO_N2O5IJ )
!  Reaction Label HET_N2O5K       
             RKI( NCELL,  431) =  BLKHET(  NCELL, IK_HETERO_N2O5K )
!  Reaction Label HET_H2NO3PIJA   
             RKI( NCELL,  432) =  BLKHET(  NCELL, IK_HETERO_H2NO3PAIJ )
!  Reaction Label HET_H2NO3PKA    
             RKI( NCELL,  433) =  BLKHET(  NCELL, IK_HETERO_H2NO3PAK )
!  Reaction Label HET_H2NO3PIB    
             RKI( NCELL,  434) =  BLKHET(  NCELL, IK_HETERO_H2NO3PBIJ )
!  Reaction Label HET_H2NO3PJB    
             RKI( NCELL,  435) =  BLKHET(  NCELL, IK_HETERO_H2NO3PBIJ )
!  Reaction Label HET_H2NO3PKB    
             RKI( NCELL,  436) =  BLKHET(  NCELL, IK_HETERO_H2NO3PBK )
!  Reaction Label OLIG_XYLENE1    
             RKI( NCELL,  438) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_XYLENE2    
             RKI( NCELL,  439) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TOLUENE1   
             RKI( NCELL,  440) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TOLUENE2   
             RKI( NCELL,  441) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_BENZENE1   
             RKI( NCELL,  442) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_BENZENE2   
             RKI( NCELL,  443) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TERPENE1   
             RKI( NCELL,  444) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_TERPENE2   
             RKI( NCELL,  445) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ISOPRENE1  
             RKI( NCELL,  446) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ISOPRENE2  
             RKI( NCELL,  447) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_SESQT1     
             RKI( NCELL,  448) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_PAH1       
             RKI( NCELL,  449) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_PAH2       
             RKI( NCELL,  450) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ALK1       
             RKI( NCELL,  451) =   9.4882D-06 * SFACT 
!  Reaction Label OLIG_ALK2       
             RKI( NCELL,  452) =   9.4882D-06 * SFACT 
!  Reaction Label PCSOA           
             RKI( NCELL,  453) =   1.2500D-11 * CFACT 
!  Reaction Label POA_AGE1        
             RKI( NCELL,  454) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE2        
             RKI( NCELL,  455) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE3        
             RKI( NCELL,  456) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE4        
             RKI( NCELL,  457) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE5        
             RKI( NCELL,  458) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE6        
             RKI( NCELL,  459) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE7        
             RKI( NCELL,  460) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE8        
             RKI( NCELL,  461) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE9        
             RKI( NCELL,  462) =   4.0000D-11 * CFACT 
!  Reaction Label POA_AGE10       
             RKI( NCELL,  463) =   4.0000D-11 * CFACT 
!  Reaction Label HET_IEPOX       
             RKI( NCELL,  464) =  BLKHET(  NCELL, IK_HETERO_IEPOX )

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
       END MODULE RXNS_FUNCTION
