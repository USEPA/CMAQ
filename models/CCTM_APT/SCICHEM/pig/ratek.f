      SUBROUTINE RATEK (TEMP,H2O,PRES,RK)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Calculate the rate constants
!            (this routine is modeled after the host model's)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
! REVISION HISTORY: 
!
!    Minor updates (Fortran 90), February 2004 (PKK, AER)
!    Minor updates, April 2005 (PKK, AER)
!    Updated for CMAQ-AERO3 option, April 2008 (PKK, AER)
!    Updated for CMAQ 5, August 2011, PKK, ENVIRON
!******************************************************************************
 
! --- MODULES

      use error_inc
      USE HOST_CHEM_INC

      IMPLICIT NONE

! --- ARGUMENTS
 
      REAL( 8 ), DIMENSION( NRXNS ) :: RK          ! rate constants
      REAL     TEMP                 ! temperature (in degrees Kelvin)
      REAL     H2O                  ! water concentration (in ppm)
      REAL     PRES                 ! pressure (in atmospheres)

! --- PARAMETERS
 
      REAL, PARAMETER :: CONSTC = 0.6

      REAL, PARAMETER :: COEF1 = 7.33981E+15    ! (degK/atm)(mpcc/ppm)

      REAL, PARAMETER :: TI300 = 1.0 / 300.0

! --- LOCALS
      INTEGER NRT            ! Loop index for reaction types
      INTEGER IRXN           ! Reaction number
      INTEGER KNUM           ! Reaction # for a relative rate coeff.
      INTEGER N              ! Loop index for reactions

      REAL RK0               ! K0 in falloff rate expressions
      REAL RK1               ! k1 in falloff rate expressions
      REAL RK2               ! K2 in falloff rate expressions
      REAL RK3               ! K3 in falloff rate expressions
      REAL XEND              ! Exponent in falloff rate expressions
      REAL( 8 ) :: CFACT     ! molec/cc to ppm conversion factor   
      REAL( 8 ) :: TINV      ! Reciprocal of temperature, /deg K
      REAL TO300             ! Temp/300

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  Set reciprocal of temperature and mole/cc to ppm conversion factor 
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      TO300 = TI300 * TEMP
      TINV = 1.0 / TEMP
      CFACT = COEF1 * PRES * TINV

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Do reaction types 1 thru 4 and 7
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
! ... Reaction Type 1, k = A
      DO NRT = 1, KTN1
         IRXN = KRX1( NRT )
         RK( IRXN ) = RTDAT( 1, IRXN )
      END DO
!
! ... Reaction Type 2, k = A*(T/300)**b
      DO NRT = 1, KTN2
         IRXN = KRX2( NRT )
         RK( IRXN ) = RTDAT( 1, IRXN ) * ( TO300 ) ** RTDAT( 2, IRXN )
      END DO
!
! ... Reaction Type 3, k = A*exp(C/T)
      DO NRT = 1, KTN3
         IRXN = KRX3( NRT )
         RK( IRXN ) = RTDAT( 1, IRXN ) * EXP ( RTDAT ( 3, IRXN ) * TINV )
      END DO
!
! ... Reaction Type 4, k = A*((T/300)**b)*exp(C/T)
      DO NRT = 1, KTN4
         IRXN = KRX4( NRT )
         RK( IRXN ) = RTDAT( 1, IRXN ) * ( TO300 ) ** RTDAT( 2, IRXN ) *
     &                EXP( RTDAT( 3, IRXN ) * TINV )
      END DO
!
! ... Reaction Type 7, k = A*(1 + 0.6*P)
      DO NRT = 1, KTN7
         IRXN = KRX7( NRT )
         RK( IRXN ) = RTDAT( 1, IRXN ) * ( 1.0 + CONSTC * PRES )
      END DO

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Do falloffs and special type %2 (ktype 8) and type %3 (ktype 9)
! convert to ppm units (Falloff Rx's are assumed ALWAYS in cm-sec units.)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRT = 1, NFALLOFF
         IRXN = IRRFALL( NRT )
         IF ( KTYPE(IRXN) == 8 ) THEN
            RK0 = RTDAT( 1, IRXN ) * EXP ( RTDAT( 2, IRXN ) * TINV )
            RK2 = RTDAT( 3, IRXN ) * EXP ( RFDAT( 1, NRT )  * TINV )
            RK3 = 1.0E+06 * CFACT * RFDAT( 2, NRT )
     &          * EXP( RFDAT( 3, NRT ) * TINV )
            RK( IRXN ) = RK0 + ( RK3 / (1.0 + RK3 / RK2 ) ) 
         ELSE IF ( KTYPE(IRXN) == 9 ) THEN
            RK1 = RTDAT( 1, IRXN ) * EXP ( RTDAT( 2, IRXN ) * TINV )
            RK2 = 1.0E+06 * CFACT * RTDAT( 3, IRXN ) *
     &               EXP ( RFDAT( 1, NRT ) * TINV)
            RK( IRXN ) = RK1 + RK2
         ELSE
            RK0 = 1.0E+06 * CFACT *
     &            RTDAT( 1, IRXN ) * ( TO300 ) **
     &            RTDAT( 2, IRXN ) * EXP( RTDAT( 3, IRXN ) * TINV )
            RK1 = RFDAT( 1, NRT ) * ( TO300 ) **
     &            RFDAT( 2, NRT ) * EXP( RFDAT( 3, NRT ) * TINV ) 
            XEND = 1.0 / (( 1.0 + ( ( 1.0 / RFDAT( 5, NRT ) ) *
     &             LOG10( RK0 / RK1 ) ) **2 ) )
            RK( IRXN ) = ( RK0 / ( 1.0 + RK0 / RK1 ) ) *
     &                     RFDAT( 4, NRT ) ** XEND
         END IF
      END DO

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Do reaction types 5 and 6 (multipliers of above reactions)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO NRT = 1, KTN5    ! rev. equil.
         IRXN = KRX5( NRT )
         KNUM = INT( RTDAT( 3, IRXN ) )
         RK( IRXN ) = RK( KNUM ) / ( RTDAT( 1, IRXN ) *
     &                EXP( RTDAT( 2, IRXN ) / TEMP ) )
      END DO

      DO NRT = 1, KTN6    ! linear dependence on another rate
         IRXN = KRX6( NRT )
         KNUM = INT( RTDAT( 2,IRXN ) )
         RK( IRXN ) = RK( KNUM ) * RTDAT( 1, IRXN )
      END DO

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! If cm-sec units were input, convert above to ppm-min units
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( KUNITS == 2 ) THEN
         DO 200 N = 1, NRXNS
            IF ( KTYPE( N ) /= 0 ) THEN  ! exclude photolytic reactions
               IF ( IORDER( N ) == 2 ) THEN   ! most likely 1st
                  RK( N ) = RK( N ) * 60.0 * CFACT
               ELSE IF ( IORDER( N ) == 3 ) THEN
                  RK( N ) = RK( N ) * 60.0 * CFACT * CFACT
               ELSE IF ( IORDER( N ) == 1 ) THEN
                  RK( N ) = RK( N ) * 60.0
               ELSE IF ( IORDER( N ) == 0 ) THEN
                  RK( N ) = RK( N ) * 60.0 / CFACT
               END IF
            END IF
200      CONTINUE
      ELSE    ! always convert falloffs
         DO 300 N = 1, NFALLOFF
            IRXN = IRRFALL( N )
            IF ( IORDER( IRXN ) == 2 ) THEN   ! most likely 1st
               RK( IRXN ) = RK( IRXN ) * 60.0 * CFACT
            ELSE IF (IORDER(IRXN) == 3) THEN
               RK( IRXN ) = RK( IRXN ) * 60.0 * CFACT * CFACT
            END IF
300      CONTINUE
      END IF

! units now all ppm-min

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! three-body and other reactions
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  Multiply rate constants by [M], [O2], [N2], [H2], [CH4] or [H2O]
!  where needed and return
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( NWM > 0 ) THEN
         DO NRT = 1, NWM
           IRXN = NRXWM( NRT )
           RK( IRXN ) = RK( IRXN ) * ATM_AIR
         END DO
      END IF

      IF ( NWO2 > 0 ) THEN
         DO NRT = 1, NWO2
            IRXN = NRXWO2( NRT )
            RK(IRXN) = RK(IRXN) * ATM_O2
         END DO
      END IF

      IF ( NWN2 > 0 ) THEN
         DO NRT = 1, NWN2
            IRXN = NRXWN2( NRT )
            RK( IRXN ) = RK( IRXN ) * ATM_N2
         END DO
      END IF

      IF ( NWW > 0 ) THEN
         DO NRT = 1, NWW
            IRXN =  NRXWW( NRT )
            RK( IRXN ) = RK( IRXN ) * H2O
         END DO
      END IF

      IF ( NWCH4 > 0 ) THEN
         DO NRT = 1, NWCH4
            IRXN =  NRXWCH4( NRT )
            RK( IRXN ) = RK( IRXN ) * ATM_CH4
         END DO
      END IF

      IF ( NWH2 > 0 ) THEN
         DO NRT = 1, NWH2
            IRXN =  NRXWH2( NRT )
            RK( IRXN ) = RK( IRXN ) * ATM_H2
         END DO
      END IF

      RETURN    ! RETURN RATE CONTANTS FOR GAS PHASE REACTIONS
      END
