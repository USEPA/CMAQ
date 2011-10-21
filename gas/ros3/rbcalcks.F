
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/gas/ros3/rbcalcks.F,v 1.3 2011/10/21 16:11:09 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE CALCKS( )

C**********************************************************************
 
C  Function: To compute thermal and photolytic reaction rate 
C            coefficients for each reaction.
 
C  Preconditions: Photolysis rates for individual species must have 
C                 been calculated and stored in RKPHOT. Expects 
C                 temperature in deg K, pressure in atm., water
C                 vapor in ppmV, and J-values in /min. 
  
C  Key Subroutines/Functions Called: None
 
C  Revision History: Prototype created by Jerry Gipson, August, 2004.
C                      Adapted from CALCKS in CMAQ SMVGEAR
C                    31 Jan 05 J.Young: get BLKSIZE from dyn alloc horizontal
C                    & vertical domain specifications module (GRID_CONF)
C                    28 Jun 10 J.Young: remove unnecessary modules and includes
C***********************************************************************

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE RBDATA                ! Rosenbrock solver data

      IMPLICIT NONE
      
C..Includes:

      INCLUDE SUBST_RXCMMN 

C..Arguments: None

C..Parameters: 

      REAL, PARAMETER :: COEF1 = 7.33981E+15  ! Molec/cc to ppm conv factor

      REAL, PARAMETER :: CONSTC = 0.6         ! Constant for reaction type 7

      REAL, PARAMETER :: TI300 = 1.0 / 300.0

C..External Functions: None

C..Local Variables:
      INTEGER NRT                  ! Loop index for reaction types
      INTEGER IRXN                 ! Reaction number
      INTEGER JNUM                 ! J-value species # from PHOT
      INTEGER KNUM                 ! Reaction # for a relative rate coeff.
      INTEGER N                    ! Loop index for reactions
      INTEGER NCELL                ! Loop index for # of cells in the block

      REAL RK0                     ! K0 in falloff rate expressions
      REAL RK1                     ! k1 in falloff rate expressions
      REAL RK2                     ! K2 in falloff rate expressions
      REAL RK3                     ! K3 in falloff rate expressions
      REAL XEND                    ! Exponent in falloff rate expressions
      REAL CFACT( BLKSIZE )        ! molec/cc to ppm conversion factor   
      REAL( 8 ) :: TINV( BLKSIZE ) ! Reciprocal of temperature, /deg K
         
C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set-up some conversion factors 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NCELL = 1, NUMCELLS
         TINV(  NCELL ) = 1.0 / BLKTEMP( NCELL )
         CFACT( NCELL ) = COEF1 * BLKPRES( NCELL ) * TINV( NCELL )
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set photolytic rate coefficients using the photolysis J-values that
c  are stored in RJBLK
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( .NOT. LSUNLIGHT ) THEN
      
c..If dark in every cell, zero all photolysis rate coefficients      
         DO NRT = 1, NMPHOT
            IRXN = IPH( NRT,1 )
            DO NCELL = 1, NUMCELLS
               RKI( NCELL,IRXN ) = 0.0
            END DO
         END DO

      ELSE
      
c..otherwise do all absolute rates first and then relative rates
         DO NRT = 1, NMPHOT
            IF ( IPH( NRT,3 ) .NE. 0 ) THEN
               IRXN = IPH( NRT,1 )
               JNUM = IPH( NRT,2 )
               DO NCELL = 1, NUMCELLS
                  RKI( NCELL,IRXN ) = RTDAT( 1,IRXN )
     &                              * RJBLK( NCELL,JNUM )
               END DO
            END IF
         END DO

         DO NRT = 1, NMPHOT
            IF ( IPH( NRT,3 ) .EQ. 0 ) THEN
               IRXN = IPH( NRT,1 )
               KNUM = IPH( NRT,2 )
               DO NCELL = 1, NUMCELLS
                  RKI( NCELL,IRXN ) = RTDAT( 1,IRXN ) * RKI( NCELL,KNUM )
               END DO
            END IF
         END DO
      END IF 
       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do reaction type 1: k=A
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRT = 1, KTN1
         IRXN = KRX1( NRT )
         DO NCELL = 1, NUMCELLS
            RKI( NCELL,IRXN ) = RTDAT( 1,IRXN )
         END DO
      END DO
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do reaction type 2: k=A*(T/300)**B
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRT = 1, KTN2
         IRXN = KRX2( NRT )
         DO NCELL = 1, NUMCELLS
            RKI( NCELL,IRXN ) = RTDAT( 1,IRXN )
     &                        * ( BLKTEMP( NCELL ) * TI300 ) ** RTDAT( 2,IRXN )
         END DO
      END DO
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do reaction type 3: k=A*exp(C/T)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRT = 1, KTN3
         IRXN = KRX3( NRT )
         DO NCELL = 1, NUMCELLS
            RKI( NCELL,IRXN ) = RTDAT( 1,IRXN )
     &                        * EXP( RTDAT( 3,IRXN ) * TINV( NCELL ) )
         END DO
      END DO
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do reaction type 4: k=A*((T/300)**B)*exp(C/T)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRT = 1, KTN4
         IRXN = KRX4( NRT )
         DO NCELL = 1, NUMCELLS
            RKI( NCELL,IRXN ) = RTDAT( 1,IRXN )
     &                        * ( BLKTEMP( NCELL ) * TI300 ) ** RTDAT( 2,IRXN )
     &                        * EXP( RTDAT( 3,IRXN ) * TINV( NCELL ) )
         END DO
      END DO
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do reaction type 7: k=A*(1+0.6*P)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRT = 1, KTN7
         IRXN = KRX7( NRT )
         DO NCELL = 1, NUMCELLS
            RKI( NCELL,IRXN ) = RTDAT( 1,IRXN )
     &                        * ( 1.0 + CONSTC * BLKPRES( NCELL ) )
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do fall offs and special type %2 (ktype 8)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
      DO 100 NRT = 1, NFALLOFF
         IRXN = IRRFALL( NRT )

         IF ( KTYPE( IRXN ) .EQ. 8 ) THEN
            DO NCELL = 1, NUMCELLS
               RK0 = RTDAT( 1,IRXN ) * EXP( RTDAT( 2,IRXN ) * TINV( NCELL ) )
               RK2 = RTDAT( 3,IRXN ) * EXP( RFDAT( 1,NRT )  * TINV( NCELL ) )
               RK3 = 1.0E+06 * CFACT( NCELL ) * RFDAT( 2,NRT )
     &             * EXP( RFDAT( 3,NRT ) * TINV( NCELL ) )
               RKI( NCELL,IRXN ) = RK0 + ( RK3 / ( 1.0 + RK3 / RK2 ) ) 
            END DO

         ELSE IF ( KTYPE( IRXN ) .EQ. 9 ) THEN
            DO NCELL = 1, NUMCELLS
               RK1 = RTDAT( 1,IRXN ) * EXP( RTDAT( 2,IRXN ) * TINV( NCELL ) )
               RK2 = 1.0E+06 * CFACT( NCELL ) * RTDAT( 3,IRXN )
     &             * EXP( RFDAT( 1, NRT )  * TINV( NCELL ) )
     &              
               RKI( NCELL,IRXN ) = RK1 + RK2 
            END DO
         ELSE
            DO NCELL = 1, NUMCELLS 
               RK0 = 1.0E+06 * CFACT( NCELL ) * RTDAT( 1,IRXN )
     &             * ( BLKTEMP( NCELL ) * TI300 ) ** RTDAT( 2,IRXN )
     &             * EXP( RTDAT( 3,IRXN ) * TINV( NCELL ) )
               RK1 = RFDAT( 1,NRT )
     &             * ( BLKTEMP( NCELL ) * TI300 ) ** RFDAT( 2,NRT )
     &             * EXP( RFDAT( 3,NRT ) * TINV( NCELL ) ) 
               XEND = 1.0 / ( ( 1.0 + ( ( 1.0 / RFDAT( 5, NRT ) )
     &              * LOG10( RK0 / RK1 ) ) ** 2 ) )
               RKI( NCELL,IRXN ) = ( RK0 / ( 1.0 + RK0 / RK1 ) )
     &                           * RFDAT( 4,NRT ) ** XEND
            END DO    
         END IF
100   CONTINUE

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Do reaction type 5 and 6 (multipliers of above reactions)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO NRT = 1, KTN5
         IRXN = KRX5( NRT )
         KNUM = INT( RTDAT( 3, IRXN ) )
         DO NCELL=1,NUMCELLS  
            RKI( NCELL,IRXN ) = RKI( NCELL,KNUM ) / ( RTDAT( 1,IRXN ) *
     &                      EXP( RTDAT( 2, IRXN ) / BLKTEMP( NCELL ) ) )
         END DO
      END DO

      DO NRT = 1, KTN6
         IRXN = KRX6( NRT )
         KNUM = INT( RTDAT( 2,IRXN ) )
         DO NCELL = 1, NUMCELLS
            RKI( NCELL,IRXN ) = RKI( NCELL,KNUM ) * RTDAT( 1, IRXN )
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Convert to ppm units as needed
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF ( KUNITS .EQ. 2 ) THEN
         DO 200 N = 1, NRXNS

            IF ( KTYPE( N ) .NE. 0 ) THEN   ! Do all non-photolytic reactions

               IF ( IORDER( N ) .EQ. 2 ) THEN
                  DO NCELL = 1, NUMCELLS
                     RKI( NCELL,N ) = RKI( NCELL,N ) * 60.0
     &                              * CFACT( NCELL )
                  END DO

               ELSE IF ( IORDER( N ) .EQ. 3 ) THEN
                  DO NCELL = 1, NUMCELLS
                     RKI( NCELL,N ) = RKI( NCELL,N ) * 60.0
     &                              * CFACT( NCELL ) * CFACT( NCELL )
                  END DO

               ELSE IF ( IORDER( N ) .EQ. 1 ) THEN
                  DO NCELL = 1, NUMCELLS
                     RKI( NCELL,N ) = RKI( NCELL,N ) * 60.0 
                  END DO

               ELSE IF ( IORDER( N ) .EQ. 0 ) THEN
                  DO NCELL = 1, NUMCELLS
                     RKI( NCELL,N ) = RKI( NCELL,N ) * 60.0
     &                              / CFACT( NCELL )
                  END DO
               END IF

            END IF

200      CONTINUE
      ELSE

         DO 300 N = 1, NFALLOFF      ! Do Falloff reactions only
            IRXN = IRRFALL( N )

            IF ( IORDER( IRXN ) .EQ. 2 ) THEN
               DO NCELL = 1, NUMCELLS
                  RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * 60.0
     &                              * CFACT( NCELL )
               END DO

            ELSE IF ( IORDER( IRXN ) .EQ. 3 ) THEN
               DO NCELL = 1, NUMCELLS
                  RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * 60.0
     &                              * CFACT( NCELL ) * CFACT( NCELL )
               END DO
            END IF
300      CONTINUE
      END IF
            
  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Multiply rate constants by [M], [O2], [N2], or [H2O] where needed
c  and return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
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
               RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * BLKCH2O( NCELL )
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

      IF ( NWH2 .GT. 0 ) THEN
         DO NRT = 1, NWH2
            IRXN = NRXWH2( NRT )
            DO NCELL = 1, NUMCELLS
               RKI( NCELL,IRXN ) = RKI( NCELL,IRXN ) * ATM_H2
            END DO
         END DO
      END IF

      RETURN

      END



