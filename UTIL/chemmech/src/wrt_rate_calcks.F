
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
C $Header: /project/work/rep/arc/CCTM/src/gas/ros3/rbcalcks.F,v 1.3 2011/10/21 16:11:09 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE WRT_CALCKS( )

C**********************************************************************
 
C  Function: Write out reaction rate constant for mechanisms
 
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

      USE MECHANISM_DATA

      IMPLICIT NONE
      
C..Includes: None

C..Arguments: None

C..Parameters: 

      REAL, PARAMETER :: COEF1 = 7.33981E+15  ! Molec/cc to ppm conv factor

      REAL, PARAMETER :: CONSTC = 0.6         ! Constant for reaction type 7

      REAL, PARAMETER :: TI300 = 1.0 / 300.0

C..External Functions: None

      INTEGER, EXTERNAL :: JUNIT   ! defines IO unit

C..Local Variables:
      INTEGER NRT                  ! Loop index for reaction types
      INTEGER IRXN                 ! Reaction number
      INTEGER JNUM                 ! J-value species # from PHOT
      INTEGER KNUM                 ! Reaction # for a relative rate coeff.
      INTEGER N                    ! Loop index for reactions
      
      CHARACTER(132) :: LINEOUT

      INTEGER IOUT                 ! IO unit number
       
C***********************************************************************

        IOUT = JUNIT()
        
        OPEN(IOUT,FILE = TRIM(OUTDIR) // '/calc_rconst.F', STATUS='UNKNOWN')

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set-up some conversion factors 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      WRITE(IOUT, 99880)
      
      LINEOUT = 'IF ( LSUNLIGHT ) THEN' 
      WRITE(IOUT,'(11X,A )')TRIM(LINEOUT)
      
!c..If dark in every cell, zero all photolysis rate coefficients      
!      LINEOUT = 'C..If dark in every cell, zero all photolysis rate coefficients'
!      WRITE(IOUT,'(/ A /)')LINEOUT

!      DO NRT = 1, NMPHOT
!         IRXN = IPH( NRT,1 )
!98701    FORMAT(15X,'RKI( NCELL, ', I4 ,' ) = 0.0D+0' )
!         WRITE(IOUT, 98701)IRXN
!      END DO

!      LINEOUT = 'ELSE'
!      WRITE(IOUT,'(/ 11X,A )')TRIM(LINEOUT)

c..write out all absolute rates first and then relative rates dependent on photolysis rates
      LINEOUT = 'C..set all absolute rates first and then relative rates'
      WRITE(IOUT,'(A /)')LINEOUT
         
      DO NRT = 1, NMPHOT
         IF ( IPH( NRT,3 ) .NE. 0 ) THEN
            IRXN = IPH( NRT,1 )
            JNUM = IPH( NRT,2 )

            IF( RTDAT( 1,IRXN ) .EQ. 0.0D0 )THEN
98502           FORMAT('!!!',12X,'RKI( NCELL, ', I4,'  ) = ', 1PD10.4,' * RJBLK( NCELL, ', I4,' ) ! ', A )
                WRITE(IOUT,98502)IRXN, RTDAT( 1,IRXN ), JNUM, 
     &         'Reaction ' // TRIM(RXLABEL( IRXN)) // ' <--- '// TRIM( PHOTAB( JNUM ) )
                CYCLE
            END IF           
            
            IF( RTDAT( 1,IRXN ) .NE. 1.0 )THEN
98702           FORMAT(15X,'RKI( NCELL, ', I4,'  ) = ', 1PD10.4,' * RJBLK( NCELL, ', I4,' ) ! ', A )
                WRITE(IOUT,98702)IRXN, RTDAT( 1,IRXN ), JNUM, 
     &         'Reaction ' // TRIM(RXLABEL( IRXN)) // ' <--- '// TRIM( PHOTAB( JNUM ) )
            ELSE
98602          FORMAT(15X,'RKI( NCELL, ', I4,'  ) = RJBLK( NCELL, ', I4,' )',13X,' ! ', A )
               WRITE(IOUT,98602)IRXN, JNUM, 
     &        'Reaction ' // TRIM(RXLABEL( IRXN)) // ' <--- '// TRIM( PHOTAB( JNUM ) )
            END IF
                               
        END IF
      END DO

         DO NRT = 1, NMPHOT
            IF ( IPH( NRT,3 ) .EQ. 0 ) THEN
               IRXN = IPH( NRT,1 )
               KNUM = IPH( NRT,2 )

98703          FORMAT(11X,'RKI( NCELL, ', I4,'  ) = ', 1PD12.4,' * RKI( NCELL, ', I4,' ) ! ', A )
               WRITE(IOUT,98703)IRXN, RTDAT( 1,IRXN ), KNUM, 
     &             'Reaction ' // RXLABEL( IRXN )
            END IF
         END DO
      LINEOUT = 'END IF'
      WRITE(IOUT,'(/ 11X,A /)')TRIM(LINEOUT)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Do heterogeneous reactions
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      LINEOUT = 'C  Do reaction type -1: Heterogeneous reactions '
      IF( NHETERO .GT. 0 )WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)


      DO NRT = 1, MHETERO
         IRXN = IHETERO( NRT,1 )
         JNUM = IHETERO( NRT,2 )
         IF( RTDAT( 1,IRXN ) .NE. 1.0 )THEN
98812        FORMAT(11X,'RKI( NCELL, ', I4,'  ) = ', 1PD12.4,' * KHETERO( NCELL, ', I4,' )   ! ', A )
             WRITE(IOUT,98812)IRXN, RTDAT( 1,IRXN ), JNUM, 
     &       'Reaction ' // TRIM(RXLABEL( IRXN)) // ' <--- '// TRIM( PHOTAB( JNUM ) )
         ELSE
98822        FORMAT(11X,'RKI( NCELL, ', I4,'  ) = KHETERO( NCELL, ', I4,' ) ! ', A )
             WRITE(IOUT,98822)IRXN, JNUM, 
     &      'Reaction ' // TRIM(RXLABEL( IRXN)) // ' <--- '// TRIM( HETERO( JNUM ) )
         END IF
      END DO

      WRITE( IOUT, 99000)
       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do reaction type 1: k=A
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LINEOUT = 'C  Do reaction type 1: k=A '
      IF(  KTN1 .GT. 0 )THEN
         WRITE( IOUT, 98999)
         WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
      DO NRT = 1, KTN1
         IRXN = KRX1( NRT )

98704    FORMAT(11X,'RKI( NCELL, ', I4, ' ) = ', 1PD12.4, ' ! ', A )
         WRITE(IOUT, 98704)IRXN, RTDAT( 1,IRXN ), 
     &             'Reaction ' // RXLABEL( IRXN )
      END DO
      WRITE( IOUT, 99000)
      END IF
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do reaction type 2: k=A*(T/300)**B
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LINEOUT = 'C  Do reaction type 2: k=A*(T/300)**B '
      IF(  KTN2 .GT. 0 )THEN
         WRITE( IOUT, 98999)
         WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
         DO NRT = 1, KTN2
            IRXN = KRX2( NRT )
98705       FORMAT(11X,'RKI( NCELL, ', I4,' ) = ', 1PD12.4, ' * ( TEMPOT300( NCELL ) ** ', 1PD12.4 ,' ) ! ', A)
            WRITE(IOUT, 98705)IRXN, RTDAT( 1,IRXN ), RTDAT( 2,IRXN ), 
     &                   'Reaction ' // RXLABEL( IRXN )
         
         END DO
         WRITE( IOUT, 99000)
      END IF
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do reaction type 3: k=A*exp(C/T)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LINEOUT = 'C  Do reaction type 3: k=A*exp(C/T) '
      IF(  KTN3 .GT. 0 )THEN
         WRITE( IOUT, 98999)
         WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
         DO NRT = 1, KTN3
            IRXN = KRX3( NRT )
         
98706       FORMAT(11X,'RKI( NCELL, ', I4,' ) = ', 1PD12.4,' * EXP( ', 1PD12.4,' * TINV( NCELL )  ) ! ', A)
            WRITE(IOUT, 98706)IRXN, RTDAT( 1,IRXN ), RTDAT( 3,IRXN ), 
     &                'Reaction ' // RXLABEL( IRXN )
         END DO
         WRITE( IOUT, 99000)
      END IF
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do reaction type 4: k=A*((T/300)**B)*exp(C/T)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LINEOUT = 'C  Do reaction type 4: k=A*((T/300)**B)*exp(C/T) '
      IF(  KTN4 .GT. 0 )THEN
         WRITE( IOUT, 98999)
         WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
         DO NRT = 1, KTN4
            IRXN = KRX4( NRT )
98707       FORMAT(11X,'RKI( NCELL, ', I4,' ) = ', 1PD12.4,' * ( TEMPOT300( NCELL ) ** ', 1PD12.4, 
     &              ' ) * EXP( ', 1PD12.4,'  * TINV( NCELL ) )  ! ', A )
            WRITE(IOUT, 98707)IRXN, RTDAT( 1,IRXN ), RTDAT( 2,IRXN ), RTDAT( 3,IRXN ), 
     &                'Reaction ' // RXLABEL( IRXN )
         
         END DO
         WRITE( IOUT, 99000)
      END IF
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do reaction type 7: k=A*(1+0.6*P)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( KTN7 .GT. 0 )THEN
          WRITE( IOUT, 98999)

          LINEOUT = 'C  Do reaction type 7: k=A*(1+0.6*P) '
          WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
         
         DO NRT = 1, KTN7
            IRXN = KRX7( NRT )
98708       FORMAT(11X,'RKI( NCELL, ', I4,' ) = ', 1PD12.4,' * ( 1.0 + CONSTC * BLKPRES( NCELL ) )  ! ', A ) 
            WRITE(IOUT, 98708)IRXN, RTDAT( 1,IRXN ), 
     &                'Reaction ' // RXLABEL( IRXN )
         END DO
         WRITE( IOUT, 99000)
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Do fall offs and special type %2 (ktype 8)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
      IF( NFALLOFF .GT. 0 )THEN
         WRITE( IOUT, 98999)

         LINEOUT = 'C  Do fall offs: ktype 8, 9 and special type %2  '
         WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
      
         DO 100 NRT = 1, NFALLOFF
            IRXN = IRRFALL( NRT )
         
            IF ( KTYPE( IRXN ) .EQ. 8 ) THEN
         
                  WRITE(IOUT,98709)RTDAT( 1,IRXN ), RTDAT( 2,IRXN )
                  WRITE(IOUT,98710)RTDAT( 3,IRXN ), RFDAT( 1,NRT )
                  WRITE(IOUT,98711, ADVANCE = 'NO' )1.0E+6 * RFDAT( 2,NRT )
                  WRITE(IOUT,98712)RFDAT( 3,NRT )
                  WRITE(IOUT,98713)IRXN, 
     &                   'Reaction ' // RXLABEL( IRXN )
            
98709                FORMAT(11X,'RK0 = ', 1PD12.4,' * EXP( ', 1PD12.4,' * TINV( NCELL ) )')
98710                FORMAT(11X,'RK2 = ', 1PD12.4,' * EXP( ', 1PD12.4,' * TINV( NCELL ) )')
98711                FORMAT(11X,'RK3 = ', 1PD12.4,' * CFACT( NCELL )')
98712                FORMAT(' * EXP( ', 1PD12.4, ' * TINV( NCELL ) )')
98713                FORMAT(11X,'RKI( NCELL,', I4,' ) = RK0 + ( RK3 / ( 1.0 + RK3 / RK2 ) )  ! ', A/)
             
            ELSE IF ( KTYPE( IRXN ) .EQ. 9 ) THEN
         
                IF( RFDAT( 2, NRT ) .EQ. 0.0D0 )THEN
                  WRITE(IOUT,98714)RTDAT( 1,IRXN ), RTDAT( 2,IRXN )
                ELSE
                  WRITE(IOUT,98761)RTDAT( 1,IRXN ), RTDAT( 2,IRXN ), RFDAT( 2, NRT )
                ENDIF
         
                IF( RFDAT( 3, NRT ) .EQ. 0.0D0 .AND. RFDAT( 4, NRT ) .EQ. 0.0D0)THEN
                  WRITE(IOUT,98715)1.0E+6 * RTDAT( 3,IRXN ),RFDAT( 1, NRT )
!                  WRITE(IOUT,98716)
                ELSE
                  WRITE(IOUT,98765)1.0E+6 * RTDAT( 3,IRXN ),RFDAT( 1, NRT ),RFDAT( 3, NRT )
                  WRITE(IOUT,98766)RFDAT( 4, NRT ),RFDAT( 5, NRT )
                END IF
         
         
                  IF( RFDAT( 3, NRT ) .EQ. 0.0D0 )THEN
                      WRITE(IOUT,98717)IRXN, 'Reaction ' // RXLABEL( IRXN )
                  ELSE
                      WRITE(IOUT,98767)IRXN, 'Reaction ' // RXLABEL( IRXN )
                  END IF
               
98714             FORMAT(11X,'RK1 = ', 1PD12.4,' * EXP( ', 1PD12.4,' * TINV( NCELL ) )')
98761             FORMAT(11X,'RK1 = ', 1PD12.4,' * EXP( ', 1PD12.4,' * TINV( NCELL ) )'
     &                 /  5X,'&         *   TEMPOT300( NCELL ) **',1PD12.4 )  
         
98715             FORMAT(11X,'RK2 = ', 1PD12.4, ' * CFACT( NCELL ) * EXP( ', 1PD12.4,' * TINV( NCELL ) )')
98716             FORMAT(11X,'RK3 =   0.0D0' )
         
98765             FORMAT(11X,'RK2 = ', 1PD12.4, ' * CFACT( NCELL ) * EXP( ', 1PD12.4,' * TINV( NCELL ) )'
     &                 /  5X,'&         *   TEMPOT300( NCELL ) **',1PD12.4 )  
98766             FORMAT(11X,'RK3 = ', 1PD12.4,' * EXP( ', 1PD12.4,' * TINV( NCELL ) )')
         
98717             FORMAT(11X,'RKI( NCELL, ', I4,' ) = RK1 + RK2 ! ', A /)
98767             FORMAT(11X,'RKI( NCELL, ', I4,' ) = RK1 + RK2 + RK3 ! ', A /)
         
         
            ELSE

98718             FORMAT(11X,'RK0 = ', 1PD12.4, ' * CFACT( NCELL ) ' )
98719             FORMAT(' * ( TEMPOT300( NCELL ) ** ', 1PD12.4, ' )' )
98720             FORMAT(' * EXP( ', 1PD12.4,' * TINV( NCELL ) )')
                  WRITE(IOUT,98718, ADVANCE = 'NO' ) 1.0E+06 * RTDAT( 1,IRXN )
                  WRITE(IOUT,98719, ADVANCE = 'NO')RTDAT( 2,IRXN )
                  WRITE(IOUT,98720)RTDAT( 3,IRXN )
                  
98721             FORMAT(11X,'RK1 = ', 1PD12.4 )
98722             FORMAT(' * ( TEMPOT300( NCELL ) ** ', 1PD12.4, ' )' )
98723             FORMAT(' * EXP( ', 1PD12.4,' * TINV( NCELL ) )')
                  WRITE(IOUT,98721, ADVANCE = 'NO')RFDAT( 1,NRT )
                  WRITE(IOUT,98722, ADVANCE = 'NO')RFDAT( 2,NRT )
                  WRITE(IOUT,98723)RFDAT( 3,NRT )
                  
                  WRITE(IOUT,98750, ADVANCE = 'NO' )RFDAT( 5, NRT )
                  WRITE(IOUT,98751 )
98750             FORMAT(11X,'XEND = 1.0 / ( ( 1.0 + ( ( 1.0 / ', 1PD12.4,' )')
98751             FORMAT(' * LOG10( RK0 / RK1 ) ) ** 2 ) )' )                                   
                  
98725             FORMAT(11X,'RKI( NCELL, ', I4,' ) = ( RK0 / ( 1.0 + RK0 / RK1 ) )')
98726             FORMAT(' * ', 1PD12.4,' ** XEND ! ', A /)
                  WRITE(IOUT,98725, ADVANCE = 'NO' )IRXN
                  WRITE(IOUT,98726)RFDAT( 4,NRT ), 
     &                'Reaction ' // RXLABEL( IRXN )
   
            END IF
100      CONTINUE
         WRITE( IOUT, 99000)
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Do reaction type 5 and 6 (multipliers of above reactions)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LINEOUT = 'C  Do reaction type 5 (multipliers of above reactions) '
      IF(  KTN5 .GT. 0 )THEN
           WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
           WRITE( IOUT, 98999)
           DO NRT = 1, KTN5
              IRXN = KRX5( NRT )
              KNUM = INT( RTDAT( 3, IRXN ) )
              WRITE(IOUT,98727)IRXN, KNUM, RTDAT( 1,IRXN )
              WRITE(IOUT,98728)RTDAT( 2, IRXN ), 
     &                        'Reaction ' // RXLABEL( IRXN )
98727         FORMAT(11X,'RKI( NCELL, ', I4,' ) = RKI( NCELL, ', I4,' ) / (', 1PD12.4,' *')
98728         FORMAT(5X,' &                      EXP( ', 1PD12.4,' * TINV( NCELL ) ) ) ! ', A )          
           END DO
           WRITE( IOUT, 98999)
       END IF

      IF(  KTN6 .GT. 0 )THEN
           LINEOUT = 'C  Do reaction type 6 (multipliers of above reactions) '
           WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
           WRITE( IOUT, 98999)
            
           DO NRT = 1, KTN6
              IRXN = KRX6( NRT )
              KNUM = INT( RTDAT( 2,IRXN ) )

              IF( RTDAT( 1, IRXN ) .NE. 1.0 )THEN
                 WRITE(IOUT,98729)IRXN, RTDAT( 1, IRXN ), KNUM, 
     &                  'Reaction ' // RXLABEL( IRXN )
              ELSE
                 WRITE(IOUT,98629)IRXN, KNUM, 
     &                 'Reaction ' // RXLABEL( IRXN )
              END IF
98629         FORMAT(11X,'RKI( NCELL, ', I4,' ) = RKI( NCELL, ', I4,')  ! ', A )      
98729         FORMAT(11X,'RKI( NCELL, ', I4,' ) = ', 1PD12.4, ' * RKI( NCELL, ', I4,')  ! ', A )      
           END DO
           WRITE( IOUT, 99000)
       END IF
      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Convert to ppm units as needed
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF ( KUNITS .EQ. 2 ) THEN
         LINEOUT = 'C  Convert non-photolytic rate constants to ppm/min '
         WRITE(IOUT,'(A /)')TRIM(LINEOUT)

         WRITE( IOUT, 98999)

         DO N = 1, NRXNS

            IF ( KTYPE( N ) .GT. 0 ) THEN   ! Do all non-photolytic reactions

               SELECT CASE ( IORDER( N ) )
                 CASE( 2 )
                   WRITE(IOUT,98730)N,N
98730              FORMAT(11X,'RKI( NCELL, ', I4,' ) = 60.0D+0 * CFACT( NCELL ) * RKI( NCELL, ', I4,' ) ')
                 CASE( 3 )
                   WRITE(IOUT,98731)N,N
98731              FORMAT(11X,'RKI( NCELL, ', I4,' ) = 60.0D+0 * CFACT( NCELL ) * CFACT( NCELL ) * RKI( NCELL, ', I4,' ) ')
                 CASE( 1 ) 
                   WRITE(IOUT,98732)N,N
98732              FORMAT(11X,'RKI( NCELL, ', I4,' ) = 60.0D+0 * RKI( NCELL, ', I4,' ) ')
                 CASE( 0 ) 
                   WRITE(IOUT,98733)N,N
98733              FORMAT(11X,'RKI( NCELL, ', I4,' ) = 60.0D+0 * RKI( NCELL, ', I4,' ) / CFACT( NCELL ) ')
               END SELECT

            END IF
            
         END DO
         
         WRITE( IOUT, 99000 )

      ELSE
         LINEOUT = 'C  Convert Falloff reactions constants to ppm/min '
         WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
 
         IF( NFALLOFF .GT. 0 )THEN
      
            WRITE( IOUT, 98999 )
            DO N = 1, NFALLOFF      ! Do Falloff reactions only
               IRXN = IRRFALL( N )

               IF ( IORDER( IRXN ) .EQ. 2 ) THEN
                 WRITE(IOUT,98730)N,N
               ELSE IF ( IORDER( IRXN ) .EQ. 3 ) THEN
                  WRITE(IOUT,98731)N,N
                END IF
            END DO
            WRITE( IOUT, 99000 )
 
         END IF

      END IF
            
      IF( ( NWM + NWO2 + NWN2 + NWW + NWCH4 + NWH2 ) .GT. 0 )THEN
          WRITE( IOUT, 98999 )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Multiply rate constants by [M], [O2], [N2], or [H2O] where needed
c  and return
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( NWM .GT. 0 ) THEN
            LINEOUT = 'C Multiply rate constants by air number mixing ratio where needed'
            WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
            DO NRT = 1, NWM
               IRXN = NRXWM( NRT )
               WRITE(IOUT,98734)IRXN,IRXN,'ATM_AIR'
98734          FORMAT(11X,'RKI( NCELL, ', I4, ' ) = RKI( NCELL, ', I4,' ) * ', A )
            END DO
         END IF

         IF ( NWO2 .GT. 0 ) THEN
            LINEOUT = 'C Multiply rate constants by [O2] where needed'
            WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
            DO NRT = 1, NWO2
               IRXN = NRXWO2( NRT )
               WRITE(IOUT,98734)IRXN,IRXN,'ATM_O2'
            END DO
         END IF
         
         IF ( NWN2 .GT. 0 ) THEN
            LINEOUT = 'C Multiply rate constants by [N2] where needed'
            WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
            DO NRT = 1, NWN2
               IRXN = NRXWN2( NRT )
              WRITE(IOUT,98734)IRXN,IRXN,'ATM_N2'
            END DO
         END IF
         
         IF ( NWW .GT. 0 ) THEN
            LINEOUT = 'C Multiply rate constants by [H2O] where needed'
            WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
            DO NRT = 1, NWW
               IRXN = NRXWW( NRT )
               WRITE(IOUT,98734)IRXN,IRXN,'BLKCH2O( NCELL )'
            END DO
         END IF
         
         IF ( NWCH4 .GT. 0 ) THEN
            LINEOUT = 'C Multiply rate constants by [CH4] where needed'
            WRITE(IOUT,'(/ A / )')TRIM(LINEOUT)
            DO NRT = 1, NWCH4
               IRXN = NRXWCH4( NRT )
               WRITE(IOUT,98734)IRXN,IRXN,'ATM_CH4'
            END DO
         END IF
         
         IF ( NWH2 .GT. 0 ) THEN
            LINEOUT = 'C Multiply rate constants by [H2] where needed'
            WRITE(IOUT,'(/ A /)')TRIM(LINEOUT)
            DO NRT = 1, NWH2
               IRXN = NRXWH2( NRT )
               WRITE(IOUT,98734)IRXN,IRXN,'ATM_H2'
            END DO
         END IF
         
         WRITE( IOUT, 99000 )
      
      END IF

      
      WRITE(IOUT,99991)
      
      RETURN
      
99880 FORMAT(7X,'SUBROUTINE CALC_RCONST( BLKTEMP, BLKPRES, BLKCH2O, RJBLK, RKI, NUMCELLS )' //
     & 'C**********************************************************************' //
     & 'C  Function: To compute thermal and photolytic reaction rate' /
     & 'C            coefficients for each reaction.' //
     & 'C  Preconditions: Photolysis rates for individual species must have' /
     & 'C                 been calculated and stored in RJBLK. Expects' /
     & 'C                 temperature in deg K, pressure in atm., water' /
     & 'C                 vapor in ppmV, and J-values in /min.' /
     & 'C  Key Subroutines/Functions Called: None ' /
     & 'C***********************************************************************'///
     & '        USE GRID_CONF           ! horizontal & vertical domain specifications'/
     & '        USE AEROSOL_CHEMISTRY   ! rates for heterogeneous reactions' //
     & '        IMPLICIT NONE  ' //
     & 'C..Includes:'//
     & '        INCLUDE SUBST_RXCMMN ' //
     & 'C  Arguments: None ' //
     & '        REAL( 8 ), INTENT( IN  ) :: BLKTEMP( : )      ! temperature, deg K '/
     & '        REAL( 8 ), INTENT( IN  ) :: BLKPRES( : )      ! Reciprocal of temperature, Atm '/
     & '        REAL( 8 ), INTENT( IN  ) :: BLKCH2O( : )      ! water mixing ratio, ppm '/
     & '        REAL( 8 ), INTENT( IN  ) :: RJBLK  ( :, : )   ! photolysis rates, 1/min '/ 
     & '        REAL( 8 ), INTENT( OUT ) :: RKI    ( :, : )   ! reaction rate constants, ppm/min '/
     & '        INTEGER,   INTENT( IN  ) :: NUMCELLS          ! Number of cells in block ' /
     & 'C..Parameters: ' //
     & '        REAL( 8 ), PARAMETER :: COEF1  = 7.33981D+15     ! Molec/cc to ppm conv factor ' /
     & '        REAL( 8 ), PARAMETER :: CONSTC = 0.6D+0          ! Constant for reaction type 7' /
     & '        REAL( 8 ), PARAMETER :: TI300  = 1.0D+0/300.0D+0 ! reciprocal of 300 deg K' /
     & 'C..External Functions: None' //
     & 'C..Local Variables:' //
     & '        REAL( 8 ) :: CFACT( BLKSIZE )     ! molec/cc to ppm conversion factor   '/
     & '        REAL( 8 ) :: TINV ( BLKSIZE )     ! Reciprocal of temperature, 1/(deg K) '/
     & '        REAL( 8 ) :: TEMPOT300( BLKSIZE ) ! temperature divided by 300 K, dimensionaless '/
     & '        INTEGER   :: NRT                  ! Loop index for reaction types '/
     & '        INTEGER   :: IRXN                 ! Reaction number'/
     & '        INTEGER   :: JNUM                 ! J-value species # from PHOT)'/
     & '        INTEGER   :: KNUM                 ! Reaction # for a relative rate coeff.'/
     & '        INTEGER   :: N                    ! Loop index for reactions'/
     & '        INTEGER   :: NCELL                ! Loop index for # of cells in the block' //
     & '        REAL      :: RK0                  ! K0 in falloff rate expressions'/
     & '        REAL      :: RK1                  ! k1 in falloff rate expressions'/
     & '        REAL      :: RK2                  ! K2 in falloff rate expressions'/
     & '        REAL      :: RK3                  ! K3 in falloff rate expressions'/
     & '        REAL      :: XEND                 ! Exponent in falloff rate expressions'/
     & '        LOGICAL   :: LSUNLIGHT            ! Is there sunlight in Block? ' ///
     & 'C...Determine whether day or night'/
     & '        IF( MINVAL( RJBLK ) .GT. 0.0D0 )THEN '/
     & '            LSUNLIGHT = .TRUE.'/
     & '        ELSE'/
     & '            LSUNLIGHT = .FALSE.'/
     & '        END IF'//
     & 'C...initialize rate constants'/
     & '        RKI = 0.0D0'//
     & 'C...compute cell properties ' /
     & '        DO NCELL = 1, NUMCELLS ' /
     & '           TINV( NCELL )      = 1.0D+0 / BLKTEMP( NCELL ) '/
     & '           CFACT( NCELL )     = COEF1 * BLKPRES( NCELL ) * TINV( NCELL )'/
     & '           TEMPOT300( NCELL ) = BLKTEMP( NCELL ) * TI300 ' / 
     & '        END DO' //
     & 'C...set rate constants' /
     & '        DO NCELL = 1, NUMCELLS ' //)
     
99991  FORMAT(7X // 7X,'RETURN' // 7X,'END SUBROUTINE CALC_RCONST')
98999  FORMAT(/ 7X,'DO NCELL   = 1, NUMCELLS ' /)     
99000  FORMAT(/ 7X,'END DO' /)     
      END



