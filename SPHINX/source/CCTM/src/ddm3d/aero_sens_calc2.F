#ifdef sens
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

C ===================================================================
C This subroutine calculates second-order sensitivity of ISORROPIAII
C
C Written by Wenxian Zhang in August 2011
C
C 27 September 2013: Sergey L. Napelenok 
C    --- implemented into CMAQv5.0.2
C    --- disabled hddm activity and water sensitivity calculations
C    --- to finish code development for these
C 08 September 2014: Sergey L. Napelenok
C    --- some minor bug fixes
C 
C Reference: 
C Zhang, W., Capps, S. L., Hu, Y., Nenes, A., Napelenok, S. L., & 
C     Russell, A. G. (2012). Development of the high-order decoupled 
C     direct method in three dimensions for particulate matter: 
C     enabling advanced sensitivity analysis in air quality models. 
C     Geoscientific Model Development, 5(2), 355-368. 
C     doi: 10.5194/gmd-5-355-2012
C ===================================================================

      SUBROUTINE AERO_SENS_CALC2(STOT,SENS,S1,S2,S1D,S2D,SCASI,FCOL)

c     USE DDM3D_DEFN, ONLY : WRFLAG

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      INTEGER, INTENT (INOUT) :: FCOL( : )
      DOUBLE PRECISION, INTENT( IN    ) :: STOT( : )
      DOUBLE PRECISION, INTENT( OUT   ) :: SENS( : )     !OUTPUT, HDDM
      DOUBLE PRECISION, INTENT( IN    ) :: S1( : )       !INPUT, 1ST ORDER SENS
      DOUBLE PRECISION, INTENT( IN    ) :: S2( : )       !INPUT, 2ND ORDER SENS
      DOUBLE PRECISION, INTENT( IN    ) :: S1D( : )      !INPUT, 1ST ORDER SENS
      DOUBLE PRECISION, INTENT( IN    ) :: S2D( : )      !INPUT, 2ND ORDER SENS
      CHARACTER( 15 ),  INTENT( IN    ) :: SCASI         ! (input) subcase number from ISOROPIA

      INTEGER FROW(NSEN)
      DOUBLE PRECISION :: COEF(NSEN,NSEN) !COEFFICIENT MATRIX 
      DOUBLE PRECISION SENSD(NSEN)
      DOUBLE PRECISION DGAMA(NIONSPC,NPAIR)   !dGAMA/dA
      DOUBLE PRECISION RGAMA(NPAIR)       ! RHS FROM 2ND-ORDER GAMA SENSITIVITIES

c     INTEGER, SAVE :: LOGDEV
c     LOGICAL, SAVE :: FIRSTIME = .TRUE.


c     IF ( FIRSTIME ) THEN
c        FIRSTIME = .FALSE.
c        LOGDEV = INIT3 ()
c     ENDIF

      INTEGER I

      CC = SCASI(1:1)

C *** INITIALIZE SINI ***

      DO I = 1,NSEN
         SINI(I) = 0.D0
      ENDDO

      SINI(iMBNA)  = STOT(1)
      SINI(iMBSO4) = STOT(2)
      SINI(iMBNH4) = STOT(3)
      SINI(iMBNO3) = STOT(4)
      SINI(iMBCL)  = STOT(5)
      SINI(iMBCA)  = STOT(6)
      SINI(iMBK)   = STOT(7)
      SINI(iMBMG)  = STOT(8)

c *** SET ROW AND COL FLAGS ***

      CALL FLAGS( FROW, FCOL )      

C *** CALCULATE DGAMA ***

c     IF (CC.EQ.'A'.OR.CC.EQ.'B'.OR.CC.EQ.'C'.OR.
c    &    CC.EQ.'D'.OR.CC.EQ.'E'.OR.CC.EQ.'F'.OR.
c    &    CC.EQ.'G'.OR.CC.EQ.'H'.OR.CC.EQ.'I'.OR.
c    &    CC.EQ.'J') THEN
c        CALL DELGAMA1( DGAMA )
c        CALL RHSGAMA1( RGAMA, S1D, S2D )
c     ELSE
c        CALL DELGAMA2( DGAMA,frow )
c        CALL RHSGAMA2( RGAMA, S1D, S2D )
c     ENDIF

      DGAMA = 0.0D0 ! set to ignore activity
      RGAMA = 0.0D0 ! set to ignore activity

C *** CALCULATE COEFFICIENT MATRIX ***

      CALL AMAT( COEF, FROW, FCOL, DGAMA )

C *** CREATE THE RIGHT HAND SIDE ***

      CALL RHS( S1D, S2D, RGAMA, FROW )

C *** SOLVE SENSITIVITIES ***

      CALL EQNSLV( FROW, FCOL, COEF, SENS, SENSD )

C *** ADJUST FOR MINOR SPECIES ***

c     DO I = 1,NPAIR
c        SGAMA(I) = 0.D0
c        DO J = 1,NIONSPC
c           SGAMA(I) = SGAMA(I)+DGAMA(J,I)*SENS(J)
c        ENDDO
c        SGAMA(I) = SGAMA(I) + RGAMA(I)
c     ENDDO

      SGAMA = 0.0D0 ! set to ignore activity

c     IF (CC.EQ.'B'.OR.CC.EQ.'C') THEN
c        CALL HDCALCNH3(SENS, S1, S2)
c     ELSEIF (CC.EQ.'E'.OR.CC.EQ.'F') THEN
c        CALL HDCALCNA( SENS, S1, S2)
c     ELSEIF (CC.EQ.'I'.OR.CC.EQ.'J'.OR.
c    &        CC.EQ.'L'.OR.CC.EQ.'K') THEN
c        CALL HDCALCNHA(SENS, S1, S2)
c        CALL HDCALCNH3(SENS, S1, S2)
c     ELSEIF (CC.EQ.'D'.OR.CC.EQ.'G'.OR.CC.EQ.'H'.OR.
c    &        CC.EQ.'O'.OR.CC.EQ.'M'.OR.CC.EQ.'P') THEN
c        CALL HDCALCHS4(SENS, S1, S2)
c     ENDIF

C *** END OF HDDMSENS ***

      RETURN
      END SUBROUTINE AERO_SENS_CALC2
         
      SUBROUTINE RHSGAMA1( RG, S1, S2 )

c     IMPLICIT NONE
        
      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'
      INCLUDE 'dact.inc'
  
      DOUBLE PRECISION RG(NPAIR)
      DOUBLE PRECISION S1(NSEN),S2(NSEN)
      DOUBLE PRECISION RI, RHG
      DOUBLE PRECISION RG0(NPAIR)
      DOUBLE PRECISION RX(3,4), RY(3,4)
      DOUBLE PRECISION RF1(3), RF2(4)
      DOUBLE PRECISION ISR, IM5, IM1, IM15
      DOUBLE PRECISION SUMZS1, SUMZS2, DN, DN2, DN3
      INTEGER I, J
      DOUBLE PRECISION ZPL, XPL, ZMI, XMI, CH, RXIJ, RYJI

      SUMZS1 = 0.D0
      SUMZS2 = 0.D0
c     DO I = 1, NIONSPC
      DO I = 1, NIONS
         SUMZS1 = SUMZS1 +Z(I)*Z(I)*S1(I)
         SUMZS2 = SUMZS2 +Z(I)*Z(I)*S2(I)
      ENDDO
      
      IF (IONIC.GE.100.d0) THEN
         RI = 0.D0
      ELSE
         RI =  2.D0 *IONIC*S1(jH2O)*S2(jH2O)
     &        +0.5D0*SUMZS1*S2(jH2O)
     &        +0.5D0*SUMZS2*S1(jH2O)
         RI = RI/WATER/WATER
      ENDIF

      ISR  = SQRT(IONIC)
      IM5  = IONIC**(-0.5D0)
      IM1  = IONIC**(-1.D0)
      IM15 = IONIC**(-1.5D0)
      DN   = 1.D0+ISR
      DN2  = DN**2.D0
      DN3  = DN**3.D0
      RHG   = IM5*RI/DN2 -(0.5D0*IM15+1.5D0*IM1)*SI1*SI2/DN3
  
      CALL RDKMFUL(RG0,NPAIR,IONIC,SNGL(TEMP),RI,RHG,SI1,SI2)

      DO I = 1, 3
         ZPL = Z(I)
         XPL = MOLALD(I)/WATER
         DO J = 1, 4
            ZMI = Z(J+3)
            XMI = MOLALD(J+3)/WATER
            CH  = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            RXIJ =                                      -XPL*RI/IONIC
     &             -     (S2(jH2O)*S1(I) +S1(jH2O)*S2(I))/WATER/WATER
     &             -               (SI2*S1(I) +SI1*S2(I))/WATER/IONIC
     &             +     XPL*(S2(jH2O)*SI1 +S1(jH2O)*SI2)/WATER/IONIC
     &             +           2.D0*XPL*S1(jH2O)*S2(jH2O)/WATER/WATER
     &             +                     2.D0*XPL*SI1*SI2/IONIC/IONIC

            RYJI =                                      -XMI*RI/IONIC
     &             - (S2(jH2O)*S1(J+3) +S1(jH2O)*S2(J+3))/WATER/WATER
     &             -           (SI2*S1(J+3) +SI1*S2(J+3))/WATER/IONIC
     &             +     XMI*(S2(jH2O)*SI1 +S1(jH2O)*SI2)/WATER/IONIC
     &             +           2.D0*XMI*S1(jH2O)*S2(jH2O)/WATER/WATER
     &             +                     2.D0*XMI*SI1*SI2/IONIC/IONIC
 
            RX(I,J) = CH*RXIJ
            RY(I,J) = CH*RYJI

            RF1(I) =  SY2(I,J)*(SG01(IJMAP(I,J)) +ZPL*ZMI*SH1)
     &              + SY1(I,J)*(SG02(IJMAP(I,J)) +ZPL*ZMI*SH2)
     &              + Y(I,J)  * RG0(IJMAP(I,J))
     &              + Y(I,J)  * RHG *ZPL *ZMI
     &              + RY(I,J) *(G0P(IJMAP(I,J)) +ZPL*ZMI*H)

            RF2(J) =  SX2(I,J)*(SG01(IJMAP(I,J)) +ZPL*ZMI*SH1)
     &              + SX1(I,J)*(SG02(IJMAP(I,J)) +ZPL*ZMI*SH2)
     &              + X(I,J)  * RG0(IJMAP(I,J))
     &              + X(I,J)  * RHG *ZPL *ZMI
     &              + RX(I,J) *(G0P(IJMAP(I,J)) +ZPL*ZMI*H)
 
            RG(IJMAP(I,J)) =    ZPL*ZMI*(
     &                    (RF1(I)/ZPL + RF2(J)/ZMI)/(ZPL + ZMI) -RHG)
         ENDDO
      ENDDO
  
      RG(mLC) = 0.2 *(3.0 *RG(mNH42S4) + 2.0 *RG(mNH4HS4))
  
      DO I = 1, NPAIR
         IF (GAMA(I).LE.1.d-5 .OR. GAMA(I).GE.1.d5) THEN
            RG(I) = 0.0
         ENDIF
      ENDDO
  
      RETURN
      END 
  
      SUBROUTINE RHSGAMA2( RG, S1, S2 )

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'
      INCLUDE 'dact.inc'
  
      DOUBLE PRECISION RG(NPAIR)
      DOUBLE PRECISION S1(NSEN),S2(NSEN)
      DOUBLE PRECISION RI, RHG
      DOUBLE PRECISION RG0(NPAIR)
      DOUBLE PRECISION RX(6,4), RY(6,4)
      DOUBLE PRECISION RF1(6), RF2A(4), RF2B(4)
      DOUBLE PRECISION ISR, IM5, IM1, IM15
      INTEGER I, J
      DOUBLE PRECISION ZPL, XPL, SUMZS1, SUMZS2, DN, DN2, DN3, ZMI, XMI
      DOUBLE PRECISION CH, RXIJ, RYJI

      SUMZS1 = 0.D0
      SUMZS2 = 0.D0
c     DO I = 1, NIONSPC
      DO I = 1, NIONS
         SUMZS1 = SUMZS1 +Z(I)*Z(I)*S1(I)
         SUMZS2 = SUMZS2 +Z(I)*Z(I)*S2(I)
      ENDDO
      
      IF (IONIC.GE.100.d0) THEN
         RI = 0.D0
      ELSE
         RI =  2.D0 *IONIC*S1(jH2O)*S2(jH2O)
     &        +0.5D0*SUMZS1*S2(jH2O)
     &        +0.5D0*SUMZS2*S1(jH2O)
         RI = RI/WATER/WATER
      ENDIF

      ISR  = SQRT(IONIC)
      IM5  = IONIC**(-0.5D0)
      IM1  = IONIC**(-1.D0)
      IM15 = IONIC**(-1.5D0)
      DN   = 1.D0+ISR
      DN2  = DN**2.D0
      DN3  = DN**3.D0
      RHG   = IM5*RI/DN2 -(0.5D0*IM15+1.5D0*IM1)*SI1*SI2/DN3
  
      CALL RDKMFUL2(RG0,NPAIR,IONIC,SNGL(TEMP),RI,RHG,SI1,SI2)

      DO I = 1, 3
         ZPL = Z(I)
         XPL = MOLALD(I)/WATER
         DO J = 1, 4
            ZMI = Z(J+3)
            XMI = MOLALD(J+3)/WATER
            CH  = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            RXIJ =                                      -XPL*RI/IONIC
     &             -     (S2(jH2O)*S1(I) +S1(jH2O)*S2(I))/WATER/WATER
     &             -               (SI2*S1(I) +SI1*S2(I))/WATER/IONIC
     &             +     XPL*(S2(jH2O)*SI1 +S1(jH2O)*SI2)/WATER/IONIC
     &             +           2.D0*XPL*S1(jH2O)*S2(jH2O)/WATER/WATER
     &             +                     2.D0*XPL*SI1*SI2/IONIC/IONIC

            RYJI =                                      -XMI*RI/IONIC
     &             - (S2(jH2O)*S1(J+3) +S1(jH2O)*S2(J+3))/WATER/WATER
     &             -           (SI2*S1(J+3) +SI1*S2(J+3))/WATER/IONIC
     &             +     XMI*(S2(jH2O)*SI1 +S1(jH2O)*SI2)/WATER/IONIC
     &             +           2.D0*XMI*S1(jH2O)*S2(jH2O)/WATER/WATER
     &             +                     2.D0*XMI*SI1*SI2/IONIC/IONIC
 
            RX(I,J) = CH*RXIJ
            RY(I,J) = CH*RYJI

            RF1(I) =  SY2(I,J)*(SG01(IJMAP(I,J)) +ZPL*ZMI*SH1)
     &              + SY1(I,J)*(SG02(IJMAP(I,J)) +ZPL*ZMI*SH2)
     &              + Y(I,J)  * RG0(IJMAP(I,J))
     &              + Y(I,J)  * RHG *ZPL *ZMI
     &              + RY(I,J) *(G0P(IJMAP(I,J)) +ZPL*ZMI*H)

            RF2A(J) =  SX2(I,J)*(SG01(IJMAP(I,J)) +ZPL*ZMI*SH1)
     &              + SX1(I,J)*(SG02(IJMAP(I,J)) +ZPL*ZMI*SH2)
     &              + X(I,J)  * RG0(IJMAP(I,J))
     &              + X(I,J)  * RHG *ZPL *ZMI
     &              + RX(I,J) *(G0P(IJMAP(I,J)) +ZPL*ZMI*H)
 
           RG(IJMAP(I,J)) =    ZPL*ZMI*(
     &                    (RF1(I)/ZPL + RF2A(J)/ZMI)/(ZPL + ZMI) -RHG)
         ENDDO
      ENDDO
  
      DO 100 I = 4, 6
         ZPL = Z(I+4)
         XPL = MOLALD(I+4)/WATER
         DO 100 J = 1, 4
 
            IF(J.EQ.3.AND.I.EQ.4) GOTO 100
            IF(J.EQ.3.AND.I.EQ.6) GOTO 100
            IF(J.EQ.2.AND.I.EQ.4) GOTO 100

            ZMI = Z(J+3)
            XMI = MOLALD(J+3)/WATER
            CH  = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            RXIJ   =                                    -XPL*RI/IONIC
     &             - (S2(jH2O)*S1(I+4) +S1(jH2O)*S2(I+4))/WATER/WATER
     &             -           (SI2*S1(I+4) +SI1*S2(I+4))/WATER/IONIC
     &             +     XPL*(S2(jH2O)*SI1 +S1(jH2O)*SI2)/WATER/IONIC
     &             +           2.D0*XPL*S1(jH2O)*S2(jH2O)/WATER/WATER
     &             +                     2.D0*XPL*SI1*SI2/IONIC/IONIC

            RYJI   =                                    -XMI*RI/IONIC
     &             - (S2(jH2O)*S1(J+3) +S1(jH2O)*S2(J+3))/WATER/WATER
     &             -           (SI2*S1(J+3) +SI1*S2(J+3))/WATER/IONIC
     &             +     XMI*(S2(jH2O)*SI1 +S1(jH2O)*SI2)/WATER/IONIC
     &             +           2.D0*XMI*S1(jH2O)*S2(jH2O)/WATER/WATER
     &             +                     2.D0*XMI*SI1*SI2/IONIC/IONIC
 
            RX(I,J) = CH*RXIJ
            RY(I,J) = CH*RYJI

            RF1(I)  = SY2(I,J)*(SG01(IJMAP(I,J)) +ZPL*ZMI*SH1)
     &              + SY1(I,J)*(SG02(IJMAP(I,J)) +ZPL*ZMI*SH2)
     &              + Y(I,J)  * RG0(IJMAP(I,J))
     &              + Y(I,J)  * RHG *ZPL *ZMI
     &              + RY(I,J) *(G0P(IJMAP(I,J)) +ZPL*ZMI*H)

            RF2B(J) = SX2(I,J)*(SG01(IJMAP(I,J)) +ZPL*ZMI*SH1)
     &              + SX1(I,J)*(SG02(IJMAP(I,J)) +ZPL*ZMI*SH2)
     &              + X(I,J)  * RG0(IJMAP(I,J))
     &              + X(I,J)  * RHG *ZPL *ZMI
     &              + RX(I,J) *(G0P(IJMAP(I,J)) +ZPL*ZMI*H)
 
            RG(IJMAP(I,J)) =    ZPL*ZMI*(
     &                    (RF1(I)/ZPL + RF2B(J)/ZMI)/(ZPL + ZMI) -RHG)
 100  CONTINUE
  
      RG(mCASO4) = 0.0
      RG(mLC)    = 0.2 *(3.0 *RG(mNH42S4) + 2.0 *RG(mNH4HS4))
  
      DO I = 1, NPAIR
         IF (GAMA(I).LE.1.d-5 .OR. GAMA(I).GE.1.d5) THEN
            RG(I) = 0.0
         ENDIF
      ENDDO
  
      RETURN
      END
  
      SUBROUTINE RDKMFUL(RG0,NPAIRS,IONIC,TEMP,RI,RHG,SI1,SI2)

      IMPLICIT NONE

      INTEGER N,NPAIRS
      REAL IONIC,TEMP
      DOUBLE PRECISION RI, RHG, RG0(NPAIRS)
      DOUBLE PRECISION SI1, SI2
  
      INTEGER   NPAIRD, I, J, K
      PARAMETER (NPAIRD=10)               ! Number of ion pairs whose Q value is available
      INTEGER   IG(NPAIRD)
      DATA IG / 1,2,3,4,5,6,7,8,10,11 /
      REAL    ZI(NPAIRD)                ! Mapping of Q to the internal order of ion pairs
      DATA ZI / 1., 2., 1., 2., 1., 1., 2., 1., 1., 1. /
      REAL  Q(NPAIRD)                 ! Kusik-Meissner parameters (see KMFUL)
      DATA Q  / 2.23,-0.19,-0.39,-0.25,-1.15,0.82,-0.1,
     &          8.0,2.6,6.0 /

      REAL SION  ! sln 13sep13
      REAL AGAMA ! sln 23sep13

      REAL*8 TI, CF1, CF2, RCF2

      SION = SQRT(IONIC) ! sln 13sep13
      AGAMA = 0.511*(298.0/TEMP)**1.5 ! Debye Huckel const. at T  ! sln 23sep13



      DO I = 1, NPAIRD
         CALL RDMKBI(RG0(IG(I)), IONIC, Q(I), ZI(I), RI, RHG,
     &               SI1, SI2)
      ENDDO
  
      TI  = TEMP-273.0
      IF (ABS(TI-25.0) .GT. 1.0) THEN
         CF1  = 1.125-0.005*TI
         CF2  = (CF1-1.)*(0.039*IONIC**0.92-0.41*SION/(1.+SION))
         RCF2 = (CF1-1.)*(-.00287*IONIC**(-1.08)*SI1*SI2
     &                    +.03588*RI
     &                    -.41*RHG/AGAMA)

         DO I = 1, NPAIRD
            RG0(IG(I)) = CF1*RG0(IG(I)) - RCF2*ZI(I)
         ENDDO
      ENDIF
  
      RG0( 9) = RG0( 6) + RG0( 8) - RG0(11)
      RG0(12) = RG0( 1) + RG0( 8) - RG0(11)
  
      RETURN
      END
  
      SUBROUTINE RDKMFUL2(RG0,NPAIRS,IONIC,TEMP,RI,RHG,SI1,SI2)

      IMPLICIT NONE

      INTEGER N,NPAIRS
      REAL IONIC,TEMP
      DOUBLE PRECISION RI, RHG, RG0(NPAIRS)
      DOUBLE PRECISION SI1, SI2
  
      INTEGER   NPAIRD, I, J, K
      PARAMETER (NPAIRD=10)               ! Number of ion pairs whose Q value is available
      INTEGER   IG(NPAIRD)
      DATA IG / 1,2,3,4,5,6,7,8,10,11 /
      REAL    ZI(NPAIRD)                ! Mapping of Q to the internal order of ion pairs
      DATA ZI / 1., 2., 1., 2., 1., 1., 2., 1., 1., 1. /
      REAL  Q(NPAIRD)                 ! Kusik-Meissner parameters (see KMFUL)
      DATA Q  / 2.23,-0.19,-0.39,-0.25,-1.15,0.82,-0.1,
     &          8.0,2.6,6.0 /

      REAL SION  ! sln 13sep13
      REAL AGAMA ! sln 23sep13

      REAL*8 TI, CF1, CF2, RCF2

      SION = SQRT(IONIC) ! sln 13sep13
      AGAMA = 0.511*(298.0/TEMP)**1.5 ! Debye Huckel const. at T  ! sln 23sep13

      DO I = 1, NPAIRD
         CALL RDMKBI(RG0(IG(I)), IONIC, Q(I), ZI(I), RI, RHG,
     &               SI1, SI2)
      ENDDO
 
      TI  = TEMP-273.0
      IF (ABS(TI-25.0) .GT. 1.0) THEN
         CF1  = 1.125-0.005*TI
         CF2  = (CF1-1.)*(0.039*IONIC**0.92-0.41*SION/(1.+SION))
         RCF2 = (CF1-1.)*(-.00287*IONIC**(-1.08)*SI1*SI2
     &                    +.03588*RI
     &                    -.41*RHG/AGAMA)

         DO I = 1, NPAIRD
            RG0(IG(I)) = CF1*RG0(IG(I)) - RCF2*ZI(I)
         ENDDO
      ENDIF
  
      RG0( 9) = RG0( 6) + RG0( 8) - RG0(11)
      RG0(12) = RG0( 1) + RG0( 8) - RG0(11)
  
      RETURN
      END
  
      SUBROUTINE RDMKBI(RG0, IONIC, Q, ZIP, RI, RHG, SI1, SI2)

      IMPLICIT NONE

      REAL IONIC, Q, ZIP
      DOUBLE PRECISION RI, RHG, RI1, RI2, RG0
      DOUBLE PRECISION SI1, SI2 ! sln 4sep14 
 
      REAL B, BI, XX1, XX2
      DOUBLE PRECISION RB
 
      DOUBLE PRECISION   LN10
      PARAMETER          (LN10=2.30258509299404568402D0)

 
      B   = .75 -.065*Q
      BI  =  1. +B*(1.+.1*IONIC)**Q -B

      XX1 = .1*B*Q*(1.+.1*IONIC)**(Q-1.)/(BI*LN10)
      XX2 = .1*(Q-1.)/(1.+.1*IONIC)

      RB  =  XX1*(XX2*SI1*SI2 +RI)
 
      RG0 = ZIP*(RB -RHG)

      RETURN
      END
  
      SUBROUTINE RHS(S1, S2, RG, FROW)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'
  
      DOUBLE PRECISION S1(NSEN), S2(NSEN)
      DOUBLE PRECISION RG(NPAIR)
      INTEGER          FROW(NSEN)
      INTEGER iEQ
      DOUBLE PRECISION C1, C2 
      DOUBLE PRECISION CNA, CK, CMG

      iEQ = iK1
 
      IF (FROW(iEQ).EQ.1) THEN
         C1 = -3.*LN10
         C2 =  2.*LN10
         SINI(iEQ) = C1*RG(mH2SO4) +C2*RG(mHHSO4)
     &             + S1(   jH) *S2(   jH)/MOLALD(   jH)/MOLALD(   jH)
     &             + S1( jSO4) *S2( jSO4)/MOLALD( jSO4)/MOLALD( jSO4)
     &             - S1(jHSO4) *S2(jHSO4)/MOLALD(jHSO4)/MOLALD(jHSO4)
     &             - S1( jH2O) *S2( jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK2    
  
      IF (FROW(iEQ).EQ.1) THEN
         IF (CC.EQ.'A') THEN
            C1 = -2.*LN10
            C2 =  2.*LN10
            SINI(iEQ) = C1*RG(mNH4HS4) +C2*RG(mHHSO4)
         ELSE
            C1 = -2.*LN10
            C2 =  2.*LN10
            SINI(iEQ) = C1*RG(mNH4NO3) +C2*RG(mHNO3)
         ENDIF
         SINI(iEQ) = SINI(iEQ)
     &             + S1(jNH4)*S2(jNH4)/MOLALD(jNH4)/MOLALD(jNH4)
     &             - S1(  jH)*S2(  jH)/MOLALD(  jH)/MOLALD(  jH)
     &             - S1(jNH3)*S2(jNH3)/GNH3D/GNH3D
       ENDIF
   
      iEQ = iK3

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -2.*LN10
         SINI(iEQ) =    C1*RG(mHCL)
     &             +    S1(  jH)*S2(  jH)/MOLALD(  jH)/MOLALD(  jH)
     &             +    S1( jCL)*S2( jCL)/MOLALD( jCL)/MOLALD( jCL)
     &             -    S1(jHCL)*S2(jHCL)/GHCLD/GHCLD
     &             - 2.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK4

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -2.*LN10
         SINI(iEQ) =    C1*RG(mHNO3)
     &             +    S1(   jH)*S2(   jH)/MOLALD(   jH)/MOLALD(   jH)
     &             +    S1( jNO3)*S2( jNO3)/MOLALD( jNO3)/MOLALD( jNO3)
     &             -    S1(jHNO3)*S2(jHNO3)/GHNO3D/GHNO3D
     &             - 2.*S1( jH2O)*S2( jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK5
  
      IF (FROW(iEQ).EQ.1) THEN
         C1  = -3.*LN10
         CNA = MAX(MOLALD(jNA), TINY)
         SINI(iEQ) =    C1*RG(mNA2SO4)
     &             + 2.*S1( jNA)*S2( jNA)/CNA/CNA
     &             +    S1(jSO4)*S2(jSO4)/MOLALD(jSO4)/MOLALD(jSO4)
     &             - 3.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK6

      IF (FROW(iEQ).EQ.1) THEN
         SINI(iEQ) = S1(jHCL)*S2(jHCL)/GHCLD/GHCLD
     &             + S1(jNH3)*S2(jNH3)/GNH3D/GNH3D
      ENDIF
  
      iEQ = iK7

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -3.*LN10
         SINI(iEQ) =    C1*RG(mNH42S4)
     &             + 2.*S1(jNH4)*S2(jNH4)/MOLALD(jNH4)/MOLALD(jNH4)
     &             +    S1(jSO4)*S2(jSO4)/MOLALD(jSO4)/MOLALD(jSO4)
     &             - 3.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK8

      IF (FROW(iEQ).EQ.1) THEN
         C1  = -2.*LN10
         CNA = MAX(MOLALD(jNA),TINY)
         SINI(iEQ) =    C1*RG(mNACL)
     &             +    S1( jNA)*S2( jNA)/CNA/CNA
     &             +    S1( jCL)*S2( jCL)/MOLALD(jCL)/MOLALD(jCL)
     &             - 2.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK9

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -2.*LN10
         CNA = MAX(MOLALD(jNA),TINY)
         SINI(iEQ) =    C1*RG(mNANO3)
     &             +    S1( jNA)*S2( jNA)/CNA/CNA
     &             +    S1(jNO3)*S2(jNO3)/MOLALD(jNO3)/MOLALD(jNO3)
     &             - 2.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK10

      IF (FROW(iEQ).EQ.1) THEN
         SINI(iEQ) = S1( jNH3)*S2( jNH3)/ GNH3D/ GNH3D
     &             + S1(jHNO3)*S2(jHNO3)/GHNO3D/GHNO3D
      ENDIF
  
      iEQ = iK11

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -2.*LN10
         CNA = MAX(MOLALD(jNA),TINY)
         SINI(iEQ) =    C1*RG(mNAHSO4)
     &             +    S1(  jNA)*S2(  jNA)/CNA/CNA
     &             +    S1(jHSO4)*S2(jHSO4)/MOLALD(jHSO4)/MOLALD(jHSO4)
     &             - 2.*S1( jH2O)*S2( jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK12

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -2.*LN10
         SINI(iEQ) =    C1*RG(mNH4HS4)
     &             +    S1( jNH4)*S2( jNH4)/MOLALD( jNH4)/MOLALD( jNH4)
     &             +    S1(jHSO4)*S2(jHSO4)/MOLALD(jHSO4)/MOLALD(jHSO4)
     &             - 2.*S1( jH2O)*S2( jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK13

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -5.*LN10
         SINI(iEQ) =    C1*RG(mLC)
     &             + 3.*S1( jNH4)*S2( jNH4)/MOLALD( jNH4)/MOLALD( jNH4)
     &             +    S1(jHSO4)*S2(jHSO4)/MOLALD(jHSO4)/MOLALD(jHSO4)
     &             +    S1( jSO4)*S2( jSO4)/MOLALD( jSO4)/MOLALD( jSO4)
     &             - 5.*S1( jH2O)*S2( jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK14

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -3.*LN10
         SINI(iEQ) =    C1*RG(mCANO32)
     &             +    S1( jCA)*S2( jCA)/MOLALD( jCA)/MOLALD( jCA)
     &             + 2.*S1(jNO3)*S2(jNO3)/MOLALD(jNO3)/MOLALD(jNO3)
     &             - 3.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK15

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -3.*LN10
         SINI(iEQ) =    C1*RG(mCACL2)
     &             +    S1( jCA)*S2( jCA)/MOLALD( jCA)/MOLALD( jCA)
     &             + 2.*S1( jCL)*S2( jCL)/MOLALD( jCL)/MOLALD( jCL)
     &             - 3.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK16

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -3.*LN10
         CK = MAX(MOLALD(jK),TINY)
         SINI(iEQ) = C1*RG(mK2SO4)
     &             + 2.*S1(  jK)*S2(  jK)/CK/CK
     &             +    S1(jSO4)*S2(jSO4)/MOLALD(jSO4)/MOLALD(jSO4)
     &             - 3.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK17

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -2.*LN10
         CK = MAX(MOLALD(jK),TINY)
         SINI(iEQ) =    C1*RG(mKHSO4)
     &             +    S1(   jK)*S2(   jK)/CK/CK
     &             +    S1(jHSO4)*S2(jHSO4)/MOLALD(jHSO4)/MOLALD(jHSO4)
     &             - 2.*S1( jH2O)*S2( jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK18

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -2.*LN10
         CK = MAX(MOLALD(jK),TINY)
         SINI(iEQ) =    C1*RG(mKNO3)
     &             +    S1(  jK)*S2(  jK)/CK/CK
     &             +    S1(jNO3)*S2(jNO3)/MOLALD(jNO3)/MOLALD(jNO3)
     &             - 2.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
 
      iEQ = iK19

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -2.*LN10
         CK = MAX(MOLALD(jK),TINY)
         SINI(iEQ) =    C1*RG(mKCL)
     &             +    S1(  jK)*S2(  jK)/CK/CK
     &             +    S1( jCL)*S2( jCL)/MOLALD(jCL)/MOLALD(jCL)
     &             - 2.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK20

      IF (FROW(iEQ).EQ.1) THEN
         C1  = -2.*LN10
         CMG = MAX(MOLALD(jMG),TINY)
         SINI(iEQ) =    C1*RG(mMGSO4)
     &             +    S1( jMG)*S2( jMG)/CMG/CMG
     &             +    S1(jSO4)*S2(jSO4)/MOLALD(jSO4)/MOLALD(jSO4)
     &             - 2.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      iEQ = iK21

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -3.*LN10
         CMG = MAX(MOLALD(jMG),TINY) 
         SINI(iEQ) =    C1*RG(mMGNO32)
     &             +    S1( jMG)*S2( jMG)/CMG/CMG
     &             + 2.*S1(jNO3)*S2(jNO3)/MOLALD(jNO3)/MOLALD(jNO3)
     &             - 3.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF

      iEQ = iK22

      IF (FROW(iEQ).EQ.1) THEN
         C1 = -3.*LN10
         CMG = MAX(MOLALD(jMG),TINY)
         SINI(iEQ) = C1*RG(mMGCL2)
     &             +    S1( jMG)*S2( jMG)/CMG/CMG
     &             + 2.*S1( jCL)*S2( jCL)/MOLALD(jCL)/MOLALD(jCL)
     &             - 3.*S1(jH2O)*S2(jH2O)/WATER/WATER
      ENDIF
  
      RETURN
      END
  
      SUBROUTINE HDCALCNH3(SENS, S1, S2)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'
 
      DOUBLE PRECISION SENS(NSEN), S1(NSEN), S2(NSEN)
      DOUBLE PRECISION DPSI,GR,C,SR

c     IF (WATER.LE.TINY) RETURN
      IF ( WATER       .LE. TINY .OR.
     &     MOLAL(jNH4) .LE. TINY .OR.
     &     MOLAL(jH)   .LE. TINY .OR.
     &     GNH3        .LE. TINY      ) THEN
         RETURN
      ENDIF

      GR   = -2.D0*LN10*(SGAMA(mHNO3)-SGAMA(mNH4NO3))     !GAMA RELATED
      C    = ONE/MOLAL(jNH4) +ONE/MOLAL(jH) +ONE/GNH3
      SR   = SENS(jNH4)/MOLAL(jNH4)
     &     - SENS(  jH)/MOLAL(  jH)  
     &     - S1(jNH4)*S2(jNH4)/MOLAL(jNH4)/MOLAL(jNH4)
     &     + S1(  jH)*S2(  jH)/MOLAL(  jH)/MOLAL(  jH)
     &     + S1(jNH3)*S2(jNH3)/GNH3/GNH3                  !SENS RELATED
      DPSI = (SR +GR)/C

      SENS(jNH3) = DPSI
      SENS(jNH4) = SENS(jNH4) -DPSI
      SENS(jH  ) = SENS(JH  ) -DPSI

      RETURN
      END

      SUBROUTINE HDCALCNA(SENS, S1, S2)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION SENS(NSEN), S1(NSEN), S2(NSEN)
      DOUBLE PRECISION DELT,GR,SR,WR,C
      
c     IF (WTAER.LE.TINY) RETURN
      IF ( WATER      .LE. TINY .OR.
     &     MOLAL(jH)  .LE. TINY .OR.
     &     MOLAL(jNO3).LE. TINY      ) RETURN
 
      WR   =  2.D0*SENS(jH2O)/WATER -2.*S1(jH2O)*S2(jH2O)/WATER/WATER
      GR   = -2.D0*LN10*SGAMA(mHNO3)
  
      SR   = -     SENS(jH)/MOLAL(jH) +SINI(iMBNO3)/GHNO3
     &       +S1(   jH)*S2(   jH)/MOLAL(  jH)/MOLAL(  jH)
     &       +S1( jNO3)*S2( jNO3)/MOLAL(jNO3)/MOLAL(jNO3)
     &       -S1(jHNO3)*S2(jHNO3)/      GHNO3/      GHNO3
 
      C    =  ONE/MOLAL(jH) +ONE/MOLAL(jNO3) +ONE/GHNO3
      DELT =  (SR +WR +GR)/C

      IF (GHNO3.EQ.ZERO) THEN
        SENS(jHNO3) = ZERO
      ELSE
        SENS(jHNO3) = SINI(iMBNO3) -DELT
      ENDIF

      SENS(jNO3) = DELT
      SENS(jH  ) = SENS(jH) +DELT

      RETURN
      END
  
      SUBROUTINE HDCALCHA(SENS, S1, S2)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION SENS(NSEN), S1(NSEN), S2(NSEN)
      DOUBLE PRECISION DELT,GR,SR,WR,C
      DOUBLE PRECISION DCL, DNO 
 
c     IF (WTAER.LE.TINY) RETURN
      IF (WATER.LE.TINY) RETURN

      IF ( MOLAL(jH)   .LE. TINY .OR.
     &     MOLAL(jNO3) .LE. TINY .OR.
     &     GHNO3       .LE. TINY .OR.
     &     MOLAL(jCL)  .LE. TINY .OR.
     &     GHCL        .LE. TINY      ) THEN
         DCL = ZERO
         DNO = ZERO
         RETURN
      ENDIF
 
      WR   =  2.D0*SENS(jH2O)/WATER -2.*S1(jH2O)*S2(jH2O)/WATER/WATER
      GR   = -2.D0*LN10*SGAMA(mHCL)
  
      SR   = -  SENS(jH)/MOLAL(jH) +SINI(iMBCL)/GHCL
     &       +S1(  jH)*S2(  jH)/MOLAL( jH)/MOLAL( jH)
     &       +S1( jCL)*S2( jCL)/MOLAL(jCL)/MOLAL(jCL)
     &       -S1(jHCL)*S2(jHCL)/GHCL/GHCL
 
      C    =  ONE/MOLAL(jH) +ONE/MOLAL(jCL) +ONE/GHCL
      DELT =  (SR +WR +GR)/C

      IF (GHCL.EQ.ZERO) THEN
        SENS(jHCL) = 0.D0
      ELSE
        SENS(jHCL) = SINI(iMBCL) -DELT
      END IF

      SENS(jCL ) = DELT
      SENS(jH  ) = SENS(jH) +DELT

      RETURN
      END
  
      SUBROUTINE HDCALCNHA(SENS, S1, S2)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION SENS(NSEN), S1(NSEN), S2(NSEN)
      DOUBLE PRECISION DNO,DCL,C11,C12,C21,C22,B1,B2,SR,WR,GR

      IF (WATER.LE.TINY) THEN
         SENS(jHNO3) = SINI(iMBNO3)
         SENS(jHCL ) = SINI(iMBCL )
         RETURN
      ELSEIF (W(jTCL).LE.TINY.AND.W(jTNO3).LE.TINY) THEN
         RETURN
      ELSEIF (W(jTCL).LE.TINY) THEN
         CALL HDCALCNA(SENS, S1, S2)
      ELSEIF (W(jTNO3).LE.TINY) THEN
         CALL HDCALCHA(SENS, S1, S2)
      ENDIF

      IF( MOLAL(jH)   .LE. TINY .OR.
     &    MOLAL(jNO3) .LE. TINY .OR.
     &    GHNO3       .LE. TINY .OR.
     &    GHCL        .LE. TINY .OR.
     &    MOLAL(jCL)  .LE. TINY       ) RETURN

      C11 = ONE/MOLAL(jH) +ONE/MOLAL(jNO3) +ONE/GHNO3
      C12 = ONE/MOLAL(jH)
      C21 = C12
      C22 = ONE/MOLAL(jH) +ONE/MOLAL(jCL)  +ONE/GHCL
      SR  =-SENS(jH)/MOLAL(jH) +SINI(iMBNO3)/GHNO3
     &     +S1(   jH)*S2(   jH)/MOLAL(  jH)/MOLAL(  jH)
     &     +S1( jNO3)*S2( jNO3)/MOLAL(jNO3)/MOLAL(jNO3)
     &     -S1(jHNO3)*S2(jHNO3)/GHNO3/GHNO3
      WR  = 2.D0*SENS(jH2O)/WATER -2.*S1(jH2O)*S2(jH2O)/WATER/WATER
      GR  =-2.D0*LN10*SGAMA(mHNO3)
      B1  = SR +WR +GR
      SR  =-SENS(jH)/MOLAL(jH) +SINI(iMBCL)/GHCL
     &     +S1(  jH)*S2(  jH)/MOLAL( jH)/MOLAL( jH)
     &     +S1( jCL)*S2( jCL)/MOLAL(jCL)/MOLAL(jCL)
     &     -S1(jHCL)*S2(jHCL)/GHCL/GHCL
      GR  =-2.D0*LN10*SGAMA(mHCL)
      B2  = SR +WR +GR

      DCL = (B1*C21 -B2*C11)/(C21*C12 -C22*C11)
      IF (MOLAL(jCL).EQ.W(jTCL)) DCL = SINI(iMBCL)
      DNO = (B1 -C12*DCL)/C11
      IF (MOLAL(jNO3).EQ.W(jTNO3)) DNO = SINI(iMBNO3)
      IF (MOLAL(jCL).EQ.TINY.AND.MOLAL(jNO3).EQ.TINY) THEN
         DCL = ZERO
         DNO = ZERO
      ENDIF

      SENS(jH)    = SENS(jH)   +DCL +DNO
      SENS(jCL)   = DCL
      SENS(jNO3)  = DNO
      SENS(jHCL)  = SINI(jCL)  -DCL
      SENS(jHNO3) = SINI(jNO3) -DNO   
  
      RETURN
      END

      SUBROUTINE HDCALCHS4(SENS, S1, S2)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION SENS(NSEN), S1(NSEN), S2(NSEN)
      DOUBLE PRECISION DELTA,GR,SR,WR,C
     
 
c     IF (WATER.LE.1D1*TINY) RETURN
c     IF (MOLAL(jHSO4).EQ.ZERO) RETURN

      IF ( WATER.LE.1D1*TINY .OR.
     &     MOLAL(jHSO4) .LE. TINY .OR.
     &     MOLAL(jH)    .LE. TINY .OR.
     &     MOLAL(jSO4)  .LE. TINY      )  RETURN


      WR    = -SENS(jH2O)/WATER +S1(jH2O)*S2(jH2O)/WATER/WATER 
      GR    = -2.D0*LN10*SGAMA(mHHSO4)+3.D0*LN10*SGAMA(mH2SO4)
  
      SR    =  SENS(jH)/MOLAL(jH) +SENS( jSO4)/MOLAL( jSO4)
     &      - S1( jSO4)*S2( jSO4)/MOLAL( jSO4)/MOLAL( jSO4)
     &      - S1(   jH)*S2(   jH)/MOLAL(   jH)/MOLAL(   jH)
     &      + S1(jHSO4)*S2(jHSO4)/MOLAL(jHSO4)/MOLAL(jHSO4)
 
      C     = ONE/MOLAL(jH) +ONE/MOLAL(jSO4) +ONE/MOLAL(jHSO4)
  
      DELTA =  (WR +GR +SR)/C

      SENS(jH)    = SENS(jH) -DELTA
      SENS(jSO4)  = SENS(jSO4) -DELTA
      SENS(jHSO4) = DELTA

      RETURN
      END  

#endif

