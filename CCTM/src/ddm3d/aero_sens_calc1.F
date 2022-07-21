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
C This subroutine calculates first-order sensitivity of ISORROPIAII
C 
C Written by Wenxian Zhang in August 2011
C
C 27 September 2013: Sergey L. Napelenok 
C    --- implemented into CMAQv5.0.2
C 08 September 2014: Sergey L. Napelenok
C    --- some bug fixes and better error reporting
c 27 February 2015: Sergey L. Napelenok
c     -- disable DGAMA impacts due to instability and fix minor issues
C
C Reference: 
C Zhang, W., Capps, S. L., Hu, Y., Nenes, A., Napelenok, S. L., & 
C     Russell, A. G. (2012). Development of the high-order decoupled 
C     direct method in three dimensions for particulate matter: 
C     enabling advanced sensitivity analysis in air quality models. 
C     Geoscientific Model Development, 5(2), 355-368. 
C     doi: 10.5194/gmd-5-355-2012
C ===================================================================

#ifdef sens
      SUBROUTINE AERO_SENS_CALC1(STOT,SENS,SENSD,SCASI,FCOL)


c     USE DDM3D_DEFN, ONLY : WRFLAG
      Use aero_ddm3d, ONLY : cbsens
      USE UTILIO_DEFN         ! I/O API

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION, INTENT( IN  ) :: STOT( : )  ! (input) gas+pm total sensitivity
      DOUBLE PRECISION, INTENT( OUT ) :: SENS( : )  ! (output) partitioned SENSITIVITIES
      DOUBLE PRECISION, INTENT( OUT ) :: SENSD( : ) ! (output) partitioned SENSITIVITIES BEFORE MINOR
      CHARACTER( 15 ),  INTENT( IN  ) :: SCASI      ! (input) subcase number from ISOROPIA
      INTEGER, INTENT( IN ) :: FCOL( : )            ! Flags for matrix reduction

      INTEGER FROW( NSEN )           ! Flags for matrix reduction
      DOUBLE PRECISION COEF(NSEN,NSEN)        ! COEFFICIENT MATRIX 
      DOUBLE PRECISION DGAMA(NIONSPC,NPAIR)   ! dGAMA/dA
    
c     LOGICAL, SAVE :: FIRSTIME = .TRUE.

c     IF ( FIRSTIME ) THEN
c        FIRSTIME = .FALSE.
c        LOGDEV = INIT3 ()
c     ENDIF

      INTEGER I

      CC = SCASI(1:1)

C sln 13feb2015 prevent small number from messing up the matrix solution
      DO I = 1,NIONS
        MOLALD(I) = MAX(MOLALD(I),1.0D-12)
      END DO
      GNH3D  = MAX(GNH3D,1.0D-15)
      GHNO3D = MAX(GHNO3D,1.0D-15)
      GHCLD  = MAX(GHCLD,1.0D-15)


C *** INITIALIZE SINI ***

      DO I = 1,NSEN
         SINI(I) = 0.0D0
      ENDDO
            
      SINI(iMBNA)  = STOT(1)      
      SINI(iMBSO4) = STOT(2) 
      SINI(iMBNH4) = STOT(3)
      SINI(iMBNO3) = STOT(4)
      SINI(iMBCL)  = STOT(5)
      SINI(iMBCA)  = STOT(6)
      SINI(iMBK)   = STOT(7)
      SINI(iMBMG)  = STOT(8)


c     SINI(iCB)  = cbsens

c *** SET ROW AND COL FLAGS ***

      CALL FLAGS( FROW, FCOL )


C *** CALCULATE DGAMA ***

c     IF (CC.EQ.'A'.OR.CC.EQ.'B'.OR.CC.EQ.'C'.OR.
c    &    CC.EQ.'D'.OR.CC.EQ.'E'.OR.CC.EQ.'F'.OR.
c    &    CC.EQ.'G'.OR.CC.EQ.'H'.OR.CC.EQ.'I'.OR.
c    &    CC.EQ.'J') THEN
c       CALL DELGAMA1( DGAMA )
c     ELSE
c        CALL DELGAMA2( DGAMA,frow )
c     ENDIF

      DGAMA = 0.0D0 ! set to ignore activity

C *** CALCULATE COEFFICIENT MATRIX ***

      CALL AMAT( COEF, FROW, FCOL, DGAMA )

C *** SOLVE SENSITIVITIES ***

      CALL EQNSLV( FROW, FCOL, COEF, SENS, SENSD )

C *** ADJUST FOR MINOR SPECIES ***

c     DO I = 1,NPAIR
c        SGAMA(I) = 0.D0
c        DO J = 1,NIONSPC
c           SGAMA(I) = SGAMA(I)+DGAMA(J,I)*SENS(J)
c        ENDDO
c     ENDDO

      SGAMA = 0.0D0 ! set to ignore activity

c     IF (CC.EQ.'B'.OR.CC.EQ.'C') THEN
c        CALL DCALCNH3(SENS)
c     ELSEIF (CC.EQ.'E'.OR.CC.EQ.'F') THEN
c        CALL DCALCNA(SENS)
c     ELSEIF (CC.EQ.'I'.OR.CC.EQ.'J'.OR.
c    &        CC.EQ.'L'.OR.CC.EQ.'K') THEN
c        CALL DCALCNHA(SENS)
c        CALL DCALCNH3(SENS)
c     ELSEIF (CC.EQ.'D'.OR.CC.EQ.'G'.OR.CC.EQ.'H'.OR.
c    &        CC.EQ.'O'.OR.CC.EQ.'M'.OR.CC.EQ.'P') THEN
c        CALL DCALCHS4(SENS)
c     ENDIF

C *** END OF DDMSENS ***
      RETURN
      END  SUBROUTINE AERO_SENS_CALC1

C ============================================================================
C SET FLAGS FOR MATRIX SUBSTRACTION
C ============================================================================
      SUBROUTINE FLAGS(FROW,FCOL)

c     USE DDM3D_DEFN, ONLY : WRFLAG
      USE UTILIO_DEFN         ! I/O API

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      INTEGER FROW(NSEN),FCOL(NSEN)

c     INTEGER, SAVE :: LOGDEV
c     LOGICAL, SAVE :: FIRSTIME = .TRUE.

c     IF ( FIRSTIME ) THEN
c        FIRSTIME = .FALSE.
c        LOGDEV = INIT3 ()
c     ENDIF

      INTEGER I,J,K

      
C *** CLEAR FLAGS ***
 
      DO I = 1,NSEN
         FROW(I) = 0
         FCOL(I) = 0
      ENDDO

      CC = SCASE(1:1)

      IF (CC.EQ.'A') THEN

         FROW(iK1)    = 1
         FROW(iK2)    = 1
         FROW(iMBSO4) = 1
         FROW(iMBNH4) = 1
         FROW(iCB)    = 1
         FROW(iZSR)   = 1

         FCOL(jNH3)   = 1
         FCOL(jNH4)   = 1
         FCOL(jH)     = 1
         FCOL(jH2O)   = 1
         FCOL(jSO4)   = 1
         FCOL(jHSO4)  = 1

      ELSEIF (CC.EQ.'B'.OR.CC.EQ.'C'.OR.CC.EQ.'E'.OR.
     &        CC.EQ.'F') THEN

         FROW(iK1)    = 1
         FROW(iMBSO4) = 1
         FROW(iMBNH4) = 1
         FROW(iCB)    = 1
         FROW(iZSR)   = 1

         FCOL(jNH4)   = 1
         FCOL(jH)     = 1
         FCOL(jH2O)   = 1
         FCOL(jSO4)   = 1
         FCOL(jHSO4)  = 1        

      ELSEIF (CC.EQ.'D') THEN

         IF (NONPYS.EQ.1) THEN
            FROW(iK17) = 1
         ELSE 
            FROW(iK4)  = 1
         ENDIF

         FROW(iK2)    = 1
         FROW(iMBNO3) = 1
         FROW(iMBSO4) = 1
         FROW(iMBNH4) = 1
         FROW(iCB)    = 1
         FROW(iZSR)   = 1

         FCOL(jNH3)   = 1
         FCOL(jNH4)   = 1
         FCOL(jH)     = 1
         FCOL(jH2O)   = 1
         FCOL(jSO4)   = 1
         FCOL(jNO3)   = 1
         FCOL(jHNO3)  = 1  

      ELSEIF (CC.EQ.'G'.OR.CC.EQ.'H') THEN
    
         FROW(iK2)    = 1
         FROW(iK3)    = 1
         FROW(iK4)    = 1
         FROW(iMBNO3) = 1
         FROW(iMBSO4) = 1
         FROW(iMBNH4) = 1
         FROW(iMBCL)  = 1
         FROW(iMBNA)  = 1
         FROW(iCB)    = 1
         FROW(iZSR)   = 1

         FCOL(jNH3)   = 1
         FCOL(jNH4)   = 1
         FCOL(jH)     = 1
         FCOL(jH2O)   = 1
         FCOL(jSO4)   = 1
         FCOL(jNO3)   = 1
         FCOL(jHNO3)  = 1
         FCOL(jNA)    = 1
         FCOL(jCL)    = 1
         FCOL(jHCL)   = 1
   
      ELSEIF (CC.EQ.'I'.OR.CC.EQ.'J') THEN

         FROW(iK1)    = 1
         FROW(iMBSO4) = 1
         FROW(iMBNH4) = 1
         FROW(iMBNA)  = 1
         FROW(iCB)    = 1
         FROW(iZSR)   = 1

         FCOL(jNH4)   = 1
         FCOL(jH)     = 1
         FCOL(jH2O)   = 1
         FCOL(jSO4)   = 1
         FCOL(jHSO4)  = 1
         FCOL(jNA)    = 1

      ELSEIF (CC.EQ.'K'.OR.CC.EQ.'L') THEN
        
         FROW(iK1)    = 1
         FROW(iMBSO4) = 1
         FROW(iMBNH4) = 1
         FROW(iMBNA)  = 1
         FROW(iMBK)   = 1
         FROW(iMBMG)  = 1
         FROW(iCB)    = 1
         FROW(iZSR)   = 1

         FCOL(jNH4)   = 1
         FCOL(jH)     = 1
         FCOL(jH2O)   = 1
         FCOL(jSO4)   = 1
         FCOL(jHSO4)  = 1
         FCOL(jNA)    = 1
         FCOL(jK)     = 1
         FCOL(jMG)    = 1

      ELSEIF (CC.EQ.'O'.OR.CC.EQ.'M'.OR.
     &        CC.EQ.'P') THEN

         FROW(iK2)    = 1
         FROW(iK3)    = 1
         FROW(iK4)    = 1
         FROW(iMBNO3) = 1
         FROW(iMBSO4) = 1
         FROW(iMBNH4) = 1
         FROW(iMBCL)  = 1
         FROW(iMBNA)  = 1
         FROW(iMBK)   = 1
         FROW(iMBMG)  = 1   
         FROW(iCB)    = 1
         FROW(iZSR)   = 1

         FCOL(jNH3)   = 1
         FCOL(jNH4)   = 1
         FCOL(jH)     = 1
         FCOL(jH2O)   = 1
         FCOL(jSO4)   = 1
         FCOL(jNO3)   = 1
         FCOL(jHNO3)  = 1
         FCOL(jNA)    = 1 
         FCOL(jCL)    = 1
         FCOL(jHCL)   = 1 
         FCOL(jK)     = 1
         FCOL(jMG)    = 1

      ENDIF    

      FROW(iZSR)   = 0  ! set to ignore water
      FCOL(jH2O)   = 0  ! set to ignore water

c sln 11april2017 chlorine partitioning seems to blow up, possibly due to lack of charge balance in isorropia
c     FROW(iK3)    = 0
c     FCOL(jHCL)   = 0
c     FCOL(jCL)    = 0
c     FROW(iMBCL)  = 0

      NDIM = 0
      DO I = 1,NSEN
         IF (FROW(I).EQ.1) NDIM = NDIM + 1
      ENDDO

      RETURN    
      END

C ============================================================================
C CALCULATE dGAMA/dA, A IS IONIC SPECIES FOR A-J CASES
C ============================================================================
      SUBROUTINE DELGAMA1 ( DGAMA )

c     IMPLICIT NONE
 
      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'
      INCLUDE 'dact.inc'

      DOUBLE PRECISION DF1(NIONSPC,3),DF2(NIONSPC,4)
      DOUBLE PRECISION ZPL,ZMI,XPL,XMI
      DOUBLE PRECISION XIJ,YJI,DXIJ,DYJI
      DOUBLE PRECISION DGAMA(NIONSPC,NPAIR)
      DOUBLE PRECISION SION

      DOUBLE PRECISION CH
c     INTEGER, SAVE :: LOGDEV
c     LOGICAL, SAVE :: FIRSTIME = .TRUE.

c     IF ( FIRSTIME ) THEN
c        FIRSTIME = .FALSE.
c        LOGDEV = INIT3 ()
c     ENDIF

      INTEGER I,J,K

C 
C *** Mapping of electrolyte to ion index ***
C
      IJMAP(1,1) = mHCL
      IJMAP(1,2) = mH2SO4
      IJMAP(1,3) = mHHSO4
      IJMAP(1,4) = mHNO3
      IJMAP(2,1) = mNACL
      IJMAP(2,2) = mNA2SO4
      IJMAP(2,3) = mNAHSO4
      IJMAP(2,4) = mNANO3
      IJMAP(3,1) = mNH4CL
      IJMAP(3,2) = mNH42S4
      IJMAP(3,3) = mNH4HS4
      IJMAP(3,4) = mNH4NO3

C
C *** dI/dA ***
C
      IF (IONIC.GE.100.d0) THEN           
        DO I = 1, NIONSPC
          DI(I) = 0.0
        ENDDO
      ELSE
        DI(jH2O) = 0.0
        DO I = 1, NIONS
          DI(I) = 0.5*Z(I)*Z(I)/WATER
          DI(jH2O) = DI(jH2O) + MOLALD(I)*Z(I)*Z(I)
        ENDDO
        DI(jH2O) = -0.5*DI(jH2O)/(WATER*WATER)
      ENDIF

C
C *** dG0/dA ***
C
      CALL DKMFUL (NIONSPC,NPAIR,IONIC,SNGL(TEMP),DI,G0P,DG0)

C
C *** MULTICOMPONENT ACTIVITY COEFFICIENT ***
C
      AGAMA = 0.511*(298.0/TEMP)**1.5 ! Debye Huckel const. at T
      SION  = SQRT(IONIC)
      H     = AGAMA*SION/(1+SION)
      DH    = 0.5*AGAMA/(SION*(1.+SION)*(1.+SION))

      DO K = 1, NIONSPC
        DO I=1,3
           DF1(K,I)=0.0D0
           DF2(K,I)=0.0D0
        ENDDO
        DF2(K,4)=0.0D0
        DO I = 1, NPAIR
           DGAMA(K,I) = 0.0D0
        ENDDO
      ENDDO

      DO 100 I=1,3
         ZPL = Z(I)
         XPL = MOLALD(I)/WATER
         DO 100 J=1,4
            ZMI   = Z(J+3)
            XMI   = MOLALD(J+3)/WATER
            CH    = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            XIJ   = CH*XPL
            YJI   = CH*XMI
            X(I,J) = XIJ
            Y(I,J) = YJI

            DO K = 1, NIONSPC
              DXIJ = -XPL*DI(K)/IONIC
              DYJI = -XMI*DI(K)/IONIC
              IF (K.EQ.I) THEN
                DXIJ = DXIJ + 1./WATER
              ELSEIF (K.EQ.J+3) THEN
                DYJI = DYJI + 1./WATER
              ELSEIF (K.EQ.jH2O) THEN
                DXIJ = DXIJ - XPL/WATER
                DYJI = DYJI - XMI/WATER
              ENDIF
              DXIJ = CH*DXIJ
              DYJI = CH*DYJI
              DX(K,I,J) = DXIJ
              DY(K,I,J) = DYJI 

              DF1(K,I) = DF1(K,I)
     &                   + G0P(IJMAP(I,J)) * DYJI
     &                   + YJI * DG0(K,IJMAP(I,J))
     &                   + ZPL*ZMI*DH * YJI * DI(K)
     &                   + ZPL*ZMI*H * DYJI
              DF2(K,J) = DF2(K,J)
     &                   + G0P(IJMAP(I,J)) * DXIJ
     &                   + XIJ * DG0(K,IJMAP(I,J))
     &                   + ZPL*ZMI*DH * XIJ * DI(K)
     &                   + ZPL*ZMI*H * DXIJ
            ENDDO

100   CONTINUE
C
C     d log10(GAMA)/dA
C
      DO 110 I=1,3
         ZPL = Z(I)
         DO 110 J=1,4
            ZMI = Z(J+3)

            DO K = 1, NIONSPC
               DGAMA(K,IJMAP(I,J)) = ZPL*ZMI * (
     &                  (DF1(K,I)/ZPL + DF2(K,J)/ZMI) / (ZPL+ZMI)
     &                               - DH * DI(K) )
            ENDDO

110   CONTINUE

      DO K = 1, NIONSPC
         DGAMA(K,mLC) = 0.20 * ( 3.0*DGAMA(K,mNH42S4)
     &                           + 2.0*DGAMA(K,mNH4HS4) )
      ENDDO

      DO I = 1, NPAIR
         IF (GAMA(I).LE.1.d-5 .OR. GAMA(I).GE.1.d5) THEN
            DO K = 1, NIONSPC
               DGAMA(K,I) = 0.0
            ENDDO
          ENDIF
      ENDDO

C
C *** END OF SUBROUTINE DELGAMA1 ***
C
      RETURN
      END


C ============================================================================
C CALCULATE dGAMA/dA, A IS IONIC SPECIES FOR O-K CASES
C ============================================================================
      SUBROUTINE DELGAMA2 ( DGAMA,frow )

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'
      INCLUDE 'dact.inc'
      
      DOUBLE PRECISION  DF1(NIONSPC,6),DF2A(NIONSPC,4),DF2B(NIONSPC,4)
      DOUBLE PRECISION  ZPL,ZMI,XPL,XMI
      DOUBLE PRECISION XIJ,YJI,DXIJ,DYJI
      DOUBLE PRECISION DGAMA(NIONSPC,NPAIR)
      DOUBLE PRECISION SION

      DOUBLE PRECISION CH

      integer frow(nsen)
      INTEGER I,J,K

C 
C *** Mapping of electrolyte to ion index ***
C
      IJMAP(1,1) = mHCL
      IJMAP(1,2) = mH2SO4
      IJMAP(1,3) = mHHSO4
      IJMAP(1,4) = mHNO3
      IJMAP(2,1) = mNACL
      IJMAP(2,2) = mNA2SO4
      IJMAP(2,3) = mNAHSO4
      IJMAP(2,4) = mNANO3
      IJMAP(3,1) = mNH4CL
      IJMAP(3,2) = mNH42S4
      IJMAP(3,3) = mNH4HS4
      IJMAP(3,4) = mNH4NO3
      IJMAP(4,1) = mCACL2
      IJMAP(4,2) = mCASO4
      IJMAP(4,3) = 0
      IJMAP(4,4) = mCANO32
      IJMAP(5,1) = mKCL
      IJMAP(5,2) = mK2SO4
      IJMAP(5,3) = mKHSO4
      IJMAP(5,4) = mKNO3
      IJMAP(6,1) = mMGCL2
      IJMAP(6,2) = mMGSO4
      IJMAP(6,3) = 0
      IJMAP(6,4) = mMGNO32

C
C *** dI/dA ***
C
      IF (IONIC.GE.100.d0) THEN           
        DO I = 1, NIONSPC
          DI(I) = 0.0
        ENDDO
      ELSE
        DI(jH2O) = 0.0
        DO I = 1, NIONS
          DI(I) = 0.5*Z(I)*Z(I)/WATER
          DI(jH2O) = DI(jH2O) + MOLALD(I)*Z(I)*Z(I)
        ENDDO
        DI(jH2O) = -0.5*DI(jH2O)/(WATER*WATER)
      ENDIF

C
C *** dG0/dA ***
C
      CALL DKMFUL2 (NIONSPC,NPAIR,IONIC,SNGL(TEMP),DI,G0P,DG0)
C
C *** MULTICOMPONENT ACTIVITY COEFFICIENT ***
C
      AGAMA = 0.511*(298.0/TEMP)**1.5 ! Debye Huckel const. at T
      SION  = SQRT(IONIC)
      H     = AGAMA*SION/(1+SION)
      DH    = 0.5*AGAMA/(SION*(1.+SION)*(1.+SION))
      DO K = 1, NIONSPC
        DO I=1,4
           DF1(K,I)=0.0
           DF2A(K,I)=0.0
           DF2B(K,I)=0.0
        ENDDO
        DF1(K,5)=0.0
        DF1(K,6)=0.0
        DO I = 1, NPAIR
           DGAMA(K,I) = 0.0
        ENDDO
      ENDDO

      DO 100 I=1,3
         ZPL = Z(I)
         XPL = MOLALD(I)/WATER
         DO 100 J=1,4
            ZMI   = Z(J+3)
            XMI   = MOLALD(J+3)/WATER
            CH    = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            XIJ   = CH*XPL
            YJI   = CH*XMI
            X(I,J) = XIJ
            Y(I,J) = YJI

            DO K = 1, NIONSPC
              DXIJ = -XPL*DI(K)/IONIC
              DYJI = -XMI*DI(K)/IONIC
              IF (K.EQ.I) THEN
                DXIJ = DXIJ + 1./WATER
              ELSEIF (K.EQ.J+3) THEN
                DYJI = DYJI + 1./WATER
              ELSEIF (K.EQ.jH2O) THEN
                DXIJ = DXIJ - XPL/WATER
                DYJI = DYJI - XMI/WATER
              ENDIF
              DXIJ = CH*DXIJ
              DYJI = CH*DYJI
              DX(K,I,J) = DXIJ
              DY(K,I,J) = DYJI

              DF1(K,I) = DF1(K,I)
     &                   + G0P(IJMAP(I,J)) * DYJI
     &                   + YJI * DG0(K,IJMAP(I,J))
     &                   + ZPL*ZMI*DH * YJI * DI(K)
     &                   + ZPL*ZMI*H * DYJI
              DF2A(K,J) = DF2A(K,J)
     &                   + G0P(IJMAP(I,J)) * DXIJ
     &                   + XIJ * DG0(K,IJMAP(I,J))
     &                   + ZPL*ZMI*DH * XIJ * DI(K)
     &                   + ZPL*ZMI*H * DXIJ
            ENDDO

100   CONTINUE

      DO 110 I=1,3
         ZPL = Z(I)
         DO 110 J=1,4
            ZMI = Z(J+3)

            DO K = 1, NIONSPC
               DGAMA(K,IJMAP(I,J)) = ZPL*ZMI * (
     &                  (DF1(K,I)/ZPL + DF2A(K,J)/ZMI) / (ZPL+ZMI)
     &                               - DH * DI(K) )
            ENDDO

110   CONTINUE

      DO 120 I=4,6
         ZPL = Z(I+4)
         XPL = MOLALD(I+4)/WATER

         DO 120 J=1,4
            ZMI   = Z(J+3)

            IF(J.EQ.3.AND.I.EQ.4) GOTO 120
            IF(J.EQ.3.AND.I.EQ.6) GOTO 120
            IF(J.EQ.2.AND.I.EQ.4) GOTO 120

            XMI   = MOLALD(J+3)/WATER
            CH    = 0.25*(ZPL+ZMI)*(ZPL+ZMI)/IONIC
            XIJ   = CH*XPL
            YJI   = CH*XMI
            X(I,J) = XIJ
            Y(I,J) = YJI
            DO K = 1, NIONSPC
              DXIJ = -XPL*DI(K)/IONIC
              DYJI = -XMI*DI(K)/IONIC
              IF (K.EQ.I) THEN
                DXIJ = DXIJ + 1./WATER
              ELSEIF (K.EQ.J+3) THEN
                DYJI = DYJI + 1./WATER
              ELSEIF (K.EQ.jH2O) THEN
                DXIJ = DXIJ - XPL/WATER
                DYJI = DYJI - XMI/WATER
              ENDIF
              DXIJ = CH*DXIJ
              DYJI = CH*DYJI
              DX(K,I,J) = DXIJ
              DY(K,I,J) = DYJI
              DF1(K,I) = DF1(K,I)
     &                   + G0P(IJMAP(I,J)) * DYJI
     &                   + YJI * DG0(K,IJMAP(I,J))
     &                   + ZPL*ZMI*DH * YJI * DI(K)
     &                   + ZPL*ZMI*H * DYJI
              DF2B(K,J) = DF2B(K,J)
     &                   + G0P(IJMAP(I,J)) * DXIJ
     &                   + XIJ * DG0(K,IJMAP(I,J))
     &                   + ZPL*ZMI*DH * XIJ * DI(K)
     &                   + ZPL*ZMI*H * DXIJ
            ENDDO

 120     CONTINUE

      DO 130 I= 4,6
         ZPL = Z(I+4)
         DO 130 J=1,4
            ZMI = Z(J+3)

            IF(J.EQ.3.AND.I.EQ.4) GOTO 130
            IF(J.EQ.3.AND.I.EQ.6) GOTO 130
            DO K = 1, NIONSPC
               DGAMA(K,IJMAP(I,J)) = ZPL*ZMI * (
     &                  (DF1(K,I)/ZPL + DF2B(K,J)/ZMI) / (ZPL+ZMI)
     &                               - DH * DI(K) )
            ENDDO

130   CONTINUE

      DO K = 1, NIONSPC
         DGAMA(K,mLC) = 0.20 * ( 3.0*DGAMA(K,mNH42S4)
     &                           + 2.0*DGAMA(K,mNH4HS4) )
      ENDDO


      DO I = 1, NPAIR
         IF (GAMA(I).LE.1.d-5 .OR. GAMA(I).GE.1.d5) THEN
            DO K = 1, NIONSPC
               DGAMA(K,I) = 0.0
            ENDDO
          ENDIF
      ENDDO

C
C *** END OF SUBROUTINE DELGAMA2 ***
C
      RETURN
      END

C =============================================================================
C CALCULATE dG0/dA, CASES A - J
C =============================================================================
      SUBROUTINE DKMFUL (N,NPAIRS,IONIC,TEMP,DELI,G0,DELG0)

      IMPLICIT NONE

      INTEGER   N, NPAIRS
      REAL      IONIC,SION,CUBI,TEMP
      DOUBLE PRECISION DELI(N), G0(NPAIRS), DELG0(N,NPAIRS)
      DOUBLE PRECISION TI, CF1, CF2, CF2D

      INTEGER   NPAIRD, I, J, K
      PARAMETER (NPAIRD=10)               ! Number of ion pairs whose Q value is available
      INTEGER, SAVE :: IG(NPAIRD)
      DATA IG / 1,2,3,4,5,6,7,8,10,11 /
      REAL, SAVE :: ZI(NPAIRD)                ! Mapping of Q to the internal order of ion pairs
      DATA ZI / 1., 2., 1., 2., 1., 1., 2., 1., 1., 1. /
      REAL, SAVE :: Q(NPAIRD)                 ! Kusik-Meissner parameters (see KMFUL)
      DATA Q  / 2.23,-0.19,-0.39,-0.25,-1.15,0.82,-0.1,
     &          8.0,2.6,6.0 /

C
      SION = SQRT(IONIC)
      CUBI = IONIC*IONIC*IONIC

C
C *** Coefficients at 25 oC
C
      DO I = 1, NPAIRD
         CALL DMKBI(N,Q(I),IONIC,SION,CUBI,ZI(I),G0(IG(I)),DELI,
     &                                          DELG0(1,IG(I)))
      ENDDO

C
C *** Correct for T other than 298 K
C
      TI  = TEMP-273.0
      IF (ABS(TI-25.0) .GT. 1.0) THEN
         CF1 = 1.125-0.005*TI
         CF2 = (CF1-1.)*(0.039*IONIC**0.92-0.41*SION/(1.+SION))
         CF2D = (CF1-1.)*( .03588/IONIC**.08
     &                    -.205/(SION*(1.+SION)*(1.+SION)) )
         DO I = 1, NPAIRD
            G0(IG(I)) = CF1*G0(IG(I)) - CF2*ZI(I)
            DO K = 1, N
               DELG0(K,IG(I)) = CF1*DELG0(K,IG(I)) - ZI(I)*CF2D*DELI(K)
            ENDDO
         ENDDO
      ENDIF
C
      G0( 9) = G0( 6) + G0( 8) - G0(11)
      G0(12) = G0( 1) + G0( 8) - G0(11)
      DO K = 1, N
         DELG0(K, 9) = DELG0(K, 6) + DELG0(K, 8) - DELG0(K,11)
         DELG0(K,12) = DELG0(K, 1) + DELG0(K, 8) - DELG0(K,11)
      ENDDO

      RETURN
C
C *** END OF SUBROUTINE DDMKMFUL
C
      END


C =============================================================================
C CALCULATE dG0/dA, CASES O - K
C =============================================================================
      SUBROUTINE DKMFUL2 (N,NPAIRS,IONIC,TEMP,DELI,G0,DELG0)

      IMPLICIT NONE

      INTEGER   N, NPAIRS
      REAL      IONIC,SION,CUBI,TEMP
      DOUBLE PRECISION DELI(N), G0(NPAIRS), DELG0(N,NPAIRS)
      DOUBLE PRECISION TI, CF1, CF2, CF2D

      INTEGER   NPAIRD, I, K
      PARAMETER (NPAIRD=18)               ! Number of ion pairs whose Q value is available
      INTEGER, SAVE :: IG(NPAIRD)
      DATA IG / 1,2,3,4,5,6,7,8,10,11,15,16,17,19,20,21,22,23 /
      REAL,    SAVE :: ZI(NPAIRD)                ! Mapping of Q to the internal order of ion pairs
      DATA ZI / 1., 2., 1., 2., 1., 1., 2., 1., 1., 1., 2., 2., 
     &          2., 1., 1., 4., 2., 2. /
      REAL, SAVE ::    Q(NPAIRD)                 ! Kusik-Meissner parameters (see KMFUL)
      DATA Q  / 2.23,-0.19,-0.39,-0.25,-1.15,0.82,-0.1,8.0,2.6,6.0,
     &          0.93,2.4,-0.25,-2.33,0.92,0.15,2.32,2.9 /
C
C
      SION = SQRT(IONIC)
      CUBI = IONIC*IONIC*IONIC
C
C *** Coefficients at 25 oC
C
      DO I = 1, NPAIRD
         CALL DMKBI(N,Q(I),IONIC,SION,CUBI,ZI(I),G0(IG(I)),DELI,
     &                                          DELG0(1,IG(I)))
      ENDDO

C
C *** Correct for T other than 298 K
C
      TI  = TEMP-273.0
      IF (ABS(TI-25.0) .GT. 1.0) THEN
         CF1 = 1.125-0.005*TI
         CF2 = (CF1-1.)*(0.039*IONIC**0.92-0.41*SION/(1.+SION))
         CF2D = (CF1-1.)*( .03588/IONIC**.08
     &                    -.205/(SION*(1.+SION)*(1.+SION)) )
         DO I = 1, NPAIRD
            G0(IG(I)) = CF1*G0(IG(I)) - CF2*ZI(I)
            DO K = 1, N
               DELG0(K,IG(I)) = CF1*DELG0(K,IG(I)) - ZI(I)*CF2D*DELI(K)
            ENDDO
         ENDDO
      ENDIF
C
      G0( 9) = G0( 6) + G0( 8) - G0(11)
      G0(12) = G0( 1) + G0( 8) - G0(11)
      G0(18) = G0( 8) + G0(20) - G0(11)
      DO K = 1, N
         DELG0(K, 9) = DELG0(K, 6) + DELG0(K, 8) - DELG0(K,11)
         DELG0(K,12) = DELG0(K, 1) + DELG0(K, 8) - DELG0(K,11)
         DELG0(K,18) = DELG0(K, 8) + DELG0(K,20) - DELG0(K,11)
      ENDDO

      RETURN
C
C *** END OF SUBROUTINE DKMFUL2
C
      END


C =============================================================================
C dG0/dA AT 25C
C =============================================================================


      SUBROUTINE DMKBI (N,Q,IONIC,SION,CUBI,ZIP,G,DELI,DELG)
C
      IMPLICIT NONE
        
      INTEGER            N, K
      REAL               IONIC, SION, CUBI, Q, ZIP
      DOUBLE PRECISION   G, DELI(N), DELG(N)
      DOUBLE PRECISION   B, C, XX, BI, XX1, XX2, XX3, XX4
      DOUBLE PRECISION   LN10
      PARAMETER          (LN10=2.30258509299404568402D0)
C
      B=.75-.065*Q
      C= 1.0
      IF (IONIC.LT.6.0) C=1.+.055*Q*EXP(-.023*CUBI)
      XX=-0.5107*SION/(1.+C*SION)
      BI=(1.+B*(1.+.1*IONIC)**Q-B)
      G =ZIP*LOG10(BI) + ZIP*XX
      XX1 = .1*B*Q*(1.+.1*IONIC)**(Q-1.)/(BI*LN10)
      IF (IONIC.LT.6.0) THEN
         XX2 = 0.5/SION+.003795*Q*CUBI*EXP(-.023*CUBI)
      ELSE
         XX2 = 0.5/SION
      ENDIF
      XX3 = (1.+C*SION)*(1.+C*SION)
      XX4 = ZIP*(XX1-.5107*XX2/XX3)

      DO K = 1, N
        
         DELG(K) = XX4 * DELI(K)
      ENDDO
      
C
C *** END OF SUBROUTINE DDMMKBI
C
      RETURN
      END

C =============================================================================
C PREPARE COEFFICIENT MATRIX
C =============================================================================

      SUBROUTINE AMAT(AM,FROW,FCOL,DGAMA)

      USE UTILIO_DEFN
c     USE DDM3D_DEFN, ONLY : WRFLAG

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      INTEGER FROW(NSEN),FCOL(NSEN)
      DOUBLE PRECISION AM(NSEN,NSEN)
      DOUBLE PRECISION DGAMA(NIONSPC,NPAIR)

      DOUBLE PRECISION AM_TEMP(NSEN)

c     INTEGER, SAVE :: LOGDEV
c     LOGICAL, SAVE :: FIRSTIME = .TRUE.

c     IF ( FIRSTIME ) THEN
c        FIRSTIME = .FALSE.
c        LOGDEV = INIT3 ()
c     ENDIF

c     DO I = 1,NSEN
c        DO J = 1,NSEN
c           AM(I,J) = 0.D0
c        ENDDO
c     ENDDO

      INTEGER I,J,K, iEQ
      REAL*8 C1, C2


      AM = 0.D0 

      iEQ = iK1
 
      IF (FROW(iEQ).EQ.1) THEN

         C1 =  3.*LN10
         C2 = -2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mH2SO4)+C2*DGAMA(I,mHHSO4)
         ENDDO

         AM(iEQ,jH)    = AM(iEQ,jH)    + ONE/MOLALD(jH)
         AM(iEQ,jSO4)  = AM(iEQ,jSO4)  + ONE/MOLALD(jSO4)
         AM(iEQ,jHSO4) = AM(iEQ,jHSO4) - ONE/MOLALD(jHSO4)
         AM(iEQ,JH2O)  = AM(iEQ,JH2O)  - ONE/WATER
      ENDIF

      iEQ = iK2    
  
      IF (FROW(iEQ).EQ.1) THEN
         IF (CC.EQ.'A') THEN
            C1 =  2.*LN10
            C2 = -2.*LN10
            DO I = 1, NIONSPC
               AM(iEQ,I) = C1*DGAMA(I,mNH4HS4)+C2*DGAMA(I,mHHSO4)
            ENDDO
         ELSE
            C1 =  2.*LN10
            C2 = -2.*LN10
            DO I = 1, NIONSPC
               AM(iEQ,I) = C1*DGAMA(I,mNH4NO3)+C2*DGAMA(I,mHNO3)
            ENDDO
         ENDIF
            AM(iEQ,jH)    = AM(iEQ,jH)    - ONE/MOLALD(jH)
            AM(iEQ,jNH3)  =               - ONE/GNH3D
            AM(iEQ,jNH4)  = AM(iEQ,jNH4)  + ONE/MOLALD(jNH4)
      ENDIF

      iEQ = iK3

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mHCL)
         ENDDO
         AM(iEQ,jH)    = AM(iEQ,jH)    +    ONE/MOLALD(jH)
         AM(iEQ,jCL)   = AM(iEQ,jCL)   +    ONE/MOLALD(jCL)
         AM(iEQ,jHCL)  =               -    ONE/GHCLD
         AM(iEQ,JH2O)  = AM(iEQ,JH2O)  - 2.*ONE/WATER
      ENDIF


      iEQ = iK4

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mHNO3)
         ENDDO
         AM(iEQ,jH)    = AM(iEQ,jH)    +    ONE/MOLALD(jH)
         AM(iEQ,jNO3)  = AM(iEQ,jNO3)  +    ONE/MOLALD(jNO3)
         AM(iEQ,jHNO3) =               -    ONE/GHNO3D
         AM(iEQ,JH2O)  = AM(iEQ,JH2O)  - 2.*ONE/WATER
      ENDIF

      iEQ = iK5

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  3.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mNA2SO4)
         ENDDO
         AM(iEQ,jNA)   = AM(iEQ,jNA)   + 2.*ONE/MAX(MOLALD(jNA),TINY)
         AM(iEQ,jSO4)  = AM(iEQ,jSO4)  +    ONE/MOLALD(jSO4)
         AM(iEQ,JH2O)  = AM(iEQ,JH2O)  - 3.*ONE/WATER
      ENDIF

      iEQ = iK6

      IF (FROW(iEQ).EQ.1) THEN
         AM(iEQ,jHCL)  = ONE/GHCLD
         AM(iEQ,jNH3)  = ONE/GNH3D
      ENDIF

      iEQ = iK7

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  3.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mNH42S4)
         ENDDO
         AM(iEQ,jNH4)  = AM(iEQ,jNH4)  + 2.*ONE/MOLALD(jNH4)
         AM(iEQ,jSO4)  = AM(iEQ,jSO4)  +    ONE/MOLALD(jSO4)
         AM(iEQ,JH2O)  = AM(iEQ,JH2O)  - 3.*ONE/WATER
      ENDIF

      iEQ = iK8

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mNACL)
         ENDDO
         AM(iEQ,jNA)  = AM(iEQ,jNA)   +    ONE/MAX(MOLALD(jNA),TINY)
         AM(iEQ,jCL)  = AM(iEQ,jCL)   +    ONE/MOLALD(jCL)
         AM(iEQ,JH2O) = AM(iEQ,JH2O)  - 2.*ONE/WATER
      ENDIF

      iEQ = iK9

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mNANO3)
         ENDDO
         AM(iEQ,jNA)  = AM(iEQ,jNA)   +    ONE/MAX(MOLALD(jNA),TINY)
         AM(iEQ,jNO3) = AM(iEQ,jNO3)  +    ONE/MOLALD(jNO3)
         AM(iEQ,JH2O) = AM(iEQ,JH2O)  - 2.*ONE/WATER
      ENDIF

      iEQ = iK10

      IF (FROW(iEQ).EQ.1) THEN
         AM(iEQ,jHNO3)  = ONE/GHNO3D
         AM(iEQ,jNH3)   = ONE/GNH3D
      ENDIF

      iEQ = iK11

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mNAHSO4)
         ENDDO
         AM(iEQ,jNA)   = AM(iEQ,jNA)    +    ONE/MAX(MOLALD(jNA),TINY)
         AM(iEQ,jHSO4) = AM(iEQ,jHSO4)  +    ONE/MOLALD(jHSO4)
         AM(iEQ,JH2O)  = AM(iEQ,JH2O)   - 2.*ONE/WATER
      ENDIF

      iEQ = iK12

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mNH4HS4)
         ENDDO
         AM(iEQ,jNH4)  = AM(iEQ,jNH4)   +    ONE/MOLALD(jNH4)
         AM(iEQ,jHSO4) = AM(iEQ,jHSO4)  +    ONE/MOLALD(jHSO4)
         AM(iEQ,JH2O)  = AM(iEQ,JH2O)   - 2.*ONE/WATER
      ENDIF

      iEQ = iK13

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  5.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mLC)
         ENDDO
         AM(iEQ,jNH4)  = AM(iEQ,jNH4)  + 3.*ONE/MOLALD(jNH4)
         AM(iEQ,jHSO4) = AM(iEQ,jHSO4) +    ONE/MOLALD(jHSO4)
         AM(iEQ,jSO4)  = AM(iEQ,jSO4)  +    ONE/MOLALD(jSO4)
         AM(iEQ,JH2O)  = AM(iEQ,JH2O)  - 5.*ONE/WATER
      ENDIF

      iEQ = iK14

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  3.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mCANO32)
         ENDDO
         AM(iEQ,jCA)  = AM(iEQ,jCA)   +    ONE/MOLALD(jCA)
         AM(iEQ,jNO3) = AM(iEQ,jNO3)  + 2.*ONE/MOLALD(jNO3)
         AM(iEQ,JH2O) = AM(iEQ,JH2O)  - 3.*ONE/WATER
      ENDIF

      iEQ = iK15

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  3.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mCACL2)
         ENDDO
         AM(iEQ,jCA)  = AM(iEQ,jCA)   +    ONE/MOLALD(jCA)
         AM(iEQ,jCL)  = AM(iEQ,jCL)   + 2.*ONE/MOLALD(jCL)
         AM(iEQ,JH2O) = AM(iEQ,JH2O)  - 3.*ONE/WATER
      ENDIF

      iEQ = iK16

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  3.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mK2SO4)
         ENDDO
         AM(iEQ,jK)    = AM(iEQ,jK)   + 2.*ONE/MAX(MOLALD(jK),TINY)
         AM(iEQ,jSO4)  = AM(iEQ,jSO4)  +    ONE/MOLALD(jSO4)
         AM(iEQ,JH2O)  = AM(iEQ,JH2O)  - 3.*ONE/WATER
      ENDIF

      iEQ = iK17

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mKHSO4)
         ENDDO
         AM(iEQ,jK)    = AM(iEQ,jK)     +    ONE/MAX(MOLALD(jK),TINY)
         AM(iEQ,jHSO4) = AM(iEQ,jHSO4)  +    ONE/MOLALD(jHSO4)
         AM(iEQ,JH2O)  = AM(iEQ,JH2O)   - 2.*ONE/WATER
      ENDIF

      iEQ = iK18

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mKNO3)
         ENDDO
         AM(iEQ,jK)   = AM(iEQ,jK)    +    ONE/MAX(MOLALD(jK),TINY)
         AM(iEQ,jNO3) = AM(iEQ,jNO3)  +    ONE/MOLALD(jNO3)
         AM(iEQ,JH2O) = AM(iEQ,JH2O)  - 2.*ONE/WATER
      ENDIF

      iEQ = iK19

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mKCL)
         ENDDO
         AM(iEQ,jK)   = AM(iEQ,jK)    +    ONE/MAX(MOLALD(jK),TINY)
         AM(iEQ,jCL)  = AM(iEQ,jCL)   +    ONE/MOLALD(jCL)
         AM(iEQ,JH2O) = AM(iEQ,JH2O)  - 2.*ONE/WATER
      ENDIF

      iEQ = iK20

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  2.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mMGSO4)
         ENDDO
         AM(iEQ,jMG)  = AM(iEQ,jMG)   +    ONE/MOLALD(jMG)
         AM(iEQ,jSO4) = AM(iEQ,jSO4)  +    ONE/MOLALD(jSO4)
         AM(iEQ,JH2O) = AM(iEQ,JH2O)  - 2.*ONE/WATER
      ENDIF

      iEQ = iK21

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  3.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mMGNO32)
         ENDDO
         AM(iEQ,jMG)  = AM(iEQ,jMG)   +    ONE/MOLALD(jMG)
         AM(iEQ,jNO3) = AM(iEQ,jNO3)  + 2.*ONE/MOLALD(jNO3)
         AM(iEQ,JH2O) = AM(iEQ,JH2O)  - 3.*ONE/WATER
      ENDIF

      iEQ = iK22

      IF (FROW(iEQ).EQ.1) THEN
         C1 =  3.*LN10
         DO I = 1, NIONSPC
            AM(iEQ,I) = C1*DGAMA(I,mMGCL2)
         ENDDO
         AM(iEQ,jMG)  = AM(iEQ,jMG)   +    ONE/MOLALD(jMG)
         AM(iEQ,jCL)  = AM(iEQ,jCL)   + 2.*ONE/MOLALD(jCL)
         AM(iEQ,JH2O) = AM(iEQ,JH2O)  - 3.*ONE/WATER
      ENDIF

      iEQ = iMBNA

      IF (FROW(iEQ).EQ.1) THEN
         AM(iEQ,jNA)     = 1.
         AM(iEQ,jNACL)   = 1.
         AM(iEQ,jNA2SO4) = 2.
         AM(iEQ,jNAHSO4) = 1.
         AM(iEQ,jNANO3)  = 1.
      ENDIF

      iEQ = iMBSO4

      IF (FROW(iEQ).EQ.1) THEN
         AM(iEQ,jSO4)    = 1.
         AM(iEQ,jHSO4)   = 1.
         AM(iEQ,jNH42S4) = 1.
         AM(iEQ,jNH4HS4) = 1.
         AM(iEQ,jNA2SO4) = 1.
         AM(iEQ,jNAHSO4) = 1.
         AM(iEQ,jLC)     = 2.
         AM(iEQ,jK2SO4)  = 1.
         AM(iEQ,jKHSO4)  = 1.
         AM(iEQ,jMGSO4)  = 1.
         AM(iEQ,jCASO4)  = 1.
      ENDIF

      iEQ = iMBNH4

      IF (FROW(iEQ).EQ.1) THEN
         AM(iEQ,jNH3)    = 1.
         AM(iEQ,jNH4)    = 1.
         AM(iEQ,jNH4CL)  = 1.
         AM(iEQ,jNH42S4) = 2.
         AM(iEQ,jNH4HS4) = 1.
         AM(iEQ,jNH4NO3) = 1.
         AM(iEQ,jLC)     = 3.
      ENDIF

      iEQ = iMBNO3

      IF (FROW(iEQ).EQ.1) THEN
         AM(iEQ,jHNO3)   = 1.
         AM(iEQ,jNO3)    = 1.
         AM(iEQ,jNH4NO3) = 1.
         AM(iEQ,jNANO3)  = 1.
         AM(iEQ,jCANO32) = 2.
         AM(iEQ,jKNO3)   = 1.
         AM(iEQ,jMGNO32) = 2.
      ENDIF

      iEQ = iMBCL

      IF (FROW(iEQ).EQ.1) THEN
         AM(iEQ,jHCL)    = 1.
         AM(iEQ,jCL)     = 1.
         AM(iEQ,jNH4CL)  = 1.
         AM(iEQ,jNACL)   = 1.
         AM(iEQ,jCACL2)  = 2.
         AM(iEQ,jKCL)    = 1.
         AM(iEQ,jMGCL2)  = 2.
      ENDIF

      iEQ = iMBCA

      IF (FROW(iEQ).EQ.1) THEN
         AM(iEQ,jCA)     = 1.
         AM(iEQ,jCACL2)  = 1.
         AM(iEQ,jCASO4)  = 1.
         AM(iEQ,jCANO32) = 1.
      ENDIF

      iEQ = iMBK

      IF (FROW(iEQ).EQ.1) THEN
         AM(iEQ,jK)      = 1.
         AM(iEQ,jKCL)    = 1.
         AM(iEQ,jK2SO4)  = 2.
         AM(iEQ,jKHSO4)  = 1.
         AM(iEQ,jKNO3)   = 1.
      ENDIF

      iEQ = iMBMG

      IF (FROW(iEQ).EQ.1) THEN
         AM(iEQ,jMG)     = 1.
         AM(iEQ,jMGCL2)  = 1.
         AM(iEQ,jMGSO4)  = 1.
         AM(iEQ,jMGNO32) = 1.
      ENDIF

      iEQ = iCB
      IF(MOLALD(jH).LT.TINY) THEN
         AM(iEQ,jH)    =  1.
         AM(iEQ,jH2O)  =  0
      ELSE
         AM(iEQ,jH)    =  1. + XKW*RH*(WATER/MOLALD(jH))**2.
         AM(iEQ,jH2O)  = -2.*XKW*RH*WATER/MOLALD(jH)
      ENDIF

      AM(iEQ,jNA)   =  1.
      AM(iEQ,jNH4)  =  1.
      AM(iEQ,jMG)   =  2.
      AM(iEQ,jCA)   =  2.
      AM(iEQ,jK)    =  1.
      AM(iEQ,jSO4)  = -2.
      AM(iEQ,jHSO4) = -1.
      AM(iEQ,jCL)   = -1.
      AM(iEQ,jNO3)  = -1.

      iEQ = iZSR

      DO I = 1, NSEN
         AM_TEMP(I) = AM(iEQ,I)
      END DO

      CALL DZSR(AM_TEMP)

      DO I = 1, NSEN
         AM(iEQ,I) = AM_TEMP(I)
      END DO

C
C *** END OF SUBROUTINE AMAT ***
C
      RETURN
      END


      SUBROUTINE DZSR(SW)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION SW(NSEN),TSW(NSEN),RHS
      DOUBLE PRECISION SO4I  ,HSOI ,AML5
      DOUBLE PRECISION FRNH4 ,FRCA ,FRK ,FRMG ,FRSO4 ,FRCL ,FRNO3,
     &                 DFRNH4,DFRCA,DFRK,DFRMG,DFRSO4,DFRCL,DFRNO3

      DOUBLE PRECISION HSO4I, FRNA, DFRNA, DDRCL

      SW(jH2O) = -1.D0
      RHS      = 0.D0


      FRNH4 = ZERO
      FRCA  = ZERO
      FRK   = ZERO
      FRMG  = ZERO
      FRSO4 = ZERO
      FRCL  = ZERO
      FRNO3 = ZERO

      IF (CC.EQ.'A') THEN
         SW(jSO4)  = 1./M0(mNH42S4)
         SW(jHSO4) = 1./M0(mNH42S4)
      ELSEIF (CC.EQ.'C'.OR.CC.EQ.'F'.OR.
     &        CC.EQ.'J'.OR.CC.EQ.'K') THEN
         SW(jSO4)  =   1./M0(mH2SO4)
         SW(jHSO4) =   1./M0(mH2SO4)
         SW(jNH4)  =  -1./M0(mH2SO4) +1./M0(mNH4HS4)
         SW(jNA)   =  -1./M0(mH2SO4) +1./M0(mNAHSO4)
         SW(jCA)   =  -1./M0(mH2SO4) +1./M0(mCASO4)
         SW(jK)    =  -1./M0(mH2SO4) +1./M0(mKHSO4)
         SW(jMG)   =  -1./M0(mH2SO4) +1./M0(mMGSO4)
      ELSEIF (CC.EQ.'B'.OR.CC.EQ.'E') THEN 
         SO4I  = MOLALD(jSO4 ) -MOLALD(jH)
         HSO4I = MOLALD(jHSO4) +MOLALD(jH)
         IF (SO4I.LT.HSO4I) THEN
            SW(jSO4)  =  1./M0(mLC) -1./M0(mNH4HS4)
            SW(jHSO4) =              1./M0(mNH4HS4)
            SW(jH)    = -1./M0(mLC) +2./M0(mNH4HS4)
         ELSE
            SW(jSO4)  =              1./M0(mNH42S4)
            SW(jHSO4) =  1./M0(mLC) -1./M0(mNH42S4)
            SW(jH)    =  1./M0(mLC) -2./M0(mNH42S4)
         ENDIF
      ELSEIF (CC.EQ.'D') THEN
         SW(jSO4)  = 1./M0(mNH42S4)
         SW(jHSO4) = 1./M0(mNH42S4)
         AML5      = MOLALD(3)-2.D0*(MOLALD(jSO4)+MOLALD(jHSO4))
         IF(AML5.LT.MOLALD(jNO3)) THEN
            SW(jNH4)  =            1./M0(mNH4NO3)
            SW(jSO4)  = SW(jSO4)  -2./M0(mNH4NO3)
            SW(jHSO4) = SW(jHSO4) -2./M0(mNH4NO3)                  
         ELSE
            SW(jNO3)  = 1./M0(mNH4NO3)
         ENDIF
      ELSEIF (CC.EQ.'G'.OR.CC.EQ.'O') THEN
         SW(jNA)   = 0.5/M0(mNA2SO4) -0.5/M0(mNH42S4)
         SW(jK)    = 0.5/M0(mK2SO4)  -0.5/M0(mNH42S4)
         SW(jMG)   = 1.0/M0(mMGSO4)  -1.0/M0(mNH42S4)
         SW(jSO4)  =                   1./M0(mNH42S4)
         SW(jHSO4) =                   1./M0(mNH42S4)
         FRNH4 =          MOLALD(jNH4)+MOLALD(jNA)+MOLALD(jK)
     &          +MOLALD(jMG)-2.D0*(MOLALD(jSO4)+MOLALD(jHSO4))
         IF (FRNH4.LT.MOLAL(jNO3)) THEN
            SW(jNH4)  =            1./M0(mNH4NO3)
            SW(jNA)   = SW(jNA)   +1./M0(mNH4NO3)
            SW(jSO4)  = SW(jSO4)  -2./M0(mNH4NO3)
            SW(jHSO4) = SW(jHSO4) -2./M0(mNH4NO3)
         ELSE
            SW(jNO3)  =            1./M0(mNH4NO3)
            FRNH4 = FRNH4 - MOLALD(jNO3)
            IF (FRNH4.LT.MOLALD(jCL)) THEN
               SW(jNH4 ) =            1./M0(mNH4CL)
               SW(jNA  ) = SW(jNA  ) +1./M0(mNH4CL)
               SW(jSO4 ) = SW(jSO4 ) -2./M0(mNH4CL)
               SW(jHSO4) = SW(jHSO4) -2./M0(mNH4CL)
               SW(jNO3 ) = SW(jNO3 ) +1./M0(mNH4CL)               
            ELSE
               SW(jCL)   =            1./M0(mNH4CL)
            ENDIF
         ENDIF
      ELSEIF (CC.EQ.'H'.OR.CC.EQ.'M') THEN
         RHS        =       0.5*SINI(iMBK  )/M0(mK2SO4 )
         SW(jK2SO4) =                    -1./M0(mK2SO4 )
         RHS        = RHS      +SINI(iMBMG )/M0(mMGSO4 )
         SW(jMGSO4) =                    -1./M0(mMGSO4 )         
         RHS        = RHS      +SINI(iMBSO4)/M0(mNA2SO4)
     &                         -SINI(iMBCA )/M0(mNA2SO4)
     &                     -0.5*SINI(iMBK  )/M0(mNA2SO4)
     &                         -SINI(iMBMG )/M0(mNA2SO4)
         SW(jNA2SO4) =                   -1./M0(mNA2SO4)
         FRNA  = W(jTNA) -2.*W(jTSO4)
         DFRNA = SINI(iMBNA) -2.*SINI(iMBSO4)
         IF (FRNA.LT.W(jTNO3)) THEN                       !1)
            RHS = RHS +DFRNA/M0(mNANO3)
            SW(jNANO3) = -1./M0(mNANO3)
            FRNO3 = MOLALD(jNO3) -FRNA + CNANO3
            IF (FRNO3.GT.ZERO) THEN                         !2) 
               IF (FRNO3.LT.MOLALD(jNH4)) THEN                  !3)
                  RHS        = RHS     -DFRNA/M0(mNH4NO3)
                  SW(jNO3)   =             1./M0(mNH4NO3)
                  SW(jNANO3) = SW(jNANO3) +1./M0(mNH4NO3)
                  FRNH4 = MOLALD(jNH4) -FRNO3
                  IF (FRNH4.LT.MOLALD(jCL)) THEN                  !4)
                     RHS        = RHS     +DFRNA/M0(mNH4CL)
                     SW(jNH4)   =             1./M0(mNH4CL)
                     SW(jNO3)   = SW(jNO3)   -1./M0(mNH4CL)
                     SW(jNANO3) = SW(jNANO3) -1./M0(mNH4CL)
                  ELSE                                            !4)
                     SW(jCL)    =             1./M0(mNH4CL)
                  ENDIF                                           !4)
               ELSE                                            !3)
                  SW(jNH4) =                  1./M0(mNH4NO3)
               ENDIF                                           !3)
            ELSE                                            !2)
               IF (MOLALD(jCL).LT.MOLALD(jNH4)) THEN            !3)
                  SW(jCL)      =              1./M0(mNH4CL)
               ELSE                                            !3)
                  SW(jNH4)     =              1./M0(mNH4CL)
               ENDIF                                           !3)
            ENDIF                                           !2)
         ELSE                                             !1)
            RHS = RHS + SINI(iMBNO3)/M0(mNANO3)
            SW(jNANO3) = -1./M0(mNANO3)
            FRNA  = FRNA  - W(jTNO3)
            DFRNA = DFRNA - SINI(iMBNO3)
            IF (FRNA.LT.W(jTCL)) THEN                        !2)
               RHS = RHS +DFRNA/M0(mNACL)
               SW(jNACL) =  -1./M0(mNACL)
               FRCL  = MOLALD(jCL) -FRNA +CNACL
               FRNH4 = MOLALD(jNH4)
               IF (FRCL.GT.ZERO) THEN                          !3)
                  IF (FRCL.LT.FRNH4) THEN                         !4)
                     RHS       = RHS    -DFRNA/M0(mNH4CL)
                     SW(jCL) =              1./M0(mNH4CL)
                     SW(jNACL) = SW(jNACL) +1./M0(mNH4CL)
                  ELSE                                            !4)
                     SW(jNH4)  =            1./M0(mNH4CL)
                  ENDIF                                           !4)
               ENDIF                                           !3)
            ELSE                                            !2)
               RHS = RHS + SINI(iMBCL)/M0(mNACL)
               SW(jNACL) =        -1./M0(mNACL)
            ENDIF                                           !2)
         ENDIF                                           !1)
      ELSEIF (CC.EQ.'I') THEN
         CALL DCALCI1A(TSW)
         RHS = TSW(jNA2SO4)/M0(mNA2SO4) +TSW(jNAHSO4)/M0(mNAHSO4)
     &        +TSW(jNH42S4)/M0(mNH42S4) +TSW(jNH4HS4)/M0(mNH4HS4)
     &        +TSW(jLC)    /M0(mLC)
         SW(jNA2SO4) = -1./M0(mNA2SO4)
         SW(jNAHSO4) = -1./M0(mNAHSO4)
         SW(jNH42S4) = -1./M0(mNH42S4)
         SW(jNH4HS4) = -1./M0(mNH4HS4)
         SW(jLC)     = -1./M0(mLC)
      ELSEIF (CC.EQ.'P') THEN

        IF(W(jTSO4).LT.W(jTCA)) THEN                    !CASO4,K2SO4,MGSO4
          FRCA  = W(jTCA)-W(jTSO4)                      
          DFRCA = SINI(iMBCA)-SINI(iMBSO4)
          FRK   = W(jTK)
          DFRK  = SINI(iMBK)
          FRMG  = W(jTMG)
          DFRMG = SINI(iMBMG)
        ELSE
          FRCA   = ZERO
          FRSO4  = W(jTSO4)-W(jTCA)
          DFRSO4 = SINI(iMBSO4) -SINI(iMBCA)
          IF(FRSO4.LT.0.5*W(jTK)) THEN
            RHS   = DFRSO4/M0(mK2SO4)
            FRK   = W(jTK)-2.*FRSO4
            DFRK  = SINI(iMBK) -2.*DFRSO4
            FRMG  = W(jTMG)
            DFRMG = SINI(iMBMG)
          ELSE
            RHS    = 0.5*SINI(iMBK)/M0(mK2SO4)
            FRK    = ZERO
            FRSO4  = FRSO4  -0.5*W(jTK)
            DFRSO4 = DFRSO4 -0.5*SINI(iMBK)
            RHS    = RHS    +DFRSO4/M0(mMGSO4)
            FRMG   = W(jTMG)-FRSO4
            DFRMG  = SINI(iMBMG) -DFRSO4
          ENDIF
        ENDIF
        IF(W(jTNA).LT.W(jTCL)) THEN                      !NACL
          FRNA  = ZERO
          DFRNA = ZERO
          FRCL  = W(jTCL) -W(jTNA)
          DFRCL = SINI(iMBCL) -SINI(iMBNA)
          RHS   = RHS +SINI(iMBNA)/M0(mNACL)
        ELSE
          FRCL  = ZERO
          DDRCL = ZERO
          FRNA  = W(jTNA) -W(jTCL)
          DFRNA = SINI(iMBNA) -SINI(iMBCL)
          RHS   = RHS +SINI(iMBCL)/M0(mNACL)
        ENDIF

        IF(FRCA.GT.ZERO) THEN                            !CANO32
          IF(FRCA.LT.0.5*W(jTNO3)) THEN
            RHS    = RHS          + DFRCA/M0(mCANO32)
            FRCA   = ZERO
            DFRCA  = ZERO
            FRNO3  = W(jTNO3)     - 2.*FRCA
            DFRNO3 = SINI(iMBNO3) - 2.*DFRCA
          ELSE
            RHS    = RHS   + 0.5*SINI(iMBNO3)/M0(mCANO32)
            FRNO3  = ZERO
            DFRNO3 = ZERO
            FRCA   = FRCA  - 0.5*W(jTNO3)
            DFRCA  = DFRCA - 0.5*SINI(iMBNO3)
          ENDIF
        ENDIF

        IF(FRCA.GT.ZERO.AND.FRCL.GT.ZERO) THEN             !CACL2
           IF(FRCA.LT.0.5*FRCL) THEN
            RHS   = RHS     + DFRCA/M0(mCACL2)
            FRCA  = ZERO
            DFRCA = ZERO
            FRCL  = FRCL    - 2.*FRCA
            DFRCL = DFRCL   - 2.*DFRCA
          ELSE
            RHS   = RHS     + 0.5*DFRCL/M0(mCACL2)
            FRCL  = ZERO
            DFRCL = ZERO
            FRCA  = FRCA    - 0.5*FRCL
            DFRCA = DFRCA   - 0.5*DFRCL
          ENDIF
        ENDIF         

        IF(FRMG.GT.ZERO.AND.FRNO3.GT.ZERO) THEN             !MGNO32
          IF(FRMG.LT.0.5*FRNO3) THEN
            RHS    = RHS    +DFRMG/M0(mMGNO32)
            FRMG   = ZERO
            DFRMG  = ZERO
            FRNO3  = FRNO3  -2.*FRMG
            DFRNO3 = DFRNO3 -2.*DFRMG
          ELSE
            RHS    = RHS   + 0.5*DFRNO3/M0(mMGNO32)
            FRNO3  = ZERO
            DFRNO3 = ZERO
            FRMG   = FRMG  - 0.5*FRNO3
            DFRMG  = DFRMG - 0.5*DFRNO3
          ENDIF
        ENDIF

        IF(FRMG.GT.ZERO.AND.FRCL.GT.ZERO) THEN               !MGCL2
           IF(FRMG.LT.0.5*FRCL) THEN
            RHS   = RHS   + DFRMG/M0(mMGCL2)
            FRMG  = ZERO
            DFRMG = ZERO
            FRCL  = FRCL  - 2.*FRMG
            DFRCL = DFRCL - 2.*DFRMG
          ELSE
            RHS   = RHS   + 0.5*DFRCL/M0(mMGCL2)
            FRCL  = 0.0
            DFRCL = 0.0
            FRMG  = FRMG  - 0.5*FRCL
            DFRMG = DFRMG - 0.5*DFRCL
          ENDIF
        ENDIF

        IF(FRNA.GT.ZERO.AND.FRNO3.GT.ZERO) THEN             !NANO3
          IF(FRNA.LT.FRNO3) THEN
            RHS    = RHS    + DFRNA/M0(mNANO3)
            FRNA   = ZERO
            DFRNA  = ZERO
            FRNO3  = FRNO3  - FRNA
            DFRNO3 = DFRNO3 - DFRNA
          ELSE
            RHS    = RHS   + DFRNO3/M0(mNANO3)
            FRNO3  = ZERO
            DFRNO3 = ZERO
            FRNA   = FRNA  - FRNO3
            DFRNA  = DFRNA - DFRNO3
          ENDIF
        ENDIF

        IF(FRK.GT.ZERO.AND.FRCL.GT.ZERO) THEN             !KCL
          IF(FRK.LT.FRCL) THEN
            RHS    = RHS   + DFRK/M0(mKCL)
            FRK    = ZERO
            DFRK   = ZERO
            FRCL   = FRCL  - FRK
            DFRCL  = DFRCL - DFRK
          ELSE
            RHS    = RHS  + DFRCL/M0(mKCL)
            FRCL   = ZERO
            DFRCL  = ZERO
            FRK    = FRK  - FRCL
            DFRK   = DFRK - DFRCL
          ENDIF
        ENDIF

        IF(FRK.GT.ZERO.AND.FRNO3.GT.ZERO) THEN             !KNO3
          IF(FRK.LT.FRNO3) THEN
            RHS    = RHS    + DFRK/M0(mKNO3)
            FRK    = ZERO
            DFRK   = ZERO
            FRNO3  = FRNO3  - FRK
            DFRNO3 = DFRNO3 - DFRK
          ELSE
            RHS    = RHS    + DFRNO3/M0(mKNO3)
            FRNO3  = ZERO
            DFRNO3 = ZERO
            FRK    = FRK    - FRNO3
            DFRK   = DFRK   - DFRNO3
          ENDIF
        ENDIF

      ELSEIF(CC.EQ.'L') THEN
         CALL DCALCL1A(TSW)
         RHS = TSW(jNA2SO4)/M0(mNA2SO4) +TSW(jNAHSO4)/M0(mNAHSO4)
     &        +TSW(jNH42S4)/M0(mNH42S4) +TSW(jNH4HS4)/M0(mNH4HS4)
     &        +TSW(jLC)    /M0(mLC)     +TSW(jK2SO4 )/M0(mK2SO4 )
     &        +TSW(jMGSO4 )/M0(mMGSO4 ) +TSW(jKHSO4 )/M0(mKHSO4 )
         SW(jNA2SO4) = -1./M0(mNA2SO4)
         SW(jNAHSO4) = -1./M0(mNAHSO4)
         SW(jNH42S4) = -1./M0(mNH42S4)
         SW(jNH4HS4) = -1./M0(mNH4HS4)
         SW(jLC    ) = -1./M0(mLC    )
         SW(jK2SO4 ) = -1./M0(mK2SO4 )
         SW(jMGSO4 ) = -1./M0(mMGSO4 )
         SW(jKHSO4 ) = -1./M0(mKHSO4 )              
      ENDIF

      SINI(iZSR) = -RHS
C
C *** END OF SUBROUTINE DZSR
C
      RETURN
      END

      SUBROUTINE DCALCI1A(TSW)
     
c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION TSW(NSEN)
      DOUBLE PRECISION FRSO4
      INTEGER I,J,K

      DO I = 1, NSEN
         TSW(I) = 0.0
      ENDDO

      TSW(jNA2SO4) = .5*SINI(iMBNA)
      FRSO4 = W(jTSO4) -0.5D0*W(jTNA) -2.D0*W(jTNH4)/3.D0
      IF (FRSO4.LE.TINY) THEN
        TSW(jLC)     = 2.*SINI(iMBSO4) -SINI(iMBNA) -SINI(iMBNH4)
        TSW(jNH42S4) = 2.*SINI(iMBNH4) -3.*SINI(iMBSO4) +1.5*SINI(iMBNA)
      ELSE
        IF (FRSO4.LE.W(jTNH4)/3.D0) THEN
          TSW(jNH4HS4) = 3.*SINI(iMBSO4) -1.5*SINI(iMBNA)
     &                  -2.*SINI(iMBNH4)
          TSW(jLC)     = SINI(iMBNH4) -SINI(iMBSO4) +.5*SINI(iMBNA)
        ELSE
          TSW(jNH4HS4) = SINI(iMBNH4)
          IF (0.5D0*W(jTNA).GT.TINY) THEN
            TSW(jNAHSO4) = 2.*SINI(iMBSO4) -SINI(iMBNA) -2.*SINI(iMBNH4)
            TSW(jNA2SO4) = SINI(iMBNH4) +SINI(iMBNA) -SINI(iMBSO4)
          ENDIF
        ENDIF
      ENDIF         
C
C *** END OF DCALCI1A
C
      RETURN
      END
                    
      SUBROUTINE DCALCL1A(TSW)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION TSW(NSEN)
      DOUBLE PRECISION FRSO4
      INTEGER I,J,K

      DOUBLE PRECISION DFRSO4, FRSO4T, DFRSO4T

      DO I = 1, NSEN
         TSW(I) = 0.0
      ENDDO

      FRSO4  = W(jTSO4)     -W(jTCA)
      DFRSO4 = SINI(iMBSO4) -SINI(iMBCA)
      TSW(jK2SO4) = 0.5D0*SINI(iMBK)
      FRSO4  = FRSO4        -0.5D0*W(jTK)
      DFRSO4 = DFRSO4       -0.5D0*SINI(iMBK)
      TSW(jNA2SO4) = 0.5D0*SINI(iMBNA)
      FRSO4  = FRSO4        -0.5D0*W(jTNA)
      DFRSO4 = DFRSO4       -0.5D0*SINI(iMBNA)
      TSW(jMGSO4) = SINI(iMBMG)
      FRSO4  = FRSO4        -W(jTMG)
      DFRSO4 = DFRSO4       -SINI(iMBMG)
  
      FRSO4T  = FRSO4 -2.D0*W(jTNH4)/3.D0
      DFRSO4T = DFRSO4 -2.D0*SINI(iMBNH4)/3.D0
      IF (FRSO4T.LE.TINY) THEN
        TSW(jLC)     = 2.D0*DFRSO4 -SINI(iMBNH4)
        TSW(jNH42S4) = 2.D0*SINI(iMBNH4) -3.D0*DFRSO4
      ELSE
        FRSO4  = FRSO4T
        DFRSO4 = DFRSO4T
        IF (FRSO4T.LE.W(jTNH4)/3.D0) THEN
          TSW(jNH4HS4) = 3.D0*DFRSO4
          TSW(jLC)     = SINI(iMBNH4)/3.D0 -DFRSO4
        ELSE
          TSW(jNH4HS4) = SINI(iMBNH4)
          IF (0.5D0*W(jTNA).GT.TINY) THEN
            FRSO4 = FRSO4 - W(jTNH4)/3.D0
            IF (FRSO4.GT.ZERO) THEN
               DFRSO4 = DFRSO4 -SINI(iMBNH4)/3.D0
               TSW(jNAHSO4) = 2.D0*DFRSO4
               TSW(jNA2SO4) = TSW(jNA2SO4)-DFRSO4
            ENDIF
          ENDIF
          IF (0.5D0*W(jTK).GT.TINY) THEN
             FRSO4 = FRSO4 - W(jTNH4)/3.D0
             IF (FRSO4.GT.ZERO) THEN
               DFRSO4 = DFRSO4 -SINI(iMBNH4)/3.D0
               TSW(jKHSO4 ) = 2.D0*DFRSO4
               TSW(jK2SO4 ) = TSW(jK2SO4)-DFRSO4
              ENDIF
          ENDIF
        ENDIF
      ENDIF         
C
C *** END OF DCALCI1A
C
      RETURN
      END


      SUBROUTINE EQNSLV(FROW,FCOL,COEF,SENS,SENSD)

c     USE DDM3D_DEFN, ONLY : WRFLAG
      USE UTILIO_DEFN         ! I/O API
      USE RUNTIME_VARS, ONLY: LOGDEV

c     IMPLICIT NONE
      
      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      INTEGER FROW(NSEN),FCOL(NSEN)
      DOUBLE PRECISION COEF(NSEN,NSEN)
      DOUBLE PRECISION SENS(NSEN),SENSD(NSEN)
      DIMENSION IPVT(NDIM)
      DOUBLE PRECISION AA(NDIM,NDIM),BB(NDIM)
      DOUBLE PRECISION AAT(NDIM*NDIM)
      INTEGER I,J,K
      INTEGER ICOL, INFO, IROW, IPVT

c     INTEGER, SAVE :: LOGDEV
c     LOGICAL, SAVE :: FIRSTIME = .TRUE.

c     IF ( FIRSTIME ) THEN
c        FIRSTIME = .FALSE.
c        LOGDEV = INIT3 ()
c     ENDIF


C
C *** ELIMINATE A-MATRIX AND B-VECTOR ***
C
      I = 0
      DO IROW = 1,NSEN
         IF(FROW(IROW).EQ.1) THEN
           I = I+1
           J = 0
           DO ICOL = 1,NSEN
             IF(FCOL(ICOL).EQ.1) THEN
               J = J+1
               AA(I,J) = COEF(IROW,ICOL)
             ENDIF
           ENDDO
           BB(I) = SINI(IROW)
         ENDIF
      ENDDO


C
C *** LU-DECOMPOSITION ***
C
      K = 0
      DO J = 1,NDIM
         DO I = 1,NDIM
            K = K+1
            AAT(K) = AA(I,J)
         ENDDO
      ENDDO

      INFO = 0
      CALL dgefa(AAT,NDIM,NDIM,IPVT,INFO)

      IF (INFO.NE.0) THEN
         WRITE(LOGDEV,*) 'ERROR IN DGEFA'

         write(LOGDEV,*) 'IONS', MOLALD
         write(LOGDEV,*) 'GASES', GNH3, GHNO3, GHCL, GNH3D, GHNO3D, GHCLD
         write(LOGDEV,*) 'WATER', WATER

         WRITE(LOGDEV,*) 'ISOROPIA case = ',SCASE
         WRITE(LOGDEV,*) 'ISOROPIA W =',W
         WRITE(LOGDEV,*) 'ndim = ',ndim
         WRITE(LOGDEV,*) 'frow = ',frow
         WRITE(LOGDEV,*) 'fcol = ',fcol
         WRITE(LOGDEV,*) 'AA = '
         do i = 1,ndim
            write(LOGDEV,*), i
            write(LOGDEV,*) (AA(i,j),j=1,ndim)
         enddo
         write(LOGDEV,*) 'BB = ',(BB(i),i=1,ndim)
         STOP
      ENDIF
C
C *** SOLVE THE EQUATION SYSTEM ***
C
      CALL dgesl(AAT,NDIM,NDIM,IPVT,BB,0)

C
C *** MAP SENSITIVITIES ***
C
      I = 0
      DO ICOL = 1,NSEN
         IF(FCOL(ICOL).EQ.1) THEN
           I = I+1
           SENSD(ICOL) = BB(I)
           SENS(ICOL ) = BB(I)
         ENDIF
      ENDDO

C
C *** END OF EQNSLV ***
C
      RETURN
      END

      SUBROUTINE DCALCNH3(SENS)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'
 
      DOUBLE PRECISION SENS(NSEN)
      DOUBLE PRECISION DPSI,GR,C,SR
     
c     INTEGER, SAVE :: LOGDEV
c     LOGICAL, SAVE :: FIRSTIME = .TRUE.

c     IF ( FIRSTIME ) THEN
c        FIRSTIME = .FALSE.
c        LOGDEV = INIT3 ()
c     ENDIF


c     IF (WATER.LE.TINY) RETURN
      IF ( WATER       .LE. TINY .OR.
     &     MOLAL(jNH4) .LE. TINY .OR.
     &     MOLAL(jH)   .LE. TINY .OR.
     &     GNH3        .LE. TINY      ) THEN
         RETURN
      ENDIF

      GR   = -2.D0*LN10*(SGAMA(mHNO3)-SGAMA(mNH4NO3))     !GAMA RELATED
      C    = ONE/MOLAL(jNH4) +ONE/MOLAL(jH) +ONE/GNH3
      SR   = SENS(jNH4)/MOLAL(jNH4) -SENS(jH)/MOLAL(jH)    !SENS RELATED
      DPSI = (SR +GR)/C

      SENS(jNH3) = DPSI
      SENS(jNH4) = SENS(jNH4) -DPSI
      SENS(jH  ) = SENS(JH  ) -DPSI

      RETURN
      END

      SUBROUTINE DCALCNA(SENS)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION SENS(NSEN)
      DOUBLE PRECISION DELT,GR,SR,WR,C
      
c     IF (WTAER.LE.TINY) RETURN
      IF ( WATER      .LE. TINY .OR. 
     &     MOLAL(jH)  .LE. TINY .OR. 
     &     MOLAL(jNO3).LE. TINY      ) RETURN
 
      WR   =  2.D0*SENS(jH2O)/WATER
      GR   = -2.D0*LN10*SGAMA(mHNO3)
      SR   = -SENS(jH)/MOLAL(jH) +SINI(iMBNO3)/GHNO3
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

      SUBROUTINE DCALCNHA(SENS)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION SENS(NSEN)
      DOUBLE PRECISION DNO,DCL,C11,C12,C21,C22,B1,B2,SR,WR,GR

c     INTEGER, SAVE :: LOGDEV
c     LOGICAL, SAVE :: FIRSTIME = .TRUE.

c     IF ( FIRSTIME ) THEN
c        FIRSTIME = .FALSE.
c        LOGDEV = INIT3 ()
c     ENDIF

      IF (WATER.LE.TINY) THEN
         SENS(jHNO3) = SINI(iMBNO3)
         SENS(jHCL ) = SINI(iMBCL )
         RETURN
      ELSEIF (W(jTCL).LE.TINY.AND.W(jTNO3).LE.TINY) THEN
         RETURN
      ELSEIF (W(jTCL).LE.TINY) THEN
         CALL DCALCNA(SENS)
      ELSEIF (W(jTNO3).LE.TINY) THEN
         CALL DCALCHA(SENS)
      ENDIF

      IF ( MOLAL(jH)   .LE. TINY .OR.
     &     MOLAL(jNO3) .LE. TINY .OR.
     &     GHNO3       .LE. TINY .OR.
     &     MOLAL(jCL)  .LE. TINY .OR.
     &     GHCL        .LE. TINY      ) THEN
         DCL = ZERO
         DNO = ZERO
         RETURN
      ENDIF

      C11 = ONE/MOLAL(jH) +ONE/MOLAL(jNO3) +ONE/GHNO3
      C12 = ONE/MOLAL(jH)
      C21 = C12
      C22 = ONE/MOLAL(jH) +ONE/MOLAL(jCL)  +ONE/GHCL
      SR  =-SENS(jH)/MOLAL(jH) +SINI(iMBNO3)/GHNO3
      WR  = 2.D0*SENS(jH2O)/WATER
      GR  =-2.D0*LN10*SGAMA(mHNO3)
      B1  = SR +WR +GR
      SR  =-SENS(jH)/MOLAL(jH) +SINI(iMBCL)/GHCL
      GR  =-2.D0*LN10*SGAMA(mHCL)
      B2  = SR +WR +GR

      DCL = (B1*C21 -B2*C11)/(C21*C12 -C22*C11)
      IF (MOLAL(jCL).EQ.W(jTCL)) DCL = SINI(iMBCL)
      DNO = (B1 -C12*DCL)/C11
      IF (MOLAL(jNO3).EQ.W(jTNO3)) DNO = SINI(iMBNO3)

c     IF (MOLAL(jCL).EQ.TINY.AND.MOLAL(jNO3).EQ.TINY) THEN
      IF (MOLAL(jCL).LE.TINY.AND.MOLAL(jNO3).LE.TINY) THEN
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
      
      SUBROUTINE DCALCHA(SENS)

c     IMPLICIT NONE
        
      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION SENS(NSEN)
      DOUBLE PRECISION DELT,GR,SR,WR,C
      
c     IF (WTAER.LE.TINY) RETURN
      IF (WATER      .LE. TINY .OR. 
     &    MOLAL(jH)  .LE. TINY .OR.
     &    MOLAL(jCL) .LE. TINY     ) RETURN
 
      WR   =  2.D0*SENS(jH2O)/WATER
      GR   = -2.D0*LN10*SGAMA(mHCL)
      SR   = -SENS(jH)/MOLAL(jH) +SINI(iMBCL)/GHCL
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


      SUBROUTINE DCALCHS4(SENS)

c     IMPLICIT NONE

      INCLUDE 'isrpia.inc'
      INCLUDE 'aero_sens_data.inc'

      DOUBLE PRECISION SENS(NSEN)
      DOUBLE PRECISION DELTA,GR,SR,WR,C
      
c     IF (WATER.LE.1D1*TINY) RETURN
c     IF (MOLAL(jHSO4).EQ.ZERO) RETURN

      IF ( WATER.LE.1D1*TINY .OR. 
     &     MOLAL(jHSO4) .LE. TINY .OR.
     &     MOLAL(jH)    .LE. TINY .OR.
     &     MOLAL(jSO4)  .LE. TINY      )  RETURN

      WR    =  ONE*SENS(jH2O)/WATER
      GR    =  2.D0*LN10*SGAMA(mHHSO4)-3.D0*LN10*SGAMA(mH2SO4)
      SR    = -SENS(jH)/MOLAL(jH) -SENS(jSO4)/MOLAL(jSO4)
      C     = -ONE/MOLAL(jH) -ONE/MOLAL(jSO4) -ONE/MOLAL(jHSO4)
      DELTA =  (WR +GR +SR)/C

      SENS(jH)    = SENS(jH) -DELTA
      SENS(jSO4)  = SENS(jSO4) -DELTA
      SENS(jHSO4) = DELTA

      RETURN
      END 

      subroutine dgefa(a,lda,n,ipvt,info)

      implicit none

      integer lda,n,ipvt(n),info
      double precision a(lda,n)

c
c     dgefa factors a double precision matrix by gaussian elimination.
c
c     dgefa is usually called by dgeco, but it can be called
c     directly with a saving in time if  rcond  is not needed.
c     (time for dgeco) = (1 + 9/n)*(time for dgefa) .
c
c     on entry
c
c        a       double precision(lda, n)
c                the matrix to be factored.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c     on return
c
c        a       an upper triangular matrix and the multipliers
c                which were used to obtain it.
c                the factorization can be written  a = l*u  where
c                l  is a product of permutation and unit lower
c                triangular matrices and  u  is upper triangular.
c
c        ipvt    integer(n)
c                an integer vector of pivot indices.
c
c        info    integer
c                = 0  normal value.
c                = k  if  u(k,k) .eq. 0.0 .  this is not an error
c                     condition for this subroutine, but it does
c                     indicate that dgesl or dgedi will divide by zero
c                     if called.  use  rcond  in dgeco for a reliable
c                     indication of singularity.
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,dscal,idamax
c
c     internal variables
c
      double precision t
      integer idamax,j,k,kp1,l,nm1
c
c
c     gaussian elimination with partial pivoting
c
      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k = 1, nm1
         kp1 = k + 1
c
c        find l = pivot index
c
         l = idamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l
c
c        zero pivot implies this column already triangularized
c
         if (a(l,k) .eq. 0.0d0) go to 40
c
c           interchange if necessary
c
            if (l .eq. k) go to 10
               t = a(l,k)
               a(l,k) = a(k,k)
               a(k,k) = t
   10       continue
c
c           compute multipliers
c
            t = -1.0d0/a(k,k)
            call dscal(n-k,t,a(k+1,k),1)
c
c           row elimination with column indexing
c
            do 30 j = kp1, n
               t = a(l,j)
               if (l .eq. k) go to 20
                  a(l,j) = a(k,j)
                  a(k,j) = t
   20          continue
               call daxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
   30       continue
         go to 50
   40    continue
            info = k
   50    continue
   60 continue
   70 continue
      ipvt(n) = n
      if (a(n,n) .eq. 0.0d0) info = n
      return
      end

c .................................................................

      subroutine dgesl(a,lda,n,ipvt,b,job)
      
      implicit none

      integer lda,n,ipvt(n),job
      double precision a(lda,n),b(n)
c
c     dgesl solves the double precision system
c     a * x = b  or  trans(a) * x = b
c     using the factors computed by dgeco or dgefa.
c
c     on entry
c
c        a       double precision(lda, n)
c                the output from dgeco or dgefa.
c
c        lda     integer
c                the leading dimension of the array  a .
c
c        n       integer
c                the order of the matrix  a .
c
c        ipvt    integer(n)
c                the pivot vector from dgeco or dgefa.
c
c        b       double precision(n)
c                the right hand side vector.
c
c        job     integer
c                = 0         to solve  a*x = b ,
c                = nonzero   to solve  trans(a)*x = b  where
c                            trans(a)  is the transpose.
c
c     on return
c
c        b       the solution vector  x .
c
c     error condition
c
c        a division by zero will occur if the input factor contains a
c        zero on the diagonal.  technically this indicates singularity
c        but it is often caused by improper arguments or improper
c        setting of lda .  it will not occur if the subroutines are
c        called correctly and if dgeco has set rcond .gt. 0.0
c        or dgefa has set info .eq. 0 .
c
c     to compute  inverse(a) * c  where  c  is a matrix
c     with  p  columns
c           call dgeco(a,lda,n,ipvt,rcond,z)
c           if (rcond is too small) go to ...
c           do 10 j = 1, p
c              call dgesl(a,lda,n,ipvt,c(1,j),0)
c        10 continue
c
c     linpack. this version dated 08/14/78 .
c     cleve moler, university of new mexico, argonne national lab.
c
c     subroutines and functions
c
c     blas daxpy,ddot
c
c     internal variables
c
      double precision ddot,t
      integer k,kb,l,nm1
c
      nm1 = n - 1
      if (job .ne. 0) go to 50
c
c        job = 0 , solve  a * x = b
c        first solve  l*y = b
c
         if (nm1 .lt. 1) go to 30
         do 20 k = 1, nm1
            l = ipvt(k)
            t = b(l)
            if (l .eq. k) go to 10
               b(l) = b(k)
               b(k) = t
   10       continue
            call daxpy(n-k,t,a(k+1,k),1,b(k+1),1)
   20    continue
   30    continue
c
c        now solve  u*x = y
c
         do 40 kb = 1, n
            k = n + 1 - kb
            b(k) = b(k)/a(k,k)
            t = -b(k)
            call daxpy(k-1,t,a(1,k),1,b(1),1)
   40    continue
      go to 100
   50 continue
c
c        job = nonzero, solve  trans(a) * x = b
c        first solve  trans(u)*y = b
c
         do 60 k = 1, n
            t = ddot(k-1,a(1,k),1,b(1),1)
            b(k) = (b(k) - t)/a(k,k)
   60    continue
c
c        now solve trans(l)*x = y
c
         if (nm1 .lt. 1) go to 90
         do 80 kb = 1, nm1
            k = n - kb
            b(k) = b(k) + ddot(n-k,a(k+1,k),1,b(k+1),1)
            l = ipvt(k)
            if (l .eq. k) go to 70
               t = b(l)
               b(l) = b(k)
               b(k) = t
   70       continue
   80    continue
   90    continue
  100 continue
      return
      end

c .................................................................

      subroutine daxpy(n,da,dx,incx,dy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      implicit none

      double precision dx(*),dy(*),da
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dy(iy) = dy(iy) + da*dx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dy(i) = dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        dy(i) = dy(i) + da*dx(i)
        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end

c .................................................................

      double precision function ddot(n,dx,incx,dy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dy(*),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      ddot = 0.0d0
      dtemp = 0.0d0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        dtemp = dtemp + dx(ix)*dy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      ddot = dtemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dtemp = dtemp + dx(i)*dy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) +
     *   dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + dx(i + 4)*dy(i + 4)
   50 continue
   60 ddot = dtemp
      return
      end

c .................................................................

      subroutine dscal(n,da,dx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      implicit none

      double precision da,dx(*)
      integer i,incx,m,mp1,n,nincx
c
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      nincx = n*incx
      do 10 i = 1,nincx,incx
        dx(i) = da*dx(i)
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        dx(i) = da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        dx(i) = da*dx(i)
        dx(i + 1) = da*dx(i + 1)
        dx(i + 2) = da*dx(i + 2)
        dx(i + 3) = da*dx(i + 3)
        dx(i + 4) = da*dx(i + 4)
   50 continue
      return
      end

c .................................................................

      integer function idamax(n,dx,incx)
c
c     finds the index of element having max. absolute value.
c     jack dongarra, linpack, 3/11/78.
c     modified 3/93 to return if incx .le. 0.
c     modified 12/3/93, array(1) declarations changed to array(*)
c
      double precision dx(*),dmax
      integer i,incx,ix,n
c
      idamax = 0
      if( n.lt.1 .or. incx.le.0 ) return
      idamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      dmax = dabs(dx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(dabs(dx(ix)).le.dmax) go to 5
         idamax = i
         dmax = dabs(dx(ix))
    5    ix = ix + incx
   10 continue
      return
c
c        code for increment equal to 1
c
   20 dmax = dabs(dx(1))
      do 30 i = 2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamax = i
         dmax = dabs(dx(i))
   30 continue
      return
      end
#endif
