
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
C $Header$

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

       SUBROUTINE HRG1( DTC )

C**********************************************************************
C
C  FUNCTION: To solve for the concentration of NO2, NO, O3, and O3P
C            algebraically.
C
C  PRECONDITIONS: For the CB05eh51 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, May 26, 2017
C
C   18 Jul 14 B.Hutzell: revised to use real(8) variables
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE


C..INCLUDES: None


C..ARGUMENTS:
      REAL( 8 ), INTENT( IN ) :: DTC                      ! Time step


C..PARAMETERS: None


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
!     CHARACTER( 16 ), SAVE  :: PNAME = 'HRG1'   ! Prgram Name


C..SCRATCH LOCAL VARIABLES:
      REAL( 8 ) :: O1D_S               ! sum of O1D loss frequencies
      REAL( 8 ) :: O3P_S               ! stoich coeff for O3P from O1D



      REAL( 8 ) :: R1_2                ! production term for NO from NO2
      REAL( 8 ) :: R2_1                ! production term for NO2 from NO
      REAL( 8 ) :: P1, P2, P3, P12     ! production terms for NO, NO2, O3, & O3P
      REAL( 8 ) :: L1, L2, L3, L12     ! loss terms for NO, NO2, O3, O3P
      REAL( 8 ) :: L1_INV, L2_INV,
     &             L3_INV, L12_INV     ! inverse of loss terms

      REAL( 8 ) :: T1, T2, T3, T4, T5  ! intermerdiate terms
      REAL( 8 ) :: F1, F2, F3          ! intermerdiate terms
      REAL( 8 ) :: A, B, C             ! coefficients for quadratic equation
      REAL( 8 ) :: Q, XX, S1, S2       ! intermerdiate terms

      REAL( 8 ) :: RK1, RK2, RK3       ! rate constants

      REAL( 8 ) :: PO3                 ! temp variable for O3

C**********************************************************************


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O1D Section
c    1) sum of the rate constants for all O1D loss reactions
c    2) get fractional yield of O3P from O1D loss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      O1D_S =                 RKI(  10 )                        ! O1D=O
     &      +                 RKI(  11 )                        ! O1D=0.2000D+01*OH
     &      +                 RKI(  39 )                        ! O1D=OH+HO2

      O3P_S =                 RKI(  10 )                        ! O1D=O

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =                 RKI(   1 )                        ! NO2=NO+O
     &     +                 RKI(   4 ) * YC( O            )   ! NO2+O=NO
     &     +    2.0000D-01 * RKI( 184 ) * YC( ISOP         )   ! NO2+ISOP=0.2000D+...
      R1_2  = R1_2 * DTC


      P1 =                 RXRAT(  15 )      ! NO3=NO
     &   +                 RXRAT(  17 )      ! NO2+NO3=NO+NO2
     &   +                 RXRAT(  25 )      ! HONO=NO+OH
     &   +                 RXRAT(  27 )      ! HONO+HONO=NO+NO2
     &   +    2.0000D+00 * RXRAT( 374 )      ! INO+INO=0.2000D+01*NO+I2
     &   +                 RXRAT( 398 )      ! INO=NO+I
      P1    = YC0( NO ) + P1 * DTC


      L1 =                 RKI(  23 ) * YC( NO2          )   ! NO+NO2=0.2000D+...
     &   +                 RKI(  24 ) * YC( OH           )   ! NO+OH=HONO
     &   +                 RKI(  31 ) * YC( HO2          )   ! NO+HO2=HNO3
     &   +                 RKI(  56 ) * YC( XO2N         )   ! NO+XO2N=0.5000D+...
     &   +    1.4000D-01 * RKI( 133 ) * YC( TO2          )   ! NO+TO2=0.8600D+...
     &   +    1.4000D-01 * RKI( 153 ) * YC( CAO2         )   ! NO+CAO2=0.8600D+...
     &   +    1.0000D-01 * RKI( 164 ) * YC( ISOPO2       )   ! NO+ISOPO2=...
     &   +                 RKI( 175 ) * YC( XO2T         )   ! NO+XO2T=NTRM
     &   +    1.3000D-01 * RKI( 248 ) * YC( NTRMO2       )   ! NO+NTRMO2=...
     &   +                 RKI( 390 ) * YC( I            )   ! NO+I=INO
      L1    = 1.0D0 + L1 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO2 Section
c    R2_1 = production of NO2 from NO ( rates of form k[NO][x] )
c            a)  NO+O3=NO2 not included
c            b)  NO+NO3=2NO2 ( 1/2 of NO2 formation rate included )
c            c)  NO3+NO2=NO+NO2 is not included for NO2
c    P2 =  remaining NO2 production terms 
c            a)  NO+O3=NO2 not included
c            b)  NO+NO3=2NO2 (1/2 of NO2 formation rate included )
c    L2 = loss of NO2 (except rxns producing NO2 - they are in R1_2)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R2_1 =                 RKI(   6 ) * YC( O            )   ! NO+O=NO2
     &     +                 RKI(  16 ) * YC( NO3          )   ! NO+NO3=0.2000D+...
     &     +    2.0000D+00 * RKI(  22 ) * YC( NO           )   ! NO+NO=0.2000D+01*NO2
     &     +                 RKI(  30 ) * YC( HO2          )   ! NO+HO2=NO2+OH
     &     +                 RKI(  55 ) * YC( XO2          )   ! NO+XO2=NO2
     &     +                 RKI(  67 ) * YC( IEPXO2       )   ! NO+IEPXO2=NO2+...
     &     +                 RKI(  71 ) * YC( MEO2         )   ! NO+MEO2=NO2+HO2+FORM
     &     +                 RKI(  84 ) * YC( HCO3         )   ! NO+HCO3=NO2+FACD+HO2
     &     +                 RKI(  91 ) * YC( C2O3         )   ! NO+C2O3=NO2+MEO2
     &     +                 RKI( 106 ) * YC( CXO3         )   ! NO+CXO3=NO2+ALD2+...
     &     +    8.6000D-01 * RKI( 133 ) * YC( TO2          )   ! NO+TO2=0.8600D+...
     &     +                 RKI( 143 ) * YC( CRN2         )   ! NO+CRN2=NO2+CRNO
     &     +    8.6000D-01 * RKI( 153 ) * YC( CAO2         )   ! NO+CAO2=0.8600D+...
     &     +                 RKI( 155 ) * YC( OPO3         )   ! NO+OPO3=NO2+XO2+...
     &     +    9.0000D-01 * RKI( 164 ) * YC( ISOPO2       )   ! NO+ISOPO2=...
     &     +                 RKI( 189 ) * YC( CLO          )   ! NO+CLO=NO2+CL
     &     +                 RKI( 224 ) * YC( MACO3        )   ! NO+MACO3=NO2+...
     &     +                 RKI( 235 ) * YC( NALKO2       )   ! NO+NALKO2=...
     &     +                 RKI( 238 ) * YC( NOHO2        )   ! NO+NOHO2=0.1220D+...
     &     +                 RKI( 241 ) * YC( NCNO2        )   ! NO+NCNO2=0.1530D+...
     &     +                 RKI( 244 ) * YC( NCNOHO2      )   ! NO+NCNOHO2=...
     &     +    8.7000D-01 * RKI( 248 ) * YC( NTRMO2       )   ! NO+NTRMO2=...
     &     +                 RKI( 251 ) * YC( NTRIO2       )   ! NO+NTRIO2=...
     &     +                 RKI( 353 ) * YC( IO           )   ! NO+IO=NO2+I
     &     +                 RKI( 376 ) * YC( OIO          )   ! NO+OIO=NO2+IO
     &     +                 RKI( 409 ) * YC( BRO          )   ! NO+BRO=NO2+BR
      R2_1  = R2_1 * DTC


      P2 =                 RXRAT(  14 )      ! NO3=NO2+O
     &   +                 RXRAT(  16 )      ! NO+NO3=0.2000D+01*NO2
     &   +                 RXRAT(  21 )      ! N2O5=NO2+NO3
     &   +                 RXRAT(  26 )      ! OH+HONO=NO2
     &   +                 RXRAT(  27 )      ! HONO+HONO=NO2+NO
     &   +                 RXRAT(  33 )      ! PNA=NO2+HO2
     &   +                 RXRAT(  34 )      ! OH+PNA=NO2
     &   +                 RXRAT(  47 )      ! NO3+O=NO2
     &   +                 RXRAT(  48 )      ! NO3+OH=NO2+HO2
     &   +                 RXRAT(  50 )      ! NO3+O3=NO2
     &   +    2.0000D+00 * RXRAT(  51 )      ! NO3+NO3=0.2000D+01*NO2
     &   +    6.1000D-01 * RXRAT(  52 )      ! PNA=0.6100D+00*NO2+...
     &   +                 RXRAT(  53 )      ! HNO3=NO2+OH
     &   +                 RXRAT(  54 )      ! N2O5=NO2+NO3
     &   +                 RXRAT(  93 )      ! PAN=NO2+C2O3
     &   +    6.0000D-01 * RXRAT(  94 )      ! PAN=0.6000D+00*NO2+...
     &   +                 RXRAT( 108 )      ! PANX=NO2+CXO3
     &   +    6.0000D-01 * RXRAT( 109 )      ! PANX=0.6000D+00*NO2+...
     &   +                 RXRAT( 110 )      ! PANX+OH=NO2+ALD2
     &   +                 RXRAT( 123 )      ! NO3+OLE=NO2+FORM+...
     &   +                 RXRAT( 127 )      ! NO3+ETH=NO2+XO2+...
     &   +                 RXRAT( 131 )      ! IOLE+NO3=NO2+0.6400D+...
     &   +                 RXRAT( 157 )      ! OPAN=NO2+OPO3
     &   +    2.0000D-01 * RXRAT( 168 )      ! NO3+ISOP=0.2000D+...
     &   +    4.7000D-01 * RXRAT( 180 )      ! TERP+NO3=0.4700D+...
     &   +                 RXRAT( 210 )      ! CLNO2=NO2+CL
     &   +                 RXRAT( 226 )      ! MAPAN=NO2+MACO3
     &   +    6.0000D-01 * RXRAT( 227 )      ! MAPAN=0.6000D+00*NO2+...
     &   +                 RXRAT( 228 )      ! MAPAN+OH=NO2+CO+ALD2
     &   +    3.0000D-01 * RXRAT( 235 )      ! NO+NALKO2=0.1300D+...
     &   +    2.2000D-01 * RXRAT( 238 )      ! NO+NOHO2=0.1220D+...
     &   +    5.3000D-01 * RXRAT( 241 )      ! NO+NCNO2=0.1530D+...
     &   +    2.4000D-01 * RXRAT( 244 )      ! NO+NCNOHO2=0.1240D+...
     &   +    4.0000D-01 * RXRAT( 251 )      ! NO+NTRIO2=0.1400D+...
     &   +                 RXRAT( 253 )      ! NTRM=NO2+HO2+ALD2+IOLE
     &   +                 RXRAT( 254 )      ! NTRI=NO2+HO2+0.3300D+...
     &   +                 RXRAT( 255 )      ! NTRALK=NO2+HO2+...
     &   +                 RXRAT( 256 )      ! NTROH=NO2+HO2+0.3300D+...
     &   +                 RXRAT( 257 )      ! NTRCN=NO2+HO2+0.3300D+...
     &   +                 RXRAT( 258 )      ! NTRCNOH=NO2+HO2+...
     &   +                 RXRAT( 259 )      ! NTRPX=NO2+HO2+ROOH
     &   +                 RXRAT( 367 )      ! I+NO3=NO2+IO
     &   +                 RXRAT( 372 )      ! INO2=NO2+I
     &   +                 RXRAT( 373 )      ! IONO2=NO2+IO
     &   +    2.0000D+00 * RXRAT( 375 )      ! INO2+INO2=0.2000D+...
     &   +                 RXRAT( 385 )      ! IO+NO3=NO2+OIO
     &   +                 RXRAT( 399 )      ! INO2=NO2+I
     &   +                 RXRAT( 413 )      ! BR+NO3=NO2+BRO
     &   +                 RXRAT( 435 )      ! BRONO2=NO2+BRO
     &   +                 RXRAT( 437 )      ! BRNO2=NO2+BR
      P2 = YC0( NO2 ) + P2 * DTC


      L2 =                 RKI(   5 ) * YC( O            )   ! NO2+O=NO3
     &   +                 RKI(   7 ) * YC( O3           )   ! NO2+O3=NO3
     &   +                 RKI(  18 ) * YC( NO3          )   ! NO2+NO3=N2O5
     &   +                 RKI(  23 ) * YC( NO           )   ! NO2+NO=0.2000D+...
     &   +                 RKI(  28 ) * YC( OH           )   ! NO2+OH=HNO3
     &   +                 RKI(  32 ) * YC( HO2          )   ! NO2+HO2=PNA
     &   +                 RKI(  92 ) * YC( C2O3         )   ! NO2+C2O3=PAN
     &   +                 RKI( 107 ) * YC( CXO3         )   ! NO2+CXO3=PANX
     &   +                 RKI( 119 ) * YC( ROR          )   ! NO2+ROR=NTRALK
     &   +                 RKI( 137 ) * YC( CRO          )   ! NO2+CRO=CRON
     &   +                 RKI( 141 ) * YC( CRNO         )   ! NO2+CRNO=0.2000D+...
     &   +                 RKI( 156 ) * YC( OPO3         )   ! NO2+OPO3=OPAN
     &   +    8.0000D-01 * RKI( 184 ) * YC( ISOP         )   ! NO2+ISOP=0.2000D+...
     &   +                 RKI( 209 ) * YC( CL           )   ! NO2+CL=CLNO2
     &   +                 RKI( 225 ) * YC( MACO3        )   ! NO2+MACO3=MAPAN
     &   +                 RKI( 274 )                        ! NO2=0.5000D+...
     &   +                 RKI( 388 ) * YC( I            )   ! NO2+I=INO2
     &   +                 RKI( 389 ) * YC( IO           )   ! NO2+IO=IONO2
     &   +                 RKI( 426 ) * YC( BRO          )   ! NO2+BRO=BRONO2
     &   +                 RKI( 427 ) * YC( BR           )   ! NO2+BR=BRNO2
      L2     = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 =    1.5000D-01 * RXRAT(  95 )      ! C2O3+HO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT( 111 )      ! CXO3+HO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT( 229 )      ! MACO3+HO2=0.1500D+...
      P3 = YC0( O3 ) + P3 * DTC


      L3 =                 RKI(   7 ) * YC( NO2          )   ! O3+NO2=NO3
     &   +                 RKI(   8 )                        ! O3=O
     &   +                 RKI(   9 )                        ! O3=O1D
     &   +                 RKI(  12 ) * YC( OH           )   ! O3+OH=HO2
     &   +                 RKI(  13 ) * YC( HO2          )   ! O3+HO2=OH
     &   +                 RKI(  50 ) * YC( NO3          )   ! O3+NO3=NO2
     &   +                 RKI( 122 ) * YC( OLE          )   ! O3+OLE=0.1800D+...
     &   +                 RKI( 126 ) * YC( ETH          )   ! O3+ETH=FORM+...
     &   +                 RKI( 130 ) * YC( IOLE         )   ! O3+IOLE=0.6500D+...
     &   +                 RKI( 142 ) * YC( CRNO         )   ! O3+CRNO=CRN2
     &   +                 RKI( 149 ) * YC( OPEN         )   ! O3+OPEN=0.3000D-...
     &   +                 RKI( 167 ) * YC( ISOP         )   ! O3+ISOP=0.6500D+...
     &   +                 RKI( 170 ) * YC( ISPD         )   ! O3+ISPD=0.1140D+...
     &   +                 RKI( 179 ) * YC( TERP         )   ! O3+TERP=0.5700D+...
     &   +                 RKI( 187 ) * YC( CL           )   ! O3+CL=CLO
     &   +                 RKI( 350 ) * YC( I            )   ! O3+I=IO
     &   +                 RKI( 351 ) * YC( IO           )   ! O3+IO=OIO
     &   +                 RKI( 403 ) * YC( BR           )   ! O3+BR=BRO
      L3    = 1.0D0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section 
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P12 =                 RXRAT(   8 )      ! O3=O
     &    +   O3P_S *       RXRAT(   9 )      ! O3=O1D
     &    +                 RXRAT(  14 )      ! NO3=O+NO2
     &    +                 RXRAT(  42 )      ! OH+OH=O
     &    +    5.0000D-01 * RXRAT( 130 )      ! IOLE+O3=0.5000D+00*O+...
     &    +                 RXRAT( 396 )      ! IO=O+I
     &    +                 RXRAT( 433 )      ! BRO=O+BR
      P12 = YC0( O ) + P12 * DTC


      L12 =                 RKI(   2 )                        ! O=O3
     &    +                 RKI(   4 ) * YC( NO2          )   ! O+NO2=NO
     &    +                 RKI(   5 ) * YC( NO2          )   ! O+NO2=NO3
     &    +                 RKI(   6 ) * YC( NO           )   ! O+NO=NO2
     &    +                 RKI(  41 ) * YC( OH           )   ! O+OH=HO2
     &    +                 RKI(  45 ) * YC( HO2          )   ! O+HO2=OH
     &    +                 RKI(  46 ) * YC( H2O2         )   ! O+H2O2=OH+HO2
     &    +                 RKI(  47 ) * YC( NO3          )   ! O+NO3=NO2
     &    +                 RKI(  80 ) * YC( FORM         )   ! O+FORM=OH+HO2+CO
     &    +                 RKI(  87 ) * YC( ALD2         )   ! O+ALD2=C2O3+OH
     &    +                 RKI( 102 ) * YC( ALDX         )   ! O+ALDX=CXO3+OH
     &    +                 RKI( 120 ) * YC( OLE          )   ! O+OLE=0.2000D+00*ALD2+...
     &    +                 RKI( 124 ) * YC( ETH          )   ! O+ETH=FORM+0.1700D+...
     &    +                 RKI( 128 ) * YC( IOLE         )   ! O+IOLE=0.1240D+...
     &    +                 RKI( 162 ) * YC( ISOP         )   ! O+ISOP=0.7500D+...
     &    +                 RKI( 173 ) * YC( TERP         )   ! O+TERP=0.1500D+...
     &    +                 RKI( 359 ) * YC( I2           )   ! O+I2=IO+I
     &    +                 RKI( 360 ) * YC( IO           )   ! O+IO=I
      L12   = 1.0D0 + L12 * DTC

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Solution section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..compute reciprocal of loss terms
      L1_INV  = 1.0D0 / L1
      L2_INV  = 1.0D0 / L2
      L3_INV  = 1.0D0 / L3
      L12_INV = 1.0D0 / L12

c..compute specific k*delta t terms
      RK1 = RKI(   1 ) * DTC            ! J1    (NO2+hv=NO+O3P)
      RK2 = RKI(   2 ) * DTC            ! J2    (O3P+O2=O3)
      RK3 = RKI(   3 ) * DTC            ! k1_3  (NO+O3=NO2)

c..compute terms that are used to calulate a,b & c
      T1 = RK1  * L2_INV                ! J1   / ( 1.0 + Lno2 * dt )
      T2 = R1_2 * L2_INV                ! r1,2 / ( 1.0 + Lno2 * dt)
      T3 = R2_1 * L1_INV                ! r2,1 / ( 1.0 + Lno  * dt)
      T4 = RK2  * L12_INV               ! J2   / ( 1.0 + Lo3p * dt )
      T5 = T3   * P1 - T2 * P2          ! T3 * Pno - T2 * Pno2

      F1 = 1.0D0 + T2 + T3                ! factor in calculating a & b
      F2 = T1 * T4                      ! factor in calculating a & b
      F3 = L3 * L1 + RK3 * P1           ! (1 + Lo3 * dt) (1 + lno * dt )
                                        ! + k1,3 * dt * Pno

      PO3 = P3 + P12 * T4

      A = RK3 * ( F1  - F2 )

      B = F1 * F3 +  RK3 * ( F2 * ( P2 - P1 ) + PO3 +  T5 )

      C = RK3 * P1 * ( PO3 + P2 * F2 ) + F3 * T5

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B - 4.0D0 * A * C ) )

      XX = MAX( Q / A , C / Q  )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Species solutions
c       [NO]   = ( P1 + x ) / ( 1 + L1 )
c       [NO2]  = ( P2 - x ) / ( 1 + L2 )
c       [O3 ]  = ( P3 + Ko3p->O3 ) / (1 + K1,3 * [NO] + L3 )
c       [O3P]  = ( P12 + J1 * [NO2] ) / ( 1 + L12 )
c       [O1D2] = ( Ko3->o1d * [O3] ) / ( O1D_DNM )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      YCP( NO ) = MAX( 0.0D0, ( P1 + XX ) * L1_INV )

      YCP( NO2 ) = MAX( 0.0D0, ( P2 - XX ) * L2_INV )

      S1 = P12 + RK1 * YCP( NO2 )

      S2 = T4 * S1

      YCP( O3 ) = ( P3 + S2 ) / ( L3 + RK3 * YCP( NO ) )

      YCP( O ) = S1 * L12_INV

      YCP( O1D ) = RKI( 9 ) * YCP( O3 ) / O1D_S

      RETURN

      END


