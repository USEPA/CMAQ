
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
C  PRECONDITIONS: For the CB05TUCL family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, Sept. 10, 2014
C
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE


C..INCLUDES: None


C..ARGUMENTS:
      REAL( 8 ), INTENT( IN ) :: DTC                      ! Time step


C..PARAMETERS: None


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
      CHARACTER( 16 ), SAVE  :: PNAME = 'HRG1'   ! Prgram Name


C..SCRATCH LOCAL VARIABLES:
      REAL O1D_S               ! sum of O1D loss frequencies
      REAL O3P_S               ! stoich coeff for O3P from O1D

      REAL EXN_S               ! sum of NO2EX loss frequencies
      REAL NO2_S               ! stoich coeff for NO2 from NO2EX


      REAL R1_2                ! production term for NO from NO2
      REAL R2_1                ! production term for NO2 from NO
      REAL P1, P2, P3, P12     ! production terms for NO, NO2, O3, & O3P
      REAL L1, L2, L3, L12     ! loss terms for NO, NO2, O3, O3P
      REAL L1_INV, L2_INV,
     &     L3_INV, L12_INV     ! inverse of loss terms

      REAL T1, T2, T3, T4, T5  ! intermerdiate terms
      REAL F1, F2, F3          ! intermerdiate terms
      REAL A, B, C             ! coefficients for quadratic equation
      REAL Q, XX, S1, S2       ! intermerdiate terms

      REAL RK1, RK2, RK3       ! rate constants

      REAL PO3                 ! temp variable for O3

C**********************************************************************


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O1D Section
c    1) sum of the rate constants for all O1D loss reactions
c    2) get fractional yield of O3P from O1D loss
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      O1D_S =           RKI( NCELL,  10 )                    ! O1D=O
     &      +           RKI( NCELL,  11 )                    ! O1D=2*OH
     &      +           RKI( NCELL,  38 )                    ! O1D=OH+HO2

      O3P_S =           RKI( NCELL,  10 )                    ! O1D=O

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =           RKI( NCELL,   1 )                    ! NO2=NO+O
     &     +           RKI( NCELL,   4 ) * YC( NCELL, O        )   ! NO2+O=NO
     &     +   0.200 * RKI( NCELL, 172 ) * YC( NCELL, ISOP     )   ! NO2+ISOP=0.2*NO+...
      R1_2  = R1_2 * DTC


      P1 =           RXRAT( NCELL,  15 )      ! NO3=NO
     &   +           RXRAT( NCELL,  17 )      ! NO2+NO3=NO+NO2
     &   +           RXRAT( NCELL,  25 )      ! HONO=NO+OH
     &   +           RXRAT( NCELL,  27 )      ! HONO+HONO=NO+NO2
      P1    = YC0( NCELL, NO ) + P1 * DTC


      L1 =           RKI( NCELL,  23 ) * YC( NCELL, NO2      )   ! NO+NO2=2*HONO
     &   +           RKI( NCELL,  24 ) * YC( NCELL, OH       )   ! NO+OH=HONO
     &   +           RKI( NCELL,  55 ) * YC( NCELL, XO2N     )   ! NO+XO2N=NTR
     &   +   0.140 * RKI( NCELL, 129 ) * YC( NCELL, TO2      )   ! NO+TO2=0.86*NO2+...
     &   +   0.140 * RKI( NCELL, 149 ) * YC( NCELL, CAO2     )   ! NO+CAO2=0.86*NO2+...
      L1    = 1.0 + L1 * DTC


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
      R2_1 =           RKI( NCELL,   6 ) * YC( NCELL, O        )   ! NO+O=NO2
     &     +           RKI( NCELL,  16 ) * YC( NCELL, NO3      )   ! NO+NO3=2*NO2
     &     +   2.000 * RKI( NCELL,  22 ) * YC( NCELL, NO       )   ! NO+NO=2*NO2
     &     +           RKI( NCELL,  30 ) * YC( NCELL, HO2      )   ! NO+HO2=NO2+OH
     &     +           RKI( NCELL,  54 ) * YC( NCELL, XO2      )   ! NO+XO2=NO2
     &     +           RKI( NCELL,  67 ) * YC( NCELL, MEO2     )   ! NO+MEO2=NO2+HO2+FORM
     &     +           RKI( NCELL,  80 ) * YC( NCELL, HCO3     )   ! NO+HCO3=NO2+FACD+HO2
     &     +           RKI( NCELL,  87 ) * YC( NCELL, C2O3     )   ! NO+C2O3=NO2+MEO2
     &     +           RKI( NCELL, 102 ) * YC( NCELL, CXO3     )   ! NO+CXO3=NO2+ALD2+...
     &     +   0.860 * RKI( NCELL, 129 ) * YC( NCELL, TO2      )   ! NO+TO2=0.86*NO2+...
     &     +           RKI( NCELL, 139 ) * YC( NCELL, CRN2     )   ! NO+CRN2=NO2+CRNO
     &     +   0.860 * RKI( NCELL, 149 ) * YC( NCELL, CAO2     )   ! NO+CAO2=0.86*NO2+...
     &     +           RKI( NCELL, 151 ) * YC( NCELL, OPO3     )   ! NO+OPO3=NO2+XO2+...
     &     +           RKI( NCELL, 177 ) * YC( NCELL, CLO      )   ! NO+CLO=NO2+CL
      R2_1  = R2_1 * DTC


      P2 =           RXRAT( NCELL,  14 )      ! NO3=NO2+O
     &   +           RXRAT( NCELL,  16 )      ! NO+NO3=2*NO2
     &   +           RXRAT( NCELL,  21 )      ! N2O5=NO2+NO3
     &   +           RXRAT( NCELL,  26 )      ! OH+HONO=NO2
     &   +           RXRAT( NCELL,  27 )      ! HONO+HONO=NO2+NO
     &   +           RXRAT( NCELL,  32 )      ! PNA=NO2+HO2
     &   +           RXRAT( NCELL,  33 )      ! OH+PNA=NO2
     &   +           RXRAT( NCELL,  46 )      ! NO3+O=NO2
     &   +           RXRAT( NCELL,  47 )      ! NO3+OH=NO2+HO2
     &   +           RXRAT( NCELL,  49 )      ! NO3+O3=NO2
     &   +   2.000 * RXRAT( NCELL,  50 )      ! NO3+NO3=2*NO2
     &   +   0.610 * RXRAT( NCELL,  51 )      ! PNA=0.61*NO2+0.61*HO2+...
     &   +           RXRAT( NCELL,  52 )      ! HNO3=NO2+OH
     &   +           RXRAT( NCELL,  53 )      ! N2O5=NO2+NO3
     &   +           RXRAT( NCELL,  62 )      ! NTR=NO2+HO2+0.33*FORM+...
     &   +           RXRAT( NCELL,  89 )      ! PAN=NO2+C2O3
     &   +           RXRAT( NCELL,  90 )      ! PAN=NO2+C2O3
     &   +           RXRAT( NCELL, 104 )      ! PANX=NO2+CXO3
     &   +           RXRAT( NCELL, 105 )      ! PANX=NO2+CXO3
     &   +           RXRAT( NCELL, 106 )      ! PANX+OH=NO2+ALD2
     &   +           RXRAT( NCELL, 119 )      ! NO3+OLE=NO2+FORM+...
     &   +           RXRAT( NCELL, 123 )      ! NO3+ETH=NO2+XO2+2*FORM
     &   +           RXRAT( NCELL, 127 )      ! IOLE+NO3=NO2+...
     &   +           RXRAT( NCELL, 153 )      ! OPAN=NO2+OPO3
     &   +   0.200 * RXRAT( NCELL, 160 )      ! NO3+ISOP=0.2*NO2+...
     &   +   0.470 * RXRAT( NCELL, 168 )      ! TERP+NO3=0.47*NO2+...
     &   +           RXRAT( NCELL, 197 )      ! CLNO2=NO2+CL
      P2 = YC0( NCELL, NO2 ) + P2 * DTC


      L2 =           RKI( NCELL,   5 ) * YC( NCELL, O        )   ! NO2+O=NO3
     &   +           RKI( NCELL,   7 ) * YC( NCELL, O3       )   ! NO2+O3=NO3
     &   +           RKI( NCELL,  18 ) * YC( NCELL, NO3      )   ! NO2+NO3=N2O5
     &   +           RKI( NCELL,  23 ) * YC( NCELL, NO       )   ! NO2+NO=2*HONO
     &   +           RKI( NCELL,  28 ) * YC( NCELL, OH       )   ! NO2+OH=HNO3
     &   +           RKI( NCELL,  31 ) * YC( NCELL, HO2      )   ! NO2+HO2=PNA
     &   +           RKI( NCELL,  88 ) * YC( NCELL, C2O3     )   ! NO2+C2O3=PAN
     &   +           RKI( NCELL, 103 ) * YC( NCELL, CXO3     )   ! NO2+CXO3=PANX
     &   +           RKI( NCELL, 115 ) * YC( NCELL, ROR      )   ! NO2+ROR=NTR
     &   +           RKI( NCELL, 133 ) * YC( NCELL, CRO      )   ! NO2+CRO=CRON
     &   +           RKI( NCELL, 137 ) * YC( NCELL, CRNO     )   ! NO2+CRNO=2*NTR
     &   +           RKI( NCELL, 152 ) * YC( NCELL, OPO3     )   ! NO2+OPO3=OPAN
     &   +   0.800 * RKI( NCELL, 172 ) * YC( NCELL, ISOP     )   ! NO2+ISOP=0.2*NO+...
     &   +           RKI( NCELL, 196 ) * YC( NCELL, CL       )   ! NO2+CL=CLNO2
     &   +           RKI( NCELL, 215 )                    ! NO2=0.5*HONO+...
      L2     = 1.0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 =   0.200 * RXRAT( NCELL,  91 )      ! C2O3+HO2=0.2*O3+...
     &   +   0.200 * RXRAT( NCELL, 107 )      ! CXO3+HO2=0.2*O3+...
      P3 = YC0( NCELL, O3 ) + P3 * DTC


      L3 =           RKI( NCELL,   7 ) * YC( NCELL, NO2      )   ! O3+NO2=NO3
     &   +           RKI( NCELL,   8 )                    ! O3=O
     &   +           RKI( NCELL,   9 )                    ! O3=O1D
     &   +           RKI( NCELL,  12 ) * YC( NCELL, OH       )   ! O3+OH=HO2
     &   +           RKI( NCELL,  13 ) * YC( NCELL, HO2      )   ! O3+HO2=OH
     &   +           RKI( NCELL,  49 ) * YC( NCELL, NO3      )   ! O3+NO3=NO2
     &   +           RKI( NCELL, 118 ) * YC( NCELL, OLE      )   ! O3+OLE=0.18*ALD2+...
     &   +           RKI( NCELL, 122 ) * YC( NCELL, ETH      )   ! O3+ETH=FORM+...
     &   +           RKI( NCELL, 126 ) * YC( NCELL, IOLE     )   ! O3+IOLE=...
     &   +           RKI( NCELL, 138 ) * YC( NCELL, CRNO     )   ! O3+CRNO=CRN2
     &   +           RKI( NCELL, 145 ) * YC( NCELL, OPEN     )   ! O3+OPEN=...
     &   +           RKI( NCELL, 159 ) * YC( NCELL, ISOP     )   ! O3+ISOP=...
     &   +           RKI( NCELL, 162 ) * YC( NCELL, ISPD     )   ! O3+ISPD=...
     &   +           RKI( NCELL, 167 ) * YC( NCELL, TERP     )   ! O3+TERP=0.57*OH+...
     &   +           RKI( NCELL, 175 ) * YC( NCELL, CL       )   ! O3+CL=CLO
      L3    = 1.0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section 
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P12 =           RXRAT( NCELL,   8 )      ! O3=O
     &    +   O3P_S * RXRAT( NCELL,   9 )      ! O3=O1D
     &    +           RXRAT( NCELL,  14 )      ! NO3=O+NO2
     &    +           RXRAT( NCELL,  41 )      ! OH+OH=O
     &    +   0.500 * RXRAT( NCELL, 126 )      ! IOLE+O3=0.5*O+...
      P12 = YC0( NCELL, O ) + P12 * DTC


      L12 =           RKI( NCELL,   2 )                    ! O=O3
     &    +           RKI( NCELL,   4 ) * YC( NCELL, NO2      )   ! O+NO2=NO
     &    +           RKI( NCELL,   5 ) * YC( NCELL, NO2      )   ! O+NO2=NO3
     &    +           RKI( NCELL,   6 ) * YC( NCELL, NO       )   ! O+NO=NO2
     &    +           RKI( NCELL,  40 ) * YC( NCELL, OH       )   ! O+OH=HO2
     &    +           RKI( NCELL,  44 ) * YC( NCELL, HO2      )   ! O+HO2=OH
     &    +           RKI( NCELL,  45 ) * YC( NCELL, H2O2     )   ! O+H2O2=OH+HO2
     &    +           RKI( NCELL,  46 ) * YC( NCELL, NO3      )   ! O+NO3=NO2
     &    +           RKI( NCELL,  76 ) * YC( NCELL, FORM     )   ! O+FORM=OH+HO2+CO
     &    +           RKI( NCELL,  83 ) * YC( NCELL, ALD2     )   ! O+ALD2=C2O3+OH
     &    +           RKI( NCELL,  98 ) * YC( NCELL, ALDX     )   ! O+ALDX=CXO3+OH
     &    +           RKI( NCELL, 116 ) * YC( NCELL, OLE      )   ! O+OLE=0.2*ALD2+...
     &    +           RKI( NCELL, 120 ) * YC( NCELL, ETH      )   ! O+ETH=FORM+1.7*HO2+CO+...
     &    +           RKI( NCELL, 124 ) * YC( NCELL, IOLE     )   ! O+IOLE=1.24*ALD2+...
     &    +           RKI( NCELL, 157 ) * YC( NCELL, ISOP     )   ! O+ISOP=0.75*ISPD+...
     &    +           RKI( NCELL, 165 ) * YC( NCELL, TERP     )   ! O+TERP=0.15*ALDX+...
      L12   = 1.0 + L12 * DTC

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Solution section
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c..compute reciprocal of loss terms
      L1_INV  = 1.0 / L1
      L2_INV  = 1.0 / L2
      L3_INV  = 1.0 / L3
      L12_INV = 1.0 / L12

c..compute specific k*delta t terms
      RK1 = RKI( NCELL,   1 ) * DTC            ! J1    (NO2+hv=NO+O3P)
      RK2 = RKI( NCELL,   2 ) * DTC            ! J2    (O3P+O2=O3)
      RK3 = RKI( NCELL,   3 ) * DTC            ! k1_3  (NO+O3=NO2)

c..compute terms that are used to calulate a,b & c
      T1 = RK1  * L2_INV                ! J1   / ( 1.0 + Lno2 * dt )
      T2 = R1_2 * L2_INV                ! r1,2 / ( 1.0 + Lno2 * dt)
      T3 = R2_1 * L1_INV                ! r2,1 / ( 1.0 + Lno  * dt)
      T4 = RK2  * L12_INV               ! J2   / ( 1.0 + Lo3p * dt )
      T5 = T3   * P1 - T2 * P2          ! T3 * Pno - T2 * Pno2

      F1 = 1.0 + T2 + T3                ! factor in calculating a & b
      F2 = T1 * T4                      ! factor in calculating a & b
      F3 = L3 * L1 + RK3 * P1           ! (1 + Lo3 * dt) (1 + lno * dt )
                                        ! + k1,3 * dt * Pno

      PO3 = P3 + P12 * T4

      A = RK3 * ( F1  - F2 )

      B = F1 * F3 +  RK3 * ( F2 * ( P2 - P1 ) + PO3 +  T5 )

      C = RK3 * P1 * ( PO3 + P2 * F2 ) + F3 * T5

      Q = -0.5 * ( B + SIGN( 1.0, B ) * SQRT( B * B - 4.0 * A * C ) )

      XX = MAX( Q / A , C / Q  )


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Species solutions
c       [NO]   = ( P1 + x ) / ( 1 + L1 )
c       [NO2]  = ( P2 - x ) / ( 1 + L2 )
c       [O3 ]  = ( P3 + Ko3p->O3 ) / (1 + K1,3 * [NO] + L3 )
c       [O3P]  = ( P12 + J1 * [NO2] ) / ( 1 + L12 )
c       [O1D2] = ( Ko3->o1d * [O3] ) / ( O1D_DNM )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      YCP( NCELL, NO ) = MAX( 0.0, ( P1 + XX ) * L1_INV )

      YCP( NCELL, NO2 ) = MAX( 0.0, ( P2 - XX ) * L2_INV )

      S1 = P12 + RK1 * YCP( NCELL, NO2 )

      S2 = T4 * S1

      YCP( NCELL, O3 ) = ( P3 + S2 ) / ( L3 + RK3 * YCP( NCELL, NO ) )

      YCP( NCELL, O ) = S1 * L12_INV

      YCP( NCELL, O1D ) = RKI( NCELL, 9 ) * YCP( NCELL, O3 ) / O1D_S

      RETURN

      END


