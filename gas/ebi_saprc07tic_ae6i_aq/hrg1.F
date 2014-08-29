
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
C  PRECONDITIONS: For the SAPRC07TIC family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, July 18, 2014
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
      CHARACTER( 16 ), SAVE  :: PNAME = 'HRG1'   ! Prgram Name


C..SCRATCH LOCAL VARIABLES:
      REAL( 8 ) :: O1D_S               ! sum of O1D loss frequencies
      REAL( 8 ) :: O3P_S               ! stoich coeff for O3P from O1D

      REAL( 8 ) :: EXN_S               ! sum of NO2EX loss frequencies
      REAL( 8 ) :: NO2_S               ! stoich coeff for NO2 from NO2EX


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
      O1D_S =                 RKI( NCELL,  20 )                            ! O1D=0.2000D+01*OH
     &      +                 RKI( NCELL,  21 )                            ! O1D=O3P

      O3P_S =                 RKI( NCELL,  21 )                            ! O1D=O3P

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =                 RKI( NCELL,   1 )                            ! NO2=NO+O3P
     &     +                 RKI( NCELL,   5 ) * YC( NCELL, O3P              )   ! NO2+O3P=NO
      R1_2  = R1_2 * DTC


      P1 =                 RXRAT( NCELL,  15 )      ! NO2+NO3=NO+NO2
     &   +                 RXRAT( NCELL,  16 )      ! NO3=NO
     &   +                 RXRAT( NCELL,  23 )      ! HONO=NO+OH
     &   +                 RXRAT( NCELL, 555 )      ! CLNO=NO+CL
      P1    = YC0( NCELL, NO ) + P1 * DTC


      L1 =                 RKI( NCELL,  22 ) * YC( NCELL, OH               )   ! NO+OH=HONO
     &   +                 RKI( NCELL,  61 ) * YC( NCELL, RO2XC            )   ! NO+RO2XC=
     &   +                 RKI( NCELL, 554 ) * YC( NCELL, CL               )   ! NO+CL=CLNO
     &   +    1.1700D-01 * RKI( NCELL, 673 ) * YC( NCELL, ISOPO2           )   ! NO+ISOPO2=...
     &   +    6.6000D-02 * RKI( NCELL, 727 ) * YC( NCELL, NIT1OHOO         )   ! NO+NIT1OHOO=...
     &   +    1.1000D-01 * RKI( NCELL, 738 ) * YC( NCELL, MVKOO            )   ! NO+MVKOO=0.6250D+...
     &   +    1.5000D-01 * RKI( NCELL, 743 ) * YC( NCELL, MACROO           )   ! NO+MACROO=...
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
      R2_1 =                 RKI( NCELL,   4 ) * YC( NCELL, O3P              )   ! NO+O3P=NO2
     &     +                 RKI( NCELL,   9 ) * YC( NCELL, NO3              )   ! NO+NO3=0.2000D+...
     &     +    2.0000D+00 * RKI( NCELL,  10 ) * YC( NCELL, NO               )   ! NO+NO=0.2000D+01*NO2
     &     +                 RKI( NCELL,  31 ) * YC( NCELL, HO2              )   ! NO+HO2=NO2+OH
     &     +                 RKI( NCELL,  50 ) * YC( NCELL, MEO2             )   ! NO+MEO2=NO2+HCHO+HO2
     &     +                 RKI( NCELL,  56 ) * YC( NCELL, RO2C             )   ! NO+RO2C=NO2
     &     +                 RKI( NCELL,  70 ) * YC( NCELL, MECO3            )   ! NO+MECO3=NO2+CO2+...
     &     +                 RKI( NCELL,  80 ) * YC( NCELL, RCO3             )   ! NO+RCO3=NO2+RO2C+...
     &     +                 RKI( NCELL,  91 ) * YC( NCELL, BZCO3            )   ! NO+BZCO3=NO2+CO2+...
     &     +                 RKI( NCELL, 564 ) * YC( NCELL, CLO              )   ! NO+CLO=NO2+CL
     &     +    8.8300D-01 * RKI( NCELL, 673 ) * YC( NCELL, ISOPO2           )   ! NO+ISOPO2=...
     &     +                 RKI( NCELL, 684 ) * YC( NCELL, NISOPO2          )   ! NO+NISOPO2=...
     &     +                 RKI( NCELL, 691 ) * YC( NCELL, HC5OO            )   ! NO+HC5OO=NO2+...
     &     +                 RKI( NCELL, 698 ) * YC( NCELL, ISOPNOOD         )   ! NO+ISOPNOOD=...
     &     +                 RKI( NCELL, 705 ) * YC( NCELL, ISOPNOOB         )   ! NO+ISOPNOOB=...
     &     +                 RKI( NCELL, 713 ) * YC( NCELL, NIT1NO3OOA       )   ! NO+NIT1NO3OOA=...
     &     +                 RKI( NCELL, 720 ) * YC( NCELL, NIT1NO3OOB       )   ! NO+NIT1NO3OOB=...
     &     +    9.3400D-01 * RKI( NCELL, 727 ) * YC( NCELL, NIT1OHOO         )   ! NO+NIT1OHOO=...
     &     +                 RKI( NCELL, 732 ) * YC( NCELL, DIBOO            )   ! NO+DIBOO=NO2+HO2+...
     &     +    8.9000D-01 * RKI( NCELL, 738 ) * YC( NCELL, MVKOO            )   ! NO+MVKOO=0.8900D+...
     &     +    8.5000D-01 * RKI( NCELL, 743 ) * YC( NCELL, MACROO           )   ! NO+MACROO=...
     &     +                 RKI( NCELL, 748 ) * YC( NCELL, MACO3            )   ! NO+MACO3=NO2+CO+...
     &     +                 RKI( NCELL, 778 ) * YC( NCELL, IEPOXOO          )   ! NO+IEPOXOO=NO2+...
     &     +                 RKI( NCELL, 790 ) * YC( NCELL, IMACO3           )   ! NO+IMACO3=NO2+CO+...
      R2_1  = R2_1 * DTC


      P2 =                 RXRAT( NCELL,   9 )      ! NO+NO3=0.2000D+01*NO2
     &   +                 RXRAT( NCELL,  12 )      ! N2O5=NO2+NO3
     &   +                 RXRAT( NCELL,  17 )      ! NO3=NO2+O3P
     &   +                 RXRAT( NCELL,  24 )      ! OH+HONO=NO2
     &   +                 RXRAT( NCELL,  26 )      ! OH+NO3=NO2+HO2
     &   +                 RXRAT( NCELL,  28 )      ! HNO3=NO2+OH
     &   +                 RXRAT( NCELL,  33 )      ! HNO4=NO2+HO2
     &   +    6.1000D-01 * RXRAT( NCELL,  34 )      ! HNO4=0.6100D+00*NO2+...
     &   +                 RXRAT( NCELL,  35 )      ! HNO4+OH=NO2
     &   +    8.0000D-01 * RXRAT( NCELL,  39 )      ! NO3+HO2=0.8000D+...
     &   +    2.0000D+00 * RXRAT( NCELL,  40 )      ! NO3+NO3=0.2000D+01*NO2
     &   +                 RXRAT( NCELL,  47 )      ! NO2EX=NO2
     &   +                 RXRAT( NCELL,  48 )      ! NO2EX=NO2
     &   +                 RXRAT( NCELL,  53 )      ! MEO2+NO3=NO2+HO2+HCHO
     &   +                 RXRAT( NCELL,  58 )      ! RO2C+NO3=NO2
     &   +                 RXRAT( NCELL,  63 )      ! RO2XC+NO3=NO2
     &   +                 RXRAT( NCELL,  68 )      ! PAN=NO2+MECO3
     &   +    6.0000D-01 * RXRAT( NCELL,  69 )      ! PAN=0.6000D+00*NO2+...
     &   +                 RXRAT( NCELL,  72 )      ! MECO3+NO3=NO2+CO2+MEO2
     &   +                 RXRAT( NCELL,  78 )      ! PAN2=NO2+RCO3
     &   +    6.0000D-01 * RXRAT( NCELL,  79 )      ! PAN2=0.6000D+00*NO2+...
     &   +                 RXRAT( NCELL,  82 )      ! RCO3+NO3=NO2+RO2C+...
     &   +                 RXRAT( NCELL,  89 )      ! PBZN=NO2+BZCO3
     &   +    6.0000D-01 * RXRAT( NCELL,  90 )      ! PBZN=0.6000D+00*NO2+...
     &   +                 RXRAT( NCELL,  93 )      ! BZCO3+NO3=NO2+CO2+BZO+...
     &   +                 RXRAT( NCELL, 101 )      ! MAPAN=NO2+MACO3
     &   +    6.0000D-01 * RXRAT( NCELL, 102 )      ! MAPAN=0.6000D+00*NO2+...
     &   +                 RXRAT( NCELL, 128 )      ! NO+xNO2=NO2+NO
     &   +                 RXRAT( NCELL, 130 )      ! xNO2+NO3=NO2+NO3
     &   +    5.0000D-01 * RXRAT( NCELL, 131 )      ! xNO2+MEO2=0.5000D+...
     &   +    5.0000D-01 * RXRAT( NCELL, 132 )      ! xNO2+RO2C=0.5000D+...
     &   +    5.0000D-01 * RXRAT( NCELL, 133 )      ! xNO2+RO2XC=0.5000D+...
     &   +                 RXRAT( NCELL, 134 )      ! xNO2+MECO3=NO2+MECO3
     &   +                 RXRAT( NCELL, 135 )      ! xNO2+RCO3=NO2+RCO3
     &   +                 RXRAT( NCELL, 136 )      ! xNO2+BZCO3=NO2+BZCO3
     &   +                 RXRAT( NCELL, 137 )      ! xNO2+MACO3=NO2+MACO3
     &   +    1.9000D-02 * RXRAT( NCELL, 259 )      ! RNO3+OH=0.1900D-...
     &   +                 RXRAT( NCELL, 260 )      ! RNO3=NO2+0.5540D+...
     &   +                 RXRAT( NCELL, 558 )      ! CLONO=NO2+CL
     &   +                 RXRAT( NCELL, 559 )      ! CLNO2=NO2+CL
     &   +                 RXRAT( NCELL, 563 )      ! CL+NO3=NO2+CLO
     &   +                 RXRAT( NCELL, 566 )      ! CLONO2=NO2+CLO
     &   +                 RXRAT( NCELL, 568 )      ! CLONO2=NO2+CLO
     &   +    3.8000D-02 * RXRAT( NCELL, 581 )      ! RNO3+CL=0.3800D-...
     &   +    1.3000D+00 * RXRAT( NCELL, 683 )      ! NISOPO2+NO3=0.1300D+...
     &   +    3.0000D-01 * RXRAT( NCELL, 684 )      ! NO+NISOPO2=0.1300D+...
     &   +    1.5000D-01 * RXRAT( NCELL, 686 )      ! NISOPO2+MEO2=0.1500D+...
     &   +    1.5000D-01 * RXRAT( NCELL, 687 )      ! NISOPO2+RO2C=0.1500D+...
     &   +    3.0000D-01 * RXRAT( NCELL, 688 )      ! NISOPO2+NISOPO2=...
     &   +    3.0000D-01 * RXRAT( NCELL, 689 )      ! NISOPO2+MECO3=0.3000D+...
     &   +    3.4000D-01 * RXRAT( NCELL, 698 )      ! NO+ISOPNOOD=0.1340D+...
     &   +    1.7000D-01 * RXRAT( NCELL, 700 )      ! ISOPNOOD+MEO2=0.1700D+...
     &   +    1.7000D-01 * RXRAT( NCELL, 701 )      ! ISOPNOOD+RO2C=0.1700D+...
     &   +    3.4000D-01 * RXRAT( NCELL, 702 )      ! ISOPNOOD+MECO3=...
     &   +    3.6000D-01 * RXRAT( NCELL, 703 )      ! ISOPND+O3=0.3600D+...
     &   +    6.0000D-01 * RXRAT( NCELL, 705 )      ! NO+ISOPNOOB=0.1600D+...
     &   +    3.0000D-01 * RXRAT( NCELL, 707 )      ! ISOPNOOB+MEO2=0.3000D+...
     &   +    3.0000D-01 * RXRAT( NCELL, 708 )      ! ISOPNOOB+RO2C=0.3000D+...
     &   +    6.0000D-01 * RXRAT( NCELL, 709 )      ! ISOPNOOB+MECO3=...
     &   +    2.8000D-01 * RXRAT( NCELL, 710 )      ! ISOPNB+O3=0.2800D+...
     &   +                 RXRAT( NCELL, 712 )      ! NIT1NO3OOA+NO3=NO2+...
     &   +    2.0000D+00 * RXRAT( NCELL, 719 )      ! NIT1NO3OOB+NO3=...
     &   +    8.8000D-01 * RXRAT( NCELL, 720 )      ! NO+NIT1NO3OOB=0.1880D+...
     &   +    7.0000D-01 * RXRAT( NCELL, 722 )      ! NIT1NO3OOB+RO2C=...
     &   +    7.0000D-01 * RXRAT( NCELL, 723 )      ! NIT1NO3OOB+MEO2=...
     &   +                 RXRAT( NCELL, 724 )      ! NIT1NO3OOB+MECO3=NO2+...
     &   +    7.0000D-01 * RXRAT( NCELL, 725 )      ! NIT1+O3=0.7000D+...
     &   +                 RXRAT( NCELL, 750 )      ! MACO3+NO3=NO2+CO+CO2+...
     &   +                 RXRAT( NCELL, 758 )      ! MAPAN+OH=NO2+CO+HACET
     &   +                 RXRAT( NCELL, 762 )      ! ETHLN+OH=NO2+CO2+HCHO
     &   +                 RXRAT( NCELL, 763 )      ! ETHLN=NO2+HCHO+HO2+CO
     &   +                 RXRAT( NCELL, 764 )      ! PROPNN+OH=NO2+MGLY
     &   +                 RXRAT( NCELL, 765 )      ! PROPNNB+OH=NO2+MGLY
     &   +                 RXRAT( NCELL, 766 )      ! PROPNN=NO2+HCHO+MECO3
     &   +                 RXRAT( NCELL, 767 )      ! PROPNNB=NO2+HCHO+MECO3
     &   +                 RXRAT( NCELL, 769 )      ! MVKN=NO2+MECO3+HOCCHO
     &   +    8.5000D-01 * RXRAT( NCELL, 770 )      ! MACRN+OH=0.8500D+...
     &   +                 RXRAT( NCELL, 771 )      ! MACRN=NO2+HACET+CO+HO2
     &   +                 RXRAT( NCELL, 783 )      ! RNO3I+OH=NO2+HO2+PRD2
     &   +                 RXRAT( NCELL, 792 )      ! IMACO3+NO3=NO2+CO+CO2+...
     &   +                 RXRAT( NCELL, 802 )      ! IMAPAN=NO2+IMACO3
     &   +    6.0000D-01 * RXRAT( NCELL, 803 )      ! IMAPAN=0.6000D+00*NO2+...
     &   +                 RXRAT( NCELL, 810 )      ! xNO2+IMACO3=NO2+IMACO3
      P2 = YC0( NCELL, NO2 ) + P2 * DTC


      L2 =                 RKI( NCELL,   6 ) * YC( NCELL, O3P              )   ! NO2+O3P=NO3
     &   +                 RKI( NCELL,   8 ) * YC( NCELL, O3               )   ! NO2+O3=NO3
     &   +                 RKI( NCELL,  11 ) * YC( NCELL, NO3              )   ! NO2+NO3=N2O5
     &   +                 RKI( NCELL,  25 ) * YC( NCELL, OH               )   ! NO2+OH=HNO3
     &   +                 RKI( NCELL,  32 ) * YC( NCELL, HO2              )   ! NO2+HO2=HNO4
     &   +                 RKI( NCELL,  46 )                            ! NO2=NO2EX
     &   +                 RKI( NCELL,  67 ) * YC( NCELL, MECO3            )   ! NO2+MECO3=PAN
     &   +                 RKI( NCELL,  77 ) * YC( NCELL, RCO3             )   ! NO2+RCO3=PAN2
     &   +                 RKI( NCELL,  88 ) * YC( NCELL, BZCO3            )   ! NO2+BZCO3=PBZN
     &   +                 RKI( NCELL, 100 ) * YC( NCELL, MACO3            )   ! NO2+MACO3=MAPAN
     &   +                 RKI( NCELL, 103 ) * YC( NCELL, TBUO             )   ! NO2+TBUO=RNO3
     &   +                 RKI( NCELL, 105 ) * YC( NCELL, BZO              )   ! NO2+BZO=NPHE
     &   +                 RKI( NCELL, 556 ) * YC( NCELL, CL               )   ! NO2+CL=CLONO
     &   +                 RKI( NCELL, 557 ) * YC( NCELL, CL               )   ! NO2+CL=CLNO2
     &   +                 RKI( NCELL, 565 ) * YC( NCELL, CLO              )   ! NO2+CLO=CLONO2
     &   +                 RKI( NCELL, 714 ) * YC( NCELL, NIT1NO3OOA       )   ! NO2+NIT1NO3OOA=MAPAN
     &   +                 RKI( NCELL, 801 ) * YC( NCELL, IMACO3           )   ! NO2+IMACO3=IMAPAN
     &   +                 RKI( NCELL, 844 )                            ! NO2=0.5000D+...
      L2     = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 =    3.0000D-01 * RXRAT( NCELL,  71 )      ! MECO3+HO2=0.3000D+...
     &   +    2.5000D-01 * RXRAT( NCELL,  81 )      ! RCO3+HO2=0.2500D+...
     &   +    2.5000D-01 * RXRAT( NCELL,  92 )      ! BZCO3+HO2=0.2500D+...
     &   +    2.5000D-01 * RXRAT( NCELL, 715 )      ! NIT1NO3OOA+HO2=...
     &   +    2.5000D-01 * RXRAT( NCELL, 749 )      ! MACO3+HO2=0.2500D+...
     &   +    2.5000D-01 * RXRAT( NCELL, 791 )      ! IMACO3+HO2=0.2500D+...
      P3 = YC0( NCELL, O3 ) + P3 * DTC


      L3 =                 RKI( NCELL,   3 ) * YC( NCELL, O3P              )   ! O3+O3P=
     &   +                 RKI( NCELL,   8 ) * YC( NCELL, NO2              )   ! O3+NO2=NO3
     &   +                 RKI( NCELL,  18 )                            ! O3=O1D
     &   +                 RKI( NCELL,  19 )                            ! O3=O3P
     &   +                 RKI( NCELL,  30 ) * YC( NCELL, OH               )   ! O3+OH=HO2
     &   +                 RKI( NCELL,  36 ) * YC( NCELL, HO2              )   ! O3+HO2=OH
     &   +                 RKI( NCELL, 241 ) * YC( NCELL, AFG1             )   ! O3+AFG1=0.8260D+...
     &   +                 RKI( NCELL, 244 ) * YC( NCELL, AFG2             )   ! O3+AFG2=0.8260D+...
     &   +                 RKI( NCELL, 247 ) * YC( NCELL, AFG3             )   ! O3+AFG3=0.4710D+...
     &   +                 RKI( NCELL, 248 ) * YC( NCELL, MACR             )   ! O3+MACR=0.2080D+...
     &   +                 RKI( NCELL, 250 ) * YC( NCELL, MVK              )   ! O3+MVK=0.1640D+...
     &   +                 RKI( NCELL, 254 ) * YC( NCELL, IPRD             )   ! O3+IPRD=0.2850D+...
     &   +                 RKI( NCELL, 264 ) * YC( NCELL, ACROLEIN         )   ! O3+ACROLEIN=...
     &   +                 RKI( NCELL, 504 ) * YC( NCELL, ETHENE           )   ! O3+ETHENE=...
     &   +                 RKI( NCELL, 508 ) * YC( NCELL, PROPENE          )   ! O3+PROPENE=...
     &   +                 RKI( NCELL, 512 ) * YC( NCELL, BUTADIENE13      )   ! O3+BUTADIENE13=...
     &   +                 RKI( NCELL, 515 ) * YC( NCELL, ISOPRENE         )   ! O3+ISOPRENE=...
     &   +                 RKI( NCELL, 518 ) * YC( NCELL, APIN             )   ! O3+APIN=0.9000D-...
     &   +                 RKI( NCELL, 522 ) * YC( NCELL, ACETYLENE        )   ! O3+ACETYLENE=...
     &   +                 RKI( NCELL, 536 ) * YC( NCELL, OLE1             )   ! O3+OLE1=0.9500D-...
     &   +                 RKI( NCELL, 540 ) * YC( NCELL, OLE2             )   ! O3+OLE2=0.9400D-...
     &   +                 RKI( NCELL, 546 ) * YC( NCELL, TERP             )   ! O3+TERP=0.7800D-...
     &   +                 RKI( NCELL, 550 ) * YC( NCELL, SESQ             )   ! O3+SESQ=0.7800D-...
     &   +                 RKI( NCELL, 562 ) * YC( NCELL, CL               )   ! O3+CL=CLO
     &   +                 RKI( NCELL, 696 ) * YC( NCELL, HC5              )   ! O3+HC5=0.5000D+...
     &   +                 RKI( NCELL, 703 ) * YC( NCELL, ISOPND           )   ! O3+ISOPND=...
     &   +                 RKI( NCELL, 710 ) * YC( NCELL, ISOPNB           )   ! O3+ISOPNB=...
     &   +                 RKI( NCELL, 725 ) * YC( NCELL, NIT1             )   ! O3+NIT1=0.3000D+...
      L3    = 1.0D0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section 
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P12 =                 RXRAT( NCELL,  17 )      ! NO3=O3P+NO2
     &    +   O3P_S *       RXRAT( NCELL,  18 )      ! O3=O1D
     &    +                 RXRAT( NCELL,  19 )      ! O3=O3P
      P12 = YC0( NCELL, O3P ) + P12 * DTC


      L12 =                 RKI( NCELL,   2 )                            ! O3P=O3
     &    +                 RKI( NCELL,   3 ) * YC( NCELL, O3               )   ! O3P+O3=
     &    +                 RKI( NCELL,   4 ) * YC( NCELL, NO               )   ! O3P+NO=NO2
     &    +                 RKI( NCELL,   5 ) * YC( NCELL, NO2              )   ! O3P+NO2=NO
     &    +                 RKI( NCELL,   6 ) * YC( NCELL, NO2              )   ! O3P+NO2=NO3
     &    +                 RKI( NCELL, 249 ) * YC( NCELL, MACR             )   ! O3P+MACR=RCHO
     &    +                 RKI( NCELL, 251 ) * YC( NCELL, MVK              )   ! O3P+MVK=0.4500D+...
     &    +                 RKI( NCELL, 266 ) * YC( NCELL, ACROLEIN         )   ! O3P+ACROLEIN=RCHO
     &    +                 RKI( NCELL, 506 ) * YC( NCELL, ETHENE           )   ! O3P+ETHENE=0.8000D+...
     &    +                 RKI( NCELL, 510 ) * YC( NCELL, PROPENE          )   ! O3P+PROPENE=0.4500D+...
     &    +                 RKI( NCELL, 514 ) * YC( NCELL, BUTADIENE13      )   ! O3P+BUTADIENE13=...
     &    +                 RKI( NCELL, 516 ) * YC( NCELL, ISOPRENE         )   ! O3P+ISOPRENE=0.2500D+...
     &    +                 RKI( NCELL, 520 ) * YC( NCELL, APIN             )   ! O3P+APIN=PRD2+TRPRXN
     &    +                 RKI( NCELL, 538 ) * YC( NCELL, OLE1             )   ! O3P+OLE1=0.4500D+...
     &    +                 RKI( NCELL, 542 ) * YC( NCELL, OLE2             )   ! O3P+OLE2=0.7900D-...
     &    +                 RKI( NCELL, 548 ) * YC( NCELL, TERP             )   ! O3P+TERP=0.2370D+...
     &    +                 RKI( NCELL, 552 ) * YC( NCELL, SESQ             )   ! O3P+SESQ=0.2370D+...
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
      RK1 = RKI( NCELL,   1 ) * DTC            ! J1    (NO2+hv=NO+O3P)
      RK2 = RKI( NCELL,   2 ) * DTC            ! J2    (O3P+O2=O3)
      RK3 = RKI( NCELL,   7 ) * DTC            ! k1_3  (NO+O3=NO2)

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
      YCP( NCELL, NO ) = MAX( 0.0D0, ( P1 + XX ) * L1_INV )

      YCP( NCELL, NO2 ) = MAX( 0.0D0, ( P2 - XX ) * L2_INV )

      S1 = P12 + RK1 * YCP( NCELL, NO2 )

      S2 = T4 * S1

      YCP( NCELL, O3 ) = ( P3 + S2 ) / ( L3 + RK3 * YCP( NCELL, NO ) )

      YCP( NCELL, O3P ) = S1 * L12_INV

      YCP( NCELL, O1D ) = RKI( NCELL, 18 ) * YCP( NCELL, O3 ) / O1D_S

      RETURN

      END


