
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
C  REVISION HISTORY: Created by EBI solver program, Oct. 18, 2012
C
C**********************************************************************
      USE HRDATA

      IMPLICIT NONE


C..INCLUDES: None


C..ARGUMENTS:
      REAL DTC                      ! Time step


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
      O1D_S =           RKI(  20 )                            ! O1D=2*OH
     &       +          RKI(  21 )                            ! O1D=O3P

      O3P_S =           RKI(  21 )                            ! O1D=O3P

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =           RKI(   1 )                            ! NO2=NO+O3P
     &      +          RKI(   5 ) * YC( O3P              )   ! NO2+O3P=NO
      R1_2  = R1_2 * DTC


      P1 =           RXRAT(  15 )      ! NO2+NO3=NO+NO2
     &    +          RXRAT(  16 )      ! NO3=NO
     &    +          RXRAT(  23 )      ! HONO=NO+OH
     &    +          RXRAT( 557 )      ! CLNO=NO+CL
      P1    = YC0( NO ) + P1 * DTC


      L1 =           RKI(  22 ) * YC( OH               )   ! NO+OH=HONO
     &    +          RKI(  61 ) * YC( RO2XC            )   ! NO+RO2XC=
     &    +          RKI( 556 ) * YC( CL               )   ! NO+CL=CLNO
     &    +  0.117 * RKI( 676 ) * YC( ISOPO2           )   ! NO+ISOPO2=...
     &    +  0.066 * RKI( 730 ) * YC( NIT1OHOO         )   ! NO+NIT1OHOO=...
     &    +  0.110 * RKI( 741 ) * YC( MVKOO            )   ! NO+MVKOO=...
     &    +  0.150 * RKI( 747 ) * YC( MACROO           )   ! NO+MACROO=...
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
      R2_1 =           RKI(   4 ) * YC( O3P              )   ! NO+O3P=NO2
     &      +          RKI(   9 ) * YC( NO3              )   ! NO+NO3=2*NO2
     &      +  2.000 * RKI(  10 ) * YC( NO               )   ! NO+NO=2*NO2
     &      +          RKI(  31 ) * YC( HO2              )   ! NO+HO2=NO2+OH
     &      +          RKI(  50 ) * YC( MEO2             )   ! NO+MEO2=NO2+HCHO+HO2
     &      +          RKI(  56 ) * YC( RO2C             )   ! NO+RO2C=NO2
     &      +          RKI(  70 ) * YC( MECO3            )   ! NO+MECO3=NO2+CO2+...
     &      +          RKI(  80 ) * YC( RCO3             )   ! NO+RCO3=NO2+RO2C+...
     &      +          RKI(  91 ) * YC( BZCO3            )   ! NO+BZCO3=NO2+CO2+...
     &      +          RKI( 566 ) * YC( CLO              )   ! NO+CLO=NO2+CL
     &      +  0.883 * RKI( 676 ) * YC( ISOPO2           )   ! NO+ISOPO2=...
     &      +          RKI( 687 ) * YC( NISOPO2          )   ! NO+NISOPO2=...
     &      +          RKI( 694 ) * YC( HC5OO            )   ! NO+HC5OO=NO2+...
     &      +          RKI( 701 ) * YC( ISOPNOOD         )   ! NO+ISOPNOOD=...
     &      +          RKI( 708 ) * YC( ISOPNOOB         )   ! NO+ISOPNOOB=...
     &      +          RKI( 716 ) * YC( NIT1NO3OOA       )   ! NO+NIT1NO3OOA=...
     &      +          RKI( 723 ) * YC( NIT1NO3OOB       )   ! NO+NIT1NO3OOB=...
     &      +  0.934 * RKI( 730 ) * YC( NIT1OHOO         )   ! NO+NIT1OHOO=...
     &      +          RKI( 735 ) * YC( DIBOO            )   ! NO+DIBOO=NO2+HO2+...
     &      +  0.890 * RKI( 741 ) * YC( MVKOO            )   ! NO+MVKOO=...
     &      +  0.850 * RKI( 747 ) * YC( MACROO           )   ! NO+MACROO=...
     &      +          RKI( 752 ) * YC( MACO3            )   ! NO+MACO3=NO2+CO+...
     &      +          RKI( 782 ) * YC( IEPOXOO          )   ! NO+IEPOXOO=NO2+...
      R2_1  = R2_1 * DTC


      P2 =           RXRAT(   9 )      ! NO+NO3=2*NO2
     &    +          RXRAT(  12 )      ! N2O5=NO2+NO3
     &    +          RXRAT(  17 )      ! NO3=NO2+O3P
     &    +          RXRAT(  24 )      ! OH+HONO=NO2
     &    +          RXRAT(  26 )      ! OH+NO3=NO2+HO2
     &    +          RXRAT(  28 )      ! HNO3=NO2+OH
     &    +          RXRAT(  33 )      ! HNO4=NO2+HO2
     &    +  0.610 * RXRAT(  34 )      ! HNO4=0.61*NO2+...
     &    +          RXRAT(  35 )      ! HNO4+OH=NO2
     &    +  0.800 * RXRAT(  39 )      ! NO3+HO2=0.8*NO2+...
     &    +  2.000 * RXRAT(  40 )      ! NO3+NO3=2*NO2
     &    +          RXRAT(  47 )      ! NO2EX=NO2
     &    +          RXRAT(  48 )      ! NO2EX=NO2
     &    +          RXRAT(  53 )      ! MEO2+NO3=NO2+HO2+HCHO
     &    +          RXRAT(  58 )      ! RO2C+NO3=NO2
     &    +          RXRAT(  63 )      ! RO2XC+NO3=NO2
     &    +          RXRAT(  68 )      ! PAN=NO2+MECO3
     &    +  0.600 * RXRAT(  69 )      ! PAN=0.6*NO2+0.6*MECO3+...
     &    +          RXRAT(  72 )      ! MECO3+NO3=NO2+CO2+MEO2
     &    +          RXRAT(  78 )      ! PAN2=NO2+RCO3
     &    +  0.600 * RXRAT(  79 )      ! PAN2=0.6*NO2+0.6*RCO3+...
     &    +          RXRAT(  82 )      ! RCO3+NO3=NO2+RO2C+...
     &    +          RXRAT(  89 )      ! PBZN=NO2+BZCO3
     &    +  0.600 * RXRAT(  90 )      ! PBZN=0.6*NO2+...
     &    +          RXRAT(  93 )      ! BZCO3+NO3=NO2+CO2+BZO+...
     &    +          RXRAT( 101 )      ! MAPAN=NO2+MACO3
     &    +  0.600 * RXRAT( 102 )      ! MAPAN=0.6*NO2+...
     &    +          RXRAT( 128 )      ! NO+xNO2=NO2+NO
     &    +          RXRAT( 130 )      ! xNO2+NO3=NO2+NO3
     &    +  0.500 * RXRAT( 131 )      ! xNO2+MEO2=0.5*NO2+MEO2
     &    +  0.500 * RXRAT( 132 )      ! xNO2+RO2C=0.5*NO2+RO2C
     &    +  0.500 * RXRAT( 133 )      ! xNO2+RO2XC=0.5*NO2+RO2XC
     &    +          RXRAT( 134 )      ! xNO2+MECO3=NO2+MECO3
     &    +          RXRAT( 135 )      ! xNO2+RCO3=NO2+RCO3
     &    +          RXRAT( 136 )      ! xNO2+BZCO3=NO2+BZCO3
     &    +          RXRAT( 137 )      ! xNO2+MACO3=NO2+MACO3
     &    +  0.019 * RXRAT( 261 )      ! RNO3+OH=0.019*NO2+...
     &    +          RXRAT( 262 )      ! RNO3=NO2+0.554*xHO2+...
     &    +          RXRAT( 560 )      ! CLONO=NO2+CL
     &    +          RXRAT( 561 )      ! CLNO2=NO2+CL
     &    +          RXRAT( 565 )      ! CL+NO3=NO2+CLO
     &    +          RXRAT( 568 )      ! CLONO2=NO2+CLO
     &    +          RXRAT( 570 )      ! CLONO2=NO2+CLO
     &    +  0.038 * RXRAT( 583 )      ! RNO3+CL=0.038*NO2+HCL+...
     &    +  1.300 * RXRAT( 686 )      ! NISOPO2+NO3=1.3*NO2+...
     &    +  0.300 * RXRAT( 687 )      ! NO+NISOPO2=1.3*NO2+...
     &    +  0.150 * RXRAT( 689 )      ! NISOPO2+MEO2=0.15*NO2+...
     &    +  0.150 * RXRAT( 690 )      ! NISOPO2+RO2C=0.15*NO2+...
     &    +  0.300 * RXRAT( 691 )      ! NISOPO2+NISOPO2=...
     &    +  0.300 * RXRAT( 692 )      ! NISOPO2+MECO3=0.3*NO2+...
     &    +  0.340 * RXRAT( 701 )      ! NO+ISOPNOOD=1.34*NO2+...
     &    +  0.170 * RXRAT( 703 )      ! ISOPNOOD+MEO2=...
     &    +  0.170 * RXRAT( 704 )      ! ISOPNOOD+RO2C=...
     &    +  0.340 * RXRAT( 705 )      ! ISOPNOOD+MECO3=...
     &    +  0.360 * RXRAT( 706 )      ! ISOPND+O3=0.36*NO2+...
     &    +  0.600 * RXRAT( 708 )      ! NO+ISOPNOOB=1.6*NO2+...
     &    +  0.300 * RXRAT( 710 )      ! ISOPNOOB+MEO2=0.3*NO2+...
     &    +  0.300 * RXRAT( 711 )      ! ISOPNOOB+RO2C=0.3*NO2+...
     &    +  0.600 * RXRAT( 712 )      ! ISOPNOOB+MECO3=...
     &    +  0.280 * RXRAT( 713 )      ! ISOPNB+O3=0.28*NO2+...
     &    +          RXRAT( 715 )      ! NIT1NO3OOA+NO3=NO2+...
     &    +  2.000 * RXRAT( 722 )      ! NIT1NO3OOB+NO3=NO2+...
     &    +  0.880 * RXRAT( 723 )      ! NO+NIT1NO3OOB=...
     &    +  0.700 * RXRAT( 725 )      ! NIT1NO3OOB+RO2C=...
     &    +  0.700 * RXRAT( 726 )      ! NIT1NO3OOB+MEO2=...
     &    +          RXRAT( 727 )      ! NIT1NO3OOB+MECO3=NO2+...
     &    +  0.700 * RXRAT( 728 )      ! NIT1+O3=0.7*NO2+...
     &    +          RXRAT( 754 )      ! MACO3+NO3=NO2+CO+CO2+...
     &    +          RXRAT( 762 )      ! MAPAN+OH=NO2+CO+HACET
     &    +          RXRAT( 766 )      ! ETHLN+OH=NO2+CO2+HCHO
     &    +          RXRAT( 767 )      ! ETHLN=NO2+HCHO+HO2+CO
     &    +          RXRAT( 768 )      ! PROPNN+OH=NO2+MGLY
     &    +          RXRAT( 769 )      ! PROPNNB+OH=NO2+MGLY
     &    +          RXRAT( 770 )      ! PROPNN=NO2+HCHO+MECO3
     &    +          RXRAT( 771 )      ! PROPNNB=NO2+HCHO+MECO3
     &    +          RXRAT( 773 )      ! MVKN=NO2+MECO3+HOCCHO
     &    +  0.850 * RXRAT( 774 )      ! MACRN+OH=0.85*NO2+...
     &    +          RXRAT( 775 )      ! MACRN=NO2+HACET+CO+HO2
     &    +          RXRAT( 787 )      ! RNO3I+OH=NO2+HO2+PRD2
      P2 = YC0( NO2 ) + P2 * DTC


      L2 =           RKI(   6 ) * YC( O3P              )   ! NO2+O3P=NO3
     &    +          RKI(   8 ) * YC( O3               )   ! NO2+O3=NO3
     &    +          RKI(  11 ) * YC( NO3              )   ! NO2+NO3=N2O5
     &    +          RKI(  25 ) * YC( OH               )   ! NO2+OH=HNO3
     &    +          RKI(  32 ) * YC( HO2              )   ! NO2+HO2=HNO4
     &    +          RKI(  46 )                            ! NO2=NO2EX
     &    +          RKI(  67 ) * YC( MECO3            )   ! NO2+MECO3=PAN
     &    +          RKI(  77 ) * YC( RCO3             )   ! NO2+RCO3=PAN2
     &    +          RKI(  88 ) * YC( BZCO3            )   ! NO2+BZCO3=PBZN
     &    +          RKI( 100 ) * YC( MACO3            )   ! NO2+MACO3=MAPAN
     &    +          RKI( 103 ) * YC( TBUO             )   ! NO2+TBUO=RNO3
     &    +          RKI( 105 ) * YC( BZO              )   ! NO2+BZO=NPHE
     &    +          RKI( 558 ) * YC( CL               )   ! NO2+CL=CLONO
     &    +          RKI( 559 ) * YC( CL               )   ! NO2+CL=CLNO2
     &    +          RKI( 567 ) * YC( CLO              )   ! NO2+CLO=CLONO2
     &    +          RKI( 717 ) * YC( NIT1NO3OOA       )   ! NO2+NIT1NO3OOA=MAPAN
      L2     = 1.0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 =   0.300 * RXRAT(  71 )      ! MECO3+HO2=0.3*O3+...
     &    +  0.250 * RXRAT(  81 )      ! RCO3+HO2=0.25*O3+...
     &    +  0.250 * RXRAT(  92 )      ! BZCO3+HO2=0.25*O3+...
     &    +  0.250 * RXRAT( 718 )      ! NIT1NO3OOA+HO2=...
     &    +  0.250 * RXRAT( 753 )      ! MACO3+HO2=0.25*O3+...
      P3 = YC0( O3 ) + P3 * DTC


      L3 =           RKI(   3 ) * YC( O3P              )   ! O3+O3P=
     &    +          RKI(   8 ) * YC( NO2              )   ! O3+NO2=NO3
     &    +          RKI(  18 )                            ! O3=O1D
     &    +          RKI(  19 )                            ! O3=O3P
     &    +          RKI(  30 ) * YC( OH               )   ! O3+OH=HO2
     &    +          RKI(  36 ) * YC( HO2              )   ! O3+HO2=OH
     &    +          RKI( 241 ) * YC( AFG1             )   ! O3+AFG1=0.826*OH+...
     &    +          RKI( 244 ) * YC( AFG2             )   ! O3+AFG2=0.826*OH+...
     &    +          RKI( 247 ) * YC( AFG3             )   ! O3+AFG3=0.471*OH+...
     &    +          RKI( 248 ) * YC( MACR             )   ! O3+MACR=0.208*OH+...
     &    +          RKI( 252 ) * YC( MVK              )   ! O3+MVK=0.164*OH+...
     &    +          RKI( 256 ) * YC( IPRD             )   ! O3+IPRD=0.285*OH+...
     &    +          RKI( 266 ) * YC( ACROLEIN         )   ! O3+ACROLEIN=...
     &    +          RKI( 506 ) * YC( ETHENE           )   ! O3+ETHENE=...
     &    +          RKI( 510 ) * YC( PROPENE          )   ! O3+PROPENE=...
     &    +          RKI( 514 ) * YC( BUTADIENE13      )   ! O3+BUTADIENE13=...
     &    +          RKI( 517 ) * YC( ISOPRENE         )   ! O3+ISOPRENE=...
     &    +          RKI( 520 ) * YC( APIN             )   ! O3+APIN=...
     &    +          RKI( 524 ) * YC( ACETYLENE        )   ! O3+ACETYLENE=...
     &    +          RKI( 538 ) * YC( OLE1             )   ! O3+OLE1=...
     &    +          RKI( 542 ) * YC( OLE2             )   ! O3+OLE2=...
     &    +          RKI( 548 ) * YC( TERP             )   ! O3+TERP=...
     &    +          RKI( 552 ) * YC( SESQ             )   ! O3+SESQ=...
     &    +          RKI( 564 ) * YC( CL               )   ! O3+CL=CLO
     &    +          RKI( 699 ) * YC( HC5              )   ! O3+HC5=0.5*MGLY+...
     &    +          RKI( 706 ) * YC( ISOPND           )   ! O3+ISOPND=...
     &    +          RKI( 713 ) * YC( ISOPNB           )   ! O3+ISOPNB=...
     &    +          RKI( 728 ) * YC( NIT1             )   ! O3+NIT1=...
      L3    = 1.0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section 
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P12 =           RXRAT(  17 )      ! NO3=O3P+NO2
     &     +  O3P_S * RXRAT(  18 )      ! O3=O1D
     &     +          RXRAT(  19 )      ! O3=O3P
      P12 = YC0( O3P ) + P12 * DTC


      L12 =           RKI(   2 )                            ! O3P=O3
     &     +          RKI(   3 ) * YC( O3               )   ! O3P+O3=
     &     +          RKI(   4 ) * YC( NO               )   ! O3P+NO=NO2
     &     +          RKI(   5 ) * YC( NO2              )   ! O3P+NO2=NO
     &     +          RKI(   6 ) * YC( NO2              )   ! O3P+NO2=NO3
     &     +          RKI( 250 ) * YC( MACR             )   ! O3P+MACR=RCHO
     &     +          RKI( 253 ) * YC( MVK              )   ! O3P+MVK=0.45*RCHO+...
     &     +          RKI( 268 ) * YC( ACROLEIN         )   ! O3P+ACROLEIN=RCHO
     &     +          RKI( 508 ) * YC( ETHENE           )   ! O3P+ETHENE=0.8*HO2+...
     &     +          RKI( 512 ) * YC( PROPENE          )   ! O3P+PROPENE=0.45*RCHO+...
     &     +          RKI( 516 ) * YC( BUTADIENE13      )   ! O3P+BUTADIENE13=...
     &     +          RKI( 518 ) * YC( ISOPRENE         )   ! O3P+ISOPRENE=...
     &     +          RKI( 522 ) * YC( APIN             )   ! O3P+APIN=PRD2+TRPRXN
     &     +          RKI( 540 ) * YC( OLE1             )   ! O3P+OLE1=0.45*RCHO+...
     &     +          RKI( 544 ) * YC( OLE2             )   ! O3P+OLE2=0.079*RCHO+...
     &     +          RKI( 550 ) * YC( TERP             )   ! O3P+TERP=0.237*RCHO+...
     &     +          RKI( 554 ) * YC( SESQ             )   ! O3P+SESQ=0.237*RCHO+...
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
      RK1 = RKI(   1 ) * DTC            ! J1    (NO2+hv=NO+O3P)
      RK2 = RKI(   2 ) * DTC            ! J2    (O3P+O2=O3)
      RK3 = RKI(   7 ) * DTC            ! k1_3  (NO+O3=NO2)

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
      YCP( NO ) = MAX( 0.0, ( P1 + XX ) * L1_INV )

      YCP( NO2 ) = MAX( 0.0, ( P2 - XX ) * L2_INV )

      S1 = P12 + RK1 * YCP( NO2 )

      S2 = T4 * S1

      YCP( O3 ) = ( P3 + S2 ) / ( L3 + RK3 * YCP( NO ) )

      YCP( O3P ) = S1 * L12_INV

      YCP( O1D ) = RKI( 18 ) * YCP( O3 ) / O1D_S

      RETURN

      END


