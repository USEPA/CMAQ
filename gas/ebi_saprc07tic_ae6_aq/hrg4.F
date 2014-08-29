
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

       SUBROUTINE HRG4( DTC )

C**********************************************************************
C
C  FUNCTION:  To solve for the concentration of NO3 and N2O5
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


C..INCLUDES: NONE


C..ARGUMENTS:
      REAL( 8 ), INTENT( IN ) :: DTC             ! Time step


C..PARAMETERS: NONE


C..EXTERNAL FUNCTIONS: NONE


C..SAVED LOCAL VARIABLES:
      CHARACTER( 16 ), SAVE   ::  PNAME = 'HRG4'    ! Program name


C..SCRATCH LOCAL VARIABLES:
      REAL( 8 ) ::   A, B, C, Q   ! Quadratic equation terms
      REAL( 8 ) ::   CMN          ! Temp scalar
      REAL( 8 ) ::   L15          ! Loss of NO3
      REAL( 8 ) ::   L16          ! Loss of N2O5
      REAL( 8 ) ::   P15          ! Production of NO3
      REAL( 8 ) ::   K15_15       ! Kno3+no3 * delta t
      REAL( 8 ) ::   R15_16       ! Kn2o5-->no3 * delta t
      REAL( 8 ) ::   R16_15       ! Kno3+no2-->n2o5[NO2] * delta t


C**********************************************************************


c..Production of NO3 (except from N2O5 )
      P15 =                 RXRAT( NCELL,   6 )      ! O3P+NO2=NO3
     &    +                 RXRAT( NCELL,   8 )      ! O3+NO2=NO3
     &    +                 RXRAT( NCELL,  27 )      ! OH+HNO3=NO3
     &    +    3.9000D-01 * RXRAT( NCELL,  34 )      ! HNO4=0.3900D+00*NO3+...
     &    +    4.0000D-01 * RXRAT( NCELL,  69 )      ! PAN=0.4000D+00*NO3+0.6000D+...
     &    +    4.0000D-01 * RXRAT( NCELL,  79 )      ! PAN2=0.4000D+00*NO3+...
     &    +    4.0000D-01 * RXRAT( NCELL,  90 )      ! PBZN=0.4000D+00*NO3+...
     &    +    4.0000D-01 * RXRAT( NCELL, 102 )      ! MAPAN=0.4000D+00*NO3+...
     &    +                 RXRAT( NCELL, 567 )      ! CLONO2=NO3+CL
     &    +                 RXRAT( NCELL, 569 )      ! CL+CLONO2=NO3+CL2
     &    +    3.1000D-01 * RXRAT( NCELL, 698 )      ! ISOPNOOD+NO=0.3100D+00*NO3+...
     &    +    1.5500D-01 * RXRAT( NCELL, 700 )      ! ISOPNOOD+MEO2=0.1550D+...
     &    +    1.5500D-01 * RXRAT( NCELL, 701 )      ! ISOPNOOD+RO2C=0.1550D+...
     &    +    3.1000D-01 * RXRAT( NCELL, 702 )      ! ISOPNOOD+MECO3=0.3100D+...
     &    +                 RXRAT( NCELL, 768 )      ! MVKN+OH=NO3+0.6500D+...
     &    +    1.5000D-01 * RXRAT( NCELL, 770 )      ! MACRN+OH=0.1500D+00*NO3+...
     &    +    4.0000D-01 * RXRAT( NCELL, 803 )      ! IMAPAN=0.4000D+00*NO3+...
     &    +    8.1000D-01 * RXRAT( NCELL, 839 )      ! IMAPAN+OH=0.8100D+00*NO3+...

c..Loss frequency of NO3 ( except NO3 + NO3 if present )
      L15 =                 RKI( NCELL,   9 ) * YC( NCELL, NO               )   ! NO3+NO=0.2000D+...
     &    +                 RKI( NCELL,  11 ) * YC( NCELL, NO2              )   ! NO3+NO2=N2O5
     &    +                 RKI( NCELL,  15 ) * YC( NCELL, NO2              )   ! NO3+NO2=NO+NO2
     &    +                 RKI( NCELL,  16 )                            ! NO3=NO
     &    +                 RKI( NCELL,  17 )                            ! NO3=NO2+O3P
     &    +                 RKI( NCELL,  26 ) * YC( NCELL, OH               )   ! NO3+OH=HO2+NO2
     &    +                 RKI( NCELL,  39 ) * YC( NCELL, HO2              )   ! NO3+HO2=0.8000D+...
     &    +                 RKI( NCELL,  53 ) * YC( NCELL, MEO2             )   ! NO3+MEO2=HCHO+...
     &    +                 RKI( NCELL,  58 ) * YC( NCELL, RO2C             )   ! NO3+RO2C=NO2
     &    +                 RKI( NCELL,  63 ) * YC( NCELL, RO2XC            )   ! NO3+RO2XC=NO2
     &    +                 RKI( NCELL,  72 ) * YC( NCELL, MECO3            )   ! NO3+MECO3=MEO2+...
     &    +                 RKI( NCELL,  82 ) * YC( NCELL, RCO3             )   ! NO3+RCO3=NO2+...
     &    +                 RKI( NCELL,  93 ) * YC( NCELL, BZCO3            )   ! NO3+BZCO3=NO2+...
     &    +                 RKI( NCELL, 201 ) * YC( NCELL, HCHO             )   ! NO3+HCHO=HNO3+HO2+CO
     &    +                 RKI( NCELL, 204 ) * YC( NCELL, CCHO             )   ! NO3+CCHO=HNO3+MECO3
     &    +                 RKI( NCELL, 207 ) * YC( NCELL, RCHO             )   ! NO3+RCHO=HNO3+RCO3
     &    +                 RKI( NCELL, 227 ) * YC( NCELL, GLY              )   ! NO3+GLY=HNO3+...
     &    +                 RKI( NCELL, 230 ) * YC( NCELL, MGLY             )   ! NO3+MGLY=HNO3+CO+...
     &    +                 RKI( NCELL, 233 ) * YC( NCELL, CRES             )   ! NO3+CRES=HNO3+BZO
     &    +                 RKI( NCELL, 239 ) * YC( NCELL, BALD             )   ! NO3+BALD=HNO3+BZCO3
     &    +                 RKI( NCELL, 255 ) * YC( NCELL, IPRD             )   ! NO3+IPRD=0.1500D+...
     &    +                 RKI( NCELL, 262 ) * YC( NCELL, HOCCHO           )   ! NO3+HOCCHO=HNO3+...
     &    +                 RKI( NCELL, 265 ) * YC( NCELL, ACROLEIN         )   ! NO3+ACROLEIN=...
     &    +                 RKI( NCELL, 505 ) * YC( NCELL, ETHENE           )   ! NO3+ETHENE=xHO2+...
     &    +                 RKI( NCELL, 509 ) * YC( NCELL, PROPENE          )   ! NO3+PROPENE=...
     &    +                 RKI( NCELL, 513 ) * YC( NCELL, BUTADIENE13      )   ! NO3+BUTADIENE13=...
     &    +                 RKI( NCELL, 519 ) * YC( NCELL, APIN             )   ! NO3+APIN=0.5600D-...
     &    +                 RKI( NCELL, 537 ) * YC( NCELL, OLE1             )   ! NO3+OLE1=0.7720D+...
     &    +                 RKI( NCELL, 541 ) * YC( NCELL, OLE2             )   ! NO3+OLE2=0.4000D+...
     &    +                 RKI( NCELL, 547 ) * YC( NCELL, TERP             )   ! NO3+TERP=0.2270D+...
     &    +                 RKI( NCELL, 551 ) * YC( NCELL, SESQ             )   ! NO3+SESQ=0.2270D+...
     &    +                 RKI( NCELL, 563 ) * YC( NCELL, CL               )   ! NO3+CL=CLO+NO2
     &    +                 RKI( NCELL, 682 ) * YC( NCELL, ISOPRENE         )   ! NO3+ISOPRENE=NISOPO2
     &    +                 RKI( NCELL, 683 ) * YC( NCELL, NISOPO2          )   ! NO3+NISOPO2=...
     &    +                 RKI( NCELL, 711 ) * YC( NCELL, NIT1             )   ! NO3+NIT1=0.6000D+...
     &    +                 RKI( NCELL, 712 ) * YC( NCELL, NIT1NO3OOA       )   ! NO3+NIT1NO3OOA=...
     &    +                 RKI( NCELL, 719 ) * YC( NCELL, NIT1NO3OOB       )   ! NO3+NIT1NO3OOB=...
     &    +                 RKI( NCELL, 750 ) * YC( NCELL, MACO3            )   ! NO3+MACO3=NO2+CO+...
     &    +                 RKI( NCELL, 787 ) * YC( NCELL, MACR             )   ! NO3+MACR=0.5000D+...
     &    +                 RKI( NCELL, 792 ) * YC( NCELL, IMACO3           )   ! NO3+IMACO3=NO2+...

c..Loss frequency of N2O5
      L16 =                 RKI( NCELL,  12 )                            ! N2O5=NO2+NO3
     &    +                 RKI( NCELL,  13 )                            ! N2O5=0.2000D+01*HNO3
     &    +                 RKI( NCELL,  14 )                            ! N2O5=0.2000D+01*HNO3
     &    +                 RKI( NCELL, 843 )                            ! N2O5=0.2000D+01*HNO3

c..K15_15, R15_16, and R16_15 terms
      K15_15  = RKI( NCELL,  40 ) * DTC

      R15_16  = ( RKI( NCELL,  12 ) ) * DTC 


      R16_15  = RKI( NCELL,  11 ) * YC( NCELL, NO2 ) * DTC

c..Solution of quadratic equation to get NO3 & N2O5
      CMN = 1.0D0 + L16 * DTC
      A = 2.0D0 * K15_15 * CMN
      B = CMN * ( 1.0D0 + L15 * DTC ) - R15_16 * R16_15
      C = CMN * ( YC0( NCELL, NO3 ) + P15 * DTC ) +  R15_16 * YC0( NCELL, N2O5 )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )
      YCP( NCELL, NO3 ) = MAX( Q / A , -C / Q  )
      YCP( NCELL, N2O5 ) = ( YC0( NCELL, N2O5 ) + R16_15 * YCP( NCELL, NO3 ) ) / CMN

      RETURN

      END
