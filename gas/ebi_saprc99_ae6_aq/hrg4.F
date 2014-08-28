
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
C  PRECONDITIONS: For the SAPRC99 family of mechanisms
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
     &    +                 RXRAT( NCELL,  27 )      ! HO+HNO3=NO3
     &    +    3.9000D-01 * RXRAT( NCELL,  34 )      ! HNO4=0.3900D+00*NO3+...

c..Loss frequency of NO3 ( except NO3 + NO3 if present )
      L15 =                 RKI( NCELL,   9 ) * YC( NCELL, NO       )   ! NO3+NO=0.2000D+...
     &    +                 RKI( NCELL,  11 ) * YC( NCELL, NO2      )   ! NO3+NO2=N2O5
     &    +                 RKI( NCELL,  14 ) * YC( NCELL, NO2      )   ! NO3+NO2=NO+NO2
     &    +                 RKI( NCELL,  15 )                    ! NO3=NO
     &    +                 RKI( NCELL,  16 )                    ! NO3=NO2+O3P
     &    +                 RKI( NCELL,  26 ) * YC( NCELL, HO       )   ! NO3+HO=HO2+NO2
     &    +                 RKI( NCELL,  39 ) * YC( NCELL, HO2      )   ! NO3+HO2=0.8000D+...
     &    +                 RKI( NCELL,  48 ) * YC( NCELL, C_O2     )   ! NO3+C_O2=HCHO+...
     &    +                 RKI( NCELL,  53 ) * YC( NCELL, RO2_R    )   ! NO3+RO2_R=NO2+HO2
     &    +                 RKI( NCELL,  58 ) * YC( NCELL, R2O2     )   ! NO3+R2O2=NO2
     &    +                 RKI( NCELL,  65 ) * YC( NCELL, RO2_N    )   ! NO3+RO2_N=NO2+...
     &    +                 RKI( NCELL,  73 ) * YC( NCELL, CCO_O2   )   ! NO3+CCO_O2=C_O2+NO2
     &    +                 RKI( NCELL,  83 ) * YC( NCELL, RCO_O2   )   ! NO3+RCO_O2=NO2+...
     &    +                 RKI( NCELL,  94 ) * YC( NCELL, BZCO_O2  )   ! NO3+BZCO_O2=NO2+...
     &    +                 RKI( NCELL, 106 ) * YC( NCELL, MA_RCO3  )   ! NO3+MA_RCO3=NO2+...
     &    +                 RKI( NCELL, 129 ) * YC( NCELL, HCHO     )   ! NO3+HCHO=HNO3+HO2+CO
     &    +                 RKI( NCELL, 132 ) * YC( NCELL, CCHO     )   ! NO3+CCHO=HNO3+CCO_O2
     &    +                 RKI( NCELL, 135 ) * YC( NCELL, RCHO     )   ! NO3+RCHO=HNO3+RCO_O2
     &    +                 RKI( NCELL, 148 ) * YC( NCELL, GLY      )   ! NO3+GLY=HNO3+...
     &    +                 RKI( NCELL, 151 ) * YC( NCELL, MGLY     )   ! NO3+MGLY=HNO3+CO+...
     &    +                 RKI( NCELL, 154 ) * YC( NCELL, PHEN     )   ! NO3+PHEN=HNO3+BZ_O
     &    +                 RKI( NCELL, 156 ) * YC( NCELL, CRES     )   ! NO3+CRES=HNO3+BZ_O
     &    +                 RKI( NCELL, 157 ) * YC( NCELL, NPHE     )   ! NO3+NPHE=HNO3+...
     &    +                 RKI( NCELL, 160 ) * YC( NCELL, BALD     )   ! NO3+BALD=HNO3+...
     &    +                 RKI( NCELL, 163 ) * YC( NCELL, METHACRO )   ! NO3+METHACRO=...
     &    +                 RKI( NCELL, 172 ) * YC( NCELL, ISOPROD  )   ! NO3+ISOPROD=...
     &    +                 RKI( NCELL, 187 ) * YC( NCELL, ETHENE   )   ! NO3+ETHENE=RO2_R+...
     &    +                 RKI( NCELL, 191 ) * YC( NCELL, ISOPRENE )   ! NO3+ISOPRENE=...
     &    +                 RKI( NCELL, 195 ) * YC( NCELL, TRP1     )   ! NO3+TRP1=0.4740D+...
     &    +                 RKI( NCELL, 213 ) * YC( NCELL, OLE1     )   ! NO3+OLE1=0.8240D+...
     &    +                 RKI( NCELL, 217 ) * YC( NCELL, OLE2     )   ! NO3+OLE2=0.3910D+...

c..Loss frequency of N2O5
      L16 =                 RKI( NCELL,  12 )                    ! N2O5=NO2+NO3
     &    +                 RKI( NCELL,  13 )                    ! N2O5=0.2000D+01*HNO3
     &    +                 RKI( NCELL, 225 )                    ! N2O5=0.2000D+01*HNO3

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
