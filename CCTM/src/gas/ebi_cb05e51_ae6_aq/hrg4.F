
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
C  PRECONDITIONS: For the CB05e51 family of mechanisms
C
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Created by EBI solver program, May 26, 2017
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
!     CHARACTER( 16 ), SAVE   ::  PNAME = 'HRG4'    ! Program name


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
      P15 =                 RXRAT(   5 )      ! O+NO2=NO3
     &    +                 RXRAT(   7 )      ! NO2+O3=NO3
     &    +                 RXRAT(  29 )      ! OH+HNO3=NO3
     &    +    3.9000D-01 * RXRAT(  52 )      ! PNA=0.3900D+00*NO3+0.6100D+...
     &    +    4.0000D-01 * RXRAT(  94 )      ! PAN=0.4000D+00*NO3+0.6000D+...
     &    +    4.0000D-01 * RXRAT( 109 )      ! PANX=0.4000D+00*NO3+...
     &    +    4.0000D-01 * RXRAT( 227 )      ! MAPAN=0.4000D+00*NO3+...

c..Loss frequency of NO3 ( except NO3 + NO3 if present )
      L15 =                 RKI(  14 )                        ! NO3=NO2+O
     &    +                 RKI(  15 )                        ! NO3=NO
     &    +                 RKI(  16 ) * YC( NO           )   ! NO3+NO=0.2000D+...
     &    +                 RKI(  17 ) * YC( NO2          )   ! NO3+NO2=NO+NO2
     &    +                 RKI(  18 ) * YC( NO2          )   ! NO3+NO2=N2O5
     &    +                 RKI(  47 ) * YC( O            )   ! NO3+O=NO2
     &    +                 RKI(  48 ) * YC( OH           )   ! NO3+OH=HO2+NO2
     &    +                 RKI(  49 ) * YC( HO2          )   ! NO3+HO2=HNO3
     &    +                 RKI(  50 ) * YC( O3           )   ! NO3+O3=NO2
     &    +                 RKI(  81 ) * YC( FORM         )   ! NO3+FORM=HNO3+HO2+CO
     &    +                 RKI(  89 ) * YC( ALD2         )   ! NO3+ALD2=C2O3+HNO3
     &    +                 RKI( 104 ) * YC( ALDX         )   ! NO3+ALDX=CXO3+HNO3
     &    +                 RKI( 123 ) * YC( OLE          )   ! NO3+OLE=NO2+FORM+...
     &    +                 RKI( 127 ) * YC( ETH          )   ! NO3+ETH=NO2+XO2+...
     &    +                 RKI( 131 ) * YC( IOLE         )   ! NO3+IOLE=0.1180D+...
     &    +                 RKI( 136 ) * YC( CRES         )   ! NO3+CRES=0.3000D+...
     &    +                 RKI( 140 ) * YC( CRON         )   ! NO3+CRON=CRNO+HNO3
     &    +                 RKI( 150 ) * YC( OPEN         )   ! NO3+OPEN=OPO3+HNO3
     &    +                 RKI( 152 ) * YC( CAT1         )   ! NO3+CAT1=CRO+HNO3
     &    +                 RKI( 168 ) * YC( ISOP         )   ! NO3+ISOP=0.2000D+...
     &    +                 RKI( 171 ) * YC( ISPD         )   ! NO3+ISPD=0.3570D+...
     &    +                 RKI( 180 ) * YC( TERP         )   ! NO3+TERP=0.4700D+...

c..Loss frequency of N2O5
      L16 =                 RKI(  19 )                        ! N2O5=0.2000D+01*HNO3
     &    +                 RKI(  20 )                        ! N2O5=0.2000D+01*HNO3
     &    +                 RKI(  21 )                        ! N2O5=NO3+NO2
     &    +                 RKI(  54 )                        ! N2O5=NO2+NO3
     &    +                 RKI( 267 )                        ! N2O5=HNO3+H2NO3PIJ
     &    +                 RKI( 268 )                        ! N2O5=HNO3+H2NO3PK

c..K15_15, R15_16, and R16_15 terms
      K15_15  = RKI(  51 ) * DTC

      R15_16  = ( RKI(  21 )
     &        +   RKI(  54 ) ) * DTC 


      R16_15  = RKI(  18 ) * YC( NO2 ) * DTC

c..Solution of quadratic equation to get NO3 & N2O5
      CMN = 1.0D0 + L16 * DTC
      A = 2.0D0 * K15_15 * CMN
      B = CMN * ( 1.0D0 + L15 * DTC ) - R15_16 * R16_15
      C = CMN * ( YC0( NO3 ) + P15 * DTC ) +  R15_16 * YC0( N2O5 )

      Q = -0.5D0 * ( B + SIGN( 1.0D0, B ) * SQRT( B * B + 4.0D0 * A * C ) )
      YCP( NO3 ) = MAX( Q / A , -C / Q  )
      YCP( N2O5 ) = ( YC0( N2O5 ) + R16_15 * YCP( NO3 ) ) / CMN

      RETURN

      END
