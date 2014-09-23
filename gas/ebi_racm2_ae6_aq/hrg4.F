
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
C  PRECONDITIONS: For the RACM2 family of mechanisms
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
      P15 =    2.0000D-01 * RXRAT( NCELL,   9 )      ! HNO4=0.2000D+00*NO3+...
     &    +                 RXRAT( NCELL,  33 )      ! PAN=NO3+MO2
     &    +                 RXRAT( NCELL,  37 )      ! O3+NO2=NO3
     &    +                 RXRAT( NCELL,  55 )      ! NO2+O3P=NO3
     &    +                 RXRAT( NCELL,  57 )      ! HNO3+HO=NO3
     &    +                 RXRAT( NCELL, 120 )      ! PAN+HO=NO3+XO2+HCHO
     &    +                 RXRAT( NCELL, 121 )      ! PPN+HO=NO3+XO2+HCHO

c..Loss frequency of NO3 ( except NO3 + NO3 if present )
      L15 =                 RKI( NCELL,   5 )                   ! NO3=NO
     &    +                 RKI( NCELL,   6 )                   ! NO3=O3P+NO2
     &    +                 RKI( NCELL,  58 ) * YC( NCELL, HO      )   ! NO3+HO=HO2+NO2
     &    +                 RKI( NCELL,  59 ) * YC( NCELL, HO2     )   ! NO3+HO2=0.7000D+...
     &    +                 RKI( NCELL,  60 ) * YC( NCELL, NO      )   ! NO3+NO=0.2000D+...
     &    +                 RKI( NCELL,  61 ) * YC( NCELL, NO2     )   ! NO3+NO2=NO+NO2
     &    +                 RKI( NCELL,  63 ) * YC( NCELL, NO2     )   ! NO3+NO2=N2O5
     &    +                 RKI( NCELL, 141 ) * YC( NCELL, ETE     )   ! NO3+ETE=0.8000D+...
     &    +                 RKI( NCELL, 142 ) * YC( NCELL, OLT     )   ! NO3+OLT=0.4300D+...
     &    +                 RKI( NCELL, 143 ) * YC( NCELL, OLI     )   ! NO3+OLI=0.1100D+...
     &    +                 RKI( NCELL, 144 ) * YC( NCELL, DIEN    )   ! NO3+DIEN=0.9000D+...
     &    +                 RKI( NCELL, 145 ) * YC( NCELL, ISO     )   ! NO3+ISO=ISON
     &    +                 RKI( NCELL, 146 ) * YC( NCELL, API     )   ! NO3+API=0.1000D+...
     &    +                 RKI( NCELL, 147 ) * YC( NCELL, LIM     )   ! NO3+LIM=0.7100D+...
     &    +                 RKI( NCELL, 148 ) * YC( NCELL, HCHO    )   ! NO3+HCHO=HO2+CO+HNO3
     &    +                 RKI( NCELL, 149 ) * YC( NCELL, ACD     )   ! NO3+ACD=ACO3+HNO3
     &    +                 RKI( NCELL, 150 ) * YC( NCELL, ALD     )   ! NO3+ALD=RCO3+HNO3
     &    +                 RKI( NCELL, 151 ) * YC( NCELL, MACR    )   ! NO3+MACR=0.6800D+...
     &    +                 RKI( NCELL, 152 ) * YC( NCELL, UALD    )   ! NO3+UALD=HO2+XO2+...
     &    +                 RKI( NCELL, 153 ) * YC( NCELL, GLY     )   ! NO3+GLY=HO2+...
     &    +                 RKI( NCELL, 154 ) * YC( NCELL, MGLY    )   ! NO3+MGLY=ACO3+CO+...
     &    +                 RKI( NCELL, 155 ) * YC( NCELL, PHEN    )   ! NO3+PHEN=0.4000D+...
     &    +                 RKI( NCELL, 156 ) * YC( NCELL, CSL     )   ! NO3+CSL=0.4000D+...
     &    +                 RKI( NCELL, 157 ) * YC( NCELL, EPX     )   ! NO3+EPX=0.5000D+...
     &    +                 RKI( NCELL, 158 ) * YC( NCELL, MCT     )   ! NO3+MCT=MCTO+HNO3
     &    +                 RKI( NCELL, 159 ) * YC( NCELL, MPAN    )   ! NO3+MPAN=MACP+NO2
     &    +                 RKI( NCELL, 322 ) * YC( NCELL, MO2     )   ! NO3+MO2=HO2+HCHO+NO2
     &    +                 RKI( NCELL, 323 ) * YC( NCELL, ETHP    )   ! NO3+ETHP=HO2+NO2+ACD
     &    +                 RKI( NCELL, 324 ) * YC( NCELL, HC3P    )   ! NO3+HC3P=0.2540D+...
     &    +                 RKI( NCELL, 325 ) * YC( NCELL, HC5P    )   ! NO3+HC5P=0.4880D+...
     &    +                 RKI( NCELL, 326 ) * YC( NCELL, HC8P    )   ! NO3+HC8P=0.8200D+...
     &    +                 RKI( NCELL, 327 ) * YC( NCELL, ETEP    )   ! NO3+ETEP=HO2+NO2+...
     &    +                 RKI( NCELL, 328 ) * YC( NCELL, OLTP    )   ! NO3+OLTP=0.4700D+...
     &    +                 RKI( NCELL, 329 ) * YC( NCELL, OLIP    )   ! NO3+OLIP=0.8600D+...
     &    +                 RKI( NCELL, 330 ) * YC( NCELL, BENP    )   ! NO3+BENP=HO2+NO2+...
     &    +                 RKI( NCELL, 331 ) * YC( NCELL, TLP1    )   ! NO3+TLP1=NO2+BALD
     &    +                 RKI( NCELL, 332 ) * YC( NCELL, TOLP    )   ! NO3+TOLP=HO2+NO2+...
     &    +                 RKI( NCELL, 333 ) * YC( NCELL, PER1    )   ! NO3+PER1=0.5000D+...
     &    +                 RKI( NCELL, 334 ) * YC( NCELL, XYL1    )   ! NO3+XYL1=NO2+BALD
     &    +                 RKI( NCELL, 335 ) * YC( NCELL, XYLP    )   ! NO3+XYLP=HO2+NO2+...
     &    +                 RKI( NCELL, 336 ) * YC( NCELL, PER2    )   ! NO3+PER2=HO2+NO2+...
     &    +                 RKI( NCELL, 337 ) * YC( NCELL, XYOP    )   ! NO3+XYOP=HO2+NO2+...
     &    +                 RKI( NCELL, 338 ) * YC( NCELL, ISOP    )   ! NO3+ISOP=HO2+NO2+...
     &    +                 RKI( NCELL, 339 ) * YC( NCELL, APIP    )   ! NO3+APIP=HO2+NO2+...
     &    +                 RKI( NCELL, 340 ) * YC( NCELL, LIMP    )   ! NO3+LIMP=HO2+NO2+...
     &    +                 RKI( NCELL, 341 ) * YC( NCELL, ACO3    )   ! NO3+ACO3=MO2+NO2
     &    +                 RKI( NCELL, 342 ) * YC( NCELL, RCO3    )   ! NO3+RCO3=ETHP+NO2
     &    +                 RKI( NCELL, 343 ) * YC( NCELL, ACTP    )   ! NO3+ACTP=ACO3+...
     &    +                 RKI( NCELL, 344 ) * YC( NCELL, MEKP    )   ! NO3+MEKP=0.6700D+...
     &    +                 RKI( NCELL, 345 ) * YC( NCELL, KETP    )   ! NO3+KETP=HO2+NO2+...
     &    +                 RKI( NCELL, 346 ) * YC( NCELL, MACP    )   ! NO3+MACP=HCHO+...
     &    +                 RKI( NCELL, 347 ) * YC( NCELL, MCP     )   ! NO3+MCP=NO2+HO2+...
     &    +                 RKI( NCELL, 348 ) * YC( NCELL, MVKP    )   ! NO3+MVKP=0.3000D+...
     &    +                 RKI( NCELL, 349 ) * YC( NCELL, UALP    )   ! NO3+UALP=HO2+NO2+...
     &    +                 RKI( NCELL, 350 ) * YC( NCELL, BALP    )   ! NO3+BALP=BAL1+NO2
     &    +                 RKI( NCELL, 351 ) * YC( NCELL, BAL1    )   ! NO3+BAL1=BAL2+NO2
     &    +                 RKI( NCELL, 352 ) * YC( NCELL, ADDC    )   ! NO3+ADDC=HO2+NO2+...
     &    +                 RKI( NCELL, 353 ) * YC( NCELL, MCTP    )   ! NO3+MCTP=NO2+MCTO
     &    +                 RKI( NCELL, 354 ) * YC( NCELL, ORAP    )   ! NO3+ORAP=NO2+GLY+HO2
     &    +                 RKI( NCELL, 355 ) * YC( NCELL, OLNN    )   ! NO3+OLNN=HO2+NO2+...
     &    +                 RKI( NCELL, 356 ) * YC( NCELL, OLND    )   ! NO3+OLND=0.2000D+...
     &    +                 RKI( NCELL, 357 ) * YC( NCELL, ADCN    )   ! NO3+ADCN=0.2000D+...
     &    +                 RKI( NCELL, 361 ) * YC( NCELL, XO2     )   ! NO3+XO2=NO2

c..Loss frequency of N2O5
      L16 =                 RKI( NCELL,  64 )                   ! N2O5=NO2+NO3
     &    +                 RKI( NCELL,  65 )                   ! N2O5=0.2000D+01*HNO3
     &    +                 RKI( NCELL, 373 )                   ! N2O5=0.2000D+01*HNO3

c..K15_15, R15_16, and R16_15 terms
      K15_15  = RKI( NCELL,  62 ) * DTC

      R15_16  = ( RKI( NCELL,  64 ) ) * DTC 


      R16_15  = RKI( NCELL,  63 ) * YC( NCELL, NO2 ) * DTC

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
