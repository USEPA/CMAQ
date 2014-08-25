
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
      O1D_S =                 RKI( NCELL,  40 )                   ! O1D=O3P
     &      +                 RKI( NCELL,  41 )                   ! O1D=O3P
     &      +                 RKI( NCELL,  42 )                   ! O1D=0.2000D+01*HO

      O3P_S =                 RKI( NCELL,  40 )                   ! O1D=O3P
     &      +                 RKI( NCELL,  41 )                   ! O1D=O3P

      O3P_S  = O3P_S / O1D_S


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  NO Section
c    R1_2 = production of NO from NO2 ( rates of form k[NO2][x] )
c           except NO2+NO3=NO+NO2 (it is treated as if it were NO3=NO )
c    P1 =   remaining NO production terms
c    L1 =   loss of NO (except rxns producing NO2 - they are in R2_1)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      R1_2 =                 RKI( NCELL,   4 )                   ! NO2=NO+O3P
     &     +                 RKI( NCELL,  54 ) * YC( NCELL, O3P     )   ! NO2+O3P=NO
      R1_2  = R1_2 * DTC


      P1 =                 RXRAT( NCELL,   5 )      ! NO3=NO
     &   +                 RXRAT( NCELL,   7 )      ! HONO=NO+HO
     &   +                 RXRAT( NCELL,  61 )      ! NO2+NO3=NO+NO2
      P1    = YC0( NCELL, NO ) + P1 * DTC


      L1 =                 RKI( NCELL,  49 ) * YC( NCELL, HO      )   ! NO+HO=HONO
     &   +                 RKI( NCELL,  51 ) * YC( NCELL, HO2     )   ! NO+HO2=HNO3
     &   +    6.5000D-02 * RKI( NCELL, 174 ) * YC( NCELL, HC3P    )   ! NO+HC3P=0.6600D+...
     &   +    1.3600D-01 * RKI( NCELL, 175 ) * YC( NCELL, HC5P    )   ! NO+HC5P=0.2000D+...
     &   +    2.6100D-01 * RKI( NCELL, 176 ) * YC( NCELL, HC8P    )   ! NO+HC8P=0.6060D+...
     &   +    3.0000D-02 * RKI( NCELL, 178 ) * YC( NCELL, OLTP    )   ! NO+OLTP=0.7800D+...
     &   +    5.0000D-02 * RKI( NCELL, 179 ) * YC( NCELL, OLIP    )   ! NO+OLIP=0.8300D+...
     &   +    8.2000D-02 * RKI( NCELL, 180 ) * YC( NCELL, BENP    )   ! NO+BENP=0.9180D+...
     &   +    5.0000D-02 * RKI( NCELL, 182 ) * YC( NCELL, TOLP    )   ! NO+TOLP=0.9500D+...
     &   +    5.0000D-02 * RKI( NCELL, 183 ) * YC( NCELL, PER1    )   ! NO+PER1=0.5000D+...
     &   +    5.0000D-02 * RKI( NCELL, 185 ) * YC( NCELL, XYLP    )   ! NO+XYLP=0.9500D+...
     &   +    5.0000D-02 * RKI( NCELL, 186 ) * YC( NCELL, PER2    )   ! NO+PER2=0.9500D+...
     &   +    5.0000D-02 * RKI( NCELL, 187 ) * YC( NCELL, XYOP    )   ! NO+XYOP=0.9500D+...
     &   +    1.2000D-01 * RKI( NCELL, 188 ) * YC( NCELL, ISOP    )   ! NO+ISOP=0.8800D+...
     &   +    1.8000D-01 * RKI( NCELL, 189 ) * YC( NCELL, APIP    )   ! NO+APIP=0.8200D+...
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
      R2_1 =                 RKI( NCELL,  48 ) * YC( NCELL, O3P     )   ! NO+O3P=NO2
     &     +                 RKI( NCELL,  50 ) * YC( NCELL, HO2     )   ! NO+HO2=NO2+HO
     &     +    2.0000D+00 * RKI( NCELL,  52 ) * YC( NCELL, NO      )   ! NO+NO=0.2000D+01*NO2
     &     +                 RKI( NCELL,  60 ) * YC( NCELL, NO3     )   ! NO+NO3=0.2000D+...
     &     +                 RKI( NCELL, 172 ) * YC( NCELL, MO2     )   ! NO+MO2=NO2+HO2+HCHO
     &     +                 RKI( NCELL, 173 ) * YC( NCELL, ETHP    )   ! NO+ETHP=NO2+HO2+ACD
     &     +    9.3500D-01 * RKI( NCELL, 174 ) * YC( NCELL, HC3P    )   ! NO+HC3P=0.9350D+...
     &     +    8.6400D-01 * RKI( NCELL, 175 ) * YC( NCELL, HC5P    )   ! NO+HC5P=0.8640D+...
     &     +    7.3900D-01 * RKI( NCELL, 176 ) * YC( NCELL, HC8P    )   ! NO+HC8P=0.7390D+...
     &     +                 RKI( NCELL, 177 ) * YC( NCELL, ETEP    )   ! NO+ETEP=NO2+HO2+...
     &     +    9.7000D-01 * RKI( NCELL, 178 ) * YC( NCELL, OLTP    )   ! NO+OLTP=0.9700D+...
     &     +    9.5000D-01 * RKI( NCELL, 179 ) * YC( NCELL, OLIP    )   ! NO+OLIP=0.9500D+...
     &     +    9.1800D-01 * RKI( NCELL, 180 ) * YC( NCELL, BENP    )   ! NO+BENP=0.9180D+...
     &     +                 RKI( NCELL, 181 ) * YC( NCELL, TLP1    )   ! NO+TLP1=NO2+BALD
     &     +    9.5000D-01 * RKI( NCELL, 182 ) * YC( NCELL, TOLP    )   ! NO+TOLP=0.9500D+...
     &     +    9.5000D-01 * RKI( NCELL, 183 ) * YC( NCELL, PER1    )   ! NO+PER1=0.9500D+...
     &     +                 RKI( NCELL, 184 ) * YC( NCELL, XYL1    )   ! NO+XYL1=NO2+BALD
     &     +    9.5000D-01 * RKI( NCELL, 185 ) * YC( NCELL, XYLP    )   ! NO+XYLP=0.9500D+...
     &     +    9.5000D-01 * RKI( NCELL, 186 ) * YC( NCELL, PER2    )   ! NO+PER2=0.9500D+...
     &     +    9.5000D-01 * RKI( NCELL, 187 ) * YC( NCELL, XYOP    )   ! NO+XYOP=0.9500D+...
     &     +    8.8000D-01 * RKI( NCELL, 188 ) * YC( NCELL, ISOP    )   ! NO+ISOP=0.8800D+...
     &     +    8.2000D-01 * RKI( NCELL, 189 ) * YC( NCELL, APIP    )   ! NO+APIP=0.8200D+...
     &     +                 RKI( NCELL, 190 ) * YC( NCELL, LIMP    )   ! NO+LIMP=NO2+...
     &     +                 RKI( NCELL, 191 ) * YC( NCELL, ACO3    )   ! NO+ACO3=NO2+MO2
     &     +                 RKI( NCELL, 192 ) * YC( NCELL, RCO3    )   ! NO+RCO3=NO2+ETHP
     &     +                 RKI( NCELL, 193 ) * YC( NCELL, ACTP    )   ! NO+ACTP=NO2+ACO3+...
     &     +                 RKI( NCELL, 194 ) * YC( NCELL, MEKP    )   ! NO+MEKP=NO2+...
     &     +                 RKI( NCELL, 195 ) * YC( NCELL, KETP    )   ! NO+KETP=NO2+...
     &     +                 RKI( NCELL, 196 ) * YC( NCELL, MACP    )   ! NO+MACP=NO2+...
     &     +                 RKI( NCELL, 197 ) * YC( NCELL, MCP     )   ! NO+MCP=NO2+...
     &     +                 RKI( NCELL, 198 ) * YC( NCELL, MVKP    )   ! NO+MVKP=NO2+...
     &     +                 RKI( NCELL, 199 ) * YC( NCELL, UALP    )   ! NO+UALP=NO2+HO2+...
     &     +                 RKI( NCELL, 200 ) * YC( NCELL, BALP    )   ! NO+BALP=NO2+BAL1
     &     +                 RKI( NCELL, 201 ) * YC( NCELL, BAL1    )   ! NO+BAL1=NO2+BAL2
     &     +                 RKI( NCELL, 202 ) * YC( NCELL, ADDC    )   ! NO+ADDC=NO2+HO2+...
     &     +                 RKI( NCELL, 203 ) * YC( NCELL, MCTP    )   ! NO+MCTP=NO2+MCTO
     &     +                 RKI( NCELL, 204 ) * YC( NCELL, ORAP    )   ! NO+ORAP=NO2+GLY+HO2
     &     +                 RKI( NCELL, 205 ) * YC( NCELL, OLNN    )   ! NO+OLNN=NO2+HO2+ONIT
     &     +                 RKI( NCELL, 206 ) * YC( NCELL, OLND    )   ! NO+OLND=0.2000D+...
     &     +                 RKI( NCELL, 207 ) * YC( NCELL, ADCN    )   ! NO+ADCN=0.2000D+...
     &     +                 RKI( NCELL, 208 ) * YC( NCELL, XO2     )   ! NO+XO2=NO2
      R2_1  = R2_1 * DTC


      P2 =                 RXRAT( NCELL,   6 )      ! NO3=NO2+O3P
     &   +                 RXRAT( NCELL,   8 )      ! HNO3=NO2+HO
     &   +    8.0000D-01 * RXRAT( NCELL,   9 )      ! HNO4=0.8000D+00*NO2+...
     &   +                 RXRAT( NCELL,  31 )      ! ONIT=NO2+HO2+0.2000D+...
     &   +                 RXRAT( NCELL,  32 )      ! PAN=NO2+ACO3
     &   +                 RXRAT( NCELL,  53 )      ! HONO+HO=NO2
     &   +                 RXRAT( NCELL,  58 )      ! NO3+HO=NO2+HO2
     &   +    7.0000D-01 * RXRAT( NCELL,  59 )      ! NO3+HO2=0.7000D+...
     &   +                 RXRAT( NCELL,  60 )      ! NO+NO3=0.2000D+01*NO2
     &   +    2.0000D+00 * RXRAT( NCELL,  62 )      ! NO3+NO3=0.2000D+01*NO2
     &   +                 RXRAT( NCELL,  64 )      ! N2O5=NO2+NO3
     &   +                 RXRAT( NCELL,  67 )      ! HNO4=NO2+HO2
     &   +                 RXRAT( NCELL,  68 )      ! HNO4+HO=NO2
     &   +                 RXRAT( NCELL, 122 )      ! MPAN+HO=NO2+HKET
     &   +                 RXRAT( NCELL, 123 )      ! ONIT+HO=NO2+HC3P
     &   +                 RXRAT( NCELL, 124 )      ! NALD+HO=NO2+XO2+HKET
     &   +    6.8000D-01 * RXRAT( NCELL, 151 )      ! MACR+NO3=0.6800D+...
     &   +    5.0000D-01 * RXRAT( NCELL, 157 )      ! EPX+NO3=0.5000D+...
     &   +                 RXRAT( NCELL, 159 )      ! MPAN+NO3=NO2+MACP
     &   +                 RXRAT( NCELL, 167 )      ! PAN=NO2+ACO3
     &   +                 RXRAT( NCELL, 169 )      ! PPN=NO2+RCO3
     &   +                 RXRAT( NCELL, 171 )      ! MPAN=NO2+MACP
     &   +                 RXRAT( NCELL, 206 )      ! NO+OLND=0.2000D+...
     &   +                 RXRAT( NCELL, 207 )      ! NO+ADCN=0.2000D+...
     &   +                 RXRAT( NCELL, 273 )      ! MCP+MO2=NO2+HO2+...
     &   +    5.0000D-01 * RXRAT( NCELL, 282 )      ! OLND+MO2=0.5000D+...
     &   +    7.0000D-01 * RXRAT( NCELL, 283 )      ! ADCN+MO2=0.7000D+...
     &   +                 RXRAT( NCELL, 309 )      ! MCP+ACO3=NO2+0.5000D+...
     &   +                 RXRAT( NCELL, 318 )      ! OLND+ACO3=NO2+0.5000D+...
     &   +    7.0000D-01 * RXRAT( NCELL, 319 )      ! ADCN+ACO3=0.7000D+...
     &   +                 RXRAT( NCELL, 322 )      ! MO2+NO3=NO2+HCHO+HO2
     &   +                 RXRAT( NCELL, 323 )      ! ETHP+NO3=NO2+HO2+ACD
     &   +                 RXRAT( NCELL, 324 )      ! HC3P+NO3=NO2+0.1400D+...
     &   +                 RXRAT( NCELL, 325 )      ! HC5P+NO3=NO2+0.5500D-...
     &   +                 RXRAT( NCELL, 326 )      ! HC8P+NO3=NO2+0.1800D+...
     &   +                 RXRAT( NCELL, 327 )      ! ETEP+NO3=NO2+HO2+...
     &   +                 RXRAT( NCELL, 328 )      ! OLTP+NO3=NO2+0.7900D+...
     &   +                 RXRAT( NCELL, 329 )      ! OLIP+NO3=NO2+0.7200D+...
     &   +                 RXRAT( NCELL, 330 )      ! BENP+NO3=NO2+HO2+...
     &   +                 RXRAT( NCELL, 331 )      ! TLP1+NO3=NO2+BALD
     &   +                 RXRAT( NCELL, 332 )      ! TOLP+NO3=NO2+HO2+DCB2
     &   +                 RXRAT( NCELL, 333 )      ! PER1+NO3=NO2+0.5000D+...
     &   +                 RXRAT( NCELL, 334 )      ! XYL1+NO3=NO2+BALD
     &   +                 RXRAT( NCELL, 335 )      ! XYLP+NO3=NO2+HO2+DCB3
     &   +                 RXRAT( NCELL, 336 )      ! PER2+NO3=NO2+HO2+MGLY+...
     &   +                 RXRAT( NCELL, 337 )      ! XYOP+NO3=NO2+HO2+...
     &   +                 RXRAT( NCELL, 338 )      ! ISOP+NO3=NO2+HO2+...
     &   +                 RXRAT( NCELL, 339 )      ! APIP+NO3=NO2+HO2+ALD+KET
     &   +                 RXRAT( NCELL, 340 )      ! LIMP+NO3=NO2+HO2+...
     &   +                 RXRAT( NCELL, 341 )      ! ACO3+NO3=NO2+MO2
     &   +                 RXRAT( NCELL, 342 )      ! RCO3+NO3=NO2+ETHP
     &   +                 RXRAT( NCELL, 343 )      ! ACTP+NO3=NO2+ACO3+HCHO
     &   +                 RXRAT( NCELL, 344 )      ! MEKP+NO3=NO2+0.6700D+...
     &   +                 RXRAT( NCELL, 345 )      ! KETP+NO3=NO2+HO2+DCB1
     &   +                 RXRAT( NCELL, 346 )      ! MACP+NO3=NO2+0.5380D+...
     &   +                 RXRAT( NCELL, 347 )      ! MCP+NO3=NO2+HO2+HCHO+HKET
     &   +                 RXRAT( NCELL, 348 )      ! MVKP+NO3=NO2+0.7000D+...
     &   +                 RXRAT( NCELL, 349 )      ! UALP+NO3=NO2+HO2+...
     &   +                 RXRAT( NCELL, 350 )      ! BALP+NO3=NO2+BAL1
     &   +                 RXRAT( NCELL, 351 )      ! BAL1+NO3=NO2+BAL2
     &   +                 RXRAT( NCELL, 352 )      ! ADDC+NO3=NO2+HO2+...
     &   +                 RXRAT( NCELL, 353 )      ! MCTP+NO3=NO2+MCTO
     &   +                 RXRAT( NCELL, 354 )      ! ORAP+NO3=NO2+GLY+HO2
     &   +                 RXRAT( NCELL, 355 )      ! OLNN+NO3=NO2+HO2+ONIT
     &   +    2.0000D+00 * RXRAT( NCELL, 356 )      ! OLND+NO3=0.2000D+...
     &   +    2.0000D+00 * RXRAT( NCELL, 357 )      ! ADCN+NO3=0.2000D+...
     &   +    5.0000D-01 * RXRAT( NCELL, 359 )      ! OLNN+OLND=0.5000D+...
     &   +                 RXRAT( NCELL, 360 )      ! OLND+OLND=NO2+0.5040D+...
     &   +                 RXRAT( NCELL, 361 )      ! XO2+NO3=NO2
      P2 = YC0( NCELL, NO2 ) + P2 * DTC


      L2 =                 RKI( NCELL,  37 ) * YC( NCELL, O3      )   ! NO2+O3=NO3
     &   +                 RKI( NCELL,  55 ) * YC( NCELL, O3P     )   ! NO2+O3P=NO3
     &   +                 RKI( NCELL,  56 ) * YC( NCELL, HO      )   ! NO2+HO=HNO3
     &   +                 RKI( NCELL,  63 ) * YC( NCELL, NO3     )   ! NO2+NO3=N2O5
     &   +                 RKI( NCELL,  66 ) * YC( NCELL, HO2     )   ! NO2+HO2=HNO4
     &   +                 RKI( NCELL, 166 ) * YC( NCELL, ACO3    )   ! NO2+ACO3=PAN
     &   +                 RKI( NCELL, 168 ) * YC( NCELL, RCO3    )   ! NO2+RCO3=PPN
     &   +                 RKI( NCELL, 170 ) * YC( NCELL, MACP    )   ! NO2+MACP=MPAN
     &   +                 RKI( NCELL, 209 ) * YC( NCELL, BAL2    )   ! NO2+BAL2=ONIT
     &   +                 RKI( NCELL, 210 ) * YC( NCELL, CHO     )   ! NO2+CHO=ONIT
     &   +                 RKI( NCELL, 211 ) * YC( NCELL, MCTO    )   ! NO2+MCTO=ONIT
     &   +                 RKI( NCELL, 374 )                   ! NO2=0.5000D+...
      L2     = 1.0D0 + L2 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3 Section
c    P3 = production of O3 except O+O2=O3
c    L3 =   loss terms for O3 except NO+O3=NO2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P3 = YC0( NCELL, O3 ) 


      L3 =                 RKI( NCELL,   1 )                   ! O3=O3P
     &   +                 RKI( NCELL,   2 )                   ! O3=O1D
     &   +                 RKI( NCELL,  34 ) * YC( NCELL, HO      )   ! O3+HO=HO2
     &   +                 RKI( NCELL,  35 ) * YC( NCELL, HO2     )   ! O3+HO2=HO
     &   +                 RKI( NCELL,  37 ) * YC( NCELL, NO2     )   ! O3+NO2=NO3
     &   +                 RKI( NCELL,  39 ) * YC( NCELL, O3P     )   ! O3+O3P=
     &   +                 RKI( NCELL, 126 ) * YC( NCELL, ETE     )   ! O3+ETE=0.8000D-...
     &   +                 RKI( NCELL, 127 ) * YC( NCELL, OLT     )   ! O3+OLT=0.2200D+...
     &   +                 RKI( NCELL, 128 ) * YC( NCELL, OLI     )   ! O3+OLI=0.4600D+...
     &   +                 RKI( NCELL, 129 ) * YC( NCELL, DIEN    )   ! O3+DIEN=0.9000D-...
     &   +                 RKI( NCELL, 130 ) * YC( NCELL, ISO     )   ! O3+ISO=0.2500D+...
     &   +                 RKI( NCELL, 131 ) * YC( NCELL, API     )   ! O3+API=0.8500D+...
     &   +                 RKI( NCELL, 132 ) * YC( NCELL, LIM     )   ! O3+LIM=0.8500D+...
     &   +                 RKI( NCELL, 133 ) * YC( NCELL, MACR    )   ! O3+MACR=0.1900D+...
     &   +                 RKI( NCELL, 134 ) * YC( NCELL, MVK     )   ! O3+MVK=0.1600D+...
     &   +                 RKI( NCELL, 135 ) * YC( NCELL, UALD    )   ! O3+UALD=0.1000D+...
     &   +                 RKI( NCELL, 136 ) * YC( NCELL, DCB1    )   ! O3+DCB1=0.5000D-...
     &   +                 RKI( NCELL, 137 ) * YC( NCELL, DCB2    )   ! O3+DCB2=0.5000D-...
     &   +                 RKI( NCELL, 138 ) * YC( NCELL, DCB3    )   ! O3+DCB3=0.5000D-...
     &   +                 RKI( NCELL, 139 ) * YC( NCELL, EPX     )   ! O3+EPX=0.5000D-...
     &   +                 RKI( NCELL, 140 ) * YC( NCELL, MCTO    )   ! O3+MCTO=MCTP
      L3    = 1.0D0 + L3 * DTC


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  O3P Section 
c    P12 = production of O3P except NO2+hv=O3P (J1)
c    L12 = loss terms
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      P12 =                 RXRAT( NCELL,   1 )      ! O3=O3P
     &    +   O3P_S *       RXRAT( NCELL,   2 )      ! O3=O1D
     &    +                 RXRAT( NCELL,   6 )      ! NO3=O3P+NO2
     &    +    9.0000D-02 * RXRAT( NCELL, 129 )      ! DIEN+O3=0.9000D-...
      P12 = YC0( NCELL, O3P ) + P12 * DTC


      L12 =                 RKI( NCELL,  38 )                   ! O3P=O3
     &    +                 RKI( NCELL,  39 ) * YC( NCELL, O3      )   ! O3P+O3=
     &    +                 RKI( NCELL,  48 ) * YC( NCELL, NO      )   ! O3P+NO=NO2
     &    +                 RKI( NCELL,  54 ) * YC( NCELL, NO2     )   ! O3P+NO2=NO
     &    +                 RKI( NCELL,  55 ) * YC( NCELL, NO2     )   ! O3P+NO2=NO3
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
      RK1 = RKI( NCELL,   4 ) * DTC            ! J1    (NO2+hv=NO+O3P)
      RK2 = RKI( NCELL,  38 ) * DTC            ! J2    (O3P+O2=O3)
      RK3 = RKI( NCELL,  36 ) * DTC            ! k1_3  (NO+O3=NO2)

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

      YCP( NCELL, O1D ) = RKI( NCELL, 2 ) * YCP( NCELL, O3 ) / O1D_S

      RETURN

      END


