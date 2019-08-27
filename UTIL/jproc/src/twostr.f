
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/twostr.f,v 1.5 2011/10/29 01:03:56 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)twostr.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.twostr.F 23 May 1997 12:44:32

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE TWOSTR ( NLEVEL, MU, RSFC, TAUU, OMU, GU,
     &                    FDR, FUP, FDN, EDR, EUP, EDN )

C*********************************************************************
C
C TWO-STREAM EQUATIONS FOR MULTIPLE LAYERS
C   based on equations from Toon et al.,  Journal of Geophysical Research
C   Volume 94, #D13  Nov. 20, 1989 Issue 
C   programmed by:  Kathleen G. Mosher
C   for Sasha Madronich, N.C.A.R. A.C.D.
C
C Now it contains 9 two-stream methods to choose from.
C   programmed  on 05.26.94 by:  Irina V.Petropavlovskikh
C   for Sasha Madronich, N.C.A.R. A.C.D.
C
C*********************************************************************

      IMPLICIT NONE

C...........PARAMETERS and their descriptions

      INTEGER, PARAMETER :: KZ = 200             ! maximum levels
      INTEGER, PARAMETER :: NROWS = 2 * KZ
      
      REAL, PARAMETER :: EPS = 1.0E-3
      REAL, PARAMETER :: PRECIS = 1.0E-7
      REAL, PARAMETER :: PI = 3.1415926535898

C...........ARGUMENTS and their descriptions

      INTEGER NLEVEL          ! number of levels

      REAL    MU              ! cosine of solar zenith angle
      REAL    RSFC            ! surface albedo
      REAL    EDN ( KZ )      ! 
      REAL    EDR ( KZ )      ! 
      REAL    EUP ( KZ )      ! 
      REAL    FDN ( KZ )      ! 
      REAL    FDR ( KZ )      ! 
      REAL    FUP ( KZ )      ! 
      REAL    GU  ( KZ )      ! unscaled asymmetry factor
      REAL    OMU ( KZ )      ! unscaled single scattering albedo
      REAL    TAUU( KZ )      ! unscaled optical depth of each layer


C...........LOCAL VARIABLES and their descriptions:

      INTEGER I
      INTEGER J
      INTEGER LEV
      INTEGER MROWS
      INTEGER NLAYER      ! number of layers in the atmosphere
      INTEGER ROW

      REAL DIVISR
      REAL DN
      REAL EXPON
      REAL EXPON0
      REAL EXPON1
      REAL F
      REAL FDN0
      REAL G
      REAL GAM1
      REAL GAM2
      REAL GAM3
      REAL GAM4
      REAL OM
      REAL PIFS
      REAL SSFC
      REAL TAU
      REAL TAUC
      REAL TAUG
      REAL TEMP
      REAL TEMPG
      REAL UP
      REAL BGAM ( KZ )
      REAL CDN  ( KZ )
      REAL CDNTN( KZ )
      REAL CUP  ( KZ )
      REAL CUPTN( KZ )
      REAL E1   ( KZ )
      REAL E2   ( KZ )
      REAL E3   ( KZ )
      REAL E4   ( KZ )
      REAL LAM  ( KZ )
      REAL MU1  ( KZ )
      REAL TAUN ( KZ )
      REAL A( NROWS )
      REAL B( NROWS )
      REAL D( NROWS )
      REAL E( NROWS )
      REAL Y( NROWS )

C...For calculations of Associated Legendre Polynomials for GAMA1,2,3,4
C    in delta-function, modified quadrature, hemispheric constant,
C    Hybrid modified Eddington-delta function metods, p633,Table1.
C    W.E.Meador and W.R.Weaver, GAS,1980,v37,p.630
C    W.J.Wiscombe and G.W. Grams, GAS,1976,v33,p2440, 
C...uncomment the following two lines and the appropriate statements further
C     down.
CCC     REAL YLM0, YLM2, YLM4, YLM6, YLM8, YLM10, YLM12, YLMS, BETA0,
CCC    &     BETA1, BETAn, amu1, subd

C*********************************************************************
C     begin body of subroutine TWOSTR

C...initial conditions:  pi*solar flux = 1;  diffuse incidence = 0

      PIFS = 1.0
      FDN0 = 0.0

      NLAYER = NLEVEL - 1

C...compute coefficients for each layer:
C...   GAM1 - GAM4 = 2-stream coefficients, different for
C...                 different approximations
C...   EXPON0 = calculation of e when TAU is zero
C...   EXPON1 = calculation of e when TAU is TAUN
C...   CUP and CDN = calculation when TAU is zero
C...   CUPTN and CDNTN = calc. when TAU is TAUN
C...   DIVISR = prevents division by zero

      TAUC = 0.
      DO I = 1, NLAYER

        G   = GU  ( I )
        TAU = TAUU( I )
        OM  = OMU ( I )

C...stay away from 1 by precision.  For g, also stay away from -1

        TEMPG = AMIN1( ABS( G ), 1.0 - PRECIS )
        G = SIGN( TEMPG, G )
        OM = AMIN1( OM, 1.0 - PRECIS )

C...delta-scaling. Have to be done for delta-Eddington approximation, 
C...  delta discrete ordinate, Practical Improved Flux Method, delta function,
C...  and Hybrid modified Eddington-delta function methods approximations

        F = G * G
        G = ( G - F ) / ( 1.0 - F )
        TAUN( I ) = ( 1.0 - OM * F ) * TAU
        OM = ( 1.0 - F ) * OM / ( 1.0 - OM * F )

C...the following gamma equations are from pg 16,289, Table 1

C...Eddington approximation(Joseph et al., 1976, JAS, 33, 2452):

        GAM1 =  ( 7.0 - OM * ( 4.0 + 3.0 * G ) ) / 4.0
        GAM2 = -( 1.0 - OM * ( 4.0 - 3.0 * G ) ) / 4.0
        GAM3 =  ( 2.0 - 3.0 * G * MU ) / 4.0
        GAM4 = 1.0 - GAM3

C...quadrature (Liou, 1973, JAS, 30, 1303-1326; 1974, JAS, 31, 1473-1475):
C        GAM1 = 1.7320508 * ( 2.0 - OM * ( 1.0 + G ) ) / 0.2
C        GAM2 = 1.7320508 * OM * ( 1.0 - G ) / 2.0
C        GAM3 = ( 1.0 - 1.7320508 * G * MU ) / 2.0
C        GAM4 = 1.0 - GAM3
         
C...hemispheric mean (Toon et al., 1089, JGR, 94, 16287):
C        GAM1 = 2.0 - OM * ( 1.0 + G )
C        GAM2 = OM * ( 1.0 - G )
C        GAM3 = ( 2.0 - G * MU ) / 4.0
C        GAM4 = 1.0 - GAM3

C...PIFM  (Zdunkovski et al.,1980, Conrib.Atmos.Phys., 53, 147-166):
C        GAM1 = 0.25 * ( 8.0 - OM * ( 5.0 + 3.0 * G ) )
C        GAM2 = 0.75 * OM * ( 1.0 - G )
C        GAM3 = 0.25 * ( 2.0 -3.0 * G * MU )
C        GAM4 = 1.0 - GAM3

C...delta discrete ordinates  (Schaller, 1979, Contrib.Atmos.Phys, 52, 17-26):
C        GAM1 = 0.5 * 1.7320508 * ( 2.0 - OM * ( 1.0 + G ) )
C        GAM2 = 0.5 * 1.7320508 * OM * ( 1.0 - G )
C        GAM3 = 0.5 * (1.0 - 1.7320508 * G * MU )
C        GAM4 = 1.0 - GAM3

C...Calculations of Associated Legendre Polynomials for GAMA1,2,3,4
C...  in delta-function, modified quadrature, hemispheric constant,
C...  Hybrid modified Eddington-delta function metods, p633,Table1.
C...  W.E.Meador and W.R.Weaver, GAS,1980,v37,p.630
C...  W.J.Wiscombe and G.W. Grams, GAS,1976,v33,p2440
C        YLM0 = 2.0
C        YLM2 = -3.0 * G * MU
C        YLM4 = 0.875 * G**3 * MU * ( 5.0 * MU**2 - 3.0 )
C        YLM6 = -0.171875 * G**5 * MU
C     &       * ( 15.0 - 70.0 * MU**2 + 63.0 * MU**4 )
C        YLM8 = 0.073242 * G**7 * MU
C     &       * ( -35.0 + 315.0 * MU**2 - 693.0 * MU**4 + 429.0 * MU**6 )
C        YLM10 = -0.008118 * G**9 * MU
C     &        * ( 315.0 - 4620.0 * MU**2 + 18018.0 * MU**4 - 25740.0
C     &        * MU**6 + 12155.0 * MU**8 )
C        YLM12 = 0.003685 * G**11 * MU
C     &        * ( -693.0 + 15015.0 * MU**2 - 90090.0 * MU**4 + 218790.0
C     &        * MU**6 - 230945.0 * MU**8 + 88179.0 * MU**10 )
C        YLMS = YLM0 + YLM2 + YLM4 + YLM6 + YLM8 + YLM10 + YLM12
C        YLMS = 0.25 * YLMS
C        BETA0 = YLMS
C
C        AMU1 = 1.0 / 1.7320508
C        YLM0 = 2.0
C        YLM2 = -3.0 * G * AMU1
C        YLM4 = 0.875 * G**3 * AMU1 * ( 5.0 * AMU1**2 - 3.0 )
C        YLM6 = -0.171875 * G**5 * AMU1
C     &       * ( 15.0 - 70.0 * AMU1**2 + 63.0 * AMU1**4 )
C        YLM8 = 0.073242 * G**7 * AMU1
C     &       * ( -35.0 + 315.0 * AMU1**2 - 693.0 * AMU1**4
C     &       + 429.0 * AMU1**6 )
C        YLM10 = -0.008118 * G**9 * AMU1
C     &        * ( 315.0 - 4620.0 * AMU1**2 + 18018.0 * AMU1**4
C     &        - 25740.0 * AMU1**6 + 12155.0 * AMU1**8 )
C        YLM12 = 0.003685 * G**11 * AMU1
C     &        * ( -693.0 + 15015.0 * AMU1**2 - 90090.0 * AMU1**4
C     &        + 218790.0 * AMU1**6 - 230945.0 * AMU1**8
C     &        + 88179.0 * AMU1**10 )
C        YLMS = YLM0 + YLM2 + YLM4 + YLM6 + YLM8 + YLM10 + YLM12
C        YLMS = 0.25 * YLMS
C        BETA1 = YLMS
C
C        BETAN = 0.25 * (2.0 - 1.5 * G - 0.21875 * G**3 - 0.085938 * G**5
C     &        - 0.045776 * G**7 )

C...Hybrid modified Eddington-delta function(Meador and Weaver,1980,JAS,37,630):
C        SUBD = 4.0 * (1.0 - G * G * ( 1.0 - MU ) )
C        GAM1 = ( 7.0 - 3.0 * G * G - OM * ( 4.0 + 3.0 * G )
C     &       + OM * G * G * ( 4.0 * BETA0 + 3.0 * G ) ) / SUBD
C        GAM2 = -( 1.0 - G * G - OM * ( 4.0 - 3.0 * G )
C     &       - OM * G * G * ( 4.0 * BETA0 + 3.0 * G - 4.0 ) ) / SUBD
C        GAM3 = BETA0
C        GAM4 = 1.0 - GAM3

C...delta function  (Meador, and Weaver, 1980, JAS, 37, 630):
C        GAM1 = ( 1.0 - OM * ( 1.0 - BETA0 ) ) / MU
C        GAM2 = OM * BETA0 / MU
C        GAM3 = BETA0
C        GAM4 = 1.0 - GAM3

C...modified quadrature (Meador, and Weaver, 1980, JAS, 37, 630):
C        GAM1 = 1.7320508 * ( 1.0 - OM * (1.0 - BETA1 ) )
C        GAM2 = 1.7320508 * OM * BETA1
C        GAM3 = BETA0
C        GAM4 = 1.0 - GAM3

C...hemispheric constant (Toon et al., 1989, JGR, 94, 16287):
C        GAM1 = 2.0 * (1.0 - OM * ( 1.0 - BETAN ) )
C        GAM2 = 2.0 * OM * BETAN
C        GAM3 = BETA0
C        GAM4 = 1.0 - GAM3

C...save mu1 for use in converting irradiance to actinic flux

        MU1( I ) = ( 1 - OM ) / ( GAM1 - GAM2 )

C...lambda = pg 16,290 equation 21
C...  big gamma = pg 16,290 equation 22
 
        LAM ( I ) = SQRT( GAM1 * GAM1 - GAM2 * GAM2 )
        BGAM( I ) = ( GAM1 - LAM( I ) ) / GAM2

        EXPON = EXP( -LAM( I ) * TAUN( I ) )

C...e1 - e4 = pg 16,292 equation 44
         
        E1( I ) = 1.0 + BGAM( I ) * EXPON
        E2( I ) = 1.0 - BGAM( I ) * EXPON
        E3( I ) = BGAM( I ) + EXPON
        E4( I ) = BGAM( I ) - EXPON

C...the following sets up for the C equations 23, and 24
C...  found on page 16,290
C...  prevent division by zero (if LAMBDA=1/MU, shift 1/MU^2 by EPS = 1.E-3
C...  which is approx equiv to shifting MU by 0.5*EPS* (MU)**3

        EXPON0 = EXP( -( TAUC ) / MU )
        EXPON1 = EXP( -( TAUC + TAUN( I ) ) / MU )

        DIVISR = LAM( I ) * LAM( I ) - 1.0 / ( MU * MU )
        TEMP   = AMAX1( EPS, ABS( DIVISR ) )
        DIVISR = SIGN( TEMP, DIVISR )

        UP = OM * PIFS
     &     * ( ( GAM1 - 1.0 / MU ) * GAM3 + GAM4 * GAM2 ) / DIVISR
        DN = OM * PIFS
     &     * ( ( GAM1 + 1.0 / MU ) * GAM4 + GAM2 * GAM3 ) / DIVISR
         
C...cup and cdn are when tau is equal to zero
C...  cuptn and cdntn are when tau is equal to taun

        CUP  ( I ) = UP * EXPON0
        CDN  ( I ) = DN * EXPON0
        CUPTN( I ) = UP * EXPON1
        CDNTN( I ) = DN * EXPON1

        TAUC = TAUC + TAUN( I )

      END DO

C...set up matrix
C...  ssfc = pg 16,292 equation 37  where pi Fs is one (unity).

      SSFC = RSFC * MU * EXP( -TAUC / MU ) * PIFS

C...MROWS = the number of rows in the matrix

      MROWS = 2 * NLAYER     
      
C...the following are from pg 16,292  equations 39 - 43.
C...   set up first row of matrix:

      I = 1
      A( 1 ) = 0.0
      B( 1 ) =  E1( I )
      D( 1 ) = -E2( I )
      E( 1 ) = FDN0 - CDN( I )

      ROW = 1

C...set up odd rows 3 thru (MROWS - 1):

      I = 0
      DO ROW = 3, MROWS - 1, 2
        I = I + 1
        A( ROW ) = E2( I ) * E3( I ) - E4( I ) * E1( I )
        B( ROW ) = E1( I ) * E1( I + 1 ) - E3( I ) * E3( I + 1 )
        D( ROW ) = E3( I ) * E4( I + 1 ) - E1( I ) * E2( I + 1 )
        E( ROW ) = E3( I ) * ( CUP( I + 1 ) - CUPTN( I ) )
     &           + E1( I ) * ( CDNTN( I ) - CDN( I + 1 ) )
      END DO

C...set up even rows 2 thru (MROWS - 2): 

      I = 0
      DO ROW = 2, MROWS - 2, 2
        I = I + 1
        A( ROW ) = E2( I + 1 ) * E1( I ) - E3( I ) * E4( I + 1 )
        B( ROW ) = E2( I ) * E2( I + 1 ) - E4( I ) * E4( I + 1 )
        D( ROW ) = E1( I + 1) * E4( I + 1 ) - E2( I + 1 ) * E3( I + 1 )
        E( ROW ) = ( CUP( I + 1 ) - CUPTN( I ) ) * E2( I + 1 )
     &           - ( CDN( I + 1 ) - CDNTN( I ) ) * E4( I + 1 )
      END DO

C...set up last row of matrix at MROWS:

      ROW = MROWS
      I   = NLAYER
      
      A( ROW ) = E1( I ) - RSFC * E3( I )
      B( ROW ) = E2( I ) - RSFC * E4( I )
      D( ROW ) = 0.0
      E( ROW ) = SSFC - CUPTN( I ) + RSFC * CDNTN( I )

C...solve tri-diagonal matrix:

      CALL TRIDIAG ( A, B, D, E, Y, MROWS )

C...unfold solution of matrix, compute output fluxes:

      ROW = 1 
      LEV = 1
      J   = 1
      TAUG = 0.0
      
C...the following equations are from pg 16,291  equations 31 & 32

      FDR( LEV ) = 1.0
      EDR( LEV ) = MU
      EDN( LEV ) = FDN0
      EUP( LEV ) = Y( ROW ) * E3( J ) - Y( ROW + 1 ) * E4( J )
     &           + CUP( J )
      FDN( LEV ) = EDN( LEV ) / MU1( LEV )
      FUP( LEV ) = EUP( LEV ) / MU1( LEV )

      DO LEV = 2, NLAYER + 1
        TAUG = TAUG + TAUN( J )
        FDR( LEV ) = EXP( -TAUG / MU )
        EDR( LEV ) = MU * FDR( LEV )
        EDN( LEV ) = Y( ROW ) * E3( J ) + Y( ROW + 1 ) * E4( J )
     &             + CDNTN( J )
        EUP( LEV ) = Y( ROW ) * E1( J ) + Y( ROW + 1 ) * E2( J )
     &             + CUPTN( J )
        FDN( LEV ) = EDN( LEV ) / MU1( J )
        FUP( LEV ) = EUP( LEV ) / MU1( J )

        ROW = ROW + 2
        J = J + 1
      END DO

      RETURN
      END
