
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/srband.f,v 1.5 2011/10/29 01:03:56 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)srband.F	1.1 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.srband.F 23 May 1997 12:44:30

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SRBAND ( NWL, STWL, MIDWL, ENDWL, COSZEN, NLAYS, AO2,
     &                    CVO2, VT, ZMID, O2ABS )
       
C*********************************************************************
C
C  this subroutine calculates effective O2 cross sections in the
C    Schumann-Runge band, using the formulae from Allen and Frederick,
C    J.Atmos.Sci., v39, p2066 (1982).  The coefficients are those
C    modified by Wuebbles (LLL report, 1982).
C
C*********************************************************************

      IMPLICIT NONE

C...........PARAMETERS and their descriptions

      INTEGER, PARAMETER :: MXWL  = 130 ! number of wavelength bands
      INTEGER, PARAMETER :: NJ    = 200 ! maximum levels
      INTEGER, PARAMETER :: NWLO2 = 11  ! # of wl bands for O2 SR data

C...........ARGUMENTS and their descriptions

      INTEGER      NWL                ! number of wavelength bands
      INTEGER      NLAYS              ! total # of atm layers

      REAL         COSZEN             ! cosine zenith angle
      REAL         STWL( MXWL )       ! wavelength bands starting point
      REAL         MIDWL( MXWL )      ! wavelength bands midpoint
      REAL         ENDWL( MXWL )      ! wavelength bands ending point
      REAL         CVO2( NJ )         ! vertical column O2
      REAL         VT  ( NJ )         ! average temp of column
      REAL         ZMID( NJ )         ! altitude of midpoint of layer
      REAL         AO2( NJ, MXWL )    ! layered O2 cross sections
      REAL         O2ABS( MXWL )      ! O2 cross sections

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(1), SAVE :: TYPE = 'B' ! cs spectra type (B=beginning wl)

      INTEGER      IWL                ! wavelength index
      INTEGER      IWLO2              ! wavelength index for ref O2 data
      INTEGER      ILAY               ! layer index
      INTEGER      N20                ! layer at 20 km

      REAL         E10                ! e**10
      REAL         X1                 ! 
      REAL         X2                 ! x1**2
      REAL         X3                 ! x1**3
      REAL         X4                 ! x1**4
      REAL         X5                 ! x1**5
      REAL         X6                 ! x1**6
      REAL         X7                 ! x1**7
      REAL         X8                 ! x1**8
      REAL         AO20               !
      REAL         AO20LG             !
      REAL         Y1                 ! 
      REAL         Y2                 ! y1**2
      REAL         Y3                 ! y1**3
      REAL         Y4                 ! y1**4
      REAL         C                  !
      REAL         CLOG               !
      REAL         ZENDEP             !
      REAL         AO2W1( NWLO2 )     ! O2 SR data on ref WL bands
      REAL         AO2W2( MXWL )      ! O2 SR data on ET wl bands
      REAL         WLO2L( NWLO2+1 )   ! lower wl's for O2 SR data
      DATA         WLO2L / 185.185, 186.916, 188.679, 190.476, 192.308,
     &                     194.175, 196.078, 198.020, 200.000, 202.020,
     &                     204.082, 206.186/
      SAVE         WLO2L

      REAL         SRA1( NWLO2 )      ! Schumann-Runge coef a1
      DATA SRA1 / -2.158311E+01, -2.184813E+01, -2.200507E+01,
     &            -2.205527E+01, -2.205261E+01, -2.228000E+01,
     &            -2.275796E+01, -2.297610E+01, -2.506084E+01,
     &            -2.313436E+01, -2.312205E+01 /
      SAVE         SRA1

      REAL         SRA2( NWLO2 )      ! Schumann-Runge coef a2
      DATA SRA2 / -4.164652E-01, -4.753880E-01, -4.628729E-01,
     &            -4.400848E-01, -5.707936E-01, -3.960759E-01,
     &            -2.054719E-01, -5.823677E-02,  3.442774E-02,
     &             1.177283E-04,  0.000000E+00 /
      SAVE         SRA2

      REAL         SRA3( NWLO2 )      ! Schumann-Runge coef a3
      DATA SRA3 /  5.266362E-02,  4.519945E-02, -5.022541E-02,
     &            -5.687308E-03, -3.330207E-02, -2.995798E-02,
     &            -1.094205E-02, -1.007612E-01, -2.212047E-04,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA3

      REAL         SRA4( NWLO2 )      ! Schumann-Runge coef a4
      DATA SRA4 /  1.655877E-02,  3.228313E-02,  2.545036E-02,
     &             3.712279E-02,  5.959032E-02,  4.918104E-02,
     &             2.079595E-02,  2.404666E-02,  6.186041E-07,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA4

      REAL         SRA5( NWLO2 )      ! Schumann-Runge coef a5
      DATA SRA5 /  0.000000E+00,  3.079373E-03,  5.791406E-02,
     &             6.025527E-03,  1.510540E-02,  9.269080E-03,
     &             3.769638E-03,  4.761876E-02, -6.284394E-10,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA5

      REAL         SRA6( NWLO2 )      ! Schumann-Runge coef a6
      DATA SRA6 /  0.000000E+00,  0.000000E+00,  1.179966E-02,
     &             0.000000E+00,  1.000376E-03, -1.173411E-03,
     &             0.000000E+00,  4.169606E-03,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA6

      REAL         SRA7( NWLO2 )      ! Schumann-Runge coef a7
      DATA SRA7 /  0.000000E+00,  0.000000E+00, -8.296876E-03,
     &             0.000000E+00,  0.000000E+00, -2.599386E-04,
     &             0.000000E+00, -7.126663E-03,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA7

      REAL         SRA8( NWLO2 )      ! Schumann-Runge coef a8
      DATA SRA8 /  0.000000E+00,  0.000000E+00, -3.238368E-03,
     &             0.000000E+00,  0.000000E+00,  0.000000E+00,
     &             0.000000E+00, -2.263652E-03,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA8

      REAL         SRA9( NWLO2 )      ! Schumann-Runge coef a9
      DATA SRA9 /  0.000000E+00,  0.000000E+00, -3.069686E-04,
     &             0.000000E+00,  0.000000E+00,  0.000000E+00,
     &             0.000000E+00, -1.971653E-04,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRA9

      REAL         SRB1( NWLO2 )      ! Schumann-Runge coef b1
      DATA SRB1 / -2.431640E+03, -3.701955E+01, -1.086239E+03,
     &            -1.213108E+03, -8.334575E+01, -2.139117E+02,
     &            -3.281301E+02,  3.033416E+03, -2.535815E+00,
     &            -4.474937E+00, -2.996639E+00 /
      SAVE         SRB1

      REAL         SRB2( NWLO2 )      ! Schumann-Runge coef b2
      DATA SRB2 /  4.729722E+02,  3.623290E+00,  1.981847E+02,
     &             2.277459E+02,  7.944254E+00,  2.612729E+01,
     &             4.307004E+01, -5.978911E+02,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRB2

      REAL         SRB3( NWLO2 )      ! Schumann-Runge coef b3
      DATA SRB3 / -3.452121E+01, -8.929223E-02, -1.359057E+01,
     &            -1.612207E+01, -1.898894E-01, -1.036749E+00,
     &            -1.870019E+00,  4.370384E+01,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRB3

      REAL         SRB4( NWLO2 )      ! Schumann-Runge coef b4
      DATA SRB4 /  1.120677E+00,  0.000000E+00,  4.155845E-01,
     &             5.101389E-01,  0.000000E+00,  1.317695E-02,
     &             2.674331E-02, -1.406715E+00,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRB4

      REAL         SRB5( NWLO2 )      ! Schumann-Runge coef b5
      DATA SRB5 / -1.365618E-02,  0.000000E+00, -4.788462E-03,
     &            -6.090518E-03,  0.000000E+00,  0.000000E+00,
     &             0.000000E+00,  1.683967E-02,  0.000000E+00,
     &             0.000000E+00,  0.000000E+00 /
      SAVE         SRB5

C*********************************************************************
C     begin body of subroutine SRBAND

C...initialize cross sections:
           
      DO ILAY = 1, NLAYS
        DO IWL = 1, NWL
          AO2( ILAY, IWL ) = O2ABS( IWL )
        END DO
      END DO

C...correct as needed
C... use formula for 20-50 km.
C... below 20 km, use 20 km value
C... find layer near 20 km

      DO ILAY = 1, NLAYS
        IF ( ZMID( ILAY ) .GT. 20.0 ) THEN
          N20 = ILAY
          GO TO 301
        END IF
      END DO

301   CONTINUE

      E10 = ALOG( 10.0 )
      DO ILAY = N20, NLAYS
      
        DO IWLO2 = 1, NWLO2
        
          X1 = ALOG( 4.696E-23 * CVO2( ILAY ) / 0.2095 ) / E10
          IF ( WLO2L( IWLO2 ) .GE. 200.0 ) X1 = VT( ILAY )
          X2 = X1 * X1
          X3 = X2 * X1
          X4 = X3 * X1
          X5 = X4 * X1
          X6 = X5 * X1
          X7 = X6 * X1
          X8 = X7 * X1
          AO20LG = SRA1( IWLO2 )      + SRA2( IWLO2 ) * X1
     &           + SRA3( IWLO2 ) * X2 + SRA4( IWLO2 ) * X3
     &           + SRA5( IWLO2 ) * X4 + SRA6( IWLO2 ) * X5
     &           + SRA7( IWLO2 ) * X6 + SRA8( IWLO2 ) * X7
     &           + SRA9( IWLO2 ) * X8
          AO20 = 10.0**AO20LG

          Y1 = ALOG( CVO2( ILAY ) ) / E10
          Y2 = Y1 * Y1
          Y3 = Y2 * Y1
          Y4 = Y3 * Y1
          CLOG = SRB1( IWLO2 )      + SRB2( IWLO2 ) * Y1
     &         + SRB3( IWLO2 ) * Y2 + SRB4( IWLO2 ) * Y3
     &         + SRB5( IWLO2 ) * Y4
          C = 10.0**CLOG
          ZENDEP = COSZEN**C
          
          AO2W1( IWLO2 ) = AO20 * ZENDEP
          
        END DO

C...now transfrom the computed O2 data from their
C...  reference wavelength bands to the same wavelength bands
C...  as the extraterrestrial irradiance data

        CALL INTAVG ( WLO2L, AO2W1, NWLO2+1, TYPE,
     &                STWL,  ENDWL, AO2W2, NWL )

        DO IWL = 1, NWL
          IF ( ENDWL( IWL ) .LE. 205.0 ) THEN
            AO2( ILAY, IWL ) = AO2W2( IWL )
          END IF
        END DO

      END DO

C...assign values below 20 km

      DO ILAY = 1, N20 - 1
        DO IWL = 1, NWL
          AO2( ILAY, IWL ) = AO2( N20, IWL )
        END DO
      END DO

      RETURN
      END
