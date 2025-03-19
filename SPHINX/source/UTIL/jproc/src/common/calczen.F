
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/calczen.F,v 1.5 2011/10/29 01:03:52 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)calczen.F	1.2 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.calczen.F 04 Jun 1997 10:48:01

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE CALCZEN ( LAT, LONG, IDATE, GMT, ZENITH )
        
C*********************************************************************
C
C  this subroutine calculates solar zenith angle for a
C     particulat time and location.  Must specify:
C  INPUT:
c       LAT - latitude in decimal degrees
C       LONG - longitude in decimal degrees
C       IDATE - Date at Greenwich - specify year (yyyy), month (mm),
C               day format is six-digit integer:  yyyymmdd
C       GMT  - Greenwich mean time - decimal military eg.
C               22.75 = 45 min after ten pm gmt
C  OUTPUT
C       Zenith
C
C*********************************************************************

      IMPLICIT NONE

      INCLUDE SUBST_CONST        ! commonly used constants

C...........ARGUMENTS and their descriptions:

      INTEGER      IDATE              ! date (yyyymmdd)

      REAL         LAT                ! latitude (decimal degrees)
      REAL         LONG               ! longitude (decimal degrees)
      REAL         GMT                ! Greenwich mean time (dec.milt)
      REAL         ZENITH             ! zenith angle (degrees)
            
C...........LOCAL VARIABLES and their descriptions:

      INTEGER      IMN( 12 )          ! number of days in each month
      DATA IMN /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      SAVE         IMN

      INTEGER      IIYEAR             ! year (yyyy)
      INTEGER      LEAP               ! # leap years since 1974
      INTEGER      NYEARS             ! years since 1974
      INTEGER      NOLEAP             ! # nonleap years since 1974
      INTEGER      IJD                ! julian days (ddd)
      INTEGER      IN                 ! month pointer (mm-1)
      INTEGER      IMTH               ! month (mm)
      INTEGER      I                  ! month index
      INTEGER      IDAY               ! day (dd)
      INTEGER      JD                 ! julian days since 1974 ref
        
      REAL         LBGMT              ! 
      REAL         LZGMT              ! 
      REAL         ML                 ! geometric mean longitude (deg)
      REAL         RLT                ! latitude (radians)
      REAL         YREF               ! number of days to 1974
      REAL         YR                 ! number of days to yyyy
      REAL         D                  ! jul. days since 1974 + hh frac
      REAL         RML                ! geometric mean long (rad)
      REAL         W                  ! mean longitude of perigee (deg)
      REAL         EC                 ! eccentricity 
      REAL         EPSI               ! mean obliquity of ecliptic (deg)
      REAL         YT                 ! tan**2 (mean obl. of eclipt.)
      REAL         PEPSI              ! mean obliquity of ecliptic (rad)
      REAL         CW                 ! cosine mean long. perigee
      REAL         WR                 ! mean longitude of perigee (rad)
      REAL         SW                 ! sine mean long. perigee
      REAL         SSW                ! sine 2*mean long. perigee
      REAL         EYT                !
      REAL         FEQT               !
      REAL         FEQT1              !
      REAL         FEQT2              !
      REAL         FEQT3              !
      REAL         FEQT4              !
      REAL         FEQT5              !
      REAL         FEQT6              !
      REAL         FEQT7              !
      REAL         REQT               !
      REAL         EQT                !
      REAL         RA                 ! right ascension (degrees)
      REAL         TAB                !
      REAL         RRA                ! right ascension (radians)
      REAL         RDECL              ! declination angle (rad)
      REAL         CSZ                ! cosine (zenith angle)
      REAL         ZPT                ! 
      REAL         ZR                 ! zenith angle (radians)
      
C*********************************************************************
C     begin body of subroutine CALZEN2        

C...convert to radians

      RLT = LAT * PI180

C...parse date

      IIYEAR = IDATE / 10000
      IMTH = ( IDATE - IIYEAR * 10000 ) / 100
      IDAY = IDATE - IIYEAR * 10000 - IMTH * 100

C...identify and correct leap years

      IF ( MOD( IIYEAR, 4 ) .EQ. 0 ) THEN
         IMN( 2 ) = 29
      ELSE
         IMN( 2 ) = 28
      END IF

C...count days from Dec.31,1973 to Jan 1, YEAR, then add to 2,442,047.5

      YREF =  2442047.5
      NYEARS = IIYEAR - 1974
      LEAP = ( NYEARS + 1 ) / 4
      IF ( NYEARS .LE. -1 ) LEAP = ( NYEARS - 2 ) / 4
      NOLEAP = NYEARS - LEAP
      YR = YREF + 365.0 * NOLEAP + 366.0 * LEAP

      IJD = 0
      IN = IMTH - 1
      
      IF ( IN .EQ. 0 ) THEN
        IJD = IDAY
      ELSE IF ( IN .GT. 0 ) THEN      
        DO 101 I = 1, IN
          IJD = IJD + IMN( I )
101     CONTINUE
        IJD = IJD + IDAY
      END IF

C...print julian days current "ijd"

      JD = IJD + ( YR - YREF )
      D = JD + GMT / 24.0

C...calc geom mean longitude

      ML = 279.2801988 + 0.9856473354 * D + 2.267E-13 * D * D
      RML = ML * PI180

C...calc equation of time in sec
C... w = mean long of perigee
C... e = eccentricity
C... epsi = mean obliquity of ecliptic

      W = 282.4932328 + 4.70684E-5 * D + 3.39E-13 * D * D
      WR = W * PI180
      EC = 1.6720041E-2 - 1.1444E-9 * D - 9.4E-17 * D * D
      EPSI = 23.44266511 - 3.5626E-7 * D - 1.23E-15 * D * D
      PEPSI = EPSI * PI180
      YT = ( TAN( PEPSI / 2.0 ) )**2
      CW = COS( WR )
      SW = SIN( WR )
      SSW = SIN( 2.0 * WR )
      EYT = 2.0 * EC * YT
      FEQT1 = SIN( RML ) * ( -EYT * CW - 2.0 * EC * CW )
      FEQT2 = COS( RML ) * ( 2.0 * EC * SW - EYT * SW )
      FEQT3 = SIN( 2.0 * RML ) * ( YT - ( 5.0 * EC**2 / 4.0 ) 
     &      * ( CW**2 - SW**2 ) )
      FEQT4 = COS( 2.0 * RML ) * ( 5.0 * EC**2 * SSW / 4.0 )
      FEQT5 = SIN( 3.0 * RML ) * ( EYT * CW )
      FEQT6 = COS( 3.0 * RML ) * ( -EYT * SW )
      FEQT7 = -SIN( 4.0 * RML ) * ( 0.5 * YT**2 )
      FEQT = FEQT1 + FEQT2 + FEQT3 + FEQT4 + FEQT5 + FEQT6 + FEQT7
      EQT = FEQT * 13751.0

C...convert eq of time from sec to deg

      REQT = EQT / 240.0

C...calc right ascension in rads

      RA = ML - REQT
      RRA = RA * PI180

C...calc declination in rads, deg

      TAB = 0.43360 * SIN( RRA )
      RDECL = ATAN( TAB )

C...calc local hour angle

      LBGMT = 12.0 - EQT / 3600.0 + LONG * 24.0 / 360.0
      LZGMT = 15.0 * ( GMT - LBGMT )
      ZPT = LZGMT * PI180
      CSZ = SIN( RLT ) * SIN( RDECL ) + COS( RLT ) * COS( RDECL )
     &    * COS( ZPT )
      ZR = ACOS( CSZ )
      ZENITH = ZR / PI180
     
      RETURN
      END
