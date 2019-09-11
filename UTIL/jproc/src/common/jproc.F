
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/jproc.F,v 1.8 2011/10/29 01:03:53 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)jproc.F 1.3 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.jproc.F 04 Jul 1997 09:39:12

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      PROGRAM JPROC
           
C*********************************************************************
C
C  This program calculates photolytic rate constants for
C     atmospheric molecules specified by the chemical mechanism
C     reader.  J-values are output in a TABLE format, dimensioned
C     by hour angle, latitudinal band, and vertical height.  The
C     contents of the table have been modulated by climatological
C     profiles of temperture, pressure, and ozone.  In addition,
C     if Total Ozone Column Measurements are available, the ozone
C     profile is scaled to the measured TOC value.
C
C*********************************************************************
C
C  Revision History:
C
C  Date     Who           Changes Made
C  -------  ------------  --------------------------------------------
C  10/6/09  S.Roselle     Increased dimensions on the output jtable
C                         to accommodate Southern Hemisphere (issue 
C                         reported by Erick Sperandio) and Global
C                         applications:  extended to all latitudinal
C                         bands (-90 to +90); added more hour angles 
C                         (up to 12 hrs from local noon); increased
C                         vertical extent to 20km; increased resolution
C                         in the upper troposphere (suggested by
C                         Barron Henderson); updated code that applies
C                         temperature/pressure adjustments to CS/QY 
C                         data (issue reported by Barron Henderson)
C  7/15/96  S.Roselle     Final modification for targeted IOV version...
C                         now uses radiation routines from Madronich's
C                         TUV model.  Finalized CSQY and ET input
C                         formats and links to the chemistry mechanism
C                         reader.
C  4/15/96  S.Roselle     Modified to read TOMS data and rescale
C                         ozone profiles to fit total ozone column
C                         values if TOMS data are available
C  1/30/96  S.Roselle     Significant modification to CSQY input...Now
C                         reads CSQY on any wavelength distribution and
C                         integrates to ET wavelength bands.  Reads in
C                         user specified CSQY files and calculated
C                         jvalues only for these specified reations.
C  6/26/95  S.Roselle&    Modified to become the Models-3 Photolysis
C           C.Jang        Rate Preprocessor with the following changes
C                         1. Follow the Models-3 coding standard.
C                         2. Produce a unique set of J-value output
C                            for RADM2, CB-IV, and SAPRC90 Mechanisms
C                            (a total of 27 photolytic reactions).
C                         3. Increase the vertical resolution of
C                            J-value output from3 levels (0,1,10 km)
C                            to 7 levels (0,1,2,3,4,5,10).
C                         4. Update the species quantum yield and
C                            absorption cross-section (the 'P2' file,
C                            now 'P3') from NASA-JPL-94 publications.
C                         5. Add comment lines to 'J3' output for the
C                            benefits of chemical mechanism reader
C                            used in the Models-3 and a clear 
C                            description of the output information.
C  04/24/95  S.Roselle    Removed as subroutine in Met Preprocessor
C                         and made into a separate program
C  89156     JKV          Modified for use as subroutine of met
C                         preprocessor.  created FOUTJ2.
C  89144     JKV          Modified to use date of form YYDDD passed
C                         from preprocessor.
C  6/7/89    .........    Cray version for RADM2
C  7/88      J.d.C &      modified
C            M.Boharneys
C  1988      b.stockwell  modified 
C  8/18/87   S.Madronich  last modified
C            S.Madronich  Program written
C
C*********************************************************************

      USE GET_ENV_VARS, FILE_UNIT => JUNIT
      USE RXNS_DATA

      IMPLICIT NONE

      INCLUDE SUBST_CONST        ! commonly used constants

      INCLUDE 'JVALPARMS.EXT'    ! jproc parameters

C...........PARAMETERS and their descriptions

      INTEGER, PARAMETER :: JVHT   = 13 ! number of output vert. levels
      INTEGER, PARAMETER :: JVTMAX = 13 ! number of hours output
      INTEGER, PARAMETER :: JVLAT  = 19 ! number of output latitudes

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(16) :: PNAME  = 'JPROC'   !  driver program name
      CHARACTER(16) :: JVFILE = 'JVALUES' ! JVALUES i/o logical name
      CHARACTER(80) :: MSG    = '    '    ! buffer for messages to output
      CHARACTER(255) EQNAME               ! full name of JVALUES file
      
      LOGICAL      TOMS_EXIST         ! TOMS data existence flag

      INTEGER      DAY                ! julian day of year
      INTEGER      IBASE              ! cloud base index
      INTEGER      IDATE              ! date (yyyymmdd)
      INTEGER      YEAR               ! year (yyyy)
      INTEGER      ILAT               ! latitude index
      INTEGER      ITIME              ! hour index
      INTEGER      ITOP               ! cloud top index
      INTEGER      JVUNIT             ! unit number for j-value output 
      INTEGER      JDT                ! julian date
      INTEGER      IWL                ! wavelength index
      INTEGER      ILEV               ! level index
      INTEGER      MONTH              ! month counter
      INTEGER      IHT                ! height index
      INTEGER      NLAYS              ! total # of atm layers
      INTEGER      NLEVS              ! number of levels
      INTEGER      IPHOT              ! reaction index
      INTEGER      NSUBKM             ! cloud sublayers/km
      INTEGER      NSURF              ! ground elev above sea level
      INTEGER      NWL                ! number of wavelength bands
      INTEGER      IOST               ! io status
      INTEGER      STATUS             ! status
      
! indices in latitude array to print rates to standard output 
      INTEGER, PARAMETER :: NLAT_PRINT    = 7
      INTEGER :: ILAT_PRINT( NLAT_PRINT ) = (/ 4, 6, 8, 10, 12, 14, 16 /)
      
      REAL         COSZEN             ! cosine zenith angle
      REAL         DF                 ! actinic flux
      REAL         DJ                 ! jvalue for one wl,lev,react
      REAL         DLAT               ! latitude
      REAL         DLONG              ! longitude for Pittsburgh
      REAL         ZENITH             ! zenith angle
      REAL         GAER               ! aerosol asymetry factor
      REAL         GCLD               ! cloud asymetry factor
      REAL         GRAY               ! asymetry fact for Rayleigh scat
      REAL         HAER               ! aerosol scale ht at atm top
      REAL         HAIR               ! air scale height
      REAL         HO3                ! ozone scale height
      REAL         OMAER              ! aerosol single scat albedo
      REAL         OMCLD              ! cloud single scat cross sect
      REAL         OMRAY              ! single scat albedo, Rayleigh
      REAL         UT                 ! time
      REAL         UT0                ! local high noon
      REAL         UTNOON             ! local high noon

      REAL         DOBNEW( JVLAT )    ! total vertical ozone column
      REAL         ACLD( NPHOTAB )    ! species dependent cloud albedo factor
      REAL         CLOUD( 48 )        ! cloud optical depth profile
      REAL         O3 ( MXLEV )       ! ozone profile
      REAL         T  ( MXLEV )       ! interpolated temp profile
      REAL         AIR( MXLEV )       ! interpolated air profile
      REAL         AER( MXLEV )       ! aerosol attenuation profile
      REAL         VAER( NJ )         ! aerosol column in layer
      REAL         VAIR( NJ )         ! air column in layer
      REAL         VCLD( NJ )         ! cloud column in layer
      REAL         VO3 ( NJ )         ! ozone column in layer
      REAL         VT  ( NJ )         ! average temp of column
      REAL         ZMID( NJ )         ! altitude of midpoint of layer
      REAL         Z   ( NJ )         ! altitude of each level
      REAL         CVO2( NJ )         ! vertical column O2
      REAL         MIDWL ( MXWL )     ! wavelength band midpoints
      REAL         STWL  ( MXWL )     ! wavelength band starting point
      REAL         ENDWL ( MXWL )     ! wavelength band ending point
      REAL         AAER  ( MXWL )     ! aerosol total vert opt depth
      REAL         ALBEDO( MXWL )     ! ground albedo
      REAL         ARAYL ( MXWL )     ! Rayleigh scat cross section
      REAL         F     ( MXWL )     ! extra-terrestrial radiation
      REAL         O2ABS ( MXWL )     ! O2 absorption cross section
      REAL         O3ABS ( MXWL )     ! O3 absorption cross section
      REAL         XDOBS( 19, 12 )    ! lat-season ozone values
      REAL         ENDIR( NJ, MXWL )  ! direct flux
      REAL         ENDN ( NJ, MXWL )  ! diffuse down-flux
      REAL         ENUP ( NJ, MXWL )  ! diffuse up-flux
      REAL         AO2  ( NJ, MXWL )  ! O2 cross section
      REAL         AO3  ( NJ, MXWL )  ! average O3 cross sect in layer
      REAL         D( NPHOTAB, NJ )    ! j-values
      REAL         QY( MXWL, NPHOTAB ) ! quantum yields
      REAL         CS( MXWL, NPHOTAB ) ! cross sections
      REAL         XT  ( 12, 19, MXLEV ) ! season-lat-vert temp profile
      REAL         XO3 ( 12, 19, MXLEV ) ! season-lat-vert ozone profile
      REAL         XAIR( 12, 19, MXLEV ) ! air concentration
      REAL         QYZ( 100, MXWL, NPHOTAB )! quantum yields T&P corrected
      REAL         CSZ( 100, MXWL, NPHOTAB )! cross section at each level
      REAL         XJVAL( NPHOTAB, JVTMAX, JVLAT, JVHT ) ! jvalues

! latitudes for jvalue file
      REAL :: XLATJV( JVLAT )  = (/ -90.0, -80.0, -70.0, -60.0, -50.0, -40.0, 
     &                              -30.0, -20.0, -10.0,   0.0,  10.0,  20.0, 30.0, 
     &                               40.0,  50.0,  60.0,  70.0,  80.0,  90.0 /)
! hours from noon for jvalue file
      REAL :: XHAJV( JVTMAX )  = (/ 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0,
     &                              9.0, 10.0, 11.0, 12.0 /)
! vertical hts (m) for jvalue file
      REAL :: XZJV( JVHT )     = (/    0.0,  1000.0,  2000.0,  3000.0, 4000.0,
     &                              5000.0,  8000.0, 10000.0, 15000.0, 20000.0,
     &                             25000.0, 30000.0, 35000.0 /)  ! <<< 13 levels

C...........FUNCTIONS and their descriptions:

      REAL         CHJ                ! Chapman function

      CHARACTER( 3 ) :: MONTHS(1:12) = (/ 
     &      'Jan','Feb','Mar','Apr','May','Jun',
     &      'Jul','Aug','Sep','Oct','Nov','Dec' /)

C*********************************************************************
C     begin body of program JPROC

      WRITE( 6, 2001 )
     &  'Program ' // TRIM( PNAME )                                   ,
     &  'This program calculates photolytic rate constants for        ',
     &  'atmospheric molecules specified by the chemical mechanism    ',
     &  'reader.  J-values are output in a TABLE format, dimensioned  ',
     &  'by hour angle, latitudinal band, and vertical height.  The   ',
     &  'contents of the table have been modulated by climatological  ',
     &  'profiles of temperture, pressure, and ozone.  In addition,   ',
     &  'if Total Ozone Column Measurements are available, the ozone  ',
     &  'profile is scaled to the measured TOC value.                ',
     &  ' '

C...get starting date
C...   this affects: (1) the calculation of the zenith angle
C...                 (2) interpolation of seasonal and monthly 
C...                     dependent T,M,O3,DOBSON
C
C...read the julian start date (yyyyddd)
        
      JDT = 1988215                 !  default:  1988215
      MSG = 'Scenario Starting Date (YYYYDDD)'
      JDT = GET_ENV_INT( 'JPROC_STDATE', MSG, JDT, STATUS )
       
      IF ( STATUS .NE. 0 ) WRITE( 6, '( 5X, A )' ) MSG

      IF ( STATUS .EQ. 1 ) THEN
        MSG = 'Environment variable improperly formatted'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        STOP
      ELSE IF ( STATUS .EQ. -1 ) THEN
        MSG = 'Environment variable set, but empty...Using default:'
        WRITE( 6, '( 5X, A, I9 )' ) MSG, JDT
      ELSE IF ( STATUS .EQ. -2 ) THEN
        MSG = 'Environment variable not set...Using default:'
        WRITE( 6, '( 5X, A, I9 )' ) MSG, JDT
      END IF


C...check for the existence of TOMS data

      TOMS_EXIST = .FALSE.
      MSG = 'TOMS data exist (Y/N)'
      TOMS_EXIST = GET_ENV_FLAG( 'JPROC_TOMSEXIST', MSG, TOMS_EXIST, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( 6, '(5X, A)' ) MSG

      IF ( STATUS .EQ. 1 ) THEN
        MSG = 'Environment variable improperly formatted'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        STOP
      ELSE IF ( STATUS .EQ. -1 ) THEN
        MSG = 'Environment variable set, but empty ... Using default:'
        WRITE( 6, '(5X, A, L9)' ) MSG, TOMS_EXIST
      ELSE IF ( STATUS .EQ. -2 ) THEN
        MSG = 'Environment variable not set ... Using default:'
        WRITE( 6, '(5X, A, L9)' ) MSG, TOMS_EXIST
      END IF

      IF ( .NOT. ( TOMS_EXIST ) ) THEN
        WRITE( 6, 2009 ) 
      END IF

C...convert julian date to year, month, and day
C...  and to yyyymmdd format

      CALL JULIAN_YEAR_MONTH_DAY (JDT,YEAR,MONTH,DAY)
      IDATE = YEAR * 10000 + MONTH * 100 + DAY

      WRITE( 6, 2005 ) TRIM( MONTHS( MONTH ) ), DAY, YEAR

C...read the extra terrestrial radiation data

      CALL READET ( NWL, STWL, MIDWL, ENDWL, F )

C...read the absorption cross section and quantum yield data

      CALL READCSQY ( NWL, STWL, ENDWL, CS, QY )

C...read the molecular oxygen absorption cross sections

      CALL READO2 ( NWL, STWL, ENDWL, O2ABS )

C...read the ozone absorption cross sections

      CALL READO3 ( NWL, STWL, ENDWL, O3ABS )

C...read the total ozone column data

      IF ( TOMS_EXIST ) CALL READTOMS ( JDT, JVLAT, XLATJV, DOBNEW )

C...read the standard atmosphere profiles

      CALL READPROF ( XAIR, AIR, XO3, XDOBS, O3, AER, XT, T )

C...specify ground albedo

      CALL SET_ALBEDO ( NWL, MIDWL, ALBEDO )

C...cloud:  specify cloud

      CALL SETCLD ( NLAYS, NLEVS, NSUBKM, IBASE, ITOP, CLOUD,
     &              OMCLD, GCLD )

C...Specify aerosols

      CALL SETAER ( NWL, MIDWL, AAER, OMAER, GAER, HAER )

C...Specify air/Rayleigh parameters

      CALL SETAIR ( NWL, MIDWL, HAIR, OMRAY, GRAY, ARAYL )

C...nsurf:  ground elevation above sea level
C...  Here can specify different altitudes for ground surface.
C...  Currently set up so that change index of surface level:
C...  1 = SEA LEVEL
C...
C...  ***Note that the altitude associated with level index depends
C...     on whether sublayering has been done over this altitude range.
C...     So for example nsurf = 2 means 1 km above sea level
C...     if ibase > 2, but if ibase = 1 then this means 1/36 km above
C...     sea level.

      NSURF = 1

C...ozone:  specify ozone vs. height parameters
c...   HO3 = ozone scale height, used to estimate ozone density column 
C...         upper boundary (50km).
C...   DOBNEW = total vertical ozone column, in milli-cm-atm.  If want 
C...            value, must specify DOBNEW here and turn on subroutine 
C...            further below.  Otherwise: O3 profiles from main data
C...            will be used.

      HO3 = 4.50

C...compute season number
C
C...latitude and longitude:
C...   the altitude profiles of air density, temperature,
C...   and ozone depend on the geographic coordinates.  Also, these are
C...   needed to calculate the zenith angle.
C...     ILAT is index for different latitudes
C...       user must supply the real values of the latitude,
C...       DLAT = funct(ILAT) for each index ILAT.  DLAT is the real
C...       latitude in degrees, e.g.
c...                 90.0  at N-pole
C...                  0.0   at equator
C...                -90.0 at S-pole
C...     All altitude-dependent data are adjusted to the selected
C...     latitude in subroutine INTERP(DLAT,IDATE)

C...set long. for Pittsburgh

      DLONG = 80.0

C...loop for latitude

      DO 200 ILAT = 1, JVLAT
        DLAT = XLATJV( ILAT )

C...   interpolate to working latitude and date
C...   if subroutine INTERP is not called, code will use standard
C...   altitude pr. for ozone, air, temperature, and aerosols

        CALL INTERP ( DLAT, IDATE, XT, XAIR, XO3, XDOBS, HO3, 
     &                T, AIR, O3 )

C...invoke subroutine O3SCAL is want to set all dobson values to
C...  user-selected value DOBNEW, otherwise WMO monthly average data
C...  are used.

        IF ( TOMS_EXIST ) CALL O3SCAL ( O3, HO3, XLATJV( ILAT ),
     &                                  DOBNEW( ILAT ) )

C...subdivide atmosphere in layers
C...  subroutine SUBGRID computes all altitude dependent quantities on
C...  grid used in radiative transfer calculation.

        CALL SUBGRID ( NWL, STWL, MIDWL, ENDWL, CS, CSZ, QY, QYZ,
     &                 AIR, HAIR, VAIR, CVO2, O3ABS, O3, HO3, VO3,
     &                 AO3, IBASE, ITOP, CLOUD, NSUBKM, VCLD,
     &                 AER, VAER, HAER, T, VT, Z, ZMID,
     &                 NLAYS, NLEVS )

C...time and zenith:  specify times for calculation
C...    note that date was already specified above
C...    UT0 = Universal Time (GMT) of first calculation
C...Select starting time, for example,
C...    UTNOON = 12. + DLONG*24./360.  -> start at local high sun

        UTNOON = 12.0 + DLONG * 24.0 / 360.0
        UT0 = UTNOON
        DO 200 ITIME = 1, JVTMAX
          UT = UT0 + XHAJV( ITIME )

          CALL CALCZEN ( DLAT, DLONG, IDATE, UT, ZENITH )

C...initialize J-values

          DO ILEV = 1, NLEVS
            DO IPHOT = 1, NPHOTAB
              D( IPHOT, ILEV ) = 0.0
            END DO
          END DO

C...if nighttime, skip radiative transfer calculation

          IF ( ZENITH .LE. 95.0 ) THEN     ! begin daytime calcs
            IF ( ZENITH .EQ. 90.0 ) ZENITH = 89.9
            COSZEN = COS( ZENITH * PI180 )
            IF ( ZENITH .GT. 75.0 ) COSZEN = 1.0 / CHJ( ZENITH )

C...subroutine SRBAND computes effective ozone cross sections in the
C...  Schumann-Runge region, using the parameterization of Allen and
C...  Frederick

            CALL SRBAND ( NWL, STWL, MIDWL, ENDWL, COSZEN, NLAYS, AO2,
     &                    CVO2, VT, ZMID, O2ABS )

C...subroutine OPTICS is the driver for the flux calculation.  Output is
C...   ENDIR(ILEV,IWL)  - irradiance of direct solar beam
C...   ENDN (ILEV,IWL)  - irradiance of down-welling diffuse light
C...   ENUP (ILEV,IWL)  - irradiance of up-welling diffuse light
C...   for each level LEV and wavelength bin IWL.

            CALL  OPTICS ( NWL, COSZEN, ENDIR, ENDN, ENUP,
     &                     VAIR, ARAYL, GRAY, OMRAY, AO2, VO3, AO3,
     &                     VCLD, GCLD, OMCLD, VAER, AAER, GAER, OMAER,
     &                     ALBEDO, NLAYS, NLEVS, NSURF )

            
            DO IWL = 1, NWL
              DO ILEV = 1, NLEVS

C...compute the actinic flux

                DF = F( IWL ) * ( ENDIR( ILEV, IWL ) 
     &                          + ENDN ( ILEV, IWL )
     &                          + ENUP ( ILEV, IWL ) )
                IF ( ILEV .LT. NSURF ) DF = 0.0

C...compute rate of photolysis (j-values) for each reaction

                DO IPHOT = 1, NPHOTAB
                  DJ = DF * CSZ( ILEV, IWL, IPHOT )
     &                    * QYZ( ILEV, IWL, IPHOT )
                  D( IPHOT, ILEV ) = D( IPHOT, ILEV ) + DJ                  
                END DO

              END DO
            END DO

          END IF     ! end daytime calculations

C...load output array and convert from 1/sec to 1/min

          DO ILEV = 1, NLEVS

            DO IHT = 1, JVHT
              IF ( FLOAT ( ( ILEV - 1 ) * 1000 ) .EQ. XZJV( IHT ) ) THEN

                DO IPHOT = 1, NPHOTAB
                  XJVAL( IPHOT, ITIME, ILAT, IHT ) = D( IPHOT, ILEV )
     &                                             * 60.0
                END DO

              END IF
            END DO

          END DO

200   CONTINUE

C...write output file with file header 

      CALL VALUE_NAME ( JVFILE, EQNAME )
      JVUNIT = FILE_UNIT( )
        
      OPEN ( UNIT = JVUNIT,
     &       FILE = EQNAME,
     &       FORM = 'FORMATTED',
     &       STATUS = 'NEW',
     &       IOSTAT = IOST )

C...check for open errors

      IF ( IOST .NE. 0) THEN
        MSG = 'Could not open the JVALUE data file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        STOP
      END IF

      WRITE( 6, 2011 ) JVUNIT, EQNAME

C...Write Julian Date to output ***

      WRITE ( JVUNIT, 2013 ) JDT

      WRITE( JVUNIT, 2015 ) JVHT
      WRITE( JVUNIT, 2017 ) ( XZJV( IHT ), IHT=1, JVHT )

      WRITE( JVUNIT, 2019 ) JVLAT
      WRITE( JVUNIT, 2021 ) ( XLATJV( ILAT ), ILAT=1, JVLAT )

      WRITE( JVUNIT, 2023 ) JVTMAX
      WRITE( JVUNIT, 2025 ) ( XHAJV( ITIME ), ITIME=1, JVTMAX )

      WRITE( JVUNIT, 2027) NPHOTAB

      DO IPHOT = 1, NPHOTAB
        ACLD( IPHOT ) = 1.0
        WRITE( JVUNIT, 2029 ) PHOTAB(IPHOT), ACLD( IPHOT )
      END DO

      DO IHT = 1, JVHT
        DO ILAT = 1, JVLAT
          DO IPHOT = 1, NPHOTAB

            WRITE( JVUNIT, 2031 ) IHT, ILAT, IPHOT
            WRITE( JVUNIT, 2033 ) ( XJVAL( IPHOT, ITIME, ILAT, IHT ),
     &                               ITIME = 1, JVTMAX )

          END DO
        END DO
      END DO
      CLOSE( JVUNIT )

      IHT = 1
      WRITE(6,2034)0.001 * XZJV( IHT ),', Mean Values versus latitude'
      WRITE(6,2035)'   Rate    ',( XLATJV( ILAT_PRINT( ILAT ) ), ILAT = 1, NLAT_PRINT )
      DO IPHOT = 1, NPHOTAB
         WRITE(6,2036)PHOTAB(IPHOT), 
     &   (SUM( XJVAL( IPHOT, 1:JVTMAX, ILAT_PRINT( ILAT ), IHT ) ) / REAL( JVTMAX ),
     &    ILAT = 1, NLAT_PRINT )
      END DO
      WRITE(6,2034)0.001 * XZJV( IHT ),', Max Values versus latitude'
      WRITE(6,2035)'   Rate    ',( XLATJV( ILAT_PRINT( ILAT ) ), ILAT = 1, NLAT_PRINT )
      DO IPHOT = 1, NPHOTAB
         WRITE(6,2036)PHOTAB(IPHOT), 
     &   (MAXVAL( XJVAL( IPHOT, 1:JVTMAX, ILAT_PRINT( ILAT ), IHT ) ), ILAT = 1, NLAT_PRINT )
      END DO
       
C...formats

2001  FORMAT( 5X, A )
2005  FORMAT( 1X, '...Date to be processed: ', A3, 1X, I2, 
     &       ', ', I4, ' starting at 0:00 UT ' )
2009  FORMAT( 1X, '...No TOMS data specified for this run.', /,
     &        '   Climatological O3 column data will be used...', / )
2011  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255, / )
2013  FORMAT( 3X, I7, 2X, '(yyyyddd) Julian Date for the file' )
2015  FORMAT( 3X, I2, 2X, 'LEVELS (m)' )
2017  FORMAT( 3X, 30( F7.1, 1X ) )
2019  FORMAT( 3X, I2, 2X, 'LATITUDES (deg)' )
2021  FORMAT( 3X, 30( F5.1, 1X ) )
2023  FORMAT( 3X, I2, 2X, 'HOUR ANGLES (from noon)' )
2025  FORMAT( 3X, 30( F5.1, 1X ) )
2027  FORMAT( 3X, I2, 2X, 'PHOTOLYTIC REACTIONS' )
2029  FORMAT( 6X, '''', A16, ''',', 5X, F3.1 )
2031  FORMAT( 1X, 3( I3, 1X ) )
2033  FORMAT( 1X, 1P, 5( E13.7, 2X ) )
2034  FORMAT('At Z = ', F6.2, A )     
2035  FORMAT(A16,10(1X,F12.2))
2036  FORMAT(A16,10(1X,ES12.4))


      STOP
      END
