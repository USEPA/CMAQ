
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

      SUBROUTINE PHOT ( CGRID, JDATE, JTIME, DTSTEP )
C----------------------------------------------------------------------
C Function:
C    PHOT, adapted from RADM, calculates the photolysis rate constants
C    to be used by the chemical solver.
C    It uses linear interpolation in time of day, height, and latitude
C    from file tabular values and optionally adjusts photolysis rates
C    above, below and in cumulus clouds.

C Preconditions: HGRD_INIT() called from PAR_INIT, which is called from DRIVER

C Subroutines/Functions called: opphot

C Revision history:
C    prototype(adaptation from RADM), Rohit Mathur, April 1993.
C    major mods, Jeff Young, May 1994 - annotated and/or "c" in col 1
C    Some argument data are interpolated data and have not been stride-
C    offset in their leading dimension (July, 1994).
C    Modified by Jerry Gipson in June, 1995 to be consistent with
C    Gear solver code
C    Modified by Shawn Roselle (Sept/Oct 1995) to read new photolysis
C    table
C    Jeff - 22 Aug 96
C    modified by S. Roselle (10/16/97) to use a new formula for calculating
C    the optical depth
C    Jeff - 3 June 98 - generalize for phot. reactions tables
C    02 October, 1998 by Al Bourgeois at LM: parallel implementation
C    23 October, 1998 by Al Bourgeois to use SUM_CHK for parallel sum.
C    30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; allocatable arrays;
C    replace INTERP3 with INTERPX
C    31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                       domain specifications in one module
C    01 Dec 08 S.Roselle: dynamic allocation of arrays in jvalue input file:
C                         allows for northern and southern hemisphere CMAQ
C                         applications (issue reported by Erick Sperandio)
C    23 Feb 11 S.Roselle: Replaced I/O API include files with UTILIO_DEFN
C    06 Apr 11 B.Hutzell: added code that opens and writes photolysis rates
C                         to an optional diagnostic file
C    07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
C    Aug 12, 15 D. Wong: Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O implementation
C    Mar 12, 19 D. Wong: Implemented centralized I/O approach, removed all MY_N clauses
C
C----------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications
      USE RXNS_DATA           ! chemical mechanism data
      USE UTILIO_DEFN         ! IO and other utility routines
      USE PHOT_MOD            ! Module with photolysis rate arrays and pointers
      
      USE CENTRALIZED_IO_MODULE, ONLY : INTERPOLATE_VAR, LAT, LON, HT
      USE RUNTIME_VARS,          ONLY : PHOTDIAG, START_DATE => STDATE, START_TIME => STTIME

#ifdef parallel
      USE SE_MODULES           ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES         ! stenex (using NOOP_UTIL_MODULE)
#endif

      IMPLICIT NONE

C include files:

      INCLUDE SUBST_FILES_ID   ! file name parameters
      INCLUDE SUBST_CONST      ! physical constants

C arguments:
      REAL,    POINTER      :: CGRID( :,:,:,: )  ! Species concentrations
      INTEGER, INTENT( IN ) :: JDATE         ! current Julian date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME         ! current time (HHMMSS)
      INTEGER, INTENT( IN ) :: DTSTEP( : )   ! time step vector (HHMMSS)

C local parameters:

      INTEGER, PARAMETER :: ONE = 1.0E0 ! numerical 1.0

      REAL, PARAMETER :: MAOMW   = MWAIR / MWWAT ! m.w. of air over m.w. of H2O

C external functions: none

C saved local variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.  ! Flag for first call to PHOT

      INTEGER, SAVE :: LOC_STDATE         ! Julian date
      INTEGER, SAVE :: LOC_STTIME         ! current time
      INTEGER, SAVE :: JPHOT              ! # of photolytic reactions
      INTEGER, SAVE :: JVHT               ! number of vertical levels
      INTEGER, SAVE :: JVTMAX             ! number of hour angles
      INTEGER, SAVE :: JVLAT              ! number of latitudes

      INTEGER, ALLOCATABLE, SAVE :: PHID( : )     ! index of phot tab name in file list

      REAL, SAVE    :: STRTHR             ! starting GMT hour
      REAL, SAVE    :: JDSTRT             ! current Julian day (DDD)

      REAL, ALLOCATABLE, SAVE :: XJVAL( :,:,:,: ) ! file jvalues
      REAL, ALLOCATABLE, SAVE :: XHAJV ( : ) ! hours from noon
      REAL, ALLOCATABLE, SAVE :: XLATJV( : ) ! latitudes of file photolytic rates
      REAL, ALLOCATABLE, SAVE :: XZJV  ( : ) ! vertical heights of file photolytic
      REAL, ALLOCATABLE, SAVE :: ACLD  ( : ) ! ??????????
      REAL, ALLOCATABLE, SAVE :: ZM    ( :,:,: )     ! layer half height agl [m]


      CHARACTER( 16 ), SAVE :: PNAME = 'PHOT'
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: PHOTNM( : )


      LOGICAL       :: NDARK          ! Are all cells in darkness?

C scratch local variables:

      CHARACTER(  16 ) :: J2FILE  = 'XJ_DATA'
      CHARACTER(  16 ) :: VARNM
      CHARACTER(  80 ) :: VARDESC      ! env var description
      CHARACTER( 120 ) :: XMSG    = ' '


      INTEGER      JVUNIT
      INTEGER      JVDATE             ! Julian date on JVALUE file
      INTEGER   :: CLDATT = 1         ! flag for cloud attenuation; 1=on,0=off
      INTEGER      NDAYS              ! local day angle
      INTEGER      NT                 ! time loop index
      INTEGER      NHT                ! height loop index
      INTEGER      NLAT               ! latitude loop index
      INTEGER      NPHOT              ! photolysis rate loop index
      INTEGER      NHTO               ! dummy file height var
      INTEGER      NLATO              ! dummy file lat var
      INTEGER      NPHOTO             ! dummy file photolysis rate var
      INTEGER      ROW
      INTEGER      COL
      INTEGER      LEV
      INTEGER      JP                 ! loop indices
      INTEGER      JVTM               ! hour angle interpolation index
      INTEGER      JLATN              ! latitude interpolation index
      INTEGER      KHTA               ! altitude interpolation index
      INTEGER      IOST               ! i/o status code
      INTEGER      ALLOCSTAT
      INTEGER      ESTAT              ! status from environment var check

      INTEGER      ITMSTEP            ! one half synchronization timestep (sec)
      INTEGER      MDATE              ! Date at time step midpoint
      INTEGER      MTIME              ! Time at time step midpoint

      REAL         CURRHR             ! current GMT hour
      REAL         THETA              ! function dummy argument
      REAL         INCANG             ! sun inclination angle
      REAL         FTIMO              ! hour angle interpolation weight
      REAL         OMFTIMO            ! 1 - FTIMO
      REAL         FLATS              ! latitude interpolation weight
      REAL         OMFLATS            ! 1 - FLATS
      REAL         ZHT                ! ht. of model layer above sea level
      REAL         FHTA               ! altitude interpolation weight
      REAL         OMFHTA             ! 1 - FHTA
      REAL         LWP                ! liquid water path--lwc*dz (g/m2)
      REAL         JVAL               ! interpolated J-values
      REAL         CTOP               ! cloud top in single dimension
      REAL         CBASE              ! cloud base in single dimension
      REAL         ZLEV               ! height in single dimension
      REAL         CLDFR              ! total fractional cloud coverage
      REAL         CLOD               ! cloud optical depth
      REAL         ZEN                ! cosine of zenith angle
      REAL         TRANS              ! transmitivity
      REAL         FCLDA              ! above cloud top factor
      REAL         FCLDB              ! below cloud base factor
      REAL         ZREL               ! in cloud height
      REAL         X1                 ! cloud attenuation interpolation term
      REAL         X2                 ! cloud attenuation interpolation term
      REAL         FCLD               ! cloud photolytic atten factor
      REAL         JWT   ( 8 )        ! combined interpolation weight
      REAL         XLHA ( NCOLS, NROWS ) ! local hour angle
      REAL         WBAR ( NCOLS, NROWS ) ! avg cloud liq water cont (g/m**3)
      REAL         CLDT ( NCOLS, NROWS ) ! cloud top, as K index
      REAL         CLDB ( NCOLS, NROWS ) ! cloud bottom, as K index
      REAL         CFRAC( NCOLS, NROWS ) ! total fractional cloud coverage
!      REAL         ZM   ( NCOLS, NROWS, NLAYS ) ! Mid-layer ht. agl (m)
      REAL         DUMP                  ! dump unwanted data read

      INTEGER, SAVE :: TSTEP     ! current timestep

! variables determining whether to write output diagnostics

      LOGICAL :: JTIME_CHK   ! To check for JTIME to write RJ values
      INTEGER, SAVE :: ODATE ! output date
      INTEGER, SAVE :: OTIME ! output time
      INTEGER, SAVE :: OSTEP ! time since last write diagnostics  

C internal functions:

      REAL         SINE             ! sine of angle given in degrees
      REAL         COSINE           ! cosine of angle given in degrees

      SINE   ( THETA ) = SIN ( PI180 * THETA )
      COSINE ( THETA ) = COS ( PI180 * THETA )


C----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
        FIRSTIME = .FALSE.
        LOC_STDATE = JDATE
        LOC_STTIME = JTIME
        STRTHR = FLOAT ( JTIME / 10000 )
        JDSTRT = FLOAT ( MOD ( JDATE, 1000 ) )
        TSTEP  = DTSTEP( 1 )  ! output timestep for photolysis diagnostic files

        JVUNIT = GETEFILE( J2FILE, .TRUE., .TRUE., PNAME )

        IF ( JVUNIT .LT. 0 ) THEN
          XMSG = 'Error opening JVALUE file'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read julian start date from the file..............................

        READ( JVUNIT, *, IOSTAT = IOST ) JVDATE

        XMSG = 'Error reading file header from JVALUE file'
        IF ( IOST .NE. 0 )
     &    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

C...note differences in start dates to the log

        XMSG = 'Date on JVALUE file differs from model start date'
        IF ( JVDATE .NE. LOC_STDATE )
     &    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )

C...read number of levels.............................................

        READ( JVUNIT, *, IOSTAT = IOST ) JVHT

        XMSG = 'Error reading number of LEVELS from JVALUE file'
        IF ( IOST .NE. 0 )
     &    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

C...allocate arrays dependent on number of levels

        ALLOCATE ( XZJV( JVHT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating XZJV'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read levels

        READ( JVUNIT, *, IOSTAT = IOST ) ( XZJV( NHT ), NHT=1, JVHT )

        XMSG = 'Error reading LEVELS from JVALUE file'
        IF ( IOST .NE. 0 )
     &    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

C...read number of latitude bands.....................................

        READ( JVUNIT, *, IOSTAT = IOST ) JVLAT

        XMSG = 'Error reading number of LATITUDES from JVALUE file'
        IF ( IOST .NE. 0 )
     &    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

C...allocate arrays dependent on number of latitudinal bands

        ALLOCATE ( XLATJV( JVLAT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating XLATJV'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read latitude bands

        READ( JVUNIT, *, IOSTAT = IOST ) ( XLATJV( NLAT ),
     &                                     NLAT=1, JVLAT )

        XMSG = 'Error reading LATITUDES from JVALUE file'
        IF ( IOST .NE. 0 )
     &    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

C...read hour number angles...........................................

        READ( JVUNIT, *, IOSTAT = IOST ) JVTMAX

        XMSG = 'Error reading number of HOURS from JVALUE file'
        IF ( IOST .NE. 0 )
     &    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

C...allocate arrays dependent on number of hour angles

        ALLOCATE ( XHAJV( JVTMAX ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating XHAJV'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read hour angles

        READ( JVUNIT, *, IOSTAT = IOST ) ( XHAJV( NT ), NT=1, JVTMAX )

        XMSG = 'Error reading HOURS from JVALUE file'
        IF ( IOST .NE. 0 )
     &    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

C...read number of reactions..........................................

        READ( JVUNIT, *, IOSTAT = IOST ) JPHOT

        XMSG = 'Error reading number of REACTIONS from JVALUE file'
        IF ( IOST .NE. 0 )
     &    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

C...make sure number of reactions is correct

        XMSG = 'Photolysis reactions on JVALUE file do not '
     &          //'match the expected list (NPHOTAB)'
        IF ( JPHOT .NE. NPHOTAB )
     &    CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )

C...allocate arrays dependent on number of photolysis reactions

        ALLOCATE ( PHOTNM( JPHOT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating PHOTNM'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( ACLD( JPHOT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating ACLD'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( PHID( JPHOT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating PHID'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read reaction id's and ACLD array

        XMSG = 'Error reading REACTIONS and ACLD from JVALUE file'
        DO NPHOT = 1, JPHOT
          READ( JVUNIT, *, IOSTAT = IOST ) PHOTNM( NPHOT ),
     &                                     ACLD( NPHOT )
          IF ( IOST .NE. 0 )
     &      CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END DO

C...check the file list

        DO NPHOT = 1, JPHOT
           PHID( NPHOT ) = 0
        END DO
        XMSG = 'File data does not have all required phot tables'
        DO NPHOT = 1, NPHOTAB
           PHID( NPHOT ) = INDEX1( PHOTAB( NPHOT ), JPHOT, PHOTNM )
           IF ( PHID( NPHOT ) .LE. 0 )
     &        CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END DO

C...allocate the XJVAL array

        ALLOCATE ( XJVAL( JPHOT, JVTMAX, JVLAT, JVHT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating XJVAL'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read the j-values

        XMSG = 'Error reading jvalues from JVALUE file'
        DO NHT = 1, JVHT
          DO NLAT = 1, JVLAT
            DO NPHOT = 1, JPHOT

              READ( JVUNIT, *, IOSTAT = IOST ) NHTO, NLATO, NPHOTO

              IF ( IOST .NE. 0 )
     &          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

              IF ( PHID( NPHOT ) .NE. 0 ) THEN

                 READ( JVUNIT, *, IOSTAT = IOST )
     &               ( XJVAL( PHID( NPHOT ), NT, NLAT, NHT ), NT = 1, JVTMAX )

                 IF ( IOST .NE. 0 )
     &             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

              ELSE

                 READ( JVUNIT, *, IOSTAT = IOST ) DUMP

                 IF ( IOST .NE. 0 )
     &             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

              END IF

            END DO
          END DO
        END DO

C...close the jvalue file

        CLOSE ( JVUNIT )

        ALLOCATE ( ZM( NCOLS,NROWS, NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
          XMSG = 'Failure allocating LAT'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        CALL INIT_PHOT_SHARED()


        ALLOCATE( ELMO_AOD_550    ( NCOLS,NROWS ) )
        IF ( ALLOCSTAT .NE. 0 ) THEN
             XMSG = 'Failure allocating 2D ELMO_AOD_550. Note that it is not used by this phot_table module'
             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        ELMO_AOD_550 = 0.0
        
        ALLOCATE ( ELMO_EXT_550( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT .NE. 0 ) THEN
             XMSG = 'Failure allocating 3D ELMO_EXT_550. Note that it is not used by this phot_table module'
             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF
        ELMO_EXT_550 = 0.0

        IF ( PHOTDIAG ) THEN
C...open the photolysis rate diagnostic files

            ODATE = START_DATE; OTIME = START_TIME; OSTEP = 0
#ifdef phot_write_start
            IF ( IO_PE_INCLUSIVE ) CALL OPPHOT ( ODATE, OTIME, DTSTEP( 1 ) )
#else
            CALL NEXTIME ( ODATE, OTIME, DTSTEP( 1 ) )  ! output timestamp ending time
            IF ( IO_PE_INCLUSIVE ) CALL OPPHOT ( ODATE, OTIME, DTSTEP( 1 ) )
! reset ODATE and OTIME for counting 
            ODATE = START_DATE; OTIME = START_TIME
#endif
        END IF  ! photdiag

#ifdef parallel_io
         IF ( .NOT. IO_PE_INCLUSIVE ) THEN
            IF ( .NOT. OPEN3( CTM_RJ_2, FSNONIO, PNAME ) ) THEN
               XMSG = 'Could not open ' // TRIM(CTM_RJ_2)
               CALL M3EXIT( PNAME, ODATE, OTIME, XMSG, XSTAT1 )
            END IF
         END IF
#endif

         CALL SUBST_BARRIER

      END IF  ! FIRSTIME

C...compute XLHA (local hr angle) deviation from noon
C...  correct for current *positive* West longitude convention

      IF ( PHOTDIAG ) THEN
         OSTEP  = OSTEP + TIME2SEC( DTSTEP( 2 ) )
         JTIME_CHK = ( OSTEP .GE. TIME2SEC( DTSTEP( 1 ) ) )
         IF ( JTIME_CHK ) THEN 
           OSTEP = 0
           CALL NEXTIME( ODATE, OTIME, DTSTEP( 1 ) )
         END IF
#ifdef phot_write_start
         JTIME_CHK = ( ODATE .EQ. STDATE .AND. OTIME .EQ. STTIME )
#endif         
      ELSE
         JTIME_CHK = .FALSE.
      END IF
      
      CURRHR = STRTHR
     &       + FLOAT ( SECSDIFF ( LOC_STDATE, LOC_STTIME, JDATE, JTIME ) )
     &       / 3600.0
      NDARK = .TRUE.
      DO ROW = 1, NROWS
        DO COL = 1, NCOLS
          XLHA( COL, ROW ) = CURRHR + LON( COL, ROW ) / 15.0 - 12.0
          NDAYS = NINT ( XLHA( COL, ROW ) / 24.0 )
          XLHA( COL, ROW ) = ABS ( XLHA( COL, ROW ) - NDAYS * 24.0 )
          IF ( XLHA( COL, ROW ) .LE. XHAJV( JVTMAX ) ) NDARK = .FALSE.
        END DO
      END DO

C...If sun below horizon at all cells, zero photolysis rates & exit
C...  (assumes sun below horizon at *all* levels!)

      IF ( NDARK ) THEN
        DO JP = 1, NPHOTAB
          DO LEV = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL =1, NCOLS
                RJ( COL, ROW, LEV, JP ) = 0.0
              END DO
            END DO
          END DO
        END DO
        WRITE( LOGDEV, 1003) JDATE, JTIME
1003    FORMAT( 8X, 'In darkness at ', I8.7, ':', I6.6,
     &          1X, 'GMT - no photolysis')

        IF ( PHOTDIAG .AND. JTIME_CHK ) THEN

          DO JP = 1, NPHOTAB
            IF ( .NOT. WRITE3( CTM_RJ_2, PHOTAB( JP ), JDATE,
     &                         JTIME, RJ( :,:,:,JP ) ) ) THEN
              XMSG = 'Could not write ' // CTM_RJ_2 // ' file'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
          END DO

          WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &           'RJ Values written to', CTM_RJ_2,
     &           'for date and time', JDATE, JTIME

        END IF ! if photdiag .and. jtime_chk .eq. 0

        RETURN
      END IF

C...Calculate mid date and time
      MDATE = JDATE
      MTIME = JTIME
      ITMSTEP = TIME2SEC( DTSTEP( 2 ) ) / 2
      CALL NEXTIME( MDATE, MTIME, SEC2TIME( ITMSTEP ) )

C...Get heights of each level

      call interpolate_var ('ZH', mdate, mtime, ZM)

C...compute clear-sky photolysis rates

      DO ROW = 1, NROWS
        DO COL = 1, NCOLS

C...Compute interpolation indices and weighting factors

C...  hr angle interpolation indices

          JVTM = 2
          DO NT = 2, JVTMAX - 1
            IF ( XLHA( COL, ROW ) .GT. XHAJV( NT ) )
     &        JVTM = NT + 1
          END DO

C...hr angle weighting factors

          FTIMO = ( XHAJV( JVTM ) - XLHA( COL, ROW ) )
     &          / ( XHAJV( JVTM ) - XHAJV( JVTM - 1 ) )
          OMFTIMO = ONE - FTIMO

c...latitude interpolation indices

          JLATN = 2

          DO NLAT = 2, JVLAT - 1
            IF ( LAT( COL, ROW ) .GT. XLATJV( NLAT ) )
     &        JLATN = NLAT + 1
          END DO

C...latitude weighting factors

          FLATS = ( XLATJV( JLATN ) - LAT( COL, ROW ) )
     &          / ( XLATJV( JLATN ) - XLATJV( JLATN - 1 ) )
          OMFLATS = ONE - FLATS

C...height interpolation indices

          DO LEV = 1, NLAYS
            ZHT = ZM( COL, ROW, LEV ) + HT( COL, ROW )
            ZHT = MIN ( MAX ( ZHT,  XZJV( 1 ) ), XZJV( JVHT ) )
            KHTA = 2

            DO NHT = 2, JVHT - 1
              IF ( ZHT .GT. XZJV( NHT ) ) KHTA = NHT + 1
            END DO

C...height weighting factors

            FHTA = ( XZJV( KHTA ) - ZHT )
     &           / ( XZJV( KHTA ) - XZJV( KHTA - 1 ) )
            OMFHTA = ONE - FHTA

C...linear interpolation weighting factors

            JWT( 1 ) = OMFTIMO * OMFLATS * OMFHTA
            JWT( 2 ) =   FTIMO * OMFLATS * OMFHTA
            JWT( 3 ) = OMFTIMO *   FLATS * OMFHTA
            JWT( 4 ) =   FTIMO *   FLATS * OMFHTA
            JWT( 5 ) = OMFTIMO * OMFLATS * FHTA
            JWT( 6 ) =   FTIMO * OMFLATS * FHTA
            JWT( 7 ) = OMFTIMO *   FLATS * FHTA
            JWT( 8 ) =   FTIMO *   FLATS * FHTA

C...Interpolate all photolysis rates at each COL, ROW

            DO JP = 1, NPHOTAB
              JVAL = JWT( 1 ) * XJVAL( JP, JVTM, JLATN, KHTA )
     &             + JWT( 2 ) * XJVAL( JP, JVTM - 1, JLATN, KHTA )
     &             + JWT( 3 ) * XJVAL( JP, JVTM, JLATN - 1, KHTA )
     &             + JWT( 4 ) * XJVAL( JP, JVTM - 1, JLATN - 1, KHTA )
     &             + JWT( 5 ) * XJVAL( JP, JVTM, JLATN, KHTA - 1 )
     &             + JWT( 6 ) * XJVAL( JP, JVTM - 1, JLATN, KHTA - 1 )
     &             + JWT( 7 ) * XJVAL( JP, JVTM, JLATN - 1, KHTA - 1 )
     &             + JWT( 8 ) * XJVAL( JP, JVTM - 1, JLATN - 1,
     &                                 KHTA - 1 )
              RJ( COL, ROW, LEV, JP ) = MAX ( JVAL, 0.0 )
            END DO

          END DO     ! LEV
        END DO     ! COL
      END DO     ! ROW

C...At this point, clear sky photolysis rates have been calculated.
C...  Only proceed if interested in cloud effects on RJ

      IF ( CLDATT .NE. 0 ) THEN

C...Get time dependent non-layered data

C...Read & Interpolate WBAR

        call interpolate_var ('WBAR', mdate, mtime, WBAR)

C..Read & Interpolate CLDT

        call interpolate_var ('CLDT', mdate, mtime, CLDT)

C..Read & Interpolate CLDB

        call interpolate_var ('CLDB', mdate, mtime, CLDB)

C...Read & Interpolate CFRAC

        call interpolate_var ('CFRAC', mdate, mtime, CFRAC)

C...inclination angle used for zenith angle calculation

        INCANG = 23.5 * SINE ( ( JDSTRT + CURRHR / 24.0 - 81.1875 )
     &                       * ( 90.0 / 91.3125 ) )

C...loop through all cell and make the cloud correction

        DO ROW = 1, NROWS
          DO COL = 1, NCOLS

            CLDFR = CFRAC( COL, ROW )

            IF ( CLDFR .GE. 1.0E-05 ) THEN

C...calculate cloud correction factors
C...  first compute the liquid water path in g/m2

              CTOP  = CLDT( COL, ROW )
              CBASE = CLDB( COL, ROW )
              LWP   = ( CTOP - CBASE ) * WBAR( COL, ROW )

C...Calculate the cloud optical depth using a formula derived from
C...  Stephens (1978), JAS(35), pp2111-2132.
C...  only calculate the cloud optical depth when the liquid water
C...  path is >= 10 g/m2

              IF ( LWP .GE. 10.0 ) THEN
                CLOD = 10.0**( 0.2633 + 1.7095 * LOG( LOG10( LWP ) ) )
              ELSE
                CLOD = 0.0
              END IF

C...If no cloud or optical depth < 5, set clear sky values.
C...  (i.e. don't do anything)

              IF ( CLOD .GE. 5.0 ) THEN

                DO LEV = 1, NLAYS
                  ZLEV  = ZM( COL, ROW, LEV )
                  ZREL = ( ZLEV - CBASE ) / ( CTOP - CBASE )

C...cos of the zenith angle, ( <= cos 60 degrees )

                  ZEN = MAX ( SINE ( LAT( COL, ROW ) ) * SINE ( INCANG )
     &                      + COSINE ( LAT( COL, ROW ) )
     &                      * COSINE ( INCANG )
     &                      * COSINE ( XLHA( COL, ROW ) * 15.0 ),
     &                      0.5
     &                      )
                  TRANS = ( 5.0 - EXP ( -CLOD ) ) / ( 4.0 + 0.42 * CLOD )

C...calculate cloud correction factors

C...  below cloud base

                  FCLDB = 1.0 + CLDFR * ( 1.6 * ZEN * TRANS - 1.0 )
                  X1 = CLDFR * ZEN * ( 1.0 - TRANS )
                  X2 = FCLDB * ( 1.0 - ZREL )

C...  above cloud top

                  DO JP = 1, NPHOTAB
                    FCLDA = 1.0 + X1 * ACLD( PHID( JP ) )

C...  in cloud - linearly interpolate between base and top value

                    FCLD = FCLDA * ZREL + X2
                    IF ( ZLEV .LT. CBASE ) FCLD = FCLDB
                    IF ( ZLEV .GT.  CTOP ) FCLD = FCLDA

                    RJ( COL, ROW, LEV, JP ) = FCLD
     &                                      * RJ( COL, ROW, LEV, JP )

                    RJ_RES( COL, ROW, LEV, JP ) = RJ( COL, ROW, LEV, JP )
                    RJ_SUB( COL, ROW, LEV, JP ) = RJ( COL, ROW, LEV, JP )
                  END DO
                END DO
              END IF
            END IF
          END DO
        END DO

      END IF

      IF ( PHOTDIAG .AND. JTIME_CHK ) THEN

        DO JP = 1, NPHOTAB
          IF ( .NOT. WRITE3( CTM_RJ_2, PHOTAB( JP ), ODATE, OTIME, RJ( :,:,:,JP ) ) ) THEN
            XMSG = 'Could not write ' // CTM_RJ_2 // ' file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
          END IF
        END DO

        WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &         'RJ Values written to', CTM_RJ_2,
     &         'for date and time', JDATE, JTIME

      END IF ! if photdiag .and. jtime_chk .eq. 0

      RETURN
      END
