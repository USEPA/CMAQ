      SUBROUTINE SCIPHOT(JDATE, JTIME, LAT, LON,
     &                   Z, TERR, WBAR, CTOP, CBASE, CLDFR, RJ)
!******************************************************************************
! Developed jointly by the ARAP Group of Titan Corporation, Princeton, NJ and 
! Atmospheric and Environmental Research, Inc., San Ramon, CA 
! under EPRI-sponsorship.
!
! FUNCTION:  Calculate the photolysis reaction rate constants
!            (This routine is copied from the host model's)
!
! PRECONDITIONS REQUIRED: 
!
! SUBROUTINES AND FUNCTIONS CALLED: 
!
!
! REVISION HISTORY: 
!
!    Minor updates (Fortran 90), February 2004 (PKK, AER)
!    Updated May 2006 for additional consistency with CMAQ (PKK, AER)
!    Updated October 2010 for consistency with CMAQ 4.7.1 (PKK, ENVIRON)
!    Updated August 2011 for consistency with CMAQ 5.0 (PKK, ENVIRON)
!
!******************************************************************************
 
! --- MODULES

      USE MULTCOMP_INC
      USE HOST_INC
      USE HOST_CHEM_INC

      IMPLICIT NONE

! --- ARGUMENTS
 
      INTEGER      JDATE              ! current Julian date (YYYYDDD)
      INTEGER      JTIME              ! current time (HHMMSS)

      REAL         LAT                ! north lat in deg
      REAL         LON                ! west long in deg
      REAL         Z                  ! height in meters, msl
      REAL         TERR               ! terrain height in meters
      REAL         WBAR               ! average liquid water content of clouds,
                                      ! in g/m**3
      REAL         CTOP               ! cloud top (meters)
      REAL         CBASE              ! cloud base (meters)
      REAL         CLDFR              ! total fractional cloud coverage  
      
      REAL         RJ( NPHOTAB )      ! J-values  (/min units)

! --- PARAMETERS

      REAL, PARAMETER :: ONE = 1.0E0       ! numerical 1.0

      REAL, PARAMETER :: PI = 3.14159265358979324  ! PI (single precision 3.141593)
      REAL, PARAMETER :: PI180  = PI / 180.0  ! degrees-to-radians:  PI/180

! --- SAVED LOCALS
 
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      INTEGER, SAVE :: STDATE         ! Starting date
      INTEGER, SAVE :: STTIME         ! Starting time
      INTEGER, SAVE :: JPHOT          ! number of photolytic reactions
      INTEGER, SAVE :: JVHT           ! number of vertical levels
      INTEGER, SAVE :: JVTMAX         ! number of hour angles
      INTEGER, SAVE :: JVLAT          ! number of latitudes
      INTEGER, ALLOCATABLE, SAVE :: PHID( : )     ! index of phot tab name in file list

      REAL, SAVE :: STRTHR            ! starting GMT hour
      REAL, SAVE :: JDSTRT            ! current Julian day (DDD)
      REAL, ALLOCATABLE, SAVE :: XJVAL( :,:,:,: ) ! file jvalues
      REAL, ALLOCATABLE, SAVE :: XHAJV ( : ) ! hours from noon
      REAL, ALLOCATABLE, SAVE :: XLATJV( : ) ! latitudes of file photolytic rates
      REAL, ALLOCATABLE, SAVE :: XZJV  ( : ) ! vertical heights of file photolytic
      REAL, ALLOCATABLE, SAVE :: ACLD  ( : ) ! ??????????

! --- SCRATCH LOCALS

      CHARACTER( 16 ) :: J2FILE = 'XJ_DATA'
      CHARACTER( 16 ) :: PNAME = 'SCIPHOT'
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: PHOTNM( : )
      CHARACTER( 255 ) :: EQNAME
      CHARACTER( 120 ) :: XMSG = ' '

      INTEGER      JVUNIT
      INTEGER      JVDATE             ! Julian date on JVALUE file
      INTEGER  ::  CLDATT = 1         ! flag for cloud attenuation; 1=on,0=off
      INTEGER      NDAYS              ! local day angle
      INTEGER      NT                 ! time loop index
      INTEGER      NHT                ! height loop index
      INTEGER      NLAT               ! latitude loop index
      INTEGER      NPHOT              ! photolysis rate loop index
      INTEGER      NHTO               ! dummy file height var
      INTEGER      NLATO              ! dummy file lat var
      INTEGER      NPHOTO             ! dummy file photolysis rate var
      INTEGER      JP                 ! loop indices
      INTEGER      JVTM               ! hour angle interpolation index
      INTEGER      JLATN              ! latitude interpolation index
      INTEGER      KHTA               ! altitude interpolation index
      INTEGER      IOST               ! i/o status code
      INTEGER      ALLOCSTAT

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
      REAL         ZTERR              ! height above terrain in meters
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
      REAL         XLHA               ! local hour angle
      REAL         DUMP               ! dump unwanted data read

! --- INTERNAL FUNCTIONS:
 
      REAL         SINE             ! sine of angle given in degrees
      REAL         COSINE           ! cosine of angle given in degrees

      SINE ( THETA )   = SIN ( PI180 * THETA )
      COSINE ( THETA ) = COS ( PI180 * THETA )
      
C----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         STDATE = JDATE
         STTIME = JTIME
         STRTHR = FLOAT ( JTIME / 10000 )
         JDSTRT = FLOAT ( MOD ( JDATE, 1000 ) )

         JVUNIT = GETEFILE( J2FILE, .TRUE., .TRUE., PNAME )
         IF ( JVUNIT < 0 ) THEN
           XMSG = 'Error opening JVALUE file'
           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...read julian start date from the file..............................

         READ( JVUNIT, *, IOSTAT = IOST ) JVDATE

         IF ( IOST /= 0 ) THEN
            XMSG = 'Error reading file header from JVALUE file'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...note differences in start dates to the log

         IF ( JVDATE /= STDATE ) THEN
            XMSG = 'Date on JVALUE file differs from model start date'
            CALL M3WARN( PNAME, STDATE, STTIME, XMSG )
         END IF

C...read number of levels.............................................
        
         READ( JVUNIT, *, IOSTAT = IOST ) JVHT

         IF ( IOST /= 0 ) THEN
            XMSG = 'Error reading number of LEVELS from JVALUE file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...allocate arrays dependent on number of levels

        ALLOCATE ( XZJV( JVHT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT /= 0 ) THEN
          XMSG = 'Failure allocating XZJV'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read levels

         READ( JVUNIT, *, IOSTAT = IOST ) ( XZJV( NHT ), NHT=1, JVHT )

         IF ( IOST /= 0 ) THEN
            XMSG = 'Error reading LEVELS from JVALUE file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...read number of latitude bands.....................................
        
         READ( JVUNIT, *, IOSTAT = IOST ) JVLAT

         IF ( IOST /= 0 ) THEN
            XMSG = 'Error reading number of LATITUDES from JVALUE file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...allocate arrays dependent on number of latitudinal bands

        ALLOCATE ( XLATJV( JVLAT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT /= 0 ) THEN
          XMSG = 'Failure allocating XLATJV'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read latitude bands

         READ( JVUNIT, *, IOSTAT = IOST ) ( XLATJV( NLAT ),
     &                                     NLAT=1, JVLAT )

         IF ( IOST /= 0 ) THEN
            XMSG = 'Error reading LATITUDES from JVALUE file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...read hour number angles...........................................

         READ( JVUNIT, *, IOSTAT = IOST ) JVTMAX

         IF ( IOST /= 0 ) THEN
            XMSG = 'Error reading number of HOURS from JVALUE file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...allocate arrays dependent on number of hour angles

        ALLOCATE ( XHAJV( JVTMAX ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT /= 0 ) THEN
          XMSG = 'Failure allocating XHAJV'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read hour angles

         READ( JVUNIT, *, IOSTAT = IOST ) ( XHAJV( NT ), NT=1, JVTMAX )

         IF ( IOST /= 0 ) THEN
            XMSG = 'Error reading HOURS from JVALUE file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...read number of reactions..........................................
 
         READ( JVUNIT, *, IOSTAT = IOST ) JPHOT

         IF ( IOST /= 0 ) THEN
            XMSG = 'Error reading number of REACTIONS from JVALUE file'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

C...make sure number of reactions is correct

         IF ( JPHOT /= NPHOTAB ) THEN
            XMSG = 'Photolysis reactions on JVALUE file do not '
     &             //'match the expected list (NPHOTAB)'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END IF

C...allocate arrays dependent on number of photolysis reactions

        ALLOCATE ( PHOTNM( JPHOT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT /= 0 ) THEN
          XMSG = 'Failure allocating PHOTNM'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( ACLD( JPHOT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT /= 0 ) THEN
          XMSG = 'Failure allocating ACLD'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

        ALLOCATE ( PHID( JPHOT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT /= 0 ) THEN
          XMSG = 'Failure allocating PHID'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read reaction id's and ACLD array

         XMSG = 'Error reading REACTIONS and ACLD from JVALUE file'
         DO NPHOT = 1, JPHOT
            READ( JVUNIT, *, IOSTAT = IOST ) PHOTNM( NPHOT ),
     &                                       ACLD( NPHOT )
            IF ( IOST /= 0 )
     &         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END DO

C...check the file list
         DO NPHOT = 1, JPHOT
            PHID( NPHOT ) = 0
         END DO
         XMSG = 'File data does not have all required phot tables'
         DO NPHOT = 1, NPHOTAB
            PHID( NPHOT ) = INDEX1( PHOTAB( NPHOT ), JPHOT, PHOTNM )
            IF ( PHID( NPHOT ) <= 0 )
     &         CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         END DO

C...allocate the XJVAL array

        ALLOCATE ( XJVAL( JPHOT, JVTMAX, JVLAT, JVHT ), STAT = ALLOCSTAT )
        IF ( ALLOCSTAT /= 0 ) THEN
          XMSG = 'Failure allocating XJVAL'
          CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
        END IF

C...read the j-values

         XMSG = 'Error reading jvalues from JVALUE file'
         DO NHT = 1, JVHT
            DO NLAT = 1, JVLAT
               DO NPHOT = 1, JPHOT

                  READ( JVUNIT, *, IOSTAT = IOST ) NHTO, NLATO, NPHOTO

                  IF ( IOST /= 0 ) 
     &               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

                  IF ( PHID( NPHOT ) /= 0 ) THEN

                     READ( JVUNIT, *, IOSTAT = IOST )
     &                ( XJVAL( PHID( NPHOT ), NT, NLAT, NHT ),
     &                                       NT = 1, JVTMAX )

                     IF ( IOST /= 0 )
     &                 CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

                  ELSE

                     READ( JVUNIT, *, IOSTAT = IOST ) DUMP

                     IF ( IOST /= 0 ) 
     &                 CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )

                  END IF
               END DO
            END DO
         END DO

C...close the jvalue file

         CLOSE ( JVUNIT )

      END IF   ! END OF FIRST TIME
 
C...compute XLHA (local hr angle) deviation from noon
C...  correct for current *positive* West longitude convention
 
      CURRHR = STRTHR 
     &       + FLOAT ( SECSDIFF ( STDATE, STTIME, JDATE, JTIME ) )
     &       / 3600.0 
      XLHA = CURRHR + LON / 15.0 - 12.0
      NDAYS = NINT ( XLHA / 24.0 )
      XLHA = ABS ( XLHA - NDAYS * 24.0 )

      IF (XLHA > XHAJV(JVTMAX)) THEN

C.... If sun below horizon, zero photolysis rates & exit

         RJ = 0.

         LFLAG_DARK = .TRUE.

         RETURN

      END IF

      LFLAG_DARK = .FALSE.
 
C...compute clear-sky photolysis rates
 
C...Compute interpolation indices and weighting factors

C.... Compute hr angle interpolation indices
      DO NT = 2, JVTMAX
         JVTM = NT
         IF (XLHA <= XHAJV(JVTM)) EXIT
      END DO
 
C...hr angle weighting factors
 
      FTIMO = ( XHAJV( JVTM ) - XLHA )
     &      / ( XHAJV( JVTM ) - XHAJV( JVTM - 1 ) )
      OMFTIMO = ONE - FTIMO

C....Compute latitude interpolation indices
 
      DO NLAT = 2, JVLAT
         JLATN = NLAT
         IF (LAT <= XLATJV(JLATN)) EXIT
      END DO

C...latitude weighting factors
 
      FLATS = ( XLATJV( JLATN ) - LAT )
     &      / ( XLATJV( JLATN ) - XLATJV( JLATN - 1 ) )
      OMFLATS = ONE - FLATS
 
C...height interpolation indices 
      ZHT = Z 
      ZHT = MIN( MAX(ZHT,XZJV(1)), XZJV(JVHT) )
 
      DO NHT = 2, JVHT
         KHTA = NHT
         IF ( ZHT <= XZJV(KHTA)) EXIT
      END DO
  
C...height weighting factors
 
      FHTA = ( XZJV( KHTA ) - ZHT )
     &     / ( XZJV( KHTA ) - XZJV( KHTA - 1 ) )
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

C....Interpolate all photolysis rates

      DO JP = 1,NPHOTAB
         JVAL = JWT( 1 ) * XJVAL( JP, JVTM, JLATN, KHTA )
     &        + JWT( 2 ) * XJVAL( JP, JVTM - 1, JLATN, KHTA )
     &        + JWT( 3 ) * XJVAL( JP, JVTM, JLATN - 1, KHTA )
     &        + JWT( 4 ) * XJVAL( JP, JVTM - 1, JLATN - 1, KHTA )
     &        + JWT( 5 ) * XJVAL( JP, JVTM, JLATN, KHTA - 1 )
     &        + JWT( 6 ) * XJVAL( JP, JVTM - 1, JLATN, KHTA - 1 )
     &        + JWT( 7 ) * XJVAL( JP, JVTM, JLATN - 1, KHTA - 1 )
     &        + JWT( 8 ) * XJVAL( JP, JVTM - 1, JLATN - 1, KHTA - 1 )
         RJ(JP) = MAX( JVAL, 0.)
      END DO

C   At this point, clear sky photolysis rates have been calculated
C...  Only proceed if interested in cloud effects on RJ

      IF ( CLDATT /= 0 ) THEN
 
C...inclination angle used for zenith angle calculation

         INCANG = 23.5 * SINE ( ( JDSTRT + CURRHR / 24.0 - 81.1875 )
     &                       * ( 90.0 / 91.3125 ) )


C...calculate cloud correction factors
C...  first compute the liquid water path in g/m2
         IF ( CLDFR >= 1.0E-05 ) THEN

            LWP   = ( CTOP - CBASE ) * WBAR

C...Calculate the cloud optical depth using a formula derived from
C...  Stephens (1978), JAS(35), pp2111-2132.
C...  only calculate the cloud optical depth when the liquid water
C...  path is >= 10 g/m2

            IF ( LWP >= 10.0 ) THEN
               CLOD = 10.0**( 0.2633 + 1.7095 * LOG( LOG10( LWP ) ) )
            ELSE
               CLOD = 0.0
            END IF

C...If no cloud or optical depth < 5, set clear sky values.
C...  (i.e. don't do anything)

            IF ( CLOD >= 5.0 ) THEN
               ZTERR = Z - TERR
               ZREL  = (ZTERR - CBASE) / (CTOP - CBASE)

C...cos of the zenith angle, ( <= cos 60 degrees )

               ZEN = MAX ( SINE ( LAT ) * SINE ( INCANG )
     &                   + COSINE ( LAT ) * COSINE ( INCANG )
     &                   * COSINE ( XLHA * 15.0 ), 0.5 )
               TRANS = ( 5.0 - EXP ( -CLOD ) ) / ( 4.0 + 0.42 * CLOD )

C...calculate cloud correction factors

C...  below cloud base

               FCLDB = 1.0 + CLDFR * ( 1.6 * ZEN * TRANS - 1.0 )
               X1 = CLDFR * ZEN * ( 1.0 - TRANS )
               X2 = FCLDB * ( 1.0 - ZREL )

C...  above cloud top

               DO JP = 1, NPHOTAB
C  * above cloud top
                  FCLDA = 1.0 + X1 * ACLD( PHID( JP ) )

C  * in cloud - linearly interpolate between base and top value

                  FCLD = FCLDA * ZREL + X2
                  IF ( ZTERR < CBASE ) FCLD = FCLDB
                  IF ( ZTERR >  CTOP ) FCLD = FCLDA
                  RJ(JP) = FCLD*RJ(JP)
               END DO
            END IF
         END IF
      END IF

      RETURN
      END
