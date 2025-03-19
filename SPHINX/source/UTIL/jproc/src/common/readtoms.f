
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/readtoms.f,v 1.6 2011/10/29 01:03:54 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)readtoms.F	1.2 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.readtoms.F 04 Jul 1997 09:40:36

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE READTOMS ( JDT, JVLAT, XLATJV, DOBNEW )

C*********************************************************************
C
C  This routine reads the daily gridded data for real-time satellite
C     total ozone column (TOC) data.  variables lat and lon contain
C     the latitudes and longitudes of the center of each of the grid
C     cells in the array ozone.                                    
C
C  HISTORY:
C     10/7/2009 S.Roselle  Updated to read TOMS datasets with different
C                          lat/lon grid sizes (e.g. OMI); expanded
C                          latitudinal band averaging to full width
C                          of TOMS dataset
C     04/11/96  S.Roselle  Modified program to conform with Models3
C                          coding standards and to average latitudinal
C                          bands of TOMS values
C     04/10/96  Program received from NASA via web/ftp
C
C*********************************************************************

      USE GET_ENV_VARS

      IMPLICIT NONE

C...........ARGUMENTS and their descriptions

      INTEGER      JDT                ! julian date (yyyyddd)
      INTEGER      JVLAT              ! number of output latitudes
      REAL         XLATJV( JVLAT )    ! latitudes
      REAL         DOBNEW( JVLAT )    ! average TOC values

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(16)  :: TMFILE = 'TOMS'     ! TOMS i/o logical name
      CHARACTER(16)  :: PNAME  = 'READTOMS'
      CHARACTER(80)  :: MSG = '    '
      CHARACTER(255) :: EQNAME              ! full name of TOMS file

      INTEGER      ILAT               ! latitude index
      INTEGER      JLAT               ! latitude index
      INTEGER      ILON               ! longitude index
      INTEGER      NLAT               ! number of input latitudes
      INTEGER      NLON               ! number of input longitudes
      INTEGER      DDD                ! julian day
      INTEGER      YYYY               ! year
      INTEGER      TJDATE             ! julian date for toms data
      INTEGER      IOST               ! io status
      INTEGER      TMUNIT             ! unit number for TOMS file
      INTEGER      ALLOCSTAT          ! allocate status
      INTEGER      COUNT( JVLAT )     ! # points included in TOC average
      INTEGER, ALLOCATABLE :: OZONE( :,: )

      REAL         STLAT, STLON       ! starting point lat,lon
      REAL         ENDLAT, ENDLON     ! ending point lat,lon
      REAL         DLAT
      REAL         DLON
      REAL, ALLOCATABLE :: LAT( : )
      REAL, ALLOCATABLE :: LON( : )

C*********************************************************************
C     begin body of program READTOMS
                                                              
C...calculate latitudes and longitudes                   

C...open the input file

      TMUNIT = JUNIT( )
      CALL VALUE_NAME ( TMFILE, EQNAME )

      OPEN ( UNIT = TMUNIT,
     &       FILE = EQNAME,
     &       FORM = 'FORMATTED',
     &       STATUS = 'OLD',
     &       IOSTAT = IOST )

C...check for open errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Could not open the TOMS data file'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        STOP
      END IF

      WRITE( 6, 2001 ) TMUNIT, EQNAME

C...read in the header lines

      READ( TMUNIT, 1001 ) DDD, YYYY
      TJDATE = YYYY * 1000 + DDD

C...check to see if julian date of file matches julian date requested
C...  and warn user if they do not match

      IF ( TJDATE .NE. JDT ) THEN
        MSG = 'Julian date of TOMS file does not match requested date '
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
      END IF
      
C...read longitude info and allocate array

      READ( TMUNIT, 1003 ) NLON, STLON, ENDLON, DLON

      ALLOCATE ( LON( NLON ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
        MSG = 'Failure allocating LON'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        STOP
      END IF

      DO ILON = 1, NLON
        LON( ILON ) = -STLON + ( ILON - 1 ) * DLON
      END DO

C...read latitude info and allocate array

      READ( TMUNIT, 1003 ) NLAT, STLAT, ENDLAT, DLAT

      ALLOCATE ( LAT( NLAT ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
        MSG = 'Failure allocating LAT'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        STOP
      END IF

      DO ILAT = 1, NLAT
        LAT( ILAT ) = -STLAT + ( ILAT - 1 ) * DLAT
      END DO

C...allocate and read in the data into the array ozone

      ALLOCATE ( OZONE( NLON, NLAT ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
        MSG = 'Failure allocating OZONE'
        WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
        STOP
      END IF

      DO ILAT = 1, NLAT
        READ( TMUNIT, 1005, IOSTAT = IOST ) ( OZONE( ILON, ILAT ),
     &                                               ILON = 1, NLON )

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading TOMS file'
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          STOP
        END IF

      END DO

C...close the input file

      CLOSE( TMUNIT )

C...process/print the ozone data

      WRITE( 6, 2003 )

      DO JLAT = 1, JVLAT

        COUNT ( JLAT ) = 0
        DOBNEW( JLAT ) = 0.0

        DO ILAT = 1, NLAT

          IF ( ( LAT( ILAT ) .GT. ( XLATJV( JLAT ) - 5.0 ) ) .AND.
     &         ( LAT( ILAT ) .LT. ( XLATJV( JLAT ) + 5.0 ) ) ) THEN

            DO ILON = 1, NLON
               IF ( OZONE( ILON, ILAT ) .GT. 0 ) THEN
                  COUNT ( JLAT ) = COUNT( JLAT ) + 1
                  DOBNEW( JLAT ) = DOBNEW( JLAT )
     &                           + FLOAT( OZONE( ILON, ILAT ) )
               END IF
            END DO

          END IF

        END DO

        IF ( COUNT( JLAT ) .GT. 0 ) THEN
          DOBNEW( JLAT ) = DOBNEW( JLAT ) / FLOAT( COUNT( JLAT ) )
          WRITE( 6, 2005 ) XLATJV( JLAT ), COUNT( JLAT ), DOBNEW( JLAT )
        ELSE
        	WRITE( 6, 2007 ) XLATJV( JLAT )
        END IF
      END DO

C...format statements

1001  FORMAT( 6X, I3, 9X, I4 )
1003  FORMAT( 13X, I4, 18X, F7.3, 6X, F7.3, 5X, F4.2 )
1005  FORMAT( 1X, 25I3 )
2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255 )
2003  FORMAT( 1X, 'Processing Total Ozone Column Data' )
2005  FORMAT( 7X, 'Latitude:', 1X, F5.1, ', N=', I5, ', Mean TOC=', F9.4 )
2007  FORMAT( 7X, 'Latitude:', 1X, F5.1, ', TOC=0.0' )

      RETURN
      END
