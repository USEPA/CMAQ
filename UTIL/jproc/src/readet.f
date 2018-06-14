
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/readet.f,v 1.5 2011/10/29 01:03:54 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)readet.F	1.2 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.readet.F 04 Jul 1997 09:39:52

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE READET ( NWL, STWL, MIDWL, ENDWL, F )
         
C*********************************************************************
C
C  the subroutine readet reads the extra terrestrial radiation file.
C    The input data are:
C
C     NWL                   - number of wavelength bands
C     STWL(mxwl)            - array of nominal starting wavelengths of
C                             spectral interval
C     MIDWL(mxwl)           - array of nominal center wavelengths of
C                             spectral interval
C     ENDWL(mxwl)           - array of nominal ending wavelengths of
C                             spectral interval
C     F(mxwl)               - extraterrestrial solar irradiance
C
C*********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

      INCLUDE 'JVALPARMS.EXT'          ! jproc parameters

C...........ARGUMENTS and their descriptions
      
      REAL         ENDWL( MXWL )       ! wavelength band upper limit
      REAL         F    ( MXWL )       ! extra-terrestrial radiation
      REAL         MIDWL( MXWL )       ! wavelength midpoints
      REAL         STWL ( MXWL )       ! wavelength band lower limit

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(1)   :: TYPE                ! cs/qy spectra type
      CHARACTER(16)  :: ETFILE = 'ET'       ! ET i/o logical name
      CHARACTER(16)  :: PNAME  = 'READET'   ! program name
      CHARACTER(80)  :: MSG    = '    '     ! message
      CHARACTER(255) :: EQNAME              ! full name of ET file

      INTEGER      ETUNIT              ! extraterrestrial rad io unit
      INTEGER      IOST                ! io status
      INTEGER      IWL                 ! wavelength index
      INTEGER      NWL                 ! # of wlbands (infile)
      INTEGER      NWLIN               ! # of wlbands (infile)

      REAL         FACTOR              ! multiplying factor for F
      REAL         WLIN( MXWLIN )      ! wl for input ET data

C*********************************************************************
C     begin body of subroutine READET

C...open and read the wavelength bands and extraterrestrial radiation

      CALL NAMEVAL ( ETFILE, EQNAME )
      ETUNIT = JUNIT( )

      OPEN( UNIT = ETUNIT,
     &      FILE = EQNAME,
     &      STATUS = 'OLD',
     &      IOSTAT = IOST )

C...check for open errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Could not open the ET data file'
        CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      WRITE( 6, 2001 ) ETUNIT, EQNAME

C...get type of data (e.g. centered, beginning, ending wavelength

101   CONTINUE

      READ( ETUNIT, 1003, IOSTAT = IOST ) TYPE

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading TYPE from ET file'
        CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      IF ( TYPE .EQ. '!' ) GO TO 101

C...read the factor to multiply irradiance by

      READ( ETUNIT, 1005, IOSTAT = IOST ) FACTOR

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading FACTOR from ET file'
        CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

C...initialize arrays

      DO IWL = 1, MXWL
        WLIN( IWL ) = 0.0
        F   ( IWL ) = 0.0
      END DO

C...loop over the number of wavelengths and continue reading

      IWL = 0
201   CONTINUE

C...read the wavelength band data

        IWL = IWL + 1
        READ( ETUNIT, *, IOSTAT = IOST ) WLIN( IWL ), F( IWL )
        F( IWL ) = F( IWL ) * FACTOR

C...check for read errors

        IF ( IOST .GT. 0 ) THEN
          MSG = 'Errors occurred while reading WL,F from ET file'
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF

C...end loop if we reach EOF, otherwise continue looping

      IF ( IOST .EQ. 0 ) GO TO 201

C...adjust loop counter index index and close file

      NWLIN = IWL - 1
      CLOSE( ETUNIT )

      WRITE( 6, 2003 ) NWLIN

C...determine wl intervals for CENTERED WLBAND data

      IF ( TYPE .EQ. 'C' ) THEN

        NWL = NWLIN
        MIDWL( 1 ) = WLIN( 1 )
        STWL ( 1 ) = 0.5 * ( ( 3.0 * WLIN( 1 ) ) -  WLIN( 2 ) )
        ENDWL( 1 ) = 0.5 * ( WLIN( 1 ) + WLIN( 2 ) )
          
        DO IWL = 2, NWLIN-1
          MIDWL( IWL ) = WLIN( IWL )
          STWL ( IWL ) = 0.5 * ( WLIN( IWL ) + WLIN ( IWL - 1 ) )
          ENDWL( IWL ) = 0.5 * ( WLIN( IWL ) + WLIN ( IWL + 1 ) )
        END DO

        MIDWL( NWL ) = WLIN( NWLIN )
        STWL ( NWL ) = 0.5 * ( WLIN( NWLIN - 1 ) + WLIN( NWLIN ) )
        ENDWL( NWL ) = 0.5 * ( ( 3.0 * WLIN( NWLIN ) )
     &               - WLIN( NWLIN - 1 ) )

C...determine wl intervals for BEGINNING WLBAND data

      ELSE IF ( TYPE .EQ. 'B' ) THEN

        NWL = NWLIN - 1

        DO IWL = 1, NWLIN - 1
          STWL ( IWL ) = WLIN( IWL )
          MIDWL( IWL ) = 0.5 * ( WLIN( IWL ) + WLIN( IWL + 1 ) )
          ENDWL( IWL ) = WLIN( IWL + 1 )
        END DO

C...determine wl intervals for ENDING WLBAND data

      ELSE IF ( TYPE .EQ. 'E' ) THEN

        NWL = NWLIN - 1

        DO IWL = 2, NWLIN
          STWL ( IWL - 1 ) = WLIN( IWL - 1 )
          MIDWL( IWL - 1 ) = 0.5 * ( WLIN( IWL - 1 ) + WLIN( IWL ) )
          ENDWL( IWL - 1 ) = WLIN( IWL )
        END DO

C...stop program if wavelength data type not found

      ELSE

        MSG = 'Unrecognized spectra type in ' // PNAME
        CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT2 )

      END IF

C...formats

1001  FORMAT( A16 )
1003  FORMAT( A1 )
1005  FORMAT( /, 4X, F10.1 )

2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255, / )
2003  FORMAT( 1X, '...Data for ', I4, ' wavelengths read from file',
     &        // )

      RETURN
      END
