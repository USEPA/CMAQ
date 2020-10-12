
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/readcsqy.F,v 1.7 2011/12/08 16:41:35 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)readcsqy.F	1.4 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.readcsqy.F 04 Jul 1997 09:39:29

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE READCSQY ( NWL, STWL, ENDWL, CS, QY )
         
C*********************************************************************
C
C  the subroutine readcsqy reads the absorption cross section/quantum
C     yield file(s).  The input data are
C
C     CS(nwl,NPHOTAB)        - absorption cross sections for NR species.
C     QY(nwl,NPHOTAB)        - quantum yields
C
C    S.Roselle  1/30/96  Subroutine created, uses generalized method
C                        for reading CS/QY data that ties in with the
C                        chemistry mechanism reader.
C    S.Roselle  7/25/96  Revised subroutine to call INTAVG, passing
C                        data type (e.g., point, centered,
C                        beginning, and ending data)
C
C*********************************************************************

      USE GET_ENV_VARS
      USE RXNS_DATA

      IMPLICIT NONE

      INCLUDE 'JVALPARMS.EXT'    ! jproc parameters

C...........ARGUMENTS and their descriptions
      
      REAL         STWL ( MXWL )       ! wavelength band lower limit
      REAL         ENDWL( MXWL )       ! wavelength band upper limit
      REAL         CS( MXWL, NPHOTAB )  ! output absorp. cross sections
      REAL         QY( MXWL, NPHOTAB )  ! output quantum yields

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(1)   :: TYPE               ! cs/qy spectra type
      CHARACTER(16)  :: PNAME = 'READCSQY' ! program name
      CHARACTER(16)  :: CQDIR = 'CSQY'     ! directory for CSQY data
      CHARACTER(16)  :: PHOTID             ! reaction id's
      CHARACTER(80)  :: MSG   = '    '     ! message
      CHARACTER(255) :: CQFILE             ! input filename buffer
      CHARACTER(255) :: EQNAME

      INTEGER      IWL                 ! wavelength index
      INTEGER      NWL                 ! # of wlbands
      INTEGER      NWLIN               ! # of wlbands (infile)
      INTEGER      IPHOT               ! reaction index
      INTEGER      CQUNIT              ! cross section/qy io unit
      INTEGER      IOST                ! io status

      REAL         FACTOR              ! multiplying factor for CS
      REAL         WLIN ( MXWLIN )     ! wl for input cs/qy data
      REAL         CSIN ( MXWLIN )     ! raw absorption cross sections
      REAL         QYIN ( MXWLIN )     ! raw quantum yields
      REAL         CSOUT( MXWL )       ! integrated absorp. cross sect.
      REAL         QYOUT( MXWL )       ! integrated quantum yields

C*********************************************************************
C     begin body of subroutine READCSQY

C...get a unit number for CSQY files

      CQUNIT = JUNIT( )

C...loop over the number of reactions, reading each file

      DO 801 IPHOT = 1, NPHOTAB

C...open input file

        CQFILE = PHOTAB( IPHOT )
        CALL VALUE_NAME ( CQDIR, EQNAME )
        CQFILE = TRIM( EQNAME ) // '/' // TRIM( CQFILE )

        OPEN( UNIT = CQUNIT,
     &        FILE = CQFILE,
     &        STATUS = 'OLD',
     &        IOSTAT = IOST )

C...check for open errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Could not open ' // PHOTAB( IPHOT ) // ' data file'
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          STOP
        END IF

        WRITE( 6, 2001 ) CQUNIT, CQFILE

C...read photolysis subgroup id

        READ( CQUNIT, 1001, IOSTAT = IOST ) PHOTID

C...check for read errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading PHOTID for ' //
     &           PHOTAB( IPHOT )
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          STOP
        END IF

C...get type of data (e.g. centered, beginning, ending, or point wavelen

101     CONTINUE

        READ( CQUNIT, 1003, IOSTAT = IOST ) TYPE

C...check for read errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading TYPE for ' //
     &           PHOTAB( IPHOT )
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          STOP
        END IF

        IF ( TYPE .EQ. '!' ) GO TO 101

C...read the factor to multiply cross sectionS by

        READ( CQUNIT, 1005, IOSTAT = IOST ) FACTOR

C...check for read errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading FACTOR for ' //
     &           PHOTAB( IPHOT )
          WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
          STOP
        END IF

C...reinitialize arrays

        DO IWL = 1, MXWL
          WLIN( IWL ) = 0.0
          CSIN( IWL ) = 0.0
          QYIN( IWL ) = 0.0
        END DO

C...loop over the number of wavelengths and continue reading

        IWL = 0
201     CONTINUE

          IWL = IWL + 1
          READ( CQUNIT, *, IOSTAT = IOST ) WLIN( IWL ), CSIN( IWL ),
     &                                     QYIN( IWL )
          CSIN( IWL ) = CSIN( IWL ) * FACTOR

C...check for read errors

          IF ( IOST .GT. 0) THEN
            MSG = 'Errors occurred while reading WL,CS,QY for ' //
     &             PHOTAB( IPHOT )
            WRITE(6,'(A)')TRIM( PNAME ) // ': ', TRIM( MSG )
            STOP
          END IF

C...end loop if we reach EOF, otherwise continue looping

        IF ( IOST .EQ. 0 ) GO TO 201

C...adjust loop counter index index and close file

        NWLIN = IWL - 1
        CLOSE( CQUNIT )

        WRITE( 6, 2003 ) NWLIN

C...transform the cs data to the same wavelength intervals as
C...  the irradiance data.

        CALL INTAVG ( WLIN, CSIN, NWLIN, TYPE,
     &                STWL, ENDWL, CSOUT, NWL )

C...transform the qy data to the same wavelength intervals as
C...  the irradiance data.

        CALL INTAVG ( WLIN, QYIN, NWLIN, TYPE,
     &                STWL, ENDWL, QYOUT, NWL )

C...load output arrays with integrated data
        
        DO IWL = 1, NWL
          CS( IWL, IPHOT ) = CSOUT( IWL )
          QY( IWL, IPHOT ) = QYOUT( IWL )
        END DO

801   CONTINUE

C...formats

1001  FORMAT( A16 )
1003  FORMAT( A1 )
1005  FORMAT( /, 4X, F10.1 )

2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255 )
2003  FORMAT( 1X, '...Data for ', I4, ' wavelengths read from file',
     &        // )

      RETURN
      END
