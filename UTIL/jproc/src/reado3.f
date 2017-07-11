
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
C $Header: /project/yoj/arc/JPROC/src/driver/jproc_table/reado3.f,v 1.5 2011/10/29 01:03:54 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)reado3.F	1.2 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.reado3.F 04 Jul 1997 09:40:19

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE READO3 ( NWL, STWL, ENDWL, O3ABS )
         
C*********************************************************************
C
C  the subroutine reads the absorption cross section
C     The input data are
C
C     O3ABS - absorption cross sections for molecular oxygen
C
C*********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

      INCLUDE 'JVALPARMS.EXT'    ! jproc parameters

C...........ARGUMENTS and their descriptions
      
      REAL         ENDWL( MXWL )       ! wavelength band upper limit
      REAL         O3ABS( MXWL )       ! output absorp. cross sections
      REAL         STWL ( MXWL )       ! wavelength band lower limit

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(1)   :: TYPE               ! cs/qy spectra type
      CHARACTER(16)  :: O3FILE = 'O3ABS'   ! input filename buffer
      CHARACTER(16)  :: PHOTID             ! reaction id's
      CHARACTER(16)  :: PNAME  = 'READO3'  ! program name
      CHARACTER(80)  :: MSG = '    '       ! message
      CHARACTER(255) :: EQNAME

      INTEGER      IOST                ! i/o status
      INTEGER      IWL                 ! wavelength index
      INTEGER      NWL                 ! # of wlbands
      INTEGER      NWLIN               ! # of wlbands (infile)
      INTEGER      O3UNIT              ! cross section io unit

      REAL         FACTOR              ! multiplying factor for CS
      REAL         CSOUT( MXWL )       ! integrated absorp. cross sect.
      REAL         WLIN( MXWLIN )      ! wl for input cs/qy data
      REAL         CSIN( MXWLIN )      ! raw absorption cross sections

C*********************************************************************
C     begin body of subroutine

C...get a unit number for CSQY files

      CALL NAMEVAL ( O3FILE, EQNAME )
      O3UNIT = JUNIT( )

C...open input file

      OPEN( UNIT = O3UNIT,
     &      FILE = EQNAME,
     &      STATUS = 'OLD',
     &      IOSTAT = IOST )

C...check for open errors

      IF ( IOST .NE. 0) THEN
        MSG = 'Could not open the O3ABS data file'
        CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      WRITE( 6, 2001 ) O3UNIT, EQNAME

C...read photolysis subgroup id

      READ( O3UNIT, 1001, IOSTAT = IOST ) PHOTID

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading PHOTID from O3ABS file'
        CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

C...get type of data (e.g. centered, beginning, ending, or point wavelen

101   CONTINUE

      READ( O3UNIT, 1003, IOSTAT = IOST ) TYPE

C...check for read errors

      IF ( IOST .NE. 0) THEN
        MSG = 'Errors occurred while reading TYPE from O3ABS file'
        CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      IF ( TYPE .EQ. '!' ) GO TO 101

C...read the factor to multiply cross sectionS by

      READ( O3UNIT, 1005, IOSTAT = IOST ) FACTOR

C...check for read errors

      IF ( IOST .NE. 0 ) THEN
        MSG = 'Errors occurred while reading FACTOR from O3ABS file'
        CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

C...reinitialize arrays

      DO IWL = 1, MXWL
        WLIN( IWL ) = 0.0
        CSIN( IWL ) = 0.0
      END DO

C...loop over the number of wavelengths and continue reading

      IWL = 0
201   CONTINUE

        IWL = IWL + 1
        READ( O3UNIT, *, IOSTAT = IOST ) WLIN( IWL ), CSIN( IWL )
        CSIN( IWL ) = CSIN( IWL ) * FACTOR

C...check for read errors

        IF ( IOST .GT. 0 ) THEN
          MSG = 'Errors occurred while reading WL,CS from O3ABS file'
          CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
        END IF

C...end loop if we reach EOF, otherwise continue looping

      IF ( IOST .EQ. 0 ) GO TO 201

C...adjust loop counter index index and close file

      NWLIN = IWL - 1
      CLOSE( O3UNIT )

      WRITE( 6, 2003 ) NWLIN

C...transform the cs data to the same wavelength intervals as
C...  the irradiance data.

        CALL INTAVG ( WLIN, CSIN, NWLIN, TYPE,
     &                STWL, ENDWL, CSOUT, NWL )

C...load output arrays with integrated data
        
      DO IWL = 1, NWL
        O3ABS( IWL ) = CSOUT( IWL )
      END DO


C...formats

1001  FORMAT( A16 )
1003  FORMAT( A1 )
1005  FORMAT( /, 4X, F10.1 )

2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255 )
2003  FORMAT( 1X, '...Data for ', I4, ' wavelengths read from file',
     &        // )

      RETURN
      END
