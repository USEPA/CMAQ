
C***********************************************************************
C   Portions of Models-3/CMAQ software were developed or based on      *
C   information from various groups: Federal Government employees,     *
C   contractors working on a United States Government contract, and    *
C   non-Federal sources (including research institutions).  These      *
C   research institutions have given the Government permission to      *
C   use, prepare derivative works, and distribute copies of their      *
C   work in Models-3/CMAQ to the public and to permit others to do     *
C   so.  EPA therefore grants similar permissions for use of the       *
C   Models-3/CMAQ software, but users are requested to provide copies  *
C   of derivative works to the Government without restrictions as to   *
C   use by others.  Users are responsible for acquiring their own      *
C   copies of commercial software associated with Models-3/CMAQ and    *
C   for complying with vendor requirements.  Software copyrights by    *
C   the MCNC Environmental Modeling Center are used with their         *
C   permissions subject to the above restrictions.                     *
C***********************************************************************


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/work/rep/JPROC/src/driver/jproc_table/readcsqy.F,v 1.4 2002/04/15 18:00:46 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)readcsqy.F	1.4 /project/mod3/JPROC/src/driver/jproc_table/SCCS/s.readcsqy.F 04 Jul 1997 09:39:29

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE READCSQY ( )
         
C*********************************************************************
C
C  the subroutine readcsqy reads the absorption cross section/quantum
C     yield file(s).  The input data are
C
C     CS(nwl,NPHOTAB)        - absorption cross sections for NR species.
C     QY(nwl,NPHOTAB)        - quantum yields
C
C
C*********************************************************************


      USE BIN_DATA
      USE JPROC_PROFILE

      IMPLICIT NONE

C...........PARAMETERS and their descriptions
      
      INTEGER, PARAMETER ::  XSTAT1 =  1            ! I/O ERROR exit status
      INTEGER, PARAMETER ::  XSTAT2 =  2            ! I/O ERROR exit status

C...........LOCAL VARIABLES and their descriptions:
      

      CHARACTER(16)  :: PNAME = 'READ_JPROC_CSQY'  ! program name
      CHARACTER(16)  :: CQDIR = 'CSQY'             ! directory for CSQY data

      CHARACTER(16)  :: PHOTID              ! reaction id's
      CHARACTER(16)  :: SPECTRA_NAME        ! JVALUES name
      CHARACTER( 1)  :: SPECTRA_TYPE        ! cs/qy spectra type
      CHARACTER(25)  :: CSQY_LABEL
      
      CHARACTER(255) :: EQNAME
      CHARACTER( 80) :: CQFILE              ! input filename buffer
      CHARACTER( 80) :: MSG    = '   '      ! message
      CHARACTER(120) :: FILE_LINE

      INTEGER      IWL                 ! wavelength index
      INTEGER      NWL                 ! # of wlbands
      INTEGER      NWLIN               ! # of wlbands (infile)
      INTEGER      IPHOT               ! reaction index
      INTEGER      CQUNIT              ! cross section/qy io unit
      INTEGER      IOST                ! io status
      INTEGER      LASTNB1
      INTEGER      LASTNB2

      REAL         STWL ( MXWL )       ! wavelength band lower limit
      REAL         ENDWL( MXWL )       ! wavelength band upper limit
      REAL         CS( MXWL, NPHOTAB ) ! output absorp. cross sections
      REAL         QY( MXWL, NPHOTAB ) ! output quantum yields

      REAL         FACTOR              ! multiplying factor for CS
      REAL         WLIN ( MXWLIN )     ! wl for input cs/qy data
      REAL         CSIN ( MXWLIN )     ! raw absorption cross sections
      REAL         QYIN ( MXWLIN )     ! raw quantum yields
      REAL         CSOUT( MXWL )       ! integrated absorp. cross sect.
      REAL         QYOUT( MXWL )       ! integrated quantum yields

      REAL      :: WLL_AVE( MXWL ) ! lower limit on wl int ETin
      REAL      :: WLU_AVE( MXWL ) ! upper limit on wl int ETin

C...........EXTERNAL FUNCTIONS and their descriptions:

      INTEGER      JUNIT               ! used to get next IO unit #
      INTEGER   :: NWL_AVE


      INTERFACE       
        SUBROUTINE WRT_CSQY_DATA( WLIN, CS_IN, QY_IN, NWLIN, SPECTRA_NAME, SPECTRA_TYPE,
     &                    WLL_AVE, WLU_AVE, CS_AVE, QY_AVE, NWL_AVE )
             USE JPROC_PHOT_DATA
             IMPLICIT NONE      
             CHARACTER( 1), INTENT( IN )  :: SPECTRA_TYPE    ! spectra type
             CHARACTER(16), INTENT( IN )  :: SPECTRA_NAME    ! spectra type
             INTEGER,       INTENT( IN )  :: NWLIN           ! number of intervals CQin
             REAL,          INTENT( IN )  :: WLIN ( MXWLIN ) ! wl for CQin
             REAL,          INTENT( IN )  :: CS_IN( MXWLIN ) ! cross-section as f(WLIN)
             REAL,          INTENT( IN )  :: QY_IN( MXWLIN ) ! quantum yield as f(WLIN)
             REAL,          INTENT( OUT)  :: WLL_AVE( MXWL ) ! lower limit on wl int ETin
             REAL,          INTENT( OUT ) :: WLU_AVE( MXWL ) ! upper limit on wl int ETin
             REAL,          INTENT( OUT ) :: CS_AVE(  MXWL ) ! cross-section as f(WL_AVE)
             REAL,          INTENT( OUT ) :: QY_AVE(  MXWL ) ! quantum yield as f(WL_AVE)
             INTEGER,       INTENT( OUT ) :: NWL_AVE
          END SUBROUTINE WRT_CSQY_DATA
      END INTERFACE

C*********************************************************************
C     begin body of subroutine READCSQY

C...get a unit number for CSQY files

      CQUNIT = 125

      CALL INIT_CXQY_MODULE()

C...loop over the number of reactions, reading each file

      DO 801 IPHOT = 1, NPHOTAB

C...open input file

        CQFILE       = PHOTAB( IPHOT )
        LASTNB1      = LEN_TRIM( CQFILE )
!        SPECTRA_NAME = CQFILE( 1:LASTNB1 )
        SPECTRA_NAME = TRIM( CQFILE )

        EQNAME  = 'CSQY_DATA_RAW'
        LASTNB2 = LEN_TRIM( EQNAME )
!       CQFILE  = EQNAME( 1:LASTNB2 ) // '/' // CQFILE( 1:LASTNB1 )
        CQFILE  = TRIM( EQNAME ) // '/' // TRIM( CQFILE )


        OPEN( UNIT = CQUNIT,
     &        FILE = CQFILE,
     &        STATUS = 'OLD',
     &        IOSTAT = IOST )

        OPEN( UNIT = CSQY_UNIT,
     &        FILE = 'CSQY_BLOCK.dat',
     &        STATUS = 'UNKNOWN',
     &        IOSTAT = IOST )
         

C...check for open errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Could not open ' // PHOTAB( IPHOT ) // ' data file'
          WRITE(*,*)MSG
	    STOP
        END IF

        WRITE( 6, 2001 ) CQUNIT, CQFILE


C...read photolysis subgroup id

        READ( CQUNIT, 1001, IOSTAT = IOST ) PHOTID



        IF( PHOT_PROCESS( IPHOT ))THEN
           WRITE(MODULE_UNIT,'(A)')'C...' // TRIM( PHOTAB(IPHOT) )
        ENDIF

C...check for read errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading PHOTID for ' //
     &           PHOTAB( IPHOT )
          WRITE(*,*)MSG
	    STOP
        END IF

C...get type of data (e.g. centered, beginning, ending, or point wavelen

101     CONTINUE

        READ( CQUNIT, '(A)', IOSTAT = IOST ) FILE_LINE

        SPECTRA_TYPE = FILE_LINE(1:1)

C...check for read errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading SPECTRA for ' //
     &           PHOTAB( IPHOT )
          WRITE(*,*)MSG
	  STOP
        END IF

        IF ( SPECTRA_TYPE .EQ. '!' )THEN
           FILE_LINE(1:1) = ' '
           IF( PHOT_PROCESS( IPHOT ))THEN
               WRITE(MODULE_UNIT,'(A)')'C..' // TRIM(FILE_LINE)
           ENDIF
           GO TO 101
        ENDIF

C...read the factor to multiply cross sectionS by

        READ( CQUNIT, 1005, IOSTAT = IOST ) FACTOR


C...check for read errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading FACTOR for ' //
     &           PHOTAB( IPHOT )
          WRITE(*,*)MSG
	    STOP
        END IF


C...reinitialize arrays

        DO IWL = 1, MXWLIN
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
          WRITE(*,*)MSG
	    STOP
          END IF

C...end loop if we reach EOF, otherwise continue looping

        IF ( IOST .EQ. 0 ) GO TO 201

C...adjust loop counter index index and close file

        NWLIN = IWL - 1
        CLOSE( CQUNIT )

C...transform the cs data to the same wavelength intervals as
C...  the irradiance data.


       WRITE(6,*)'For ',TRIM( SPECTRA_NAME ),' SPECTRA_TYPE is ',TRIM(SPECTRA_TYPE)
       
       CALL WRT_CSQY_DATA( WLIN, CSIN, QYIN, NWLIN, SPECTRA_NAME, 
     &                     SPECTRA_TYPE, WLL_AVE, WLU_AVE, CSOUT, QYOUT, NWL_AVE)
    

C...load output arrays with integrated data
        NWL = N_INLINE_BAND

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
