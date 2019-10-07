

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PROCESS_CSQYS ( )
         
C*********************************************************************
C
C  the subroutine readcsqy reads the absorption cross section/quantum
C     yield file(s) and writes them to CSQY_DATA (JTABLE_UNIT).  The input 
C     data are
C
C     CS(nwl,NPHOTAB)        - absorption cross sections for NR species.
C     QY(nwl,NPHOTAB)        - quantum yields
C
C
C*********************************************************************


      USE GET_ENV_VARS
      USE BIN_DATA
      USE CSQY_PARAMETERS

      IMPLICIT NONE

C...........PARAMETERS and their descriptions
      
      INTEGER, PARAMETER ::  XSTAT1 =  1            ! I/O ERROR exit status
      INTEGER, PARAMETER ::  XSTAT2 =  2            ! I/O ERROR exit status

C...........LOCAL VARIABLES and their descriptions:
      

      CHARACTER(16)  :: PNAME = 'PROCESS_CSQYS'  ! program name

      CHARACTER(16)  :: PHOTID              ! reaction id's
      CHARACTER(16)  :: SPECTRA_NAME        ! JVALUES name
      CHARACTER( 1)  :: SPECTRA_TYPE        ! cs/qy spectra type
      CHARACTER(25)  :: CSQY_LABEL
      
      CHARACTER(255) :: EQNAME                          ! name of environment varaible
      CHARACTER(586) :: CSQY_DATA_RAW = 'CSQY_DATA_RAW' ! directory for CSQY data
      CHARACTER(600) :: CQFILE                          ! input filename buffer
      CHARACTER(132) :: MSG    = '   '                  ! message
      CHARACTER(120) :: FILE_LINE

      LOGICAL, SAVE      :: WRITE_AE_REFRACT = .FALSE. ! .FALSE.

      CHARACTER(  5 )    :: WVL_AE_REFRAC
      CHARACTER( 13 )    :: AERO_INDEX    = 'WVL_AE_REFRAC'

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

      LOGICAL   :: EXISTS


      INTERFACE       
        SUBROUTINE WRT_CSQY_DATA( WLIN, CS_IN, QY_IN, NWLIN, SPECTRA_NAME, SPECTRA_TYPE,
     &                    WLL_AVE, WLU_AVE, CS_AVE, QY_AVE, NWL_AVE )
             CHARACTER( 1), INTENT( IN )  :: SPECTRA_TYPE    ! spectra type
             CHARACTER(16), INTENT( IN )  :: SPECTRA_NAME    ! spectra type
             INTEGER,       INTENT( IN )  :: NWLIN           ! number of intervals CQin
             REAL,          INTENT( IN )  :: WLIN ( : )      ! wl for CQin
             REAL,          INTENT( IN )  :: CS_IN( : )      ! cross-section as f(WLIN)
             REAL,          INTENT( IN )  :: QY_IN( : )      ! quantum yield as f(WLIN)
             REAL,          INTENT( OUT)  :: WLL_AVE( : )    ! lower limit on wl int ETin
             REAL,          INTENT( OUT ) :: WLU_AVE( : )    ! upper limit on wl int ETin
             REAL,          INTENT( OUT ) :: CS_AVE(  : )    ! cross-section as f(WL_AVE)
             REAL,          INTENT( OUT ) :: QY_AVE(  : )    ! quantum yield as f(WL_AVE)
             INTEGER,       INTENT( OUT ) :: NWL_AVE
          END SUBROUTINE WRT_CSQY_DATA
          SUBROUTINE WRT_CSQY_DATA_ONLY( WLIN, CS_IN, QY_IN, NWLIN, SPECTRA_NAME, SPECTRA_TYPE,
     &                    WLL_AVE, WLU_AVE, CS_AVE, QY_AVE, NWL_AVE )
             CHARACTER( 1), INTENT( IN )  :: SPECTRA_TYPE    ! spectra type
             CHARACTER(16), INTENT( IN )  :: SPECTRA_NAME    ! spectra type
             INTEGER,       INTENT( IN )  :: NWLIN           ! number of intervals CQin
             REAL,          INTENT( IN )  :: WLIN ( : )      ! wl for CQin
             REAL,          INTENT( IN )  :: CS_IN( : )      ! cross-section as f(WLIN)
             REAL,          INTENT( IN )  :: QY_IN( : )      ! quantum yield as f(WLIN)
             REAL,          INTENT( OUT)  :: WLL_AVE( : )    ! lower limit on wl int ETin
             REAL,          INTENT( OUT ) :: WLU_AVE( : )    ! upper limit on wl int ETin
             REAL,          INTENT( OUT ) :: CS_AVE(  : )    ! cross-section as f(WL_AVE)
             REAL,          INTENT( OUT ) :: QY_AVE(  : )    ! quantum yield as f(WL_AVE)
             INTEGER,       INTENT( OUT ) :: NWL_AVE
          END SUBROUTINE WRT_CSQY_DATA_ONLY
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
        SPECTRA_NAME = TRIM( CQFILE )

        EQNAME  = 'CSQY_DATA_RAW'
        LASTNB2 = LEN_TRIM( EQNAME )
        EQNAME = TRIM( CSQY_DATA_RAW )
        CALL VALUE_NAME( EQNAME, CSQY_DATA_RAW ) 
        CQFILE  = TRIM( CSQY_DATA_RAW ) // '/' // TRIM( CQFILE )

        INQUIRE( FILE = CQFILE, EXIST = EXISTS )

        IF( .NOT. EXISTS )THEN
          MSG = 'Data file, ' // TRIM( CQFILE ) // ', not found.'
          WRITE(*,*)MSG
          STOP
        END IF


        OPEN( UNIT = CQUNIT,
     &        FILE = CQFILE,
     &        STATUS = 'OLD',
     &        IOSTAT = IOST )
         

C...check for open errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Could not open ' // PHOTAB( IPHOT ) // ' data file'
          WRITE(*,*)MSG
          STOP
        END IF

        WRITE( 6, 2001 ) CQUNIT, CQFILE


C...read photolysis subgroup id

        READ( CQUNIT, '(A)', IOSTAT = IOST ) PHOTID


!!!!!!! IOST = 0

        IF( PHOT_PROCESS( IPHOT ))THEN
           WRITE(MODULE_UNIT,'(A)')'C...' // TRIM( PHOTAB(IPHOT) )
        ENDIF

C...check for read errors

        IF ( IOST .GT. 0) THEN
          MSG = 'Errors occurred while reading PHOTID for ' //
     &           PHOTAB( IPHOT )
          WRITE(*,*)MSG
          STOP
        ELSE IF ( IOST .LT. 0) THEN
          MSG = 'Premature End of File reached while reading PHOTID for ' //
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
!       READ( CQUNIT, *, IOSTAT = IOST ) FACTOR


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
        DO IWL = 2, NWLIN
          IF( WLIN( IWL ) .LE. WLIN( IWL-1 ) )THEN
                WRITE(6,'(3(A/))') TRIM(  CQFILE ) // ': contains the below error',
     &          'consequentives lines with equal or decrease wavelengths',
     &          'Last two lines read:'
                WRITE(6,*) WLIN( IWL-1 ), CSIN( IWL-1 ), QYIN( IWL-1 )
                WRITE(6,*) WLIN( IWL ),   CSIN( IWL ),   QYIN( IWL )
                STOP
          END IF
        END DO
C...transform the cs data to the same wavelength intervals as
C...  the irradiance data.


       WRITE(6,*)'For ',TRIM( SPECTRA_NAME ),' SPECTRA_TYPE is ',TRIM(SPECTRA_TYPE)
       
       IF(SPLIT_OUTPUTS)THEN
           CALL WRT_CSQY_DATA_ONLY( WLIN, CSIN, QYIN, NWLIN, SPECTRA_NAME, 
     &                              SPECTRA_TYPE, WLL_AVE, WLU_AVE, CSOUT, QYOUT, NWL_AVE)
       ELSE
! use WRT_CSQY_DATA if the old version of CSQY_DATA is wanted.
           CALL WRT_CSQY_DATA( WLIN, CSIN, QYIN, NWLIN, SPECTRA_NAME, 
     &                     SPECTRA_TYPE, WLL_AVE, WLU_AVE, CSOUT, QYOUT, NWL_AVE)       
       END IF
    

C...load output arrays with integrated data
        NWL = N_INLINE_BAND

        DO IWL = 1, NWL
          CS( IWL, IPHOT ) = CSOUT( IWL )
          QY( IWL, IPHOT ) = QYOUT( IWL )
        END DO


801   CONTINUE
      
      IF( NPHOTAB .EQ. 0 )THEN
          SPECTRA_TYPE = 'NULL'
          WLIN = 0.0;  CSIN = 0.0; NWLIN = 0; SPECTRA_NAME = 'NULL'
          IF(SPLIT_OUTPUTS)THEN
             CALL WRT_CSQY_DATA_ONLY( WLIN, CSIN, QYIN, NWLIN, SPECTRA_NAME,
     &                                SPECTRA_TYPE, WLL_AVE, WLU_AVE, CSOUT, QYOUT, NWL_AVE)
          ELSE
! use WRT_CSQY_DATA if the old version of CSQY_DATA is wanted.
             CALL WRT_CSQY_DATA( WLIN, CSIN, QYIN, NWLIN, SPECTRA_NAME,
     &                           SPECTRA_TYPE, WLL_AVE, WLU_AVE, CSOUT, QYOUT, NWL_AVE)
          END IF
      END IF

      RETURN
C...formats

1001  FORMAT( A )
1003  FORMAT( A1 )
1005  FORMAT( /, 4X, F10.1 )

2001  FORMAT( 1X, '...Opening File on UNIT ', I2, /, 1X, A255 )
2003  FORMAT( 1X, '...Data for ', I4, ' wavelengths read from file',
     &        // )

      END
