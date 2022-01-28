
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
C $Header$ 

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE AERO_REFRACT_INDEX ( NAERO_REFRACT, AERO_REFRACT_INDX, AERO_REAL_REFRACT,
     &                                AERO_IMAG_REFRACT )
         
C*********************************************************************
C
C  the subroutine reads and interpolates data files for the refractive
C  indices of the aerosol components.
C
C*********************************************************************


      USE GET_ENV_VARS       
      USE BIN_DATA
      USE CSQY_PARAMETERS

      IMPLICIT NONE
C...Arguements:

       INTEGER,         INTENT( IN )  :: NAERO_REFRACT             ! number of refractive indices
       CHARACTER( 16 ), INTENT( IN )  :: AERO_REFRACT_INDX( : )    ! names of refractive indices
       REAL,            INTENT( OUT ) :: AERO_IMAG_REFRACT( :, : ) ! imaginary part of index [Dimensionaless]
       REAL,            INTENT( OUT ) :: AERO_REAL_REFRACT( :, : ) ! real part of index [Dimensionaless]

C...........PARAMETERS and their descriptions
      
      INTEGER, PARAMETER :: XSTAT1  = 1             ! I/O ERROR exit status
      INTEGER, PARAMETER :: XSTAT2  = 2             ! Program ERROR exit status

C...........ARGUMENTS and their descriptions
      
      REAL         STWL ( MXWL )       ! wavelength band lower limit
      REAL         ENDWL( MXWL )       ! wavelength band upper limit

C...........LOCAL VARIABLES and their descriptions:

      CHARACTER(  1 ) :: SPECTRA_TYPE                  ! type of data points
      CHARACTER( 16 ) :: PNAME  = 'AERO_REFRACT_INDEX' ! program name

!      CHARACTER*16 CQDIR               ! directory for CSQY data
!      DATA         CQDIR   / 'CSQY' /
      
      CHARACTER(  16 ) :: REFRACT_ID         ! name of refractive index
      CHARACTER( 132 ) :: AERO_FILE          ! input filename buffer
      CHARACTER( 132 ) :: MSG  =  '    '     ! message

      CHARACTER( 120 ) FILE_LINE

      INTEGER      IWL                 ! wavelength index
      INTEGER      NWL                 ! # of wlbands
      INTEGER      NWLIN               ! # of wlbands (infile)
      INTEGER      IAERO               ! reaction index
      INTEGER      AEUNIT              ! cross section/qy io unit
      INTEGER      IOST                ! io status
      INTEGER      LASTNB1
      INTEGER      LASTNB2

      REAL         FACTOR                       ! multiplying factor for CS
      REAL         WLIN   ( MXWLIN )            ! wl for input cs/qy data
      REAL         REFRACT_OUT( MXWL )          ! interpolated index

      REAL, ALLOCATABLE :: REAL_REFRACT_IN ( : )   ! raw real refractive index
      REAL, ALLOCATABLE :: IMAG_REFRACT_IN ( : )   ! raw imaginary refractive index
      REAL, ALLOCATABLE :: WAVE( : ), XDUMB( : ), YDUMB( : ), ZDUMB( : )
      
      INTEGER           :: NDUMB
      REAL, ALLOCATABLE :: WAVE_OUTL( : ), WAVE_OUTU( : ), WAVE_OUTC( : )

      CHARACTER( 16 ) FILE_NAME
C...........EXTERNAL FUNCTIONS and their descriptions:

      INTEGER      JUNIT               ! used to get next IO unit #

      REAL      :: WLL_AVE( MXWL ) ! lower limit on wl int ETin
      REAL      :: WLU_AVE( MXWL ) ! upper limit on wl int ETin
      INTEGER   :: NWL_AVE

      CHARACTER(  25 ) :: CSQY_LABEL
     
      CHARACTER( 586 ) :: REFRACT_FILE
      CHARACTER(  32 ) :: ENV_VAR_NAME  
      
!      REAL, ALLOCATABLE :: AERO_IMAG_REFRACT( :, : )
!      REAL, ALLOCATABLE :: AERO_REAL_REFRACT( :, : )

      INTERFACE
        SUBROUTINE WVBIN_AVERAGE(WL_CS_IN, CS_IN, NWL_CS_IN,  
     &                         WL_QY_IN, QY_IN, NWL_QY_IN,  
     &                         SPECTRA_TYPE,
     &                         WLL_AVE, WLU_AVE, NWL_AVE, 
     &                         CS_AVE, QY_AVE )
          CHARACTER(1), INTENT( IN ) :: SPECTRA_TYPE        ! spectra type
          INTEGER, INTENT( IN )      :: NWL_AVE             ! number of intervals average 
          INTEGER, INTENT( IN )      :: NWL_CS_IN           ! number of intervals CS_IN
          INTEGER, INTENT( IN )      :: NWL_QY_IN           ! number of intervals CS_IN
          REAL, INTENT( IN )         :: WL_CS_IN( : )  ! wl for CS_IN
          REAL, INTENT( IN )         :: WL_QY_IN( : )  ! wl for QY_IN
          REAL, INTENT( IN )         :: CS_IN( : )     ! cross-section as f(WLIN)
          REAL, INTENT( IN )         :: QY_IN( : )     ! quantum yield as f(WLIN)
          REAL, INTENT( INOUT )      :: WLL_AVE( : )   ! lower limit on wl effective interval
          REAL, INTENT( INOUT )      :: WLU_AVE( : )   ! upper limit on wl effective interval
          REAL, INTENT( INOUT )      :: CS_AVE( : )    ! cross-section as f(WL_AVE)
          REAL, INTENT( INOUT )      :: QY_AVE( : )    ! quantum yield as f(WL_AVE)
        END SUBROUTINE WVBIN_AVERAGE
        SUBROUTINE WVBIN_AVERAGE_B(WL_CS_IN, CS_IN, NWL_CS_IN,  
     &                         WL_QY_IN, QY_IN, NWL_QY_IN,  
     &                         SPECTRA_TYPE,
     &                         WLL_AVE, WLU_AVE, NWL_AVE, 
     &                         CS_AVE, QY_AVE )
          CHARACTER(1), INTENT( IN ) :: SPECTRA_TYPE        ! spectra type
          INTEGER, INTENT( IN )      :: NWL_AVE             ! number of intervals average 
          INTEGER, INTENT( IN )      :: NWL_CS_IN           ! number of intervals CS_IN
          INTEGER, INTENT( IN )      :: NWL_QY_IN           ! number of intervals CS_IN
          REAL, INTENT( IN )         :: WL_CS_IN( : )  ! wl for CS_IN
          REAL, INTENT( IN )         :: WL_QY_IN( : )  ! wl for QY_IN
          REAL, INTENT( IN )         :: CS_IN( : )     ! cross-section as f(WLIN)
          REAL, INTENT( IN )         :: QY_IN( : )     ! quantum yield as f(WLIN)
          REAL, INTENT( INOUT )      :: WLL_AVE( : )   ! lower limit on wl effective interval
          REAL, INTENT( INOUT )      :: WLU_AVE( : )   ! upper limit on wl effective interval
          REAL, INTENT( INOUT )      :: CS_AVE( : )    ! cross-section as f(WL_AVE)
          REAL, INTENT( INOUT )      :: QY_AVE( : )    ! quantum yield as f(WL_AVE)
        END SUBROUTINE WVBIN_AVERAGE_B
      END INTERFACE  


C*********************************************************************

       ALLOCATE( XDUMB( MXWLIN ), ZDUMB( MXWLIN ), YDUMB( MXWLIN) )

       ALLOCATE( WAVE_OUTC( MXWLIN ), WAVE_OUTL( MXWLIN ), WAVE_OUTU( MXWLIN ))       

C...get a unit number for AERO files

      AEUNIT = 125

      LOOP_REFRACT: DO IAERO = 1, NAERO_REFRACT

C...open input file
        CALL VALUE_NAME( AERO_REFRACT_INDX( IAERO ),  REFRACT_FILE )

        AERO_FILE = TRIM( AERO_REFRACT_INDX( IAERO ) )

        OPEN( UNIT = AEUNIT,
     &        FILE = REFRACT_FILE,
     &        STATUS = 'OLD',
     &        IOSTAT = IOST )
         

C...check for open errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Could not open ' // TRIM( AERO_FILE ) // ' file at path: '
     &       // TRIM(REFRACT_FILE)          
          WRITE(*,*)MSG
          STOP
        END IF

        WRITE( 6, 2001 ) TRIM( AERO_FILE ), TRIM(REFRACT_FILE) 


C...read refractive index subgroup id

        READ( AEUNIT, 1001, IOSTAT = IOST ) REFRACT_ID

C...check for read errors

        IF ( IOST .NE. 0) THEN
          MSG = 'Errors occurred while reading data file ' //
     &           TRIM(REFRACT_FILE)
          WRITE(*,*)MSG
          STOP
        END IF

C...get type of data (e.g. centered, beginning, ending, or point wavelen

        SPECTRA_TYPE = '!'

        DO WHILE( SPECTRA_TYPE .EQ. '!' )
           READ( AEUNIT, '(A)', IOSTAT = IOST ) FILE_LINE
C...   check for read errors
           IF ( IOST .NE. 0) THEN
             MSG = 'Errors occurred while reading refractive index for ' //
     &              TRIM( AERO_REFRACT_INDX( IAERO ) )
             WRITE(*,*)MSG
             STOP
           END IF
           SPECTRA_TYPE = FILE_LINE(1:1)
        END DO

C...reinitialize arrays

        DO IWL = 1, MXWLIN
          WLIN( IWL )  = 0.0
          XDUMB( IWL ) = 0.0
          YDUMB( IWL ) = 0.0
        END DO

C...loop over the number of wavelengths and continue reading

        IWL = 0
201     CONTINUE

          IOST = 0

          DO WHILE ( IOST .EQ. 0 )
             IWL = IWL + 1
             IF( IWL .EQ. 1 )THEN
               READ( FILE_LINE, *, IOSTAT = IOST ) WLIN( IWL ), XDUMB( IWL ),
     &                                             YDUMB( IWL )             
             ELSE
               READ( AEUNIT, *, IOSTAT = IOST ) WLIN( IWL ), XDUMB( IWL ),
     &                                          YDUMB( IWL )
             END IF

C...check for read errors
             IF ( IOST .GT. 0) THEN
               MSG = 'Errors occurred while reading WL, REFRACT_INDX for ' //
     &                TRIM( AERO_REFRACT_INDX( IAERO ) )
               WRITE(*,*)MSG
               STOP
             END IF
          END DO


C...adjust loop counter index index and close file

        NWLIN = IWL + 1 ! - 1
        
        ALLOCATE( WAVE( NWLIN ), IMAG_REFRACT_IN( NWLIN ), REAL_REFRACT_IN( NWLIN ))
        print*,'NWLIN = ',NWLIN
        WAVE( 1:NWLIN-2 ) = 1000.0 * WLIN( 1:NWLIN-2 )
        REAL_REFRACT_IN( 1:NWLIN-2 ) = XDUMB( 1:NWLIN-2 )
        IMAG_REFRACT_IN( 1:NWLIN-2 ) = YDUMB( 1:NWLIN-2 )

        WAVE( NWLIN-1 )            = WAVE( NWLIN-2 ) + 4000.0
        REAL_REFRACT_IN( NWLIN-1 ) = XDUMB( NWLIN-2 )
        IMAG_REFRACT_IN( NWLIN-1 )  = YDUMB( NWLIN-2 )

        WAVE( NWLIN )            = WAVE( NWLIN-2 ) + 4500.0
        REAL_REFRACT_IN( NWLIN ) = XDUMB( NWLIN-2 )
        IMAG_REFRACT_IN( NWLIN ) = YDUMB( NWLIN-2 )

!        WRITE(6,'(3(es12.4,1X))')(WAVE(iwl),REAL_REFRACT_IN(iwl),REAL_REFRACT_IN(iwl),iwl=1,NWLIN)
        
        CLOSE( AEUNIT )

C...transform the cs data to the same wavelength intervals as
C...  the irradiance data.

         XDUMB = 1.0
         NDUMB = NWLIN

         CALL WVBIN_AVERAGE(WAVE, REAL_REFRACT_IN, NDUMB, WAVE, XDUMB, NDUMB, 'P',
     &                      WAVE_OUTL, WAVE_OUTU, MXWLIN, YDUMB, XDUMB )

         AERO_REAL_REFRACT( 1:NJO_NEW, IAERO ) = YDUMB( 1:NJO_NEW )

         CALL WVBIN_AVERAGE(WAVE, IMAG_REFRACT_IN, NDUMB, WAVE, XDUMB, NDUMB, 'P',
     &                      WAVE_OUTL, WAVE_OUTU, MXWLIN, YDUMB, XDUMB )

         AERO_IMAG_REFRACT( 1:NJO_NEW, IAERO ) = YDUMB( 1:NJO_NEW )
         
!         do iwl = 1, njo_new
!            WRITE(6,'(3(es12.4,1X))')WAVE_OUTL(iwl),AERO_REAL_REFRACT( iwl, IAERO ),
!     &      AERO_IMAG_REFRACT( iwl, IAERO )
!         end do
         
         DEALLOCATE( WAVE, REAL_REFRACT_IN, IMAG_REFRACT_IN )

      END DO LOOP_REFRACT
      

C...formats

1001  FORMAT( A16 )
1003  FORMAT( A1 )
1005  FORMAT( /, 4X, F10.1 )

2001  FORMAT( 1X, '...Processing AERO Refractive index: ', A, 
     &            ' in file: ', / A )
2003  FORMAT( 1X, '...Data for ', I4, ' wavelengths read from file',
     &        // )

      RETURN
      END
