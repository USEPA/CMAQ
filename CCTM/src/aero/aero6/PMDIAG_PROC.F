
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

!-----------------------------------------------------------------------
      MODULE PMDIAG_PROC
!-----------------------------------------------------------------------
! This module will collect all procedures used for subsetting the 
!      pm diagnostic variables requested by the user. It will also
!      open the output files and do the actual calculations of 
!      diagnostic variables of interest.
!
! Revision History:
! Ben Murphy 2020 Feb: Created
!-----------------------------------------------------------------------

      USE UTILIO_DEFN
      USE PMDIAG_DATA
      USE GRID_CONF, ONLY: NCOLS, NROWS, NLAYS, IO_PE_INCLUSIVE

      IMPLICIT NONE

      LOGICAL, SAVE :: INST_ACTIVE, AVRG_ACTIVE
      INTEGER, SAVE :: INST_LAYER_TOP, INST_LAYER_BOT,
     &                 AVRG_LAYER_TOP, AVRG_LAYER_BOT

      CHARACTER(16), ALLOCATABLE, SAVE :: 
     &               INST_PARS_REG( : ), AVRG_PARS_REG( : ),
     &               INST_PARS( : ),     AVRG_PARS( : ),
     &               INST_PARS_OUT(:),   AVRG_PARS_OUT(:),
     &               INST_UNIT_OUT(:),   AVRG_UNIT_OUT(:)

      CHARACTER(80), ALLOCATABLE, SAVE :: 
     &               INST_DESC_OUT( : ), AVRG_DESC_OUT( : )

      INTEGER, SAVE :: N_PMDIAG, N_PMDIAG_OUT,
     &                 N_PMDIAG_INST, N_PMDIAG_AVRG,
     &                 N_PMDIAG_INST_OUT, N_PMDIAG_AVRG_OUT

      INTEGER, ALLOCATABLE, SAVE :: 
     &        MAP_INST2USED( : ), MAP_AVRG2USED( : ), 
     &        USED_PMDIAG_PARS( : ), USED_PMDIAG_MODE( : ),
     &        INST_TYPE_OUT( : ), AVRG_TYPE_OUT( : ),
     &        MAP_ID2USED( :,: )

      CONTAINS

!-------------------------------------------------------------------------
      SUBROUTINE READ_PMDIAG_NML( )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the PMDIAG_DATA table.
!-------------------------------------------------------------------------
         
      USE RUNTIME_VARS, ONLY: EMISSCTRL, LOGDEV
      
      IMPLICIT NONE

      INTEGER, PARAMETER :: N_REG = 1000
      INTEGER            :: INUM, N_END, I1, I2, JNUM
      INTEGER            :: EMCTRL_NML
      INTEGER            :: STAT
      LOGICAL, SAVE      :: INSTANT  =.TRUE., 
     &                      AVERAGE  =.TRUE.!,
!     &                      OLDNAMES =.FALSE.

      CHARACTER( 16 ), SAVE :: PNAME = 'READ_PMDIAG_NML'
      CHARACTER( 200 )   :: XMSG
      INTEGER ALLOCSTAT

      NAMELIST / PMDIAG_ACTIVATE / INSTANT, AVERAGE !, OLDNAMES
      NAMELIST / PMDIAG_INST / INST_LAYER_TOP, 
     &                         INST_LAYER_BOT, INST_PARS_REG
      NAMELIST / PMDIAG_AVRG / AVRG_LAYER_TOP, 
     &                         AVRG_LAYER_BOT, AVRG_PARS_REG

      ALLOCATE( INST_PARS_REG( N_REG ), AVRG_PARS_REG( N_REG ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating INST_PARS_REG or AVRG_PARS_REG'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
 

      INST_ACTIVE      = .TRUE.
      INST_LAYER_TOP   = 1
      INST_LAYER_BOT   = 1
      INST_PARS_REG(:) = ''
      INST_PARS_REG(1) = 'ALL'
      AVRG_ACTIVE      = .TRUE.
      AVRG_LAYER_TOP   = 1
      AVRG_LAYER_BOT   = 1
      AVRG_PARS_REG(:) = ''
      AVRG_PARS_REG(1) = 'ALL'

      ! Retrieve the Name of the Emission Control File
      IF ( EMISSCTRL .EQ. "EMISSCTRL_NML" ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A)"),
     &           'You have chosen not to indicate the location of an',
     &           'Emission Control namelist file. Default settings ',
     &           'will be assumed.'
          RETURN
      END IF

      ! Open Emission Control Namelist File
      EMCTRL_NML = JUNIT()
      OPEN( FILE = EMISSCTRL, UNIT = EMCTRL_NML, STATUS = 'OLD',
     &      POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

      ! Check for Error in File Open Process
      IF ( STAT .NE. 0 ) THEN
          WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &           'emissions control namelist file: ',TRIM( EMISSCTRL )
          CALL M3EXIT( 'READ_EMISS_NAMELIST',0,0,XMSG,1 )
      END IF
 
      ! Read Toggles for Turning Instantaneous and Average Files On/Off
      REWIND( EMCTRL_NML )
      READ( NML = PMDIAG_ACTIVATE, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'PMDIAG_ACTIVATE section of the Emissions Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF
      INST_ACTIVE = INSTANT
      AVRG_ACTIVE = AVERAGE
  !    L_OLDNAMES  = OLDNAMES
      
      CALL POPULATE_PMDIAG_KEY

      ! Read Desired PMDIAG_INST parameters from input namelist
      REWIND( EMCTRL_NML )
      READ( NML = PMDIAG_INST, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'PMDIAG_INST section of the Emissions Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF

      ! Expand INST list Using Keywords
      N_END = INDEX1( '', N_REG, INST_PARS_REG ) - 1
      INUM = 1
      DO WHILE ( INUM .LE. N_END )
         CALL UPCASE( INST_PARS_REG( INUM ) )
         I1 = INDEX1( INST_PARS_REG( INUM ), N_PMDIAG_LIST, PMDIAG_LIST%NAME )
         I2 = INDEX1( INST_PARS_REG( INUM ), N_KEY, PMDIAG_KEY%WORD )

         IF ( INST_PARS_REG( INUM ) .EQ. 'ALL' ) THEN
             ! Add All Parameters to the End of the Registry List
            INST_PARS_REG( N_END+1:N_END+N_PMDIAG_LIST ) = PMDIAG_LIST%NAME
            N_END = N_END + N_PMDIAG_LIST
            INST_PARS_REG( INUM:N_END-1 ) = INST_PARS_REG( INUM+1:N_END )
            INST_PARS_REG( N_END ) = ''
            N_END = N_END - 1
         ELSE IF ( I1 .GT. 0 ) THEN
            ! Entry Matches a parameter on the PMDIAG list. 
            INUM = INUM + 1
         ELSE IF ( I2 .GT. 0 ) THEN
            ! Add Parameters associated with a keyword
            INST_PARS_REG( N_END+1:N_END+PMDIAG_KEY( I2 )%N ) =
     &                                   PMDIAG_KEY( I2 )%LIST
            N_END = N_END + PMDIAG_KEY( I2 )%N
            INST_PARS_REG( INUM:N_END-1 ) = INST_PARS_REG( INUM+1:N_END )
            INST_PARS_REG( N_END ) = ''
            N_END = N_END - 1
         ELSE
            ! Entry Doesn't Match any parameter or Keyword. Crash the
            ! model.
            XMSG = 'An Instantaneous PMDIAG Parameter ' // TRIM( INST_PARS_REG( INUM )) //
     &             ' has been requested that does not exist.'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
      END DO

      ! Capitalize Entries and Remove Duplicates
      DO INUM = 1,N_END
          CALL UPCASE( INST_PARS_REG( INUM ) )
      ENDDO
      DO INUM = 1,N_END-1
         DO JNUM = INUM+1,N_END
            IF ( INST_PARS_REG( INUM ) .EQ. INST_PARS_REG( JNUM ) ) THEN
               INST_PARS_REG( JNUM:N_END-1 ) = INST_PARS_REG( JNUM+1:N_END )
               INST_PARS_REG( N_END ) = ''
            END IF
         END DO
      END DO

      ! Populate INST_PARS parameters list
      N_PMDIAG_INST = INDEX1( '', N_REG, INST_PARS_REG ) - 1
      ALLOCATE( INST_PARS( N_PMDIAG_INST ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating INST_PARS'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      INST_PARS = INST_PARS_REG( 1:N_PMDIAG_INST )

      ! Check Top-Layer Specification
      IF ( INST_LAYER_TOP .EQ. -1 ) INST_LAYER_TOP = NLAYS
      IF ( INST_LAYER_TOP .LE. 0 .OR. INST_LAYER_TOP .GT. NLAYS ) THEN
           XMSG = 'INST_LAYER_TOP must be prescribed between 1 and NLAYS,' //
     &            ' or set to -1 to automatically choose NLAYS.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      IF ( INST_LAYER_BOT .LE. 0 .OR. INST_LAYER_BOT .GT. NLAYS ) THEN
           XMSG = 'INST_LAYER_BOT must be prescribed between 1 and NLAYS.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      IF ( INST_LAYER_BOT .GT. INST_LAYER_TOP ) THEN
           XMSG = 'INST_LAYER_BOT must be less than or equal to INST_LAYER_TOP.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      NLAY_PMDIAG_INST = INST_LAYER_TOP - INST_LAYER_BOT + 1

      ! Read Desired PMDIAG_AVRG parameters from input namelist
      REWIND( EMCTRL_NML )
      READ( NML = PMDIAG_AVRG, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'PMDIAG_AVRG section of the Emissions Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF
 
      ! Expand INST list Using Keywords
      N_END = INDEX1( '', N_REG, AVRG_PARS_REG ) - 1
      INUM = 1
      DO WHILE ( INUM .LE. N_END )
         CALL UPCASE( AVRG_PARS_REG( INUM ) )
         I1 = INDEX1( AVRG_PARS_REG( INUM ), N_PMDIAG_LIST, PMDIAG_LIST%NAME )
         I2 = INDEX1( AVRG_PARS_REG( INUM ), N_KEY, PMDIAG_KEY%WORD )

         IF ( AVRG_PARS_REG( INUM ) .EQ. 'ALL' ) THEN
             ! Add All Parameters to the End of the Registry List
            AVRG_PARS_REG( N_END+1:N_END+N_PMDIAG_LIST ) = PMDIAG_LIST%NAME
            N_END = N_END + N_PMDIAG_LIST
            AVRG_PARS_REG( INUM:N_END-1 ) = AVRG_PARS_REG( INUM+1:N_END )
            AVRG_PARS_REG( N_END ) = ''
            N_END = N_END - 1
         ELSE IF ( I1 .GT. 0 ) THEN
            ! Entry Matches a parameter on the PMDIAG list. 
            INUM = INUM + 1
         ELSE IF ( I2 .GT. 0 ) THEN
            ! Add Parameters associated with a keyword
            AVRG_PARS_REG( N_END+1:N_END+PMDIAG_KEY( I2 )%N ) =
     &                                   PMDIAG_KEY( I2 )%LIST
            N_END = N_END + PMDIAG_KEY( I2 )%N
            AVRG_PARS_REG( INUM:N_END-1 ) = AVRG_PARS_REG( INUM+1:N_END )
            AVRG_PARS_REG( N_END ) = ''
            N_END = N_END - 1
         ELSE
            ! Entry Doesn't Match any parameter or Keyword. Crash the
            ! model.
            XMSG = 'An Average PMDIAG Parameter ' // TRIM( AVRG_PARS_REG( INUM )) //
     &             ' has been requested that does not exist.'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
      END DO

      ! Capitalize Entries and Remove Duplicates
      DO INUM = 1,N_END
          CALL UPCASE( AVRG_PARS_REG( INUM ) )
      ENDDO
      DO INUM = 1,N_END-1
         DO JNUM = INUM+1,N_END
            IF ( AVRG_PARS_REG( INUM ) .EQ. AVRG_PARS_REG( JNUM ) ) THEN
               AVRG_PARS_REG( JNUM:N_END-1 ) = AVRG_PARS_REG( JNUM+1:N_END )
               AVRG_PARS_REG( N_END ) = ''
            END IF
         END DO
      END DO

      ! Populate INST_PARS parameters list
      N_PMDIAG_AVRG = INDEX1( '', N_REG, AVRG_PARS_REG ) - 1
      ALLOCATE( AVRG_PARS( N_PMDIAG_AVRG ) )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating AVRG_PARS'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      AVRG_PARS = AVRG_PARS_REG( 1:N_PMDIAG_AVRG )
 
      ! Check Top-Layer Specification
      IF ( AVRG_LAYER_TOP .EQ. -1 ) AVRG_LAYER_TOP = NLAYS
      IF ( AVRG_LAYER_TOP .LE. 0 .OR. AVRG_LAYER_TOP .GT. NLAYS ) THEN
           XMSG = 'AVRG_LAYER_TOP must be prescribed between 1 and NLAYS,' //
     &            ' or set to -1 to automatically choose NLAYS.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      IF ( AVRG_LAYER_BOT .LE. 0 .OR. AVRG_LAYER_BOT .GT. NLAYS ) THEN
           XMSG = 'AVRG_LAYER_BOT must be prescribed between 1 and NLAYS.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      IF ( AVRG_LAYER_BOT .GT. AVRG_LAYER_TOP ) THEN
           XMSG = 'AVRG_LAYER_BOT must be less than or equal to AVRG_LAYER_TOP.'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      NLAY_PMDIAG_AVRG = AVRG_LAYER_TOP - AVRG_LAYER_BOT + 1
 
      END SUBROUTINE READ_PMDIAG_NML

!-------------------------------------------------------------------------
      SUBROUTINE POPULATE_PMDIAG_KEY
!     This subroutine allocates and populates the structure of potential
!       keywords to which users may refer to activate various diagnostic
!       parameters. It also contains the definitions of the ocntents of
!       each keyword.
!-------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER ALLOCSTAT
      CHARACTER(200) :: XMSG
      CHARACTER(16), SAVE :: PNAME = 'PMDIAG_KEY'

      N_KEY = 20
      ALLOCATE( PMDIAG_KEY( N_KEY ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ! Define AMET Variables
      PMDIAG_KEY( 1 )%WORD = 'DEFAULT'
      PMDIAG_KEY( 1 )%N = 12
      ALLOCATE( PMDIAG_KEY( 1 )%LIST( 12 ),STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(1)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 1 )%LIST = (/'AMET            ','CUTOFF_FRACTIONS',
     &      'MASS            ','PM_NUM          ','PMF_NUM         ',
     &      'PMC_NUM         ','MET             ','CHEM            ', 
     &      'FINE_SPECIES    ','COARSE_SPECIES  ','PM25_SPECIES    ',
     &      'SIZE            ' /)

      PMDIAG_KEY( 2 )%WORD = 'AMET'
      PMDIAG_KEY( 2 )%N = 35
      ALLOCATE( PMDIAG_KEY( 2 )%LIST( 35 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(2)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 2 )%LIST = (/'PMF_MASS    ','PMF_FRM     ','PMC_MASS    ',
     &          'PMF_OC      ','PMF_EC      ','PMF_NA      ','PMF_CL      ',
     &          'PMF_SO4     ','PMF_NO3     ','PMF_NH4     ','PM25        ',
     &          'PM25_FRM    ','PM25_SO4    ','PM25_NO3    ','PM25_NH4    ',
     &          'PM25_OC     ','PM25_EC     ','PMF_FE      ','PMF_AL      ',
     &          'PMF_SI      ','PMF_TI      ','PMF_CA      ','PMF_MG      ',
     &          'PMF_K       ','PMF_MN      ','PMF_SOILIMPV','PMF_UN_IMPV1',
     &          'PMF_NCOM    ','PMF_UN_IMPV2','TNO3        ','PM_MASS     ',
     &          'PMC_MASS    ','MET         ','Tsurf       ','AOD_550     ' /)

      PMDIAG_KEY( 3 )%WORD = 'MASS'
      PMDIAG_KEY( 3 )%N = 9
      ALLOCATE( PMDIAG_KEY( 3 )%LIST( 9 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(3)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 3 )%LIST = (/'PMF_MASS','PMC_MASS','PM01    ','PM1     ',
     &              'PM25    ','PM10    ','PM25to10','PMU_MASS','PMAMS   ' /)
     
      PMDIAG_KEY( 4 )%WORD = 'NUMBER'
      PMDIAG_KEY( 4 )%N = 7
      ALLOCATE( PMDIAG_KEY( 4 )%LIST( 7 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(4)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 4 )%LIST = (/'PM_NUM ','PMF_NUM','PMC_NUM','N10    ',
     &                         'N20    ','N40    ','N100   ' /)
     
      PMDIAG_KEY( 5 )%WORD = 'SIZE'
      PMDIAG_KEY( 5 )%N = 9
      ALLOCATE( PMDIAG_KEY( 5 )%LIST( 9 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(5)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 5 )%LIST = (/'NUMBER  ','DRY_DG  ','WET_DG  ','STDEV   ',
     &              'DRY_M3  ','WET_M3  ','WET_M2  ','DRY_DENS','WET_DENS' /)
     
      PMDIAG_KEY( 6 )%WORD = 'SURFACE'
      PMDIAG_KEY( 6 )%N = 5
      ALLOCATE( PMDIAG_KEY( 6 )%LIST( 5 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(6)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 6 )%LIST = (/'PM_SRF ','PMU_SRF','PMF_SRF','PMC_SRF','WET_M2 '/)
     
      PMDIAG_KEY( 7 )%WORD = 'ORGANIC'
      PMDIAG_KEY( 7 )%N = 6
      ALLOCATE( PMDIAG_KEY( 7 )%LIST( 6 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(7)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 7 )%LIST = (/'FINE_ORG  ','PM1_OC    ','PM1_OA    ',
     &                         'PMAMS_OA  ','PMAMS_OTOC','PM25_OA   '/)

      PMDIAG_KEY( 8 )%WORD = 'MET'
      PMDIAG_KEY( 8 )%N = 3
      ALLOCATE( PMDIAG_KEY( 8 )%LIST( 3 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(8)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 8 )%LIST = (/'T ','P ','RH' /) 

      PMDIAG_KEY( 9 )%WORD = 'CHEM'
      PMDIAG_KEY( 9 )%N = 7
      ALLOCATE( PMDIAG_KEY( 9 )%LIST( 7 ),STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(9)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 9 )%LIST = (/'GAMMA_N2O5  ','GAMMA_N2O5K ','YIELD_CLNO2 ',
     &          'YIELD_CLNO2K','GAMMA_IEPOX ','K_IEPOX     ','GAMMA_IMAE  ' /)

      PMDIAG_KEY( 10 )%WORD = 'FINE_SPECIES'
      PMDIAG_KEY( 10 )%N = 17
      ALLOCATE( PMDIAG_KEY( 10 )%LIST( 17 ),STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(10)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 10 )%LIST = (/'PMF_SO4     ','PMF_NO3     ','PMF_NH4     ',
     &           'PMF_CL      ','PMF_NA      ','PMF_EC      ','PMF_OC      ',
     &           'PMF_OA      ','PMF_H2O     ','PMF_SOILIMPV','PMF_UN_IMPV1',
     &           'PMF_UN_IMPV2','PMF_HP      ','PMF_HPMOLAL ','PMF_PH      ',
     &           'FINE_ORG    ','PMF_MASS    ' /)

      PMDIAG_KEY( 11 )%WORD = 'COARSE_SPECIES'
      PMDIAG_KEY( 11 )%N = 8
      ALLOCATE( PMDIAG_KEY( 11 )%LIST( 8 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(11)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 11 )%LIST = (/'PMC_MASS','PMC_SO4 ','PMC_NO3 ','PMC_NH4 ',
     &                          'PMC_NA  ','PMC_MG  ','PMC_K   ','PMC_CA  ' /)

      PMDIAG_KEY( 12 )%WORD = 'AMS_SPECIES'
      PMDIAG_KEY( 12 )%N = 7
      ALLOCATE( PMDIAG_KEY( 12 )%LIST( 7 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(12)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 12 )%LIST = (/'PMAMS     ','PMAMS_SO4 ','PMAMS_NO3 ',
     &             'PMAMS_NH4 ','PMAMS_CL  ','PMAMS_OA  ','PMAMS_OTOC' /)

      PMDIAG_KEY( 13 )%WORD = 'PM1_SPECIES'
      PMDIAG_KEY( 13 )%N = 21
      ALLOCATE( PMDIAG_KEY( 13 )%LIST( 21 ),STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(13)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 13 )%LIST = (/'PM1        ','PM1_SO4    ','PM1_NO3    ',
     &            'PM1_NH4    ','PM1_CL     ','PM1_NA     ','PM1_EC     ',
     &            'PM1_OC     ','PM1_OA     ','PM1_MG     ','PM1_K      ',
     &            'PM1_CA     ','PM1_OTHER  ','PM1_FE     ','PM1_SI     ',
     &            'PM1_TI     ','PM1_MN     ','PM1_AL     ','PM1_SOIL   ',
     &            'PM1_UNSP1  ','PM1_UNSPCRS' /)

      PMDIAG_KEY( 14 )%WORD = 'PM25_SPECIES'
      PMDIAG_KEY( 14 )%N = 22
      ALLOCATE( PMDIAG_KEY( 14 )%LIST( 22 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(14)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 14 )%LIST = (/'PM25        ','PM25_SO4    ','PM25_NO3    ',
     &           'PM25_NH4    ','PM25_CL     ','PM25_NA     ','PM25_EC     ',
     &           'PM25_OC     ','PM25_OA     ','PM25_MG     ','PM25_K      ',
     &           'PM25_CA     ','PM25_OTHER  ','PM25_FE     ','PM25_SI     ',
     &           'PM25_TI     ','PM25_MN     ','PM25_AL     ','PM25_SOIL   ',
     &           'PM25_UNSP1  ','PM25_UNSPCRS','PM25_HP     ' /)

      PMDIAG_KEY( 15 )%WORD = 'PM25to10_SPECIES'
      PMDIAG_KEY( 15 )%N = 6
      ALLOCATE( PMDIAG_KEY( 15 )%LIST( 6 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(15)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 15 )%LIST = (/'PM25to10    ','PM25to10_SO4','PM25to10_NO3',
     &                          'PM25to10_NH4','PM25to10_CL ','PM25to10_NA ' /)

      PMDIAG_KEY( 16 )%WORD = 'OPTICAL'
      PMDIAG_KEY( 16 )%N = 2
      ALLOCATE( PMDIAG_KEY( 16 )%LIST( 2 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(16)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 16 )%LIST = (/'AOD_550   ','PM_EXT_550'/)

      PMDIAG_KEY( 17 )%WORD = 'FINE_ORG'
      PMDIAG_KEY( 17 )%N = 16
      ALLOCATE( PMDIAG_KEY( 17 )%LIST( 16 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(17)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 17 )%LIST = (/'PMF_POC     ','PMF_SOC     ','PMF_POA     ',
     &           'PMF_SOA     ','PMF_NCOM    ','PMF_OMOC    ','PMF_OTOC    ',
     &           'PMF_ASOA    ','PMF_BSOA    ','PMF_CLDGLY  ','PMF_ISOPSOA ',
     &           'PMF_IEPOXSOA','PMF_MTNSOA  ','PMF_MTSOA   ','PMF_OC      ',
     &           'PMF_OA      ' /)

      PMDIAG_KEY( 18 )%WORD = 'CUTOFF_FRACTIONS'
      PMDIAG_KEY( 18 )%N = 6
      ALLOCATE( PMDIAG_KEY( 18 )%LIST( 6 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(18)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 18 )%LIST = (/'FPM01    ','FPM1     ','FPM25    ',
     &                          'FPM10    ','FPM25to10','FAMS     ' /)

      PMDIAG_KEY( 19 )%WORD = 'TOXICS'
      PMDIAG_KEY( 19 )%N = 26
      ALLOCATE( PMDIAG_KEY( 19 )%LIST( 26 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(19)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 19 )%LIST = (/'PM25_HDIESEL','PM25_HBE    ','PM25_HCD    ',
     &           'PM25_HCR 3  ','PM25_HCR6   ','PM25_HCR    ','PM25_HPB    ',
     &           'PM25_HMN    ','PM25_HMN    ','PM25_HNI    ','PM25_HNI    ',
     &           'PM25_HAS    ','PM25_HG     ','PM10_HDIESEL','PM10_HBE    ',
     &           'PM10_HCD    ','PM10_HCR3   ','PM10_HCR6   ','PM10_HCR    ',
     &           'PM10_HPB    ','PM10_HMN    ','PM10_HMN    ','PM10_HNI    ',
     &           'PM10_HNI    ','PM10_HAS    ','PM10_HG     '  /)

      PMDIAG_KEY( 20 )%WORD = 'SIMPLE'
      PMDIAG_KEY( 20 )%N = 2
      ALLOCATE( PMDIAG_KEY( 20 )%LIST( 2 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_KEY(20)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_KEY( 20 )%LIST = (/'PM25','PM10'/)

      END SUBROUTINE POPULATE_PMDIAG_KEY


!-------------------------------------------------------------------------
      SUBROUTINE MAP_PMDIAG
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the PMDIAG_DATA table.
!-------------------------------------------------------------------------
      
      USE AERO_DATA, ONLY: N_MODE, AEROMODE
      USE CGRID_SPCS, ONLY : N_CGRID_SPC

      IMPLICIT NONE

      INTEGER N, M, P, J, IM, ILIST
      CHARACTER( 16 ), SAVE :: PNAME = 'MAP_PMDIAG'
      CHARACTER( 300 ) :: XMSG
      INTEGER ALLOCSTAT

      ! Read User-Input from Namelist input file (Emission Control File
      ! For Now)
      CALL READ_PMDIAG_NML()
      L_PMDIAG = ( INST_ACTIVE .OR. AVRG_ACTIVE )
      
      ! Map user-defined INST_PARS and AVRG_PARS arrays to the PMDIAG 
      ! parameters available on the PMDIAG_LIST. 
      N_PMDIAG = 0
      
      ALLOCATE( USED_PMDIAG_MODE( N_PMDIAG_LIST*N_MODE ),
     &          USED_PMDIAG_PARS( N_PMDIAG_LIST*N_MODE ),
     &          MAP_ID2USED( N_PMDIAG_LIST,N_MODE ),
     &          STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating USED_PMDIAG_MODE, USED_PMDIAG_PARS, ' // 
     &            'or MAP_ID2USED'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      USED_PMDIAG_MODE = 0
      USED_PMDIAG_PARS = 0
      MAP_ID2USED = 0

      ! Map Parameter ID's to PMDIAG_List Entries
      MAP_ID2LIST = 0
      DO ILIST = 1,N_PMDIAG_LIST
          MAP_ID2LIST( PMDIAG_LIST( ILIST )%ID ) = ILIST
      END DO

      IF ( INST_ACTIVE ) THEN
         ALLOCATE( MAP_INST2USED( N_PMDIAG_INST*N_MODE ),
     &             INST_PARS_OUT( N_PMDIAG_INST*N_MODE ) ,
     &             INST_TYPE_OUT( N_PMDIAG_INST*N_MODE ) ,
     &             INST_UNIT_OUT( N_PMDIAG_INST*N_MODE ) ,
     &             INST_DESC_OUT( N_PMDIAG_INST*N_MODE ),
     &             STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating MAP_INST2USED, etc., '
              CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
         N_PMDIAG_INST_OUT = 0
         DO J = 1,N_PMDIAG_INST
             N = INDEX1( INST_PARS( J ), N_PMDIAG_LIST, PMDIAG_LIST%NAME )
             IF ( N .NE. 0 ) THEN
                ! Add Entry for this Parameter
                N_PMDIAG = N_PMDIAG + 1
                N_PMDIAG_INST_OUT = N_PMDIAG_INST_OUT + 1
                USED_PMDIAG_PARS( N_PMDIAG ) = PMDIAG_LIST(N)%ID
                USED_PMDIAG_MODE( N_PMDIAG ) = 1
                MAP_ID2USED( PMDIAG_LIST(N)%ID,1 ) = N_PMDIAG
                MAP_INST2USED( N_PMDIAG_INST_OUT ) = N_PMDIAG

                ! Add Room for the Other Modes if this Parameter is
                ! Mode-Dependent
                IF ( PMDIAG_LIST( N )%MODE ) THEN
                    INST_PARS_OUT( N_PMDIAG_INST_OUT ) = 
     &                          TRIM(INST_PARS( J )) // aeromode( 1 )%suff
                    INST_TYPE_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%VAR_TYPE
                    INST_UNIT_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%UNIT
                    INST_DESC_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%DESC

                    DO IM = 2,N_MODE
                       N_PMDIAG = N_PMDIAG + 1
                       N_PMDIAG_INST_OUT = N_PMDIAG_INST_OUT + 1
                       USED_PMDIAG_PARS( N_PMDIAG ) = PMDIAG_LIST(N)%ID
                       USED_PMDIAG_MODE( N_PMDIAG ) = IM
                       MAP_ID2USED( PMDIAG_LIST(N)%ID,IM) = N_PMDIAG
                       MAP_INST2USED( N_PMDIAG_INST_OUT ) = N_PMDIAG

                       INST_PARS_OUT( N_PMDIAG_INST_OUT ) = 
     &                          TRIM(INST_PARS( J )) // aeromode( IM )%suff
                       INST_TYPE_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%VAR_TYPE
                       INST_UNIT_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%UNIT
                       INST_DESC_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%DESC
                    END DO
                ELSE
                    INST_PARS_OUT( N_PMDIAG_INST_OUT ) = INST_PARS( J )
                    INST_TYPE_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%VAR_TYPE 
                    INST_UNIT_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%UNIT     
                    INST_DESC_OUT( N_PMDIAG_INST_OUT ) = PMDIAG_LIST( N )%DESC     
                END IF
             ELSE
                XMSG = 'An Instantaneous PMDIAG Parameter '//TRIM(INST_PARS(J))//
     &                 ' has been requested that does not exist.'
                CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
         END DO
         ALLOCATE( PMDIAG_INST( NCOLS,NROWS,NLAY_PMDIAG_INST,N_PMDIAG_INST_OUT),STAT=ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating PMDIAG_INST' 
              CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
         PMDIAG_INST = 0.0
      END IF
      
      IF ( AVRG_ACTIVE ) THEN
         ALLOCATE( MAP_AVRG2USED( N_PMDIAG_AVRG*N_MODE ),
     &             AVRG_PARS_OUT( N_PMDIAG_AVRG*N_MODE ) ,
     &             AVRG_TYPE_OUT( N_PMDIAG_AVRG*N_MODE ) ,
     &             AVRG_UNIT_OUT( N_PMDIAG_AVRG*N_MODE ) ,
     &             AVRG_DESC_OUT( N_PMDIAG_AVRG*N_MODE ),
     &             STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating MAP_AVRG2USED, etc.' 
              CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF

         N_PMDIAG_AVRG_OUT = 0
         DO J = 1,N_PMDIAG_AVRG
             N = INDEX1( AVRG_PARS( J ), N_PMDIAG_LIST, PMDIAG_LIST%NAME ) 
             M = 0
             IF ( INST_ACTIVE ) 
     &            M = INDEX1( AVRG_PARS( J ), N_PMDIAG_INST, INST_PARS ) 
             IF ( N .NE. 0 ) THEN
                ! Add Entry for this Parameter
                N_PMDIAG_AVRG_OUT = N_PMDIAG_AVRG_OUT + 1

                ! Add Room for the Other Modes if this Parameter is
                ! Mode-Dependent
                IF ( PMDIAG_LIST( N )%MODE ) THEN
                    AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ) = 
     &                             TRIM(AVRG_PARS( J )) // aeromode( 1 )%suff
                    AVRG_TYPE_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%VAR_TYPE 
                    AVRG_UNIT_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%UNIT     
                    AVRG_DESC_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%DESC     
                    IF ( M .EQ. 0 ) THEN
                       N_PMDIAG = N_PMDIAG + 1
                       USED_PMDIAG_MODE( N_PMDIAG ) = 1
                       USED_PMDIAG_PARS( N_PMDIAG ) = PMDIAG_LIST(N)%ID
                       MAP_ID2USED( PMDIAG_LIST(N)%ID,1 ) = N_PMDIAG
                       MAP_AVRG2USED( N_PMDIAG_AVRG_OUT ) = N_PMDIAG
                    ELSE
                       P = INDEX1( AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ), 
     &                             N_PMDIAG_INST_OUT, INST_PARS_OUT ) 
                       MAP_AVRG2USED( N_PMDIAG_AVRG_OUT ) = MAP_INST2USED( P )
                    END IF    

                    DO IM = 2,N_MODE
                       N_PMDIAG_AVRG_OUT = N_PMDIAG_AVRG_OUT + 1
                       AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ) = 
     &                           TRIM(AVRG_PARS( J )) // aeromode( IM )%suff
                       AVRG_TYPE_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%VAR_TYPE 
                       AVRG_UNIT_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%UNIT     
                       AVRG_DESC_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%DESC     
                       IF ( M .EQ. 0 ) THEN
                          N_PMDIAG = N_PMDIAG + 1
                          USED_PMDIAG_MODE( N_PMDIAG ) = IM
                          USED_PMDIAG_PARS( N_PMDIAG ) = PMDIAG_LIST(N)%ID
                          MAP_ID2USED( PMDIAG_LIST(N)%ID,IM) = N_PMDIAG
                          MAP_AVRG2USED( N_PMDIAG_AVRG_OUT ) = N_PMDIAG
                       ELSE
                          P = INDEX1( AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ), 
     &                                N_PMDIAG_INST_OUT, INST_PARS_OUT ) 
                          MAP_AVRG2USED( N_PMDIAG_AVRG_OUT ) = MAP_INST2USED( P )
                       END IF
                    END DO
                ELSE
                    AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ) = AVRG_PARS( J )
                    AVRG_TYPE_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%VAR_TYPE 
                    AVRG_UNIT_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%UNIT     
                    AVRG_DESC_OUT( N_PMDIAG_AVRG_OUT ) = PMDIAG_LIST( N )%DESC     
                    IF ( M .EQ. 0 ) THEN
                       N_PMDIAG = N_PMDIAG + 1
                       USED_PMDIAG_PARS( N_PMDIAG ) = PMDIAG_LIST(N)%ID
                       USED_PMDIAG_MODE( N_PMDIAG ) = 1
                       MAP_ID2USED( PMDIAG_LIST(N)%ID,1 ) = N_PMDIAG
                       MAP_AVRG2USED( N_PMDIAG_AVRG_OUT ) = N_PMDIAG
                    ELSE
                       P = INDEX1( AVRG_PARS_OUT( N_PMDIAG_AVRG_OUT ), 
     &                             N_PMDIAG_INST_OUT, INST_PARS_OUT ) 
                       MAP_AVRG2USED( N_PMDIAG_AVRG_OUT ) = MAP_INST2USED( P )
                    END IF   
                END IF 

             ELSE
                XMSG = 'An Average PMDIAG Parameter '//TRIM(AVRG_PARS(J))//
     &                 ' has been requested that does not exist.'
                CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
         END DO
         ALLOCATE( PMDIAG_AVRG( NCOLS,NROWS,NLAY_PMDIAG_AVRG,N_PMDIAG_AVRG_OUT ), STAT=ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating PMDIAG_AVRG' 
              CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
         PMDIAG_AVRG = 0.0
      END IF
      
      ! Allocate Local PMDIAG Array
      ALLOCATE( PMDIAG_LOCAL( N_PMDIAG ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_LOCAL' 
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      ALLOCATE( L_CALC( N_PMDIAG ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating L_CALC'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ! Allocate local image of CGRID so it can be used each time step
      ALLOCATE( CONC_VEC( N_CGRID_SPC ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating CONC_VEC' 
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ! Map Diagnostic Variables to Model Species and Coefficients
      CALL MAP_PMDIAG_COEFFS


      END SUBROUTINE MAP_PMDIAG

!-------------------------------------------------------------------------
      SUBROUTINE MAP_PMDIAG_COEFFS
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the PMDIAG_DATA table.
!-------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IDG, N_VARS, ILIST

      PMDIAG_COEFFS( : )%L_MAPPED = .FALSE.
      
      ! Save the number of PMDIAG parameters that should be output. In the
      ! course of mapping various parameters, it may be necessary to add
      ! some used parameters that are intermediates to the desired
      ! parameter. For example, PMF_MASS must be calculated in order to
      ! then calculate AUNSP1_IMPVIJ
      N_PMDIAG_OUT = N_PMDIAG

      ! Loop through all available PMDIAG_LIST Parameters
      DO ILIST = 1,N_PMDIAG_LIST
          IDG = PMDIAG_LIST( ILIST )%ID
          
          SELECT CASE ( IDG )
             
             !!!! Coarse-Mode Parameters !!!!  

             ! Map PMC_SO4 - Coarse-Mode Sulfate
             CASE ( ID_PMC_SO4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
 
             ! Map PMC_NO3 - Coarse-Mode Nitrate
             CASE ( ID_PMC_NO3 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map PMC_NH4 - Coarse-Mode Ammonium
             CASE ( ID_PMC_NH4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map PMC_NA - Coarse-Mode Sodium
             CASE ( ID_PMC_NA ) 
               N_VARS = 3
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', 0.8373 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL', 0.0626 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ACORS', 0.0023 )
 
             ! Map PMC_MG - Coarse-Mode Magnesium
             CASE ( ID_PMC_MG ) 
               N_VARS = 3
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', 0.0997 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL', 0.0170 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ACORS', 0.0032 )
 
             ! Map PMC_K - Coarse-Mode Potassium
             CASE ( ID_PMC_K ) 
               N_VARS = 3
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', 0.0310 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL', 0.0242 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ACORS', 0.0176 )

             ! Map PMC_CA - Coarse-Mode Calcium
             CASE ( ID_PMC_CA ) 
               N_VARS = 3
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', 0.0320 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL', 0.0838 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ACORS', 0.0562 )

             
             !!!! Fine-Mode Parameters !!!!  

             ! Map PMF_SO4 - Fine-Mode Sulfate
             CASE ( ID_PMF_SO4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
 
             ! Map PMF_NO3 - Fine-Mode Nitrate
             CASE ( ID_PMF_NO3 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map PMF_NH4 - Fine-Mode Ammonium
             CASE ( ID_PMF_NH4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS , 'FINE')
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map PMF_CL - Fine-Mode Chloride
             CASE ( ID_PMF_CL ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             ! Map PMF_NA - Fine-Mode Sodium
             CASE ( ID_PMF_NA ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
 
             ! Map PMF_EC - Fine-Mode Elemental Carbon                                   
             CASE ( ID_PMF_EC )                                                         
               N_VARS = 1                                                               
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )                           
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AEC', 1.0 )                  
                                                                                        
             ! Map PMF_FE - Fine-Mode Iron
             CASE ( ID_PMF_FE ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AFE', 1.0 )
 
             ! Map PMF_AL - Fine-Mode Aluminum
             CASE ( ID_PMF_AL ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AAL', 1.0 )
 
             ! Map PMF_SI - Fine-Mode Silicon
             CASE ( ID_PMF_SI ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASI', 1.0 )
 
             ! Map PMF_TI - Fine-Mode Titanium
             CASE ( ID_PMF_TI ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ATI', 1.0 )
 
             ! Map PMF_CA - Fine-Mode Calcium
             CASE ( ID_PMF_CA ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACA', 1.0 )
 
             ! Map PMF_MG - Fine-Mode Magnesium
             CASE ( ID_PMF_MG ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AMG', 1.0 )
 
             ! Map PMF_K - Fine-Mode Potassium
             CASE ( ID_PMF_K ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AK', 1.0 )
 
             ! Map PMF_MN - Fine-Mode Manganese
             CASE ( ID_PMF_MN ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AMN', 1.0 )
 
             ! Map PMF_H2O - Fine-Mode Particle Water           
             CASE ( ID_PMF_H2O )                               
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AH2O', 1.0/19.0 )
 
             ! Map SOIL_IMPV - IMPROVE reconstruction of Soil PM
             CASE ( ID_SOIL_IMPV )
               N_VARS = 5
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'ACCUMULATION' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AAL', 2.20 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASI', 2.49 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ACA', 1.63 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'AFE', 2.42 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 5, 'AERO', 'ATI', 1.94 )
 
             ! Map PMF_UNSP1 - Retrieve Fine-Mode Unspeciated PM based 
             !   on reconstructed SOIL PM from IMPROVE method and including 
             !   NCOM in the unspeciated portion
             CASE ( ID_PMF_UNSP1 )
               N_VARS = 9
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AGG',  'PMF_MASS', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASO4', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ANO3', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'ANH4', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 5, 'AERO', 'ACL', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 6, 'AERO', 'ANA', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 7, 'AERO', 'AEC', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 8, 'AGG',  'PMF_OC', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 9, 'AGG',  'PMF_SOILIMPV', -1.0 )          
 
             ! Map PMF_UNSP2 - Retrieve Fine-Mode Unspeciated PM based 
             !   on reconstructed SOIL PM from IMPROVE method and EXCLUDING 
             !   NCOM in the unspeciated portion
             CASE ( ID_PMF_UNSP2 )
               N_VARS = 9
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AGG',  'PMF_MASS', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASO4', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ANO3', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'ANH4', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 5, 'AERO', 'ACL', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 6, 'AERO', 'ANA', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 7, 'AERO', 'AEC', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 8, 'AGG',  'PMF_OA', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 9, 'AGG',  'PMF_SOILIMPV', -1.0 )          

             ! Map PMF_HP - Fine-Mode Hydronium Ion
             CASE ( ID_PMF_HP ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AH3OP', 1.0/19.0 )
 
             ! Map PMF_NCOM - Fine-Mode Non-Carbon Organic Mass in POA
             CASE ( ID_PMF_NCOM ) 
               N_VARS = 2
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AGG', 'PMF_OA', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AGG', 'PMF_OC',-1.0 )
 
             ! Map PMF_CLDGLY - Fine-Mode SOA from glyoxal and methylglyoxal
             CASE ( ID_PMF_CLDGLY ) 
               N_VARS = 2
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AORGC', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'AGLY', 1.0 )
 
             ! Map PMF_ISOP - Fine-Mode Isoprene SOA excluding IEPOX SOA
             CASE ( ID_PMF_ISOP ) 
               N_VARS = 2
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AISO1', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'AISO2', 1.0 )
 
             ! Map PMF_IEPOX - Fine-Mode IEPOX SOA
             CASE ( ID_PMF_IEPOX ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AISO3', 1.0 )
 
             ! Map PMF_MTNSOA - Fine-Mode Monoterpene Nitrate SOA
             CASE ( ID_PMF_MTNSOA ) 
               N_VARS = 2
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AMTNO3', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'AMTHYD', 1.0 )
 
             ! Map PMF_MTSOA - Fine-Mode Monoterpene SOA excluding
             ! Nitrates
             CASE ( ID_PMF_MTSOA ) 
               N_VARS = 6
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AMT1', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'AMT2', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'AMT3', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'AMT4', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 5, 'AERO', 'AMT5', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 6, 'AERO', 'AMT6', 1.0 )
 
             
             !!!! AMS Parameters !!!!  

             ! Map AMS_SO4 - AMS-Measure Sulfate
             CASE ( ID_AMS_SO4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PMAMS' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
             
             ! Map AMS_NO3 - AMS-Measured Nitrate
             CASE ( ID_AMS_NO3 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PMAMS' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map AMS_NH4 - AMS-Measured Ammonium
             CASE ( ID_AMS_NH4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PMAMS' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map AMS_CL - AMS-Measured Chloride
             CASE ( ID_AMS_CL ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PMAMS' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             
             !!!! PM1.0 Parameters !!!!  

             ! Map PM1_SO4 - PM1 Sulfate
             CASE ( ID_PM1_SO4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
 
             ! Map PM1_NO3 - PM1 Nitrate
             CASE ( ID_PM1_NO3 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map PM1_NH4 - PM1 Ammonium
             CASE ( ID_PM1_NH4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map PM1_CL - PM1 Chloride
             CASE ( ID_PM1_CL ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             ! Map PM1_NA - PM1 Sodium
             CASE ( ID_PM1_NA ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
 
             ! Map PM1_EC - PM1 Elemental Carbon
             CASE ( ID_PM1_EC ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AEC', 1.0 )
 
             ! Map PM1_MG - PM1 Magnesium
             CASE ( ID_PM1_MG ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AMG', 1.0 )
 
             ! Map PM1_K - PM1 Potassium
             CASE ( ID_PM1_K ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AK', 1.0 )
 
             ! Map PM1_CA - PM1 Calcium
             CASE ( ID_PM1_CA ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACA', 1.0 )
 
             ! Map PM1_Other - PM1 Other 
             CASE ( ID_PM1_OT ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AOTHR', 1.0 )
 
             ! Map PM1_FE - PM1 Iron
             CASE ( ID_PM1_FE ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AFE', 1.0 )
 
             ! Map PM1_SI - PM1 Silicon
             CASE ( ID_PM1_SI ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASI', 1.0 )
 
             ! Map PM1_TI - PM1 Titanium
             CASE ( ID_PM1_TI ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ATI', 1.0 )
 
             ! Map PM1_MN - PM1 Manganese
             CASE ( ID_PM1_MN ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AMN', 1.0 )
 
             ! Map PM1_AL - PM1 Aluminum
             CASE ( ID_PM1_AL ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AAL', 1.0 )
  
             ! Map PM1_Soil - PM1.0 Reconstructed Soil for the fine
             ! mode + explicit soil for the coarse mode
             CASE ( ID_PM1_SOIL ) 
               N_VARS = 6
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASOIL', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'AAL', 2.20 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ASI', 2.49 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'ACA', 1.63 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 5, 'AERO', 'AFE', 2.42 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 6, 'AERO', 'ATI', 1.94 )
   
             ! Map PM1.0 Unspeciated PM based on reconstructed
             ! SOIL PM from IMPROVE method and including NCOM in the
             ! unspeciated portion
             CASE ( ID_PM1_UNSP1 )
               N_VARS = 9
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AGG',  'PM1', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASO4', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ANO3', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'ANH4', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 5, 'AERO', 'ACL', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 6, 'AERO', 'ANA', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 7, 'AERO', 'AEC', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 8, 'AGG',  'PM1_OC', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 9, 'AGG',  'PM1_SOIL', -1.0 )          
  
             ! Map PM1_UN - PM1.0 Unspeciated Coarse Mass
             CASE ( ID_PM1_UN ) 
               N_VARS = 3
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', 0.0 ) ! All Seaspray Cation mass has been 
                                                                           ! apportined to Na, Mg, K, and Ca
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL', 0.8124 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ACORS', 0.9207 )
              
             ! Map PM1_HP - PM1.0 Hydronium Ion
             CASE ( ID_PM1_HP ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AH3OP', 1.0/19.0 ) 
             
             !!!! PM2.5 Parameters !!!!  

             ! Map PM25_SO4 - PM2.5 Sulfate
             CASE ( ID_PM25_SO4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
 
             ! Map PM25_NO3 - PM2.5 Nitrate
             CASE ( ID_PM25_NO3 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map PM25_NH4 - PM2.5 Ammonium
             CASE ( ID_PM25_NH4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map PM25_CL - PM2.5 Chloride
             CASE ( ID_PM25_CL ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             ! Map PM25_NA - PM2.5 Sodium
             CASE ( ID_PM25_NA ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
 
             ! Map PM25_EC - PM2.5 Elemental Carbon
             CASE ( ID_PM25_EC ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AEC', 1.0 )
 
             ! Map PM25_MG - PM2.5 Magnesium
             CASE ( ID_PM25_MG ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AMG', 1.0 )
 
             ! Map PM25_K - PM2.5 Potassium
             CASE ( ID_PM25_K ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AK', 1.0 )
 
             ! Map PM25_CA - PM2.5 Calcium
             CASE ( ID_PM25_CA ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACA', 1.0 )
 
             ! Map PM25_Other - PM25 Other 
             CASE ( ID_PM25_OT ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AOTHR', 1.0 )
 
             ! Map PM25_FE - PM2.5 Iron
             CASE ( ID_PM25_FE ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AFE', 1.0 )
 
             ! Map PM25_SI - PM2.5 Silicon
             CASE ( ID_PM25_SI ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASI', 1.0 )
 
             ! Map PM25_TI - PM2.5 Titanium
             CASE ( ID_PM25_TI ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ATI', 1.0 )
 
             ! Map PM25_MN - PM2.5 Manganese
             CASE ( ID_PM25_MN ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AMN', 1.0 )
 
             ! Map PM25_AL - PM2.5 Aluminum
             CASE ( ID_PM25_AL ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AAL', 1.0 )
 
             ! Map PM25_Soil - PM2.5 Reconstructed Soil for the fine
             ! mode + explicit soil for the coarse mode
             CASE ( ID_PM25_SOIL ) 
               N_VARS = 6
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASOIL', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'AAL', 2.20 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ASI', 2.49 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'ACA', 1.63 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 5, 'AERO', 'AFE', 2.42 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 6, 'AERO', 'ATI', 1.94 )
   
             ! Map PM2.5 Unspeciated PM based on reconstructed
             ! SOIL PM from IMPROVE method and including NCOM in the
             ! unspeciated portion
             CASE ( ID_PM25_UNSP1 )
               N_VARS = 9
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AGG',  'PM25', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASO4', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ANO3', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'ANH4', -1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 5, 'AERO', 'ACL', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 6, 'AERO', 'ANA', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 7, 'AERO', 'AEC', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 8, 'AGG',  'PM25_OC', -1.0 )          
               CALL SET_PMDIAG_COEFF_MAP( IDG, 9, 'AGG',  'PM25_SOIL', -1.0 )          
 
             ! Map PM25_UN - PM2.5 Unspeciated Coarse Mass
             CASE ( ID_PM25_UN ) 
               N_VARS = 3
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', 0.0 ) ! All Seaspray Cation mass has been 
                                                                           ! apportined to Na, Mg, K, and Ca
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL', 0.8124 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ACORS', 0.9207 )
 
             ! Map PM25_HP - PM2.5 Hydronium Ion
             CASE ( ID_PM25_HP ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AH3OP', 1.0/19.0 )
 
             
             !!!! PM2.5 - 10.0 Parameters !!!!  

             ! Map PM25to10_SO4 - PM2.5-10.0 Sulfate
             CASE ( ID_PM25to10_SO4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25to10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
 
             ! Map PM25to10_NO3 - PM2.5-10.0 Nitrate
             CASE ( ID_PM25to10_NO3 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25to10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map PM25to10_NH4 - PM2.5-10.0 Ammonium
             CASE ( ID_PM25to10_NH4 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25to10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map PM25to10_CL - PM2.5-10.0 Chloride
             CASE ( ID_PM25to10_CL ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25to10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             ! Map PM25to10_NA - PM2.5-10.0 Sodium
             CASE ( ID_PM25to10_NA ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25to10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
 
             !!!! Toxics !!!!  
 
             ! Map PM25_HDSL - HAP inventory PM2.5 diesel mass
             CASE ( ID_PM25_HDSL ) 
               N_VARS = 6
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ADE_OTHR', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ADE_EC', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ADE_OC', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'ADE_SO4', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 5, 'AERO', 'ADE_NO3', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 6, 'AERO', 'ADE_K', 1.0 )
 
             ! Map PM25_HBE - HAP inventory Beryllium
             CASE ( ID_PM25_HBE ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ABE', 1.0 )
 
             ! Map PM25_HCD - HAP inventory Cadmium
             CASE ( ID_PM25_HCD ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACD', 1.0 )
 
             ! Map PM25_HCR3 - HAP inventory Chromium III
             CASE ( ID_PM25_HCR3 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACR_III', 1.0 )
 
             ! Map PM25_HCR6 - HAP inventory Chromium VI
             CASE ( ID_PM25_HCR6 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACR_VI', 1.0 )
 
             ! Map PM25_HCR - HAP inventory Total Chromium
             CASE ( ID_PM25_HCR ) 
               N_VARS = 2
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AGG', 'PM25_HCR3', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AGG', 'PM25_HCR6', 1.0 )
 
             ! Map PM25_HPB - HAP inventory Lead
             CASE ( ID_PM25_HPB ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'APB', 1.0 )
 
             ! Map PM25_HMN - HAP inventory Manganese
             CASE ( ID_PM25_HMN ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AMN_HAPS', 1.0 )
 
             ! Map PM25_HNI - HAP inventory Nickel
             CASE ( ID_PM25_HNI ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANI', 1.0 )
 
             ! Map PM25_HAS - HAP inventory Arsenic
             CASE ( ID_PM25_HAS ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AAS', 1.0 )
 
             ! Map PM25_HG - Mercury
             CASE ( ID_PM25_HG ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'APHG', 1.0 )
 
             ! Map PM10_HDSL - HAP inventory PM2.5 diesel mass
             CASE ( ID_PM10_HDSL ) 
               N_VARS = 6
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ADE_OTHR', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AERO', 'ADE_EC', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'ADE_OC', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'ADE_SO4', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 5, 'AERO', 'ADE_NO3', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 6, 'AERO', 'ADE_K', 1.0 )
 
             ! Map PM10_HBE - HAP inventory Beryllium
             CASE ( ID_PM10_HBE ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ABE', 1.0 )
 
             ! Map PM10_HCD - HAP inventory Cadmium
             CASE ( ID_PM10_HCD ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACD', 1.0 )
 
             ! Map PM10_HCR3 - HAP inventory Chromium III
             CASE ( ID_PM10_HCR3 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACR_III', 1.0 )
 
             ! Map PM10_HCR6 - HAP inventory Chromium VI
             CASE ( ID_PM10_HCR6 ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ACR_VI', 1.0 )
 
             ! Map PM10_HCR - HAP inventory Total Chromium
             CASE ( ID_PM10_HCR ) 
               N_VARS = 2
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AGG', 'PM10_HCR3', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'AGG', 'PM10_HCR6', 1.0 )
 
             ! Map PM10_HPB - HAP inventory Lead
             CASE ( ID_PM10_HPB ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'APB', 1.0 )
 
             ! Map PM10_HMN - HAP inventory Manganese
             CASE ( ID_PM10_HMN ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AMN_HAPS', 1.0 )
 
             ! Map PM10_HNI - HAP inventory Nickel
             CASE ( ID_PM10_HNI ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANI', 1.0 )
 
             ! Map PM10_HAS - HAP inventory Arsenic
             CASE ( ID_PM10_HAS ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'AAS', 1.0 )
 
             ! Map PM10_HG - Mercury
             CASE ( ID_PM10_HG ) 
               N_VARS = 1
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'APHG', 1.0 )
 
             !!!! Other Parameters !!!!  

             ! Map TNO3 - Total Inorganic Nitrate
             CASE ( ID_TNO3 ) 
               N_VARS = 2
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'ALL' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'GAS', 'HNO3', 2175.6 )
 
             ! Map TNO3TOT - Total Inorganic+Organic Nitrate
             CASE ( ID_TNO3TOT ) 
               N_VARS = 4
               CALL INIT_PMDIAG_COEFFS( IDG, N_VARS, 'ALL' )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 2, 'GAS', 'HNO3', 2175.6 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 3, 'AERO', 'AISOPNN', 2.0*62.0/226.0 )
               CALL SET_PMDIAG_COEFF_MAP( IDG, 4, 'AERO', 'AMTNO3', 62.0/231.0 )
 
         END SELECT

      END DO
      END SUBROUTINE MAP_PMDIAG_COEFFS
 
!-------------------------------------------------------------------------
      SUBROUTINE INIT_PMDIAG_COEFFS( IDG, N_VARS, AEROTYPE_TMP )
!     This subroutine allocates and initalizes all of the elements of an 
!        index IDG within the PMDIAG_COEFFS structure
!
!     IDG - the index of this diagnostic parameter in the space defined
!           by the integer index variables like ID_PMF_POC, etc.
!     N_VARS - Number of variables used to create this parameter. Does
!           not include the number of modes. For example, ASO4I+ASO4J 
!           counts as 1 variable (ASO4). It will be expanded to modes later.
!     AEROTYPE - Aerosol size range or mode to be calculated. This defines 
!           the modes considered and the inlet type if any. Options
!           include:
!             ALL, NONE, ULTRAFINE, FINE, COARSE, NUCLEATION, AITKEN,
!             ACCUMULATION, PM01, PM1, PM25, PM10, PM25to10 and PMAMS
!     
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY : N_MODE, AEROMODE

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: IDG 
      INTEGER, INTENT( IN ) :: N_VARS
      CHARACTER( * ), INTENT( IN ) ::  AEROTYPE_TMP
      CHARACTER( 20 ) AEROTYPE
      CHARACTER( 200 ) XMSG
      CHARACTER(16), SAVE :: PNAME = 'INIT_PMDIAG_COEF'
      INTEGER ALLOCSTAT

      ! Initialize All Components of PMDIAG_COEFFS
      PMDIAG_COEFFS( IDG )%L_MAPPED = .TRUE.
      PMDIAG_COEFFS( IDG )%N_VARS = N_VARS
      ALLOCATE( PMDIAG_COEFFS( IDG )%L_MODE( N_MODE ),
     &          PMDIAG_COEFFS( IDG )%L_GAS ( N_VARS ),
     &          PMDIAG_COEFFS( IDG )%L_PREC( N_VARS ),
     &          PMDIAG_COEFFS( IDG )%L_AGG ( N_VARS ),
     &          PMDIAG_COEFFS( IDG )%I_SPEC( N_VARS ),
     &          PMDIAG_COEFFS( IDG )%COEFF ( N_VARS ),
     &          STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating PMDIAG_COEFFS' 
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      PMDIAG_COEFFS( IDG )%L_GAS  ( : )   = .FALSE.
      PMDIAG_COEFFS( IDG )%L_PREC ( : )   = .FALSE.
      PMDIAG_COEFFS( IDG )%L_AGG  ( : )   = .FALSE.
      PMDIAG_COEFFS( IDG )%I_SPEC ( : )   = 0
      PMDIAG_COEFFS( IDG )%L_MODE ( : )   = .FALSE.
      PMDIAG_COEFFS( IDG )%COEFF  ( : )   = 0.
      PMDIAG_COEFFS( IDG )%INLET          = 0
      PMDIAG_COEFFS( IDG )%WETNESS        = IWET

      ! Specify the Aerosol Modes Needed and Inlet Type based on User
      ! Input through the AEROTYPE variable.
      AEROTYPE = AEROTYPE_TMP
      CALL UPCASE( AEROTYPE )
      SELECT CASE ( AEROTYPE )
         CASE ( 'ALL' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = .TRUE.
         CASE ('NONE' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = .FALSE.
         CASE ('ULTRAFINE' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%ULTRAFINE_MASK
         CASE ('FINE' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%FINE_MASK
         CASE ('COARSE' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%COARSE_MASK
         CASE ('NUCLEATION' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%NUC_MASK
         CASE ('AITKEN' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%AITKEN_MASK
         CASE ('ACCUMULATION' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%ACCUM_MASK
         CASE ('PM01' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             PMDIAG_COEFFS( IDG )%INLET = IPM01
             PMDIAG_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM1' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             PMDIAG_COEFFS( IDG )%INLET = IPM1
             PMDIAG_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM25' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             PMDIAG_COEFFS( IDG )%INLET = IPM25
             PMDIAG_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM10' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             PMDIAG_COEFFS( IDG )%INLET = IPM10
             PMDIAG_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM25TO10' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             PMDIAG_COEFFS( IDG )%INLET = IPM25to10
             PMDIAG_COEFFS( IDG )%WETNESS = IWET
         CASE ('PMAMS' )
             PMDIAG_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             PMDIAG_COEFFS( IDG )%INLET = IPMAMS
             PMDIAG_COEFFS( IDG )%WETNESS = IWET
         CASE DEFAULT
             XMSG = 'An aerosol descriptor (' // trim(AEROTYPE) // ') has been given ' //
     &              'for the mapping of a PMDIAG parameter. It is not recognized.'
             CALL M3EXIT( 'INIT_PMDIAG_COEFFS', 0, 0, XMSG, XSTAT1 )
      END SELECT


      END SUBROUTINE INIT_PMDIAG_COEFFS

!-------------------------------------------------------------------------
      SUBROUTINE SET_PMDIAG_COEFF_MAP( IDG, IVAR, TSPEC_TMP, CSPEC, COEFF )
!     This subroutine maps the modes designated by the variable CMODE to
!        to the indices available in L_MODE.
!-------------------------------------------------------------------------

      USE PRECURSOR_DATA, ONLY : N_PRECURSOR, PRECURSOR
      USE CGRID_SPCS, ONLY : N_CGRID_SPC, CGRID_NAME
      USE AERO_DATA, ONLY : N_AEROSPC, AEROSPC
      USE RUNTIME_VARS
      IMPLICIT NONE

      INTEGER, INTENT( IN ) ::  IDG, IVAR
      CHARACTER( * ), INTENT( IN ) :: CSPEC
      CHARACTER( * ), INTENT( IN ) :: TSPEC_TMP  ! AERO, PREC, GAS, or AGG
      CHARACTER( 4 ) :: TSPEC  ! AERO, PREC, GAS, or AGG
      REAL, INTENT( IN ) :: COEFF
      INTEGER  ISPEC, IUSED, JSPEC
      LOGICAL  USED
      CHARACTER( 1000 ) :: XMSG

      TSPEC = TSPEC_TMP
      CALL UPCASE( TSPEC )

      ! Find Index of CSPEC in Gas Array, Aggregate Array or AEROSPC%BULKNAME
      IF ( TSPEC .EQ. 'PREC' ) THEN 
          PMDIAG_COEFFS( IDG )%L_PREC( IVAR ) = .TRUE. 

          ! Map to Gas-Phase Aerosol Precursor
          ! Check the precursor array
          ISPEC = INDEX1( CSPEC, N_PRECURSOR, PRECURSOR(:)%NAME )
 
          IF ( ISPEC .GT. 0 ) THEN
             ! Found the Index. Save it!
             PMDIAG_COEFFS( IDG )%I_SPEC( IVAR ) = ISPEC
          ELSE
             ! Could not find the species in the Gas Precursor Array. Print an
             ! Error and Warn
             XMSG = "A PM Diagnostic variable has been requested that relies " //
     &           "on gas precursor species " // TRIM(CSPEC) // ". This species has not been " //
     &           "incorporated in the precursor array though. Please check that the table "//
     &           "in PRECURSOR_DATA is compatible with the PM diagnostics you have called "//
     &           "for in the Emissions Control Namelist."
             CALL LOG_MESSAGE( LOGDEV, XMSG )
             CALL M3WARN( 'MAP_PMDIAG_COEFFS', 0, 0, '' )
          END IF 
      ELSE IF ( TSPEC .EQ. 'GAS' ) THEN 
          ! Map to Generic Gas Species
          ! First check the precursor array. This is default because if
          ! the gas component IS an aerosol precursor (and the developer
          ! doesn't know it), then it is important to map to the
          ! precursor array because the value may have changed here
          ! during the aero module and not be reflected in the cgrid
          ! array
          ISPEC = INDEX1( CSPEC, N_PRECURSOR, PRECURSOR(:)%NAME )

          ! If it is a precursor gas
          IF ( ISPEC .NE. 0 ) THEN
             ! Reassign identity of this variable to the precursor array
             ! and not the gas section of the cgrid array
             PMDIAG_COEFFS( IDG )%L_PREC( IVAR ) = .TRUE.
             PMDIAG_COEFFS( IDG )%L_GAS( IVAR )  = .FALSE.
          ELSE
             ! Then check the CGRID array
             PMDIAG_COEFFS( IDG )%L_GAS( IVAR ) = .TRUE.
             ISPEC = INDEX1( CSPEC, N_CGRID_SPC, CGRID_NAME(:) )
          END IF
 
          IF ( ISPEC .GT. 0 ) THEN
             ! Found the Index. Save it!
             PMDIAG_COEFFS( IDG )%I_SPEC( IVAR ) = ISPEC
          ELSE
             ! Could not find the species in the CGRID Array. Print an
             ! Error and Warn
             XMSG = "A PM Diagnostic variable has been requested that relies " //
     &           "on gas species " // TRIM(CSPEC) // ". This species has not been " //
     &           "incorporated in the GC namelist though. Please check that your "//
     &           "GC_namelist is compatible with the PM diagnostics you have called "//
     &           "for in the Emmissions Control Namelist."
             CALL LOG_MESSAGE( LOGDEV, XMSG )
             CALL M3WARN( 'MAP_PMDIAG_COEFFS', 0, 0, '' )
          END IF
      
      ELSE IF ( TSPEC .EQ. 'AGG' ) THEN 
          ! Map to Index on PMDIAG_LIST, not the ID column
          PMDIAG_COEFFS( IDG )%L_AGG( IVAR ) = .TRUE.
          ISPEC = INDEX1( CSPEC, N_PMDIAG_LIST, PMDIAG_LIST%NAME )
      
          IF ( ISPEC .GT. 0 ) THEN
             ! Found the Index. Save the ID for this entry on the PMDIAG_LIST
             PMDIAG_COEFFS( IDG )%I_SPEC( IVAR ) = PMDIAG_LIST( ISPEC )%ID
             
             ! Check if it is used and add it to the USED_PMDIAG_PARS
             ! vector if not
             USED = .FALSE.
             DO JSPEC = 1,N_PMDIAG
                IF ( PMDIAG_LIST( ISPEC )%ID .EQ. USED_PMDIAG_PARS( JSPEC ) ) USED = .TRUE.
             END DO
             IF ( .NOT. USED ) THEN
                 N_PMDIAG = N_PMDIAG + 1
                 USED_PMDIAG_PARS( N_PMDIAG ) = PMDIAG_LIST( ISPEC )%ID
             END IF

          ELSE
             ! Could not find the species in the PMDIAG List. Print an
             ! Error and Crash
             XMSG = "ERROR: A PM Diagnostic variable has been requested that relies " //
     &           "on the diagnostic aggregate variable " // TRIM(CSPEC) // ". This variable "//
     &           "has not been incorporated in the PMDIAG List though. Please check that the "//
     &           "table in PMDIAG_DATA is compatible with the PM diagnostics you have defined "//
     &           "in PMDIAG_PROC."
             CALL LOG_MESSAGE( LOGDEV, XMSG )
             CALL M3EXIT( 'MAP_PMDIAG_COEFFS', 0, 0, '', XSTAT1 )
          END IF          
  
      ELSE
          ! Aerosol
          ISPEC = INDEX1( CSPEC, N_AEROSPC, AEROSPC(:)%BULKNAME )
      
          IF ( ISPEC .GT. 0 ) THEN
             ! Found the Index. Save it!
             PMDIAG_COEFFS( IDG )%I_SPEC( IVAR ) = ISPEC
          ELSE
             ! Could not find the species in the Aerosol Array. Print an
             ! Error and Warn
             XMSG = "A PM Diagnostic variable has been requested that relies " //
     &           "on the aerosol species "//TRIM(CSPEC)//". This species has not been " //
     &           "incorporated in the AE namelist though. Please check that your "//
     &           "AE_namelist is compatible with the PM diagnostics you have called "//
     &           "for in the Emmissions Control Namelist."
             CALL LOG_MESSAGE( LOGDEV, XMSG )
             CALL M3WARN( 'MAP_PMDIAG_COEFFS', 0, 0, '' )
          END IF
      END IF
      PMDIAG_COEFFS( IDG )%COEFF( IVAR ) = COEFF

      END SUBROUTINE SET_PMDIAG_COEFF_MAP

!-------------------------------------------------------------------------
      SUBROUTINE CALC_PMDIAG_AGG( IDG, VAL )
!     This subroutine sums aggregate PM diagnostic parameters using the
!         PMDIAG_COEFFS structure as a guide for summing aerosol
!         components.          
!-------------------------------------------------------------------------
      USE AERO_DATA, ONLY : WET_AERO_M3, WET_AERO_M2, MOMENT0_CONC,
     &                      AH2O_IDX, AEROSPC, AEROSPC_CONC,
     &                      WET_AERO_DENS, N_MODE, AERO_MISSING,
     &                      AEROMODE
      USE PRECURSOR_DATA, ONLY : PRECURSOR_CONC
      USE AEROMET_DATA, ONLY : AIRDENS
      USE RUNTIME_VARS

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: IDG
      REAL, INTENT( INOUT ) :: VAL
      REAL VAL2
      INTEGER IVAR, ILIST
      LOGICAL L_CALC( N_PMDIAG )
      REAl    COEFF
      INTEGER IM, WETNESS, ISPEC, INLET
      LOGICAL LMODE
      REAL    FRAC( N_MODE )

      CHARACTER( 500 ) :: XMSG

      ! If this Aggregate has not been mapped already, then return an
      ! error and crash the model
      IF ( .NOT. PMDIAG_COEFFS( IDG )%L_MAPPED ) THEN
         ILIST = MAP_ID2LIST( IDG )  
         XMSG = 'PMDIAG Parameter ' // TRIM( PMDIAG_LIST( ILIST )%NAME ) //
     &          ' has not been mapped to a procedure within PMDIAG_PROC even ' //
     &          'though it does appear to be an acceptable parameter on the ' //
     &          'PMDIAG_LIST. If you have added this parameter yourself, make ' //
     &          'sure it is being mapped to an actual calculation in CALC_PMDIAG ' //
     &          'or MAP_PMDIAG_COEFFS.'
         CALL LOG_MESSAGE( LOGDEV, XMSG )
         CALL M3EXIT( 'CALC_PMDIAG_AGG', 0, 0, '', XSTAT1 )
      END IF

      ! Initialize value of aggregate parameter
      VAL = 0.0
      
      ! Retrieve size cutoff if necessary
      IF ( PMDIAG_COEFFS( IDG )%INLET .EQ. IPMAMS ) THEN
         ! Inlet is for AMS Colection
         DO IM = 1,N_MODE
           IF ( INLET_FRAC( IPMAMS,IM,IWET ) .LT. 0. ) 
     &        CALL AERO_AMS( WET_AERO_M3( IM ),  WET_AERO_M2( IM ),
     &                       MOMENT0_CONC( IM ), AEROSPC_CONC( AH2O_IDX,IM ),
     &                       WET_AERO_DENS( IM ),AEROSPC( AH2O_IDX )%DENSITY, 
     &                       INLET_FRAC( IPMAMS,IM,IWET ) )
           FRAC( IM ) = INLET_FRAC( IPMAMS,IM,IWET )
         END DO
      ELSE IF ( PMDIAG_COEFFS(IDG)%INLET .GT. 0 .AND.
     &          PMDIAG_COEFFS(IDG)%INLET .LE. N_INLET ) THEN
         ! Inlet is for a filter with a specific size-range. Call the
         ! AERO_INLET wrapper, GET_AERO_INLET.
         INLET = PMDIAG_COEFFS( IDG )%INLET
         WETNESS = PMDIAG_COEFFS( IDG )%WETNESS
         DO IM = 1,N_MODE
           IF ( INLET_FRAC( INLET,IM,WETNESS ) .LT. 0. ) 
     &        CALL GET_AERO_INLET( INLET, IM, WETNESS,
     &                             INLET_FRAC( INLET,IM,WETNESS ) )
           FRAC( IM ) = INLET_FRAC( INLET,IM,WETNESS )
         END DO
      ELSE 
         ! No inlet was indicated or the number was outside defined
         ! bounds
         FRAC( : ) = 1.0
      END IF

      ! Loop through N_VARS and add species together
      DO IVAR = 1,PMDIAG_COEFFS( IDG )%N_VARS
          ISPEC = PMDIAG_COEFFS( IDG )%I_SPEC( IVAR )
          COEFF = PMDIAG_COEFFS( IDG )%COEFF( IVAR )

          ! First check if this is a Precursor Gas
          IF ( PMDIAG_COEFFS( IDG )%L_PREC( IVAR ) ) THEN
              VAL = VAL + REAL( PRECURSOR_CONC( ISPEC ), 4) * COEFF
          
          ! Next check if this is a Gas from CGRID
          ELSE IF  ( PMDIAG_COEFFS( IDG )%L_GAS( IVAR ) ) THEN
              VAL = VAL + CONC_VEC( ISPEC ) * COEFF * AIRDENS

          ! Next check for aggregate variable
          ELSE IF  ( PMDIAG_COEFFS( IDG )%L_AGG( IVAR ) ) THEN
              CALL CALC_PMDIAG( ISPEC, 1, VAL2 )
              VAL = VAL + VAL2 * COEFF

          ! Add Aerosol Components
          ELSE IF ( ISPEC .GT. 0 ) THEN

              DO IM = 1,N_MODE
                 ! If this mode is supposed to be added (LMODE) and the
                 ! species is not missing, then add it up
                 LMODE = PMDIAG_COEFFS( IDG )%L_MODE( IM )
                 IF ( LMODE .AND. ( .NOT. AERO_MISSING( ISPEC,IM ) ) ) 
     &               VAL = VAL + AEROSPC_CONC( ISPEC, IM ) * COEFF * FRAC( IM )

              END DO
          END IF

      END DO 

      END SUBROUTINE CALC_PMDIAG_AGG
 
!-------------------------------------------------------------------------
      RECURSIVE SUBROUTINE CALC_PMDIAG( IDG, IM, OUTVAL )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the PMDIAG_DATA table.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC,
     &                     AH2O_IDX, AORGH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS,
     &                     AEROMODE, DRY_AERO_DENS, APOC_IDX, APNCOM_IDX
      USE AEROMET_DATA, ONLY: AIRRH, PI, AIRTEMP, SRFTEMP, AIRPRES
      USE SOA_DEFN, ONLY : N_OA, OASPC, MAP_OAtoAERO
      USE PHOT_MOD, ONLY : TAU_AERO_550, AERO_EXT_550
      USE RUNTIME_VARS, ONLY : PHOTDIAG

      INTEGER, INTENT( IN ) :: IDG, IM
      REAL, INTENT( INOUT ) :: OUTVAL
      INTEGER IUSED, IMODE, IVAR, IOA
      REAL VAL4, VAL3, VAL2, VAL, FRAC( N_MODE )
      REAL PM01, PM1, PM25, PM10, PMAMS, PM25to10, POM, POC, OtoC
      REAL K, P1, P2, P3, a, K2, PMF_NO3, PMF_NO3_LOSS,
     &     PMF_NH4_LOSS, PMF_NH4, PMF_SO4, PMF_MASS,
     &     PM25_NH4, PM25_NO3, PM25_SO4, PM25_NO3_LOSS,
     &     PM25_NH4_LOSS

      ! Map this combination of ID number and mode to the USED
      ! Parameters list, which is how PMDIAG_LOCAL is defined.
      IUSED = MAP_ID2USED( IDG, IM )

      ! First determine if this parameter has been mapped already. If it
      ! has, send it back
      IF ( L_CALC( IUSED ) ) THEN
          OUTVAL = PMDIAG_LOCAL( IUSED )
          RETURN
      END IF

      OUTVAL = 0.0

      ! Determine which parameter to calculate
      SELECT CASE( IDG )

         ! Retrieve Standard Deviation of Each Mode
         CASE ( ID_STDEV )
            OUTVAL = EXP( AEROMODE_LNSG( IM ) )

         ! Retrieve Mode Dry Diameter
         CASE ( ID_DRY_DG )
            OUTVAL = DRY_AERO_DIAM( IM )  ! dry diam.

         ! Retrieve Mode Wet Diameter
         CASE ( ID_WET_DG )
            OUTVAL = WET_AERO_DIAM( IM )  ! wet diam.

         ! Retrieve Mode Wet Second Moment
         CASE ( ID_WET_M2 )
            OUTVAL = WET_AERO_M2( IM )  ! dry M2

         ! Retrieve Dry Third Moment
         CASE ( ID_DRY_M3 )
            OUTVAL = DRY_AERO_M3( IM )  ! dry M3

         ! Retrieve Wet Third Moment
         CASE ( ID_WET_M3 )
            OUTVAL = WET_AERO_M3( IM )  ! wet M3

         ! Retrieve Total Particle Number
         CASE ( ID_PM_NUM )
            OUTVAL = SUM( MOMENT0_CONC( : ) ) ! N m-3

         ! Retrieve Ultrafine-Particle Particle Number
         CASE ( ID_PMU_NUM )
            OUTVAL = SUM( MOMENT0_CONC( : ), 
     &                          MASK=AEROMODE(:)%ULTRAFINE_MASK ) ! N m-3
         
         ! Retrieve Fine-Particle Particle Number
         CASE ( ID_PMF_NUM )
            OUTVAL = SUM( MOMENT0_CONC( : ), 
     &                          MASK=AEROMODE(:)%FINE_MASK ) ! N m-3
         
         ! Retrieve Coarse-Particle Particle Number
         CASE ( ID_PMC_NUM )
            OUTVAL = SUM( MOMENT0_CONC( : ), 
     &                          MASK=AEROMODE(:)%COARSE_MASK ) ! N m-3

         ! Retrieve N10 - Number of Particles Greater than 10 nm
         CASE ( ID_N10 )
            CALL SUM_INLET_NUM( INUM10, IWET, OUTVAL )

         ! Retrieve N20 - Number of Particles Greater than 20 nm
         CASE ( ID_N20 )
            CALL SUM_INLET_NUM( INUM20, IWET, OUTVAL )

         ! Retrieve N40 - Number of Particles Greater than 40 nm
         CASE ( ID_N40 )
            CALL SUM_INLET_NUM( INUM40, IWET, OUTVAL )

         ! Retrieve N100 - Number of Particles Greater than 100 nm
         CASE ( ID_N100 )
            CALL SUM_INLET_NUM( INUM100, IWET, OUTVAL )

         ! Retrieve Total Particle Surface Area
         CASE ( ID_PM_SRF )
            OUTVAL = PI * SUM( MOMENT2_CONC( : ) ) ! N m-3

         ! Retrieve Ultrafine Particle Surface Area
         CASE (ID_PMU_SRF )
            OUTVAL = PI * SUM( MOMENT2_CONC( : ), 
     &                          MASK=AEROMODE(:)%ULTRAFINE_MASK ) ! N m-3

         ! Retrieve Fine Particle Surface Area
         CASE ( ID_PMF_SRF )
            OUTVAL = PI * SUM( MOMENT2_CONC( : ), 
     &                          MASK=AEROMODE(:)%FINE_MASK ) ! N m-3

         ! Retrieve Coarse Particle Surface Area
         CASE ( ID_PMC_SRF )
            OUTVAL = PI * SUM( MOMENT2_CONC( : ), 
     &                          MASK=AEROMODE(:)%COARSE_MASK ) ! N m-3

         ! Retrieve Total Particle Mass
         CASE ( ID_PM_MASS )
            DO IMODE = 1,N_MODE
               OUTVAL = SUM( AEROSPC_CONC( :,IMODE ), 
     &                  MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
            END DO

         ! Retrieve Ultrafine Particle Mass
         CASE ( ID_PMU_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%ULTRAFINE_MASK )
     &              OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
            END DO

         ! Retrieve Fine Particle Mass
         CASE ( ID_PMF_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%FINE_MASK )
     &              OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER )
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
            END DO

         ! Retrieve Coarse Particle Mass
         CASE ( ID_PMC_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%COARSE_MASK )
     &              OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
            END DO

         ! Retrieve Nucleation-Mode Particle Mass
         CASE ( ID_PMNUC_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%NUC_MASK )
     &              OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
            END DO

         ! Retrieve Aitken-Mode Particle Mass
         CASE ( ID_PMAIT_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%AITKEN_MASK )
     &              OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
            END DO

         ! Retrieve Accumution-Mode Particle Mass
         CASE ( ID_PMACC_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%ACCUM_MASK )
     &              OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
            END DO

         ! Retrieve Dry Mode Density [kg m-3]
         CASE ( ID_DRY_DENS )
            OUTVAL = DRY_AERO_DENS( IM )  ! dry i-mode Density

         ! Retrieve Wet Mode Density [kg m-3]
         CASE ( ID_WET_DENS )
            OUTVAL = WET_AERO_DENS( IM )  ! wet i-mode Density

         ! Retrieve PM01 Fraction in Each Mode
         CASE ( ID_FPM01 )
            CALL GET_AERO_INLET( IPM01, IM, IWET, VAL ) 
            OUTVAL = VAL

         ! Retrieve PM1 Fraction in Each Mode
         CASE ( ID_FPM1 )
            CALL GET_AERO_INLET( IPM1, IM, IWET, VAL ) 
            OUTVAL = VAL

         ! Retrieve PM25 Fraction in Each Mode
         CASE ( ID_FPM25 )
            CALL GET_AERO_INLET( IPM25, IM, IWET, VAL ) 
            OUTVAL = VAL

         ! Retrieve PM10 Fraction in Each Mode
         CASE ( ID_FPM10 )
            CALL GET_AERO_INLET( IPM10, IM, IWET, VAL ) 
            OUTVAL = VAL

         ! Retrieve PM2.5-10 Fraction in Each Mode
         CASE ( ID_FPM25to10 )
            CALL GET_AERO_INLET( IPM25to10, IM, IWET, VAL ) 
            OUTVAL = VAL

         ! Retrieve AMS Fraction in Each Mode
         CASE ( ID_FAMS )
            IF ( INLET_FRAC( IPMAMS,IM,IWET ) .LT. 0. ) THEN
               CALL AERO_AMS( WET_AERO_M3( IM ),  WET_AERO_M2( IM ),
     &                        MOMENT0_CONC( IM ), AEROSPC_CONC( AH2O_IDX,IM ),
     &                        WET_AERO_DENS( IM ),AEROSPC( AH2O_IDX )%DENSITY, 
     &                        VAL )
               INLET_FRAC( IPMAMS,IM,IWET ) = VAL
            END IF
            OUTVAL = INLET_FRAC( IPMAMS,IM,IWET )
 
         ! Retrieve Bulk PM0.1 Concentration
         CASE ( ID_PM01 )
            CALL SUM_INLET_PM( IPM01, IWET, PM01 )
            OUTVAL = PM01

         ! Retrieve Bulk PM1.0 Concentration
         CASE ( ID_PM1 )
            CALL SUM_INLET_PM( IPM1, IWET, PM1 )
            OUTVAL = PM1

         ! Retrieve Bulk PM2.5 Concentration
         CASE ( ID_PM25 )
            CALL SUM_INLET_PM( IPM25, IWET, PM25 )
            OUTVAL = PM25 

         ! Retrieve Bulk PM10.0 Concentration
         CASE ( ID_PM10 )
            CALL SUM_INLET_PM( IPM10, IWET, PM10 )
            OUTVAL = PM10
         
         ! Retrieve Bulk PM2.5-10.0 Concentration
         CASE ( ID_PM25to10 )
            CALL SUM_INLET_PM( IPM25to10, IWET, PM25to10 )
            OUTVAL = PM25to10

         ! Retrieve Bulk PM Concentration Relevant for the AMS
         CASE ( ID_PMAMS )
            PMAMS = 0.0 
            DO IMODE = 1,N_MODE
               IF ( INLET_FRAC( IPMAMS,IMODE,IWET ) .LT. 0. ) THEN
                  CALL AERO_AMS( WET_AERO_M3( IMODE ),  WET_AERO_M2( IMODE ),
     &                           MOMENT0_CONC( IMODE ), AEROSPC_CONC( AH2O_IDX,IMODE ),
     &                           WET_AERO_DENS( IMODE ),AEROSPC( AH2O_IDX )%DENSITY, 
     &                           VAL )
                  INLET_FRAC( IPMAMS,IMODE,IWET ) = VAL
               END IF
               PMAMS = PMAMS +  ( SUM( AEROSPC_CONC( :,IMODE ),
     &                                 MASK = .NOT.AEROSPC(:)%TRACER ) 
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ) ! ug m-3
     &                  * INLET_FRAC( IPMAMS,IMODE,IWET ) 
            END DO
            OUTVAL = PMAMS
         
         ! Calculate AHPMOLAL - Fine Mode Concentration of H+ in
         ! Particle Water
         CASE ( ID_PMF_HPM )
            ! Retrieve AHPLUSIJ
            CALL CALC_PMDIAG( ID_PMF_HP, 1, VAL )
            ! Retrieve AH2OIJ
            CALL CALC_PMDIAG( ID_PMF_H2O, 1, VAL2 )
            ! HPMOLAL = AHPLUSIJ / AH2OIJ * 1000.0 [mol L-1]
            OUTVAL = VAL / VAL2 * 1000.0

         ! Calculate PMF_PH - Fine-Mode pH
         CASE ( ID_PMF_PH )
            ! Retrieve HPMOLAL
            CALL CALC_PMDIAG( ID_PMF_HPM, 1, VAL )
            ! Retrieve AH2OIJ
            CALL CALC_PMDIAG( ID_PMF_H2O, 1, VAL2 )

            !PMF_PH = -1*LOG10(HPMOLAL)
            OUTVAL = BADVAL3
            IF ( VAL2 .GT. 0.01 )
     &           OUTVAL = -1.0 * LOG10( VAL )

         ! Calculate POCIJ - Fine-Mode Primary Organic Carbon Mass
         CASE ( ID_PMF_POC )
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA
                ! Just Select Primary Species
                IF ( OASPC( IOA )%PRIMARY ) THEN  
                   IF ( OASPC( IOA )%OMtoOC .gt. 0.0 ) 
     &                VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                            MASK = AEROMODE(:)%FINE_MASK ) 
     &                          / OASPC( IOA )%OMtoOC
                END IF
            END DO
            OUTVAL = VAL
      
         ! Calculate SOCIJ - Fine-Mode Secondary Organic Carbon Mass
         CASE ( ID_PMF_SOC )
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA
                ! Just Select Secondary Species
                IF ( .NOT. OASPC( IOA )%PRIMARY ) THEN  
                      VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                            MASK = AEROMODE(:)%FINE_MASK ) 
     &                          / OASPC( IOA )%OMtoOC
                END IF
            END DO
            OUTVAL = VAL 
       
         ! Calculate OCIJ - Fine-Mode Organic Carbon Mass
         CASE ( ID_PMF_OC )
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA
               VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                     MASK = AEROMODE(:)%FINE_MASK ) 
     &                   / OASPC( IOA )%OMtoOC
            END DO
            OUTVAL = VAL  

         ! Calculate PMF_POA - Fine-Mode Primary Organic Aerosol Mass
         CASE ( ID_PMF_POA )
            ! Sum up primary organic aerosol species 
            VAL = 0.0
            DO IOA = 1,N_OA
                ! Just Select Primary Species
                IF ( OASPC( IOA )%PRIMARY ) 
     &             VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                         MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL
 
         ! Calculate PMF_SOA - Fine-Mode Secondary Organic Aerosol Mass
         CASE ( ID_PMF_SOA )
            ! Sum up primary organic aerosol species 
            VAL = 0.0
            DO IOA = 1,N_OA
                ! Just Select Secondary Species
                IF ( .NOT.OASPC( IOA )%PRIMARY ) 
     &             VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                         MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL
 
         ! Calculate PMF_OA - Fine-Mode Organic Aerosol Mass
         CASE ( ID_PMF_OA )
            ! Sum up primary organic aerosol species 
            VAL = 0.0
            DO IOA = 1,N_OA
                VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                      MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL
         
         ! Calculate PMF_ASOA - Fine-Mode Anthropogenic OA Mass
         CASE ( ID_PMF_ASOA )
            ! Sum up anthropogenic oa species 
            VAL = 0.0
            DO IOA = 1,N_OA
                ! Just Select Anthropogenic Species
                IF ( .NOT.OASPC( IOA )%PRIMARY .AND. OASPC( IOA )%ANTHRO  ) 
     &             VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                         MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL
          
         ! Calculate PMF_BSOA - Fine-Mode Biogenic SOA Mass
         CASE ( ID_PMF_BSOA )
            ! Sum up biogenic soa species 
            VAL = 0.0
            DO IOA = 1,N_OA
                ! Just Select Biogenic Secondary Species
                IF ( .NOT.OASPC( IOA )%PRIMARY .AND. OASPC( IOA )%BIOG  ) 
     &             VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                         MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL
 
         ! Calculate PMF_OMOC - Fine-Mode OM:OC 
         CASE ( ID_PMF_OMOC )
            VAL  = 0.0
            VAL2 = 0.0
            CALL CALC_PMDIAG( ID_PMF_OA, 1, VAL  )
            CALL CALC_PMDIAG( ID_PMF_OC, 1, VAL2 )
            OUTVAL = VAL / ( VAL2 + TINY( 0.0 ) )
            
         ! Calculate PMF_OtoC - Fine-Mode O:C 
         CASE ( ID_PMF_OtoC )
            CALL CALC_PMDIAG( ID_PMF_OA, 1, VAL  )

            VAL2 = 0.0
            DO IOA = 1,N_OA
               IF ( MAP_OAtoAERO(IOA) .NE. APOC_IDX .AND.
     &              MAP_OAtoAERO(IOA) .NE. APNCOM_IDX ) THEN
                  ! Weight OA Sum by O:C
                  VAL2 = VAL2 + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ), 
     &                               MASK = AEROMODE(:)%FINE_MASK )        
     &                          * OASPC(IOA)%OtoC                    
               END IF
            END DO
            ! Add POC/PNCOM Contribution to Weighted O:C
            POC = SUM( AEROSPC_CONC(APOC_IDX,:),MASK = AEROMODE(:)%FINE_MASK )
            POM = POC + SUM( AEROSPC_CONC(APNCOM_IDX,:), MASK = AEROMODE(:)%FINE_MASK )
            OtoC = 12./15. * ( POM / ( POC + TINY(0.0) ) )- 14./15.  
            
            OUTVAL = ( VAL2 + OtoC*POM ) / ( VAL + TINY(0.0))

         ! Calculate PM1_OC - PM1.0 Organic Carbon Mass
         CASE ( ID_PM1_OC )
            ! Save PM1 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM1, IM, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA
               IF ( OASPC( IOA )%OMtoOC .GE. 0.0 ) 
     &              VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:))
     &                         / OASPC( IOA )%OMtoOC
            END DO
            OUTVAL = VAL  
 
         ! Calculate PM1_OM - PM1.0 Organic Aerosol Mass
         CASE ( ID_PM1_OA )
            ! Save PM1 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM1, IM, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species
            VAL = 0.0
            DO IOA = 1,N_OA
               VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:))
            END DO
            OUTVAL = VAL  
 
         ! Calculate PM25_OC - PM2.5 Organic Carbon Mass
         CASE ( ID_PM25_OC )
            ! Save PM2.5 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM25, IM, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA
               IF ( OASPC( IOA )%OMtoOC .GE. 0.0 ) 
     &              VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:) )
     &                        / OASPC( IOA )%OMtoOC
            END DO
            OUTVAL = VAL  
 
         ! Calculate PM25_OM - PM2.5 Organic Aerosol Mass
         CASE ( ID_PM25_OA )
            ! Save PM2.5 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM25, IM, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species
            VAL = 0.0
            DO IOA = 1,N_OA
               VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:) )
            END DO
            OUTVAL = VAL  
 
         ! Retrieve OA Concentration Relevant for the AMS
         CASE ( ID_AMS_OA )
            VAL2 = 0.0 
            DO IMODE = 1,N_MODE
               IF ( INLET_FRAC( IPMAMS,IMODE,IWET ) .LT. 0. ) THEN
                  CALL AERO_AMS( WET_AERO_M3( IMODE ),  WET_AERO_M2( IMODE ),
     &                           MOMENT0_CONC( IMODE ), AEROSPC_CONC( AH2O_IDX,IMODE ),
     &                           WET_AERO_DENS( IMODE ),AEROSPC( AH2O_IDX )%DENSITY, 
     &                           VAL )
                  INLET_FRAC( IPMAMS,IMODE,IWET ) = VAL
               END IF
               VAL2 = VAL2 +  SUM( AEROSPC_CONC( :,IMODE ),
     &                 MASK = (.NOT.AEROSPC(:)%TRACER .AND. AEROSPC(:)%OM) ) 
     &                          * INLET_FRAC( IPMAMS,IMODE,IWET ) 
            END DO
            OUTVAL = VAL2
              
         ! Calculate O:C relevant for AMS observations
         CASE ( ID_AMS_OtoC )
            CALL CALC_PMDIAG( ID_AMS_OA, 1, VAL  )

            VAL2 = 0.0
            VAL4 = 0.0
            DO IMODE = 1,N_MODE
               IF ( INLET_FRAC( IPMAMS,IMODE,IWET ) .LT. 0. ) THEN
                  CALL AERO_AMS( WET_AERO_M3( IMODE ),  WET_AERO_M2( IMODE ),
     &                           MOMENT0_CONC( IMODE ), AEROSPC_CONC( AH2O_IDX,IMODE ),
     &                           WET_AERO_DENS( IMODE ),AEROSPC( AH2O_IDX )%DENSITY, 
     &                           FRAC(IMODE) )
                  INLET_FRAC( IPMAMS,IMODE,IWET ) = FRAC(IMODE)
               END IF
 
               DO IOA = 1,N_OA
                  IF ( MAP_OAtoAERO(IOA) .NE. APOC_IDX .AND.
     &                 MAP_OAtoAERO(IOA) .NE. APNCOM_IDX ) THEN
                     ! Weight OA Sum by O:C
                     VAL2 = VAL2 + AEROSPC_CONC( MAP_OAtoAERO(IOA),IMODE )        
     &                         * OASPC(IOA)%OtoC * INLET_FRAC( IPMAMS,IMODE,IWET )
                  END IF
               END DO
               ! Add POC/PNCOM Contribution to Weighted O:C
               VAL3 = ( AEROSPC_CONC(APOC_IDX,IMODE) 
     &                + AEROSPC_CONC(APNCOM_IDX,IMODE) )
     &                     * INLET_FRAC( IPMAMS,IMODE,IWET )
               VAL3 = ( 12./15. * ( VAL3 / ( AEROSPC_CONC(APOC_IDX,IMODE)
     &                                      *INLET_FRAC(IPMAMS,IMODE,IWET) 
     &                           + TINY(0.0) ) ) - 14./15. ) * VAL3
               VAL4 = VAL4 + VAL3
            END DO
            OUTVAL = ( VAL2 + VAL4 ) / ( VAL + TINY(0.0))
 
         ! Retrieve Fine-Mode Particle Mass Collected by the Federal
         ! Reference Method compliant sampler
         CASE ( ID_PMF_FRM )
            K = EXP( 118.87 - 24084/SRFTEMP - 6.025*LOG( SRFTEMP ) )
            P1 = EXP( 8763/SRFTEMP + 19.12*LOG( SRFTEMP ) - 135.94 )
            P2 = EXP( 9969/SRFTEMP + 16.22*LOG( SRFTEMP ) - 122.65 )
            P3 = EXP( 13875/SRFTEMP + 24.46*LOG( SRFTEMP ) - 182.61 )
            a = 1 - AIRRH 
            IF ( AIRRH .LE. 0.61 ) THEN
                K2 = K ** 0.5
            ELSE
                K2 = ( K * ( P1 - P2*a + P3*a*a ) * ( a**1.75 ) ) ** 0.5
            END IF

            CALL CALC_PMDIAG( ID_PMF_NO3, 1, PMF_NO3 )
            PMF_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PMF_NO3 )

            PMF_NH4_LOSS = PMF_NO3_LOSS * 18.0 / 62.0

            CALL CALC_PMDIAG( ID_PMF_NH4, 1, PMF_NH4 )
            CALL CALC_PMDIAG( ID_PMF_SO4, 1, PMF_SO4 ) 
            CALL CALC_PMDIAG( ID_PMF_MASS,1, PMF_MASS) 

            OUTVAL = PMF_MASS - PMF_NO3_LOSS - PMF_NH4_LOSS
     &                      +0.24 * ( PMF_SO4 + PMF_NH4 - PMF_NH4_LOSS ) + 0.5
          
         ! Retrieve PM2.5 Particle Mass Collected by the Federal
         ! Reference Method compliant sampler
         CASE ( ID_PM25_FRM )
            K = EXP( 118.87 - 24084/SRFTEMP - 6.025*LOG( SRFTEMP ) )
            P1 = EXP( 8763/SRFTEMP + 19.12*LOG( SRFTEMP ) - 135.94 )
            P2 = EXP( 9969/SRFTEMP + 16.22*LOG( SRFTEMP ) - 122.65 )
            P3 = EXP( 13875/SRFTEMP + 24.46*LOG( SRFTEMP ) - 182.61 )
            a = 1 - AIRRH 
            IF ( AIRRH .LE. 0.61 ) THEN
                K2 = K ** 0.5
            ELSE
                K2 = ( K * ( P1 - P2*a + P3*a*a ) * ( a**1.75 ) ) ** 0.5
            END IF

            CALL CALC_PMDIAG( ID_PM25_NO3, 1, PM25_NO3 )
            PM25_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PM25_NO3 )

            PM25_NH4_LOSS = PM25_NO3_LOSS * 18.0 / 62.0

            CALL CALC_PMDIAG( ID_PM25_NH4, 1, PM25_NH4 )
            CALL CALC_PMDIAG( ID_PM25_SO4, 1, PM25_SO4 ) 
            CALL SUM_INLET_PM( IPM25, IWET, PM25 )

            OUTVAL = PM25 - PM25_NO3_LOSS - PM25_NH4_LOSS
     &                      +0.24 * ( PM25_SO4 + PM25_NH4 - PM25_NH4_LOSS ) + 0.5
          
         ! Retrieve GAMMA_N2O5IJ - fine N2O5 heterogeneous rxn probability
         CASE ( ID_GN2O5F )
            IF ( AERO_CHEM_SET ) OUTVAL = GAMMA_N2O5IJ( C1,R1,L1 )

         ! Retrieve GAMMA_N2O5K - coarse N2O5 heterogeneous rxn probability
         CASE ( ID_GN2O5C )
            IF ( AERO_CHEM_SET ) OUTVAL = GAMMA_N2O5K( C1,R1,L1 )

         ! Retrieve YCLNO2IJ - fine CLNO2 heterogeneous rxn probability
         CASE ( ID_YCLNO2F )
            IF ( AERO_CHEM_SET ) OUTVAL = YCLNO2IJ( C1,R1,L1 )

         ! Retrieve YCLNO2K - coarse CLNO2 heterogeneous rxn probability
         CASE ( ID_YCLNO2C )
            IF ( AERO_CHEM_SET ) OUTVAL = YCLNO2K( C1,R1,L1 )

         ! Retrieve GAMMA_IEPOX - heterogeneous uptake coefficient
         CASE ( ID_GIEPOX )
            IF ( AERO_CHEM_SET ) OUTVAL = GAMMA_IEPOX( C1,R1,L1 )

         ! Retrieve Particle-Phase Reaction Rate Constant for IEPOX
         CASE ( ID_KIEPOX )
            IF ( AERO_CHEM_SET ) OUTVAL = KPARTIEPOX( C1,R1,L1 )

         ! Retrieve IMAE Heterogeneous Uptake Coefficient
         CASE ( ID_GIMAE )
            IF ( AERO_CHEM_SET ) OUTVAL = GAMMA_IMAE( C1,R1,L1 )

         ! Retrieve AOD at 550 nm
         CASE ( ID_AOD550 )
            IF ( PHOTDIAG ) OUTVAL = TAU_AERO_550( C1,R1 )

         ! Retrieve Aerosol Extinction at 550 nm
         CASE ( ID_PMEXT550 )
            IF ( PHOTDIAG ) OUTVAL = AERO_EXT_550( C1,R1,L1 )

         ! Retrieve Relative Humidity
         CASE ( ID_RH )
            OUTVAL = AIRRH

         ! Retrieve Air Temperature
         CASE ( ID_TEMP )
            OUTVAL = AIRTEMP
         
         ! Retrieve 2-meter Temperature
         CASE ( ID_SRFTEMP )
            OUTVAL = SRFTEMP

         ! Retrieve Air Pressure
         CASE ( ID_PRES )
            OUTVAL = AIRPRES

         CASE DEFAULT
            ! For most parameters the PMDIAG_COEFF structure will
            ! define all of the mappings and coefficients needed for
            ! a particular summation. This can therefore be used in
            ! most cases. Parameters in this category are linear
            ! combinations of CMAQ species. For example, PMF_SO4 = 
            ! ASO4I + ASO4J.
            CALL CALC_PMDIAG_AGG( IDG, OUTVAL )

         END SELECT 
 
         ! Save the fact that this parameter has been calculated
         L_CALC( IUSED ) = .TRUE.
         PMDIAG_LOCAL( IUSED ) = OUTVAL
 

      END SUBROUTINE CALC_PMDIAG
          
!-------------------------------------------------------------------------
      SUBROUTINE LOAD_PMDIAG( C,R,L,CONC,WRITETIME )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the PMDIAG_DATA table.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC,
     &                     AH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS
      USE AEROMET_DATA, ONLY: AIRRH

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: C, R, L
      INTEGER IDG, IM, AL, IL, IUSED, IAVRG, IINST

      REAL, INTENT( IN ) :: CONC( : )

      LOGICAL, INTENT( IN ) :: WRITETIME

      ! Initalize Local Parameters
      PMDIAG_LOCAL( : ) = 0.0
      INLET_FRAC(:,:,:) = -1.
      L_CALC            = .FALSE.
      CONC_VEC( : )     = CONC( : )
      C1                = C
      R1                = R
      L1                = L

      ! Check to make sure the current layer is requested for diagnostic
      ! output.
      IF ( ( L.GE.INST_LAYER_BOT .OR. L.GE.AVRG_LAYER_BOT ) .AND.
     &     ( L.LE.INST_LAYER_TOP .OR. L.LE.AVRG_LAYER_TOP ) ) THEN
       
          ! Calculate All Aerosol Size Distribution Parameters
          CALL CALC_AERODIST_PARAMS
       
          ! Loop Through Used Parameters List and Call PMDIAG Functions 
          ! to fill in Local Array. PMDIAG_LOCAL is the same length as the
          ! PMDIAG_LIST and is ordered by the PMDIAG ID numbers so that it
          ! can be used seamlessly with the CALC_PMDIAG internal order.
          DO IUSED = 1,N_PMDIAG_OUT
             IM  = USED_PMDIAG_MODE( IUSED ) ! Map to 
             IDG = USED_PMDIAG_PARS( IUSED ) ! Map to Parameter ID numbers
       
             CALL CALC_PMDIAG( IDG, IM, PMDIAG_LOCAL( IUSED ) )
          END DO
       
          ! *** Aggregate Diagnostic Parameters in Average Array
          IF ( AVRG_ACTIVE .AND. 
     &         L.GE.AVRG_LAYER_BOT .AND. L.LE.AVRG_LAYER_TOP ) THEN
             AL = L - AVRG_LAYER_BOT + 1
             DO  IAVRG = 1,N_PMDIAG_AVRG_OUT
                IF ( USED_PMDIAG_PARS( MAP_AVRG2USED( IAVRG )) .EQ. ID_PMF_PH
     &               .AND. PMDIAG_LOCAL( MAP_AVRG2USED( IAVRG ) ) .LT. -100. ) THEN
                   ! Special Averageing for Particle pH
                   ! Add whatever the current average is right now.
                   ! Certainly this is problematic, particularly
                   ! at the beginning of an hour where PMDIAG_AVRG =
                   ! 0. It's best to either use the instantaneous
                   ! value or recalculate the average fine particle acidity
                   ! offline.
                   PMDIAG_AVRG( C,R,AL,IAVRG ) = PMDIAG_AVRG( C,R,AL,IAVRG ) 
     &                     + PMDIAG_AVRG( C,R,AL,IAVRG ) / PMDIAG_AVRG_STEP
                ELSE
                   PMDIAG_AVRG( C,R,AL,IAVRG ) = PMDIAG_AVRG( C,R,AL,IAVRG ) 
     &                                         + PMDIAG_LOCAL( MAP_AVRG2USED( IAVRG ) )
                END IF
             END DO
          END IF
       
          ! *** Populate Diagnostic Parameters to Instantaneous Array if this is a write step
          IF ( WRITETIME .AND. INST_ACTIVE .AND.
     &         L.GE.INST_LAYER_BOT .AND. L.LE.INST_LAYER_TOP ) THEN
             IL = L - INST_LAYER_BOT + 1
             DO IINST = 1,N_PMDIAG_INST_OUT
                PMDIAG_INST( C,R,IL,IINST ) = PMDIAG_LOCAL( MAP_INST2USED( IINST ) )
             END DO
          END IF
 
      END IF
      END SUBROUTINE LOAD_PMDIAG

!-------------------------------------------------------------------------
      SUBROUTINE OPEN_PMDIAG ( JDATE, JTIME, TSTEP )
! Revision history
!   ??? Frank Binkowski
!   8 Sep 01 J.Young: dyn alloc - Use HGRD_DEFN
!   03 Sep 01 David Wong: for new pario
!   13 May 04 P.Bhave: added RH to species list; removed M0 and dry M2
!                      conc's from species list
!   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
!                      domain specifications in one module
!   18 Jul 05 P.Bhave: Added mass fractions of each mode < 2.5um to output list
!   06 Apr 06 P.Bhave: Added GAMMA_N2O5 to output list
!   25 May 06 P.Bhave: Changed units of all DG variables from m to um, as 
!                      suggested by Dr. Bill Hutzell
!   11 Apr 08 J.Kelly: Added STDEVCOR and variables to account for dry & wet
!                      DGCOR, M2COR_WET, and M3COR_DRY
!   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
!   26 Sep 14 H. Pye: Added GAMMA_IEPOX and replaced blank units with na
!   11 May 16 D. Wong: - Modified the code to retreive PMDIAG information from
!                        construct PMDIAG_SPC_RECORD stored in module cgrid_spcs
!                        to provide flexibility to handle AE6 and AE61
!                      - renamed the procedure name from OPDIAM to OPPMDIAG
!-------------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

      !...Arguments:

        INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
        INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
        INTEGER, INTENT( IN ) :: TSTEP      ! output time step

      !...Local variables:

        CHARACTER( 16 ), SAVE :: PNAME = 'OPEN_PMDIAG'
        CHARACTER( 96 ) :: XMSG = ' '

        INTEGER :: L          ! loop induction variables
        INTEGER :: MDATE, MTIME

      ! Define General Output File Parameters
      FTYPE3D = GRDDED3
      GDNAM3D = GRID_NAME  ! from HGRD_DEFN
      TSTEP3D = TSTEP
      NCOLS3D = GL_NCOLS
      NROWS3D = GL_NROWS
      GDTYP3D = GDTYP_GD
      P_ALP3D = P_ALP_GD
      P_BET3D = P_BET_GD 
      P_GAM3D = P_GAM_GD
      XORIG3D = XORIG_GD
      YORIG3D = YORIG_GD
      XCENT3D = XCENT_GD
      YCENT3D = YCENT_GD
      XCELL3D = XCELL_GD
      YCELL3D = YCELL_GD
      VGTYP3D = VGTYP_GD
      VGTOP3D = VGTOP_GD
      NTHIK3D =     1

      ! Open Instantaneous PM Diagnostic File
      IF ( INST_ACTIVE ) THEN
        
         ! Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_PMDIAG_1, FSRDWR3, PNAME ) ) THEN

            ! Set output file characteristics based on COORD.EXT and open the aerosol
            ! diagnostic file
            SDATE3D = JDATE
            STIME3D = JTIME
            CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour
         
            NLAYS3D = INST_LAYER_TOP - INST_LAYER_BOT + 1
            DO L = INST_LAYER_BOT, INST_LAYER_TOP + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
         
            NVARS3D = N_PMDIAG_INST_OUT 
            VTYPE3D(1:NVARS3D) = INST_TYPE_OUT( 1:NVARS3D )
            VNAME3D(1:NVARS3D) = INST_PARS_OUT( 1:NVARS3D )
            UNITS3D(1:NVARS3D) = INST_UNIT_OUT( 1:NVARS3D )
            VDESC3D(1:NVARS3D) = INST_DESC_OUT( 1:NVARS3D )
         
            FDESC3D( 1 ) = 'Parameters Relevant for PM Formation and Distribution'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO
         
            ! Open the Instantaneous aerosol diagnostic file
            IF ( .NOT. OPEN3( CTM_PMDIAG_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// CTM_PMDIAG_1 // ' file'
               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF

         END IF
      END IF
 

      ! Open Average Output PM Diagnostic File
      IF ( AVRG_ACTIVE ) THEN
        
         ! Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_APMDIAG_1, FSRDWR3, PNAME ) ) THEN

            ! Get end time information from environment variable AVG_FILE_ENDTIME
            MDATE = JDATE; MTIME = JTIME
            IF ( END_TIME ) CALL NEXTIME ( MDATE, MTIME, TSTEP )

            ! Set output file characteristics based on COORD.EXT and open the aerosol
            ! diagnostic file
            SDATE3D = MDATE
            STIME3D = MTIME
         
            NLAYS3D = AVRG_LAYER_TOP - AVRG_LAYER_BOT + 1
            DO L = AVRG_LAYER_BOT, AVRG_LAYER_TOP + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
         
            NVARS3D = N_PMDIAG_AVRG_OUT 
            VTYPE3D(1:NVARS3D) = AVRG_TYPE_OUT( 1:NVARS3D )
            VNAME3D(1:NVARS3D) = AVRG_PARS_OUT( 1:NVARS3D )
            UNITS3D(1:NVARS3D) = AVRG_UNIT_OUT( 1:NVARS3D )
            VDESC3D(1:NVARS3D) = AVRG_DESC_OUT( 1:NVARS3D )
         
            FDESC3D( 1 ) = 'Parameters Relevant for PM Formation and Distribution'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO
         
            ! Open the Instantaneous aerosol diagnostic file
            IF ( .NOT. OPEN3( CTM_APMDIAG_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// CTM_APMDIAG_1 // ' file'
               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF
 
         END IF
      END IF
 
      END SUBROUTINE OPEN_PMDIAG 

!-------------------------------------------------------------------------
      SUBROUTINE WRITE_PMDIAG ( JDATE, JTIME, TSTEP )
! Revision history
!   20 Feb - B. Murphy: Created
!-------------------------------------------------------------------------

      USE RUNTIME_VARS

      IMPLICIT NONE 

      INCLUDE SUBST_FILES_ID

      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP(3)
      INTEGER MDATE, MTIME
      CHARACTER( 300 ) XMSG

      CHARACTER( 16 ), SAVE :: PNAME = 'WRITE_PMDIAG'
      LOGICAL, SAVE :: FIRSTIME = .TRUE.

C *** If IO Proceesor, then Write Data
      !IF ( IO_PE_INCLUSIVE ) THEN
         MDATE = JDATE
         MTIME = JTIME
         CALL NEXTIME ( MDATE, MTIME, TSTEP( 2 ) )

C *** Write data to the aerosol diagnostic file.
         IF ( INST_ACTIVE ) THEN
            IF ( .NOT. WRITE3( CTM_PMDIAG_1, 
     &           ALLVAR3, MDATE, MTIME,
     &           PMDIAG_INST(:,:,:,:) ) ) THEN
               XMSG = 'Could not write ' // CTM_PMDIAG_1 // ' file'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                     'Timestep written to', CTM_PMDIAG_1,
     &                     'for date and time', MDATE, MTIME

         END IF
     
C *** Write data to the average aerosol diagnostic file.
         IF ( AVRG_ACTIVE ) THEN
            IF ( .NOT. END_TIME ) THEN   ! ending time timestamp
               CALL NEXTIME ( MDATE, MTIME, -TSTEP(1) )
            END IF

            IF ( .NOT. WRITE3( CTM_APMDIAG_1, 
     &           ALLVAR3, MDATE, MTIME, 
     &           PMDIAG_AVRG(:,:,:,:) / 
     &           PMDIAG_AVRG_STEP ) ) THEN
               XMSG = 'Could not write ' // CTM_APMDIAG_1 // ' file'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                     'Timestep written to', CTM_APMDIAG_1,
     &                     'for date and time', MDATE, MTIME
         END IF
     
      !END IF   ! WRITETIME

      RETURN 

      END SUBROUTINE WRITE_PMDIAG

      END MODULE PMDIAG_PROC
