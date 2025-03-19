
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
      MODULE ELMO_PROC
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
      USE ELMO_DATA
      USE GRID_CONF, ONLY: NCOLS, NROWS, NLAYS, IO_PE_INCLUSIVE

      IMPLICIT NONE

      LOGICAL, SAVE :: INST_ACTIVE, AVRG_ACTIVE
      INTEGER, SAVE :: INST_LAYER_TOP, INST_LAYER_BOT,
     &                 AVRG_LAYER_TOP, AVRG_LAYER_BOT

      CHARACTER(16), ALLOCATABLE, SAVE :: 
     &               INST_VARS_NML( : ), AVRG_VARS_NML( : ),
     &               INST_PARS( : ),     AVRG_PARS( : ),
     &               INST_PARS_OUT(:),   AVRG_PARS_OUT(:),
     &               INST_UNIT_OUT(:),   AVRG_UNIT_OUT(:)

      CHARACTER(80), ALLOCATABLE, SAVE :: 
     &               INST_DESC_OUT( : ), AVRG_DESC_OUT( : )

      INTEGER, SAVE :: N_ELMO, N_ELMO_OUT, NTOT_ELMO_IDS, 
     &                 N_ELMO_INST, N_ELMO_AVRG,
     &                 N_ELMO_INST_OUT, N_ELMO_AVRG_OUT

      INTEGER, ALLOCATABLE, SAVE :: 
     &        MAP_INST2USED( : ), MAP_AVRG2USED( : ), 
     &        USED_ELMO_ID( : ), USED_ELMO_MODE( : ),
     &        INST_TYPE_OUT( : ), AVRG_TYPE_OUT( : ),
     &        MAP_ID2USED( :,: ),
     &        USED_CGRID_ID(:), USED_FAM_ID(:)

      CONTAINS

!-------------------------------------------------------------------------
      SUBROUTINE READ_ELMO_NML( )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
         
      USE RUNTIME_VARS, ONLY: MISC_CTRL, LOGDEV
      USE CGRID_SPCS, ONLY: CGRID_NAME, N_CGRID_SPC
      USE UTIL_FAMILY_MODULE, ONLY: N_Chem_Fams,ChemFamilyName,
     &                              Map_Chem_Families
      USE GRID_CONF, ONLY : MYPE
      
      IMPLICIT NONE

      INTEGER, PARAMETER :: N_NML = 1000
      INTEGER            :: INUM, J, N_END, I1, I2, I3, I4, JNUM
      INTEGER            :: FUNIT
      INTEGER            :: STAT
      LOGICAL, SAVE      :: INSTANT  =.TRUE., 
     &                      AVERAGE  =.TRUE.

      CHARACTER( 16 ), SAVE :: PNAME = 'READ_ELMO_NML'
      CHARACTER( 200 )   :: XMSG
      INTEGER IOS
      LOGICAL EXPAND_NML
      LOGICAL, ALLOCATABLE :: FAM_LIST(:)

      NAMELIST / elmo_activate / instant, average 
      NAMELIST / elmo_inst / inst_layer_top, 
     &                       inst_layer_bot, inst_vars_nml
      NAMELIST / elmo_avrg / avrg_layer_top, 
     &                       avrg_layer_bot, avrg_vars_nml

      ALLOCATE( INST_VARS_NML( N_NML ), STAT = IOS )
      CALL CHECKMEM( IOS, 'INST_VARS_NML',PNAME )
      ALLOCATE( AVRG_VARS_NML( N_NML ), STAT = IOS )
      CALL CHECKMEM( IOS, 'AVRG_VARS_NML',PNAME )
      ALLOCATE( FAM_LIST( N_CGRID_SPC ), STAT = IOS )
      CALL CHECKMEM( IOS, 'FAM_LIST',PNAME )
 

      INST_ACTIVE      = .TRUE.
      INST_LAYER_TOP   = 1
      INST_LAYER_BOT   = 1
      INST_VARS_NML(:) = ''
      AVRG_ACTIVE      = .TRUE.
      AVRG_LAYER_TOP   = 1
      AVRG_LAYER_BOT   = 1
      AVRG_VARS_NML(:) = ''

      ! Retrieve the Name of the Emission Control File
      IF ( MISC_CTRL .EQ. "MISC_CTRL_NML" ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A)"),
     &           'You have chosen not to indicate the location of an',
     &           'Emission Control namelist file. Default settings ',
     &           'will be assumed.'
          RETURN
      END IF

      ! Open Emission Control Namelist File
      FUNIT = JUNIT()
      OPEN( FILE = MISC_CTRL, UNIT = FUNIT, STATUS = 'OLD',
     &      POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

      ! Check for Error in File Open Process
      IF ( STAT .NE. 0 ) THEN
          WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &           'emissions control namelist file: ',TRIM( MISC_CTRL )
          CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
      END IF
 
      ! Read Toggles for Turning Instantaneous and Average Files On/Off
      REWIND( FUNIT )
      READ( NML = elmo_activate, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'ELMO_ACTIVATE section of the CMAQ Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF
      INST_ACTIVE = INSTANT
      AVRG_ACTIVE = AVERAGE
      L_ELMO = ( INST_ACTIVE .OR. AVRG_ACTIVE )
#ifdef mpas
      L_ELMO = .false.  
#endif
      
      IF ( .NOT. L_ELMO ) THEN
          DEALLOCATE( FAM_LIST ) 
          RETURN
      END IF

      CALL POPULATE_ELMO_KEY

      ! Read Desired ELMO_INST parameters from input namelist
      REWIND( FUNIT )
      READ( NML = elmo_inst, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'ELMO_INST section of the CMAQ Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF

      ! Expand INST list Using Keywords
      N_END = INDEX1( '', N_NML, INST_VARS_NML ) - 1
      IF ( N_END .LE. 0 ) THEN
          WRITE( XMSG, '(A,A)' ),'ERROR: Undefined variables requested ',
     &           'for Instaneous ELMO output '
          CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
      END IF
      INUM = 1
      DO WHILE ( INUM .LE. N_END )
         CALL UPCASE( INST_VARS_NML( INUM ) )
         EXPAND_NML = .FALSE.
         IF ( INST_VARS_NML( INUM )(1:1) .EQ. '*' ) THEN
             EXPAND_NML = .TRUE.
             INST_VARS_NML( INUM ) = INST_VARS_NML( INUM )(2:16)//' '
         END IF

         I1 = INDEX1( INST_VARS_NML( INUM ), N_ELMO_LIST, ELMO_LIST%NAME )
         I2 = INDEX1( INST_VARS_NML( INUM ), N_KEY, ELMO_KEY%WORD )
         I3 = INDEX1( INST_VARS_NML( INUM ), N_CGRID_SPC, CGRID_NAME )
         I4 = 0
         IF ( N_Chem_Fams .GT. 0 ) 
     &      I4 = INDEX1( INST_VARS_NML( INUM ), N_Chem_Fams, CHEMFAMILYNAME )

         IF ( INST_VARS_NML( INUM ) .EQ. 'ALL' ) THEN
            ! Add All Diagnostic Parameters to the End of the Registry List
            INST_VARS_NML( N_END+1:N_END+N_ELMO_LIST ) = ELMO_LIST%NAME
            N_END = N_END + N_ELMO_LIST
            
            ! Add All Raw Output Species to the End of the Registry List
            INST_VARS_NML( N_END+1:N_END+N_CGRID_SPC ) = CGRID_NAME(1:N_CGRID_SPC)
            N_END = N_END + N_CGRID_SPC

            ! Add All Family Names defined in the Emission Control
            ! Interface to the End of the Registry List
            IF ( N_Chem_Fams .GT. 0 ) 
     &           INST_VARS_NML( N_END+1:N_END+N_Chem_Fams ) = 
     &                        CHEMFAMILYNAME(1:N_Chem_Fams)
            N_END = N_END + N_Chem_Fams

            ! Remove "All" Keyword
            INST_VARS_NML( INUM:N_END-1 ) = INST_VARS_NML( INUM+1:N_END )
            INST_VARS_NML( N_END ) = ''
            N_END = N_END - 1
         ELSE IF ( INST_VARS_NML( INUM ) .EQ. 'ALLCONC' ) THEN
            ! Add All Raw Output Species to the End of the Registry List
            INST_VARS_NML( N_END+1:N_END+N_CGRID_SPC ) = CGRID_NAME(1:N_CGRID_SPC)
            N_END = N_END + N_CGRID_SPC

            ! Remove 'ALLCONC' Keyword from INST Parameter List
            INST_VARS_NML( INUM:N_END-1 ) = INST_VARS_NML( INUM+1:N_END )
            INST_VARS_NML( N_END ) = ''
            N_END = N_END - 1 
         ELSE IF ( I1 .GT. 0 ) THEN
            ! Entry Matches a parameter on the ELMO list. 
            INUM = INUM + 1
         ELSE IF ( I2 .GT. 0 ) THEN
            ! ELMO Keyword. Add parameters associated with a keyword
            INST_VARS_NML( N_END+1:N_END+ELMO_KEY( I2 )%N ) =
     &                                   ELMO_KEY( I2 )%LIST
            N_END = N_END + ELMO_KEY( I2 )%N
            INST_VARS_NML( INUM:N_END-1 ) = INST_VARS_NML( INUM+1:N_END )
            INST_VARS_NML( N_END ) = ''
            N_END = N_END - 1
         ELSE IF ( I3 .GT. 0 ) THEN
            ! CMAQ Raw Model Species. Go to Next Entry
            INUM = INUM + 1
         ELSE IF ( I4 .GT. 0 ) THEN
            ! CMAQ Chemical Family. Go to Next entry or expand if the * was
            ! used
            IF ( EXPAND_NML ) THEN
               CALL MAP_CHEM_FAMILIES( INST_VARS_NML( INUM ),
     &                     CGRID_NAME, N_CGRID_SPC, FAM_LIST )
               DO J = 1,N_CGRID_SPC
                  IF ( FAM_LIST(J) ) THEN
                      N_END = N_END + 1
                      INST_VARS_NML( N_END ) = CGRID_NAME( J )
                  END IF
               END DO   
               ! Remove family Name after expansion
               INST_VARS_NML( INUM:N_END-1 ) = INST_VARS_NML( INUM+1:N_END )
               INST_VARS_NML( N_END ) = ''
               N_END = N_END - 1
            ELSE
               INUM = INUM + 1
            END IF
         ELSE
            ! Entry Doesn't Match any parameter or Keyword. Crash the
            ! model.
            XMSG = 'An Instantaneous ELMO Parameter ' // TRIM( INST_VARS_NML( INUM )) //
     &             ' has been requested that does not exist.'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
      END DO

      ! Capitalize Entries and Remove Duplicates
      DO INUM = 1,N_END
          CALL UPCASE( INST_VARS_NML( INUM ) )
      ENDDO
      DO INUM = 1,N_END-1
         JNUM = INUM + 1
         DO WHILE ( JNUM .LE. N_END )
            IF ( INST_VARS_NML( INUM ) .EQ. '' ) THEN
               JNUM = N_END + 1
            ELSEIF ( INST_VARS_NML( INUM ) .EQ. INST_VARS_NML( JNUM ) ) THEN
               INST_VARS_NML( JNUM:N_END-1 ) = INST_VARS_NML( JNUM+1:N_END )
               INST_VARS_NML( N_END ) = ''
            ELSE
               JNUM = JNUM + 1
            END IF
         END DO
      END DO

      ! Populate Final INST_PARS parameters list
      N_ELMO_INST = INDEX1( '', N_NML, INST_VARS_NML ) - 1
      ALLOCATE( INST_PARS( N_ELMO_INST ), STAT=IOS )
      CALL CHECKMEM( IOS, 'INST_PARS',PNAME )
      INST_PARS = INST_VARS_NML( 1:N_ELMO_INST )

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
      NLAY_ELMO_INST = INST_LAYER_TOP - INST_LAYER_BOT + 1

      ! Read Desired ELMO_AVRG parameters from input namelist
      REWIND( FUNIT )
      READ( NML = elmo_avrg, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'ELMO_AVRG section of the CMAQ Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
      END IF
 
      ! Expand AVRG list Using Keywords
      N_END = INDEX1( '', N_NML, AVRG_VARS_NML ) - 1
      IF ( N_END .LE. 0 ) THEN
          WRITE( XMSG, '(A,A)' ),'ERROR: Undefined variables requested ',
     &           'for Average ELMO output '
          CALL M3EXIT( PNAME, 0, 0, XMSG, 1 )
      END IF
      INUM = 1
      DO WHILE ( INUM .LE. N_END )
         CALL UPCASE( AVRG_VARS_NML( INUM ) )
         EXPAND_NML = .FALSE.
         IF ( AVRG_VARS_NML( INUM )(1:1) .EQ. '*' ) THEN
             EXPAND_NML = .TRUE.
             AVRG_VARS_NML( INUM ) = AVRG_VARS_NML( INUM )(2:16)//' '
         END IF

         I1 = INDEX1( AVRG_VARS_NML( INUM ), N_ELMO_LIST, ELMO_LIST%NAME )
         I2 = INDEX1( AVRG_VARS_NML( INUM ), N_KEY, ELMO_KEY%WORD )
         I3 = INDEX1( AVRG_VARS_NML( INUM ), N_CGRID_SPC, CGRID_NAME )
         I4 = 0
         IF ( N_Chem_Fams .GT. 0 ) 
     &      I4 = INDEX1( AVRG_VARS_NML( INUM ), N_Chem_Fams, CHEMFAMILYNAME )

         IF ( AVRG_VARS_NML( INUM ) .EQ. 'ALL' ) THEN
             ! Add All Parameters to the End of the Registry List
            AVRG_VARS_NML( N_END+1:N_END+N_ELMO_LIST ) = ELMO_LIST%NAME
            N_END = N_END + N_ELMO_LIST

            ! Add All Raw Output Species to the End of the Registry List
            AVRG_VARS_NML( N_END+1:N_END+N_CGRID_SPC ) = CGRID_NAME(1:N_CGRID_SPC)
            N_END = N_END + N_CGRID_SPC

            ! Add All Family Names defined in the Emission Control
            ! Interface to the End of the Registry List
            IF ( N_Chem_Fams .GT. 0 ) 
     &           AVRG_VARS_NML( N_END+1:N_END+N_Chem_Fams ) = 
     &                        CHEMFAMILYNAME(1:N_Chem_Fams)
            N_END = N_END + N_Chem_Fams
 
            ! Remove 'ALL' Keyword from AVRG Parameter List
            AVRG_VARS_NML( INUM:N_END-1 ) = AVRG_VARS_NML( INUM+1:N_END )
            AVRG_VARS_NML( N_END ) = ''
            N_END = N_END - 1
         ELSE IF ( AVRG_VARS_NML( INUM ) .EQ. 'ALLCONC' ) THEN
            ! Add All Raw Output Species to the End of the Registry List
            AVRG_VARS_NML( N_END+1:N_END+N_CGRID_SPC ) = CGRID_NAME(1:N_CGRID_SPC)
            N_END = N_END + N_CGRID_SPC

            ! Remove 'ALLCONC' Keyword from AVRG Parameter List
            AVRG_VARS_NML( INUM:N_END-1 ) = AVRG_VARS_NML( INUM+1:N_END )
            AVRG_VARS_NML( N_END ) = ''
            N_END = N_END - 1
         ELSE IF ( I1 .GT. 0 ) THEN
            ! Entry Matches a parameter on the ELMO list. 
            INUM = INUM + 1
         ELSE IF ( I2 .GT. 0 ) THEN
            ! Add Parameters associated with a keyword
            AVRG_VARS_NML( N_END+1:N_END+ELMO_KEY( I2 )%N ) =
     &                                   ELMO_KEY( I2 )%LIST
            N_END = N_END + ELMO_KEY( I2 )%N
            AVRG_VARS_NML( INUM:N_END-1 ) = AVRG_VARS_NML( INUM+1:N_END )
            AVRG_VARS_NML( N_END ) = ''
            N_END = N_END - 1
         ELSE IF ( I3 .GT. 0 ) THEN
            ! CMAQ Raw Model Species. Go to Next Entry
            INUM = INUM + 1
         ELSE IF ( I4 .GT. 0 ) THEN
            ! CMAQ Chemical Family. Go to Next entry or expand if the * was
            ! used
            IF ( EXPAND_NML ) THEN
               CALL MAP_CHEM_FAMILIES( AVRG_VARS_NML( INUM ),
     &                     CGRID_NAME, N_CGRID_SPC, FAM_LIST )
               DO J = 1,N_CGRID_SPC
                  IF ( FAM_LIST(J) ) THEN
                      N_END = N_END + 1
                      AVRG_VARS_NML( N_END ) = CGRID_NAME( J )
                  END IF
               END DO   
               ! Remove family Name after expansion
               AVRG_VARS_NML( INUM:N_END-1 ) = AVRG_VARS_NML( INUM+1:N_END )
               AVRG_VARS_NML( N_END ) = ''
               N_END = N_END - 1
            ELSE
               INUM = INUM + 1
            END IF
 
         ELSE
            ! Entry Doesn't Match any parameter or Keyword. Crash the
            ! model.
            XMSG = 'An Average ELMO Parameter ' // TRIM( AVRG_VARS_NML( INUM )) //
     &             ' has been requested that does not exist.'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
      END DO

      ! Capitalize Entries and Remove Duplicates
      DO INUM = 1,N_END
          CALL UPCASE( AVRG_VARS_NML( INUM ) )
      ENDDO
      DO INUM = 1,N_END-1
         JNUM = INUM + 1
         DO WHILE ( JNUM .LE. N_END )
            IF ( AVRG_VARS_NML( INUM ) .EQ. '' ) THEN
               JNUM = N_END + 1
            ELSEIF ( AVRG_VARS_NML( INUM ) .EQ. AVRG_VARS_NML( JNUM ) ) THEN
               AVRG_VARS_NML( JNUM:N_END-1 ) = AVRG_VARS_NML( JNUM+1:N_END )
               AVRG_VARS_NML( N_END ) = ''
            ELSE
               JNUM = JNUM + 1
            END IF
         END DO
      END DO
       
      ! Populate INST_PARS parameters list
      N_ELMO_AVRG = INDEX1( '', N_NML, AVRG_VARS_NML ) - 1
      ALLOCATE( AVRG_PARS( N_ELMO_AVRG ), STAT = IOS )
      CALL CHECKMEM( IOS, 'AVRG_PARS',PNAME )
      AVRG_PARS = AVRG_VARS_NML( 1:N_ELMO_AVRG )
 
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
      NLAY_ELMO_AVRG = AVRG_LAYER_TOP - AVRG_LAYER_BOT + 1
 
      DEALLOCATE( FAM_LIST )

      END SUBROUTINE READ_ELMO_NML

!-------------------------------------------------------------------------
      SUBROUTINE POPULATE_ELMO_KEY
!     This subroutine allocates and populates the structure of potential
!       keywords to which users may refer to activate various diagnostic
!       parameters. It also contains the definitions of the ocntents of
!       each keyword.
!-------------------------------------------------------------------------
      USE CGRID_SPCS, ONLY: CGRID_NAME, N_CGRID_SPC
          
      IMPLICIT NONE

      INTEGER IOS, ALLOCSTAT
      CHARACTER(200) :: XMSG
      CHARACTER(16), SAVE :: PNAME = 'ELMO_KEY'

      N_KEY = 25
      ALLOCATE( ELMO_KEY( N_KEY ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY',PNAME )

      ! Define AMET Variables
      ELMO_KEY( 1 )%WORD = 'DEFAULT'
      ELMO_KEY( 1 )%N = 14
      ALLOCATE( ELMO_KEY( 1 )%LIST( 14 ),STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY(1)%LIST',PNAME )

      ELMO_KEY( 1 )%LIST = (/'AMET            ','CUTOFF_FRACTIONS',
     &      'MASS            ','PM_NUM          ','PMF_NUM         ',
     &      'PMC_NUM         ','MET             ','CHEM            ', 
     &      'FINE_SPECIES    ','COARSE_SPECIES  ','PM25_SPECIES    ',
     &      'SIZE            ','AMS             ','PM25TO10_SPECIES' /)

      ELMO_KEY( 2 )%WORD = 'AMET'
      ELMO_KEY( 2 )%N = 35
      ALLOCATE( ELMO_KEY( 2 )%LIST( 35 ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY(2)%LIST',PNAME )

      ELMO_KEY( 2 )%LIST = (/'PMF_MASS    ','PMF_FRM     ','PMC_MASS    ',
     &          'PMF_OC      ','PMF_EC      ','PMF_NA      ','PMF_CL      ',
     &          'PMF_SO4     ','PMF_NO3     ','PMF_NH4     ','PM25        ',
     &          'PM25_FRM    ','PM25_SO4    ','PM25_NO3    ','PM25_NH4    ',
     &          'PM25_OC     ','PM25_EC     ','PMF_FE      ','PMF_AL      ',
     &          'PMF_SI      ','PMF_TI      ','PMF_CA      ','PMF_MG      ',
     &          'PMF_K       ','PMF_MN      ','PMF_SOILIMPV','PMF_UN_IMPV1',
     &          'PMF_NCOM    ','PMF_UN_IMPV2','TNO3        ','PM_MASS     ',
     &          'PMC_MASS    ','MET         ','Tsurf       ','AOD_550     ' /)

      ELMO_KEY( 3 )%WORD = 'MASS'
      ELMO_KEY( 3 )%N = 11
      ALLOCATE( ELMO_KEY( 3 )%LIST( 11 ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY(3)%LIST',PNAME )

      ELMO_KEY( 3 )%LIST = (/'PMF_MASS  ','PMC_MASS  ','PM01      ','PM1       ',
     &            'PM25      ','PM10      ','PM25TO10  ','PMU_MASS  ','PMAMS     ',
     &            'PMAIT_MASS','PMACC_MASS' /)
     
      ELMO_KEY( 4 )%WORD = 'NUMBER'
      ELMO_KEY( 4 )%N = 7
      ALLOCATE( ELMO_KEY( 4 )%LIST( 7 ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY(4)%LIST',PNAME )

      ELMO_KEY( 4 )%LIST = (/'PM_NUM ','PMF_NUM','PMC_NUM','N10    ',
     &                         'N20    ','N40    ','N100   ' /)
     
      ELMO_KEY( 5 )%WORD = 'SIZE'
      ELMO_KEY( 5 )%N = 9
      ALLOCATE( ELMO_KEY( 5 )%LIST( 9 ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_KEY(5)%LIST',PNAME )

      ELMO_KEY( 5 )%LIST = (/'NUMBER  ','DRY_DG  ','WET_DG  ','STDEV   ',
     &              'DRY_M3  ','WET_M3  ','WET_M2  ','DRY_DENS','WET_DENS' /)
     
      ELMO_KEY( 6 )%WORD = 'SURFACE'
      ELMO_KEY( 6 )%N = 5
      ALLOCATE( ELMO_KEY( 6 )%LIST( 5 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(6)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 6 )%LIST = (/'PM_SRF ','PMU_SRF','PMF_SRF','PMC_SRF','WET_M2 '/)
     
      ELMO_KEY( 7 )%WORD = 'ORGANIC'
      ELMO_KEY( 7 )%N = 6
      ALLOCATE( ELMO_KEY( 7 )%LIST( 6 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(7)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 7 )%LIST = (/'FINE_ORG  ','PM1_OC    ','PM1_OA    ',
     &                         'PMAMS_OA  ','PMAMS_OTOC','PM25_OA   '/)

      ELMO_KEY( 8 )%WORD = 'MET'
      ELMO_KEY( 8 )%N = 3
      ALLOCATE( ELMO_KEY( 8 )%LIST( 3 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(8)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 8 )%LIST = (/'TA  ','PRES','RH  ' /) 

      ELMO_KEY( 9 )%WORD = 'CHEM'
      ELMO_KEY( 9 )%N = 7
      ALLOCATE( ELMO_KEY( 9 )%LIST( 7 ),STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(9)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 9 )%LIST = (/'GAMMA_N2O5  ','GAMMA_N2O5K ','YIELD_CLNO2 ',
     &          'YIELD_CLNO2K','GAMMA_IEPOX ','K_IEPOX     ','GAMMA_IMAE  ' /)

      ELMO_KEY( 10 )%WORD = 'FINE_SPECIES'
      ELMO_KEY( 10 )%N = 17
      ALLOCATE( ELMO_KEY( 10 )%LIST( 17 ),STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(10)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 10 )%LIST = (/'PMF_SO4     ','PMF_NO3     ','PMF_NH4     ',
     &           'PMF_CL      ','PMF_NA      ','PMF_EC      ','PMF_OC      ',
     &           'PMF_OA      ','PMF_H2O     ','PMF_SOILIMPV','PMF_UN_IMPV1',
     &           'PMF_UN_IMPV2','PMF_HP      ','PMF_HPMOLAL ','PMF_PH      ',
     &           'FINE_ORG    ','PMF_MASS    ' /)

      ELMO_KEY( 11 )%WORD = 'COARSE_SPECIES'
      ELMO_KEY( 11 )%N = 8
      ALLOCATE( ELMO_KEY( 11 )%LIST( 8 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(11)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 11 )%LIST = (/'PMC_MASS','PMC_SO4 ','PMC_NO3 ','PMC_NH4 ',
     &                          'PMC_NA  ','PMC_MG  ','PMC_K   ','PMC_CA  ' /)

      ELMO_KEY( 12 )%WORD = 'AMS_SPECIES'
      ELMO_KEY( 12 )%N = 7
      ALLOCATE( ELMO_KEY( 12 )%LIST( 7 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(12)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 12 )%LIST = (/'PMAMS     ','PMAMS_SO4 ','PMAMS_NO3 ',
     &             'PMAMS_NH4 ','PMAMS_CL  ','PMAMS_OA  ','PMAMS_OTOC' /)

      ELMO_KEY( 13 )%WORD = 'PM1_SPECIES'
      ELMO_KEY( 13 )%N = 21
      ALLOCATE( ELMO_KEY( 13 )%LIST( 21 ),STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(13)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 13 )%LIST = (/'PM1        ','PM1_SO4    ','PM1_NO3    ',
     &            'PM1_NH4    ','PM1_CL     ','PM1_NA     ','PM1_EC     ',
     &            'PM1_OC     ','PM1_OA     ','PM1_MG     ','PM1_K      ',
     &            'PM1_CA     ','PM1_OTHER  ','PM1_FE     ','PM1_SI     ',
     &            'PM1_TI     ','PM1_MN     ','PM1_AL     ','PM1_SOIL   ',
     &            'PM1_UNSP1  ','PM1_UNSPCRS' /)

      ELMO_KEY( 14 )%WORD = 'PM25_SPECIES'
      ELMO_KEY( 14 )%N = 22
      ALLOCATE( ELMO_KEY( 14 )%LIST( 22 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(14)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 14 )%LIST = (/'PM25        ','PM25_SO4    ','PM25_NO3    ',
     &           'PM25_NH4    ','PM25_CL     ','PM25_NA     ','PM25_EC     ',
     &           'PM25_OC     ','PM25_OA     ','PM25_MG     ','PM25_K      ',
     &           'PM25_CA     ','PM25_OTHER  ','PM25_FE     ','PM25_SI     ',
     &           'PM25_TI     ','PM25_MN     ','PM25_AL     ','PM25_SOIL   ',
     &           'PM25_UNSP1  ','PM25_UNSPCRS','PM25_HP     ' /)

      ELMO_KEY( 15 )%WORD = 'PM25TO10_SPECIES'
      ELMO_KEY( 15 )%N = 6
      ALLOCATE( ELMO_KEY( 15 )%LIST( 6 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(15)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 15 )%LIST = (/'PM25TO10    ','PM25TO10_SO4','PM25TO10_NO3',
     &                          'PM25TO10_NH4','PM25TO10_CL ','PM25TO10_NA ' /)

      ELMO_KEY( 16 )%WORD = 'OPTICAL'
      ELMO_KEY( 16 )%N = 2
      ALLOCATE( ELMO_KEY( 16 )%LIST( 2 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(16)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 16 )%LIST = (/'AOD_550   ','PM_EXT_550'/)

      ELMO_KEY( 17 )%WORD = 'FINE_ORG'
      ELMO_KEY( 17 )%N = 16
      ALLOCATE( ELMO_KEY( 17 )%LIST( 16 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(17)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 17 )%LIST = (/'PMF_POC     ','PMF_SOC     ','PMF_POA     ',
     &           'PMF_SOA     ','PMF_NCOM    ','PMF_OMOC    ','PMF_OTOC    ',
     &           'PMF_ASOA    ','PMF_BSOA    ','PMF_CLDGLY  ','PMF_ISOPSOA ',
     &           'PMF_IEPOXSOA','PMF_MTNSOA  ','PMF_MTSOA   ','PMF_OC      ',
     &           'PMF_OA      ' /)

      ELMO_KEY( 18 )%WORD = 'CUTOFF_FRACTIONS'
      ELMO_KEY( 18 )%N = 6
      ALLOCATE( ELMO_KEY( 18 )%LIST( 6 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(18)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 18 )%LIST = (/'FPM01    ','FPM1     ','FPM25    ',
     &                          'FPM10    ','FPM25TO10','FAMS     ' /)

      ELMO_KEY( 19 )%WORD = 'TOXICS'
      ELMO_KEY( 19 )%N = 21
      ALLOCATE( ELMO_KEY( 19 )%LIST( 21 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(19)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 19 )%LIST = (/'PM25_HDIESEL','PM25_HBE    ','PM25_HCD    ',
     &           'PM25_HCR3   ','PM25_HCR6   ','PM25_HCR    ','PM25_HPB    ',
     &           'PM25_HMN    ','PM25_HNI    ','PM25_HAS    ','PM25_HG     ',
     &           'PM10_HDIESEL','PM10_HBE    ','PM10_HCD    ','PM10_HCR3   ',
     &           'PM10_HCR6   ','PM10_HCR    ','PM10_HPB    ','PM10_HMN    ',
     &           'PM10_HAS    ','PM10_HG     '  /)

      ELMO_KEY( 20 )%WORD = 'SIMPLE'
      ELMO_KEY( 20 )%N = 2
      ALLOCATE( ELMO_KEY( 20 )%LIST( 2 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(20)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 20 )%LIST = (/'PM25','PM10'/)
 
      ELMO_KEY( 21 )%WORD = 'AMS'
      ELMO_KEY( 21 )%N = 5
      ALLOCATE( ELMO_KEY( 21 )%LIST( 5 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(21)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_KEY( 21 )%LIST = (/'PMAMS_CL ','PMAMS_NH4','PMAMS_NO3',
     &                          'PMAMS_OA ','PMAMS_SO4'/)
 
      ! Satellite Retrieval products
      ELMO_KEY( 22 )%WORD = 'SAT'
      ELMO_KEY( 22 )%N = 2
      ALLOCATE( ELMO_KEY( 22 )%LIST( 2 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(22)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      ELMO_KEY( 22 )%LIST = (/'SATMET ','SATCONC'/)
      
      ! Satellite Retrieval Met Products
      ELMO_KEY( 23 )%WORD = 'SATMET'
      ELMO_KEY( 23 )%N = 7
      ALLOCATE( ELMO_KEY( 23 )%LIST( 7 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(23)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      ELMO_KEY( 23 )%LIST = (/'DENS ','DZ   ','ZH   ','CFRAC','PV   ',
     &                        'PRES ','TA   '/)
 
      ! Satellite Retrieval products
      ELMO_KEY( 24 )%WORD = 'SATCONC'
      ELMO_KEY( 24 )%N = 7
      ALLOCATE( ELMO_KEY( 24 )%LIST( 7 ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(24)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      ELMO_KEY( 24 )%LIST = (/'NO2    ','SO2    ','O3     ','CO     ',
     &                        'FORM   ','NH3    ','AOD_550'/)
 
      ! All Concentrations
      ELMO_KEY( 25 )%WORD = 'ALLCONC'
      ELMO_KEY( 25 )%N = N_CGRID_SPC
      ALLOCATE( ELMO_KEY( 25 )%LIST( N_CGRID_SPC ), STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_KEY(24)'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      ELMO_KEY( 25 )%LIST = CGRID_NAME(:)
 
 

      END SUBROUTINE POPULATE_ELMO_KEY


!-------------------------------------------------------------------------
      SUBROUTINE MAP_ELMO
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
      
      USE AERO_DATA, ONLY: N_MODE, AEROMODE
      USE CGRID_SPCS, ONLY : N_CGRID_SPC, CGRID_NAME, 
     &                       CGRID_CONC_UNIT, CGRID_CONC_DESC
      USE UTIL_FAMILY_MODULE, ONLY: N_Chem_Fams,ChemFamilyName
      USE GRID_CONF, ONLY : MYPE

      IMPLICIT NONE

      INTEGER N, M, P, J, IM, ILIST, S, F
      CHARACTER( 16 ), SAVE :: PNAME = 'MAP_ELMO'
      CHARACTER( 300 ) :: XMSG
      INTEGER ALLOCSTAT, IOS

      ! Read User-Input from Namelist input file (Emission Control File
      ! For Now)
      CALL ELMO_INIT_SHARED()
      CALL READ_ELMO_NML()
      IF ( .NOT. L_ELMO ) RETURN

      ! Map user-defined INST_PARS and AVRG_PARS arrays to the ELMO 
      ! parameters available on the ELMO_LIST. 
      N_ELMO = 0
      
      ALLOCATE( USED_ELMO_MODE( N_ELMO_LIST*N_MODE+N_CGRID_SPC+N_Chem_Fams ),
     &          USED_ELMO_ID( N_ELMO_LIST*N_MODE+N_CGRID_SPC+N_Chem_Fams ),
     &          USED_CGRID_ID( N_ELMO_LIST*N_MODE+N_CGRID_SPC+N_Chem_Fams ),
     &          USED_FAM_ID( N_ELMO_LIST*N_MODE+N_CGRID_SPC+N_Chem_Fams ),
     &          MAP_ID2USED( N_ELMO_LIST+N_CGRID_SPC+N_Chem_Fams,N_MODE ),
     &          STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating USED_ELMO_MODE, USED_ELMO_ID, ' // 
     &            'or MAP_ID2USED'
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF
      USED_ELMO_MODE = 0
      USED_ELMO_ID = 0
      USED_CGRID_ID = 0
      USED_FAM_ID = 0
      MAP_ID2USED = 0

      ! Map Parameter ID's to ELMO_List Entries
      MAP_ID2LIST = 0
      DO ILIST = 1,N_ELMO_LIST
          MAP_ID2LIST( ELMO_LIST( ILIST )%ID ) = ILIST
      END DO

      ! Initialize number of IDs as equal to the number of Diagnostic
      ! Parameters
      NTOT_ELMO_IDS = N_ELMO_LIST

      IF ( INST_ACTIVE ) THEN
         ALLOCATE( MAP_INST2USED( N_ELMO_INST*N_MODE+N_CGRID_SPC+N_Chem_Fams ),
     &             INST_PARS_OUT( N_ELMO_INST*N_MODE+N_CGRID_SPC+N_Chem_Fams ) ,
     &             INST_TYPE_OUT( N_ELMO_INST*N_MODE+N_CGRID_SPC+N_Chem_Fams ) ,
     &             INST_UNIT_OUT( N_ELMO_INST*N_MODE+N_CGRID_SPC+N_Chem_Fams ) ,
     &             INST_DESC_OUT( N_ELMO_INST*N_MODE+N_CGRID_SPC+N_Chem_Fams ),
     &             STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating MAP_INST2USED, etc., '
              CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF

         ! Initialize number of Instantaneous Output Variables
         N_ELMO_INST_OUT = 0

         ! Loop over all user-requested Instantaneous parameter and link
         ! each to parameter IDs, Units, descriptions, etc.
         DO J = 1,N_ELMO_INST
             N = INDEX1( INST_PARS( J ), N_ELMO_LIST, ELMO_LIST%NAME )
             S = INDEX1( INST_PARS( J ), N_CGRID_SPC, CGRID_NAME )
             F = 0
             IF ( N_Chem_Fams .GT. 0 ) 
     &          F = INDEX1( INST_PARS( J ), N_Chem_Fams, CHEMFAMILYNAME )

             IF ( N .NE. 0 ) THEN
                ! Add Room for the Other Modes if this Parameter is
                ! Mode-Dependent
                IF ( ELMO_LIST( N )%MODE ) THEN
                   DO IM = 1,N_MODE
                      ! Add Entry for this Parameter
                      N_ELMO = N_ELMO + 1
                      N_ELMO_INST_OUT = N_ELMO_INST_OUT + 1
                      USED_ELMO_ID( N_ELMO ) = ELMO_LIST(N)%ID
                      USED_ELMO_MODE( N_ELMO ) = IM
                      MAP_ID2USED( ELMO_LIST(N)%ID,IM) = N_ELMO
                      MAP_INST2USED( N_ELMO_INST_OUT ) = N_ELMO

                      INST_PARS_OUT( N_ELMO_INST_OUT ) = 
     &                         TRIM(INST_PARS( J )) // aeromode( IM )%suff
                      INST_TYPE_OUT( N_ELMO_INST_OUT ) = ELMO_LIST( N )%VAR_TYPE
                      INST_UNIT_OUT( N_ELMO_INST_OUT ) = ELMO_LIST( N )%UNIT
                      INST_DESC_OUT( N_ELMO_INST_OUT ) = ELMO_LIST( N )%DESC
                   END DO
                ELSE
                   ! Add Entry for this Parameter
                   N_ELMO = N_ELMO + 1
                   N_ELMO_INST_OUT = N_ELMO_INST_OUT + 1
                   USED_ELMO_ID( N_ELMO ) = ELMO_LIST(N)%ID
                   USED_ELMO_MODE( N_ELMO ) = 1
                   MAP_ID2USED( ELMO_LIST(N)%ID,1 ) = N_ELMO
                   MAP_INST2USED( N_ELMO_INST_OUT ) = N_ELMO
                   INST_PARS_OUT( N_ELMO_INST_OUT ) = INST_PARS( J )
                   INST_TYPE_OUT( N_ELMO_INST_OUT ) = ELMO_LIST( N )%VAR_TYPE 
                   INST_UNIT_OUT( N_ELMO_INST_OUT ) = ELMO_LIST( N )%UNIT     
                   INST_DESC_OUT( N_ELMO_INST_OUT ) = ELMO_LIST( N )%DESC     
                END IF
             ELSEIF ( S .GT. 0 ) THEN
                ! Add Entry for CMAQ Model Species
                N_ELMO = N_ELMO + 1
                N_ELMO_INST_OUT = N_ELMO_INST_OUT + 1
                NTOT_ELMO_IDS = NTOT_ELMO_IDS + 1
                USED_ELMO_ID( N_ELMO ) = NTOT_ELMO_IDS
                USED_CGRID_ID( N_ELMO ) = S
                USED_ELMO_MODE( N_ELMO ) = 1
                MAP_ID2USED( NTOT_ELMO_IDS,1 ) = N_ELMO
                MAP_INST2USED( N_ELMO_INST_OUT ) = N_ELMO

                INST_PARS_OUT( N_ELMO_INST_OUT ) = INST_PARS( J )
                INST_TYPE_OUT( N_ELMO_INST_OUT ) = M3REAL 
                INST_UNIT_OUT( N_ELMO_INST_OUT ) = CGRID_CONC_UNIT(S)
                INST_DESC_OUT( N_ELMO_INST_OUT ) = 
     &                         'Instantaneous ' // CGRID_CONC_DESC(S)     

             ELSEIF ( F .GT. 0 ) THEN
                ! Add Entry for Chemical Family
                N_ELMO = N_ELMO + 1
                N_ELMO_INST_OUT = N_ELMO_INST_OUT + 1
                NTOT_ELMO_IDS = NTOT_ELMO_IDS + 1
                USED_ELMO_ID( N_ELMO ) = NTOT_ELMO_IDS
                USED_FAM_ID( N_ELMO ) = F
                USED_ELMO_MODE( N_ELMO ) = 1
                MAP_ID2USED( NTOT_ELMO_IDS,1 ) = N_ELMO
                MAP_INST2USED( N_ELMO_INST_OUT ) = N_ELMO

                INST_PARS_OUT( N_ELMO_INST_OUT ) = INST_PARS( J )
                INST_TYPE_OUT( N_ELMO_INST_OUT ) = M3REAL 
                INST_UNIT_OUT( N_ELMO_INST_OUT ) = 'ppmV for Gas. ug m-3 for PM'     
                INST_DESC_OUT( N_ELMO_INST_OUT ) = 'Chemical Family. See Definition in Control File.' 

             ELSE
                XMSG = 'An Instantaneous ELMO Parameter '//TRIM(INST_PARS(J))//
     &                 ' has been requested that does not exist.'
                CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
         END DO
         ALLOCATE( ELMO_INST( NCOLS,NROWS,NLAY_ELMO_INST,
     &                          N_ELMO_INST_OUT),STAT=ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating ELMO_INST' 
              CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF
         ELMO_INST = 0.0
      END IF
      
      ! Process Average Parameter User Input
      IF ( AVRG_ACTIVE ) THEN
         ALLOCATE( MAP_AVRG2USED( N_ELMO_AVRG*N_MODE ),
     &             AVRG_PARS_OUT( N_ELMO_AVRG*N_MODE ) ,
     &             AVRG_TYPE_OUT( N_ELMO_AVRG*N_MODE ) ,
     &             AVRG_UNIT_OUT( N_ELMO_AVRG*N_MODE ) ,
     &             AVRG_DESC_OUT( N_ELMO_AVRG*N_MODE ),
     &             STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating MAP_AVRG2USED, etc.' 
              CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
         END IF

         ! Initialize number of Average output variables
         N_ELMO_AVRG_OUT = 0

         ! Loop over all user-requested Average parameter and link
         ! each to parameter IDs, Units, descriptions, etc.
         DO J = 1,N_ELMO_AVRG
             N = INDEX1( AVRG_PARS( J ), N_ELMO_LIST, ELMO_LIST%NAME ) 
             S = INDEX1( AVRG_PARS( J ), N_CGRID_SPC, CGRID_NAME )
             F = 0
             IF ( N_Chem_Fams .GT. 0 ) 
     &          F = INDEX1( AVRG_PARS( J ), N_Chem_Fams, CHEMFAMILYNAME )

             ! Check if this parameter is duplicated on the
             ! instantaneous list. Don't need to duplicate calculation
             ! but do need to add it to the output
             M = 0
             IF ( INST_ACTIVE ) 
     &            M = INDEX1( AVRG_PARS( J ), N_ELMO_INST, INST_PARS ) 

             !IF ( MYPE . EQ. 14 ) THEN
          ! print *,'J = ',J,' avrg_pars_j=',avrg_pars(j)
          ! print *,'avrg_pars = ',avrg_pars(:)
          ! print *,'N_Chem_Fams = ',N_Chem_Fams
          ! print *,'chemfamilynames = ',chemfamilyname(:)
          ! print *,N, ' ',S, ' ',F, ' ',M
         !END IF


             IF ( N .NE. 0 ) THEN

                ! Add Room for the Other Modes if this Parameter is
                ! Mode-Dependent
                IF ( ELMO_LIST( N )%MODE ) THEN
                    DO IM = 1,N_MODE
                       N_ELMO_AVRG_OUT = N_ELMO_AVRG_OUT + 1
                       AVRG_PARS_OUT( N_ELMO_AVRG_OUT ) = 
     &                           TRIM(AVRG_PARS( J )) // aeromode( IM )%suff
                       AVRG_TYPE_OUT( N_ELMO_AVRG_OUT ) = ELMO_LIST( N )%VAR_TYPE 
                       AVRG_UNIT_OUT( N_ELMO_AVRG_OUT ) = ELMO_LIST( N )%UNIT     
                       AVRG_DESC_OUT( N_ELMO_AVRG_OUT ) = ELMO_LIST( N )%DESC     
                       IF ( M .EQ. 0 ) THEN
                          N_ELMO = N_ELMO + 1
                          USED_ELMO_MODE( N_ELMO ) = IM
                          USED_ELMO_ID( N_ELMO ) = ELMO_LIST(N)%ID
                          MAP_ID2USED( ELMO_LIST(N)%ID,IM) = N_ELMO
                          MAP_AVRG2USED( N_ELMO_AVRG_OUT ) = N_ELMO
                       ELSE
                          P = INDEX1( AVRG_PARS_OUT( N_ELMO_AVRG_OUT ), 
     &                                N_ELMO_INST_OUT, INST_PARS_OUT ) 
                          MAP_AVRG2USED( N_ELMO_AVRG_OUT ) = MAP_INST2USED( P )
                       END IF
                    END DO
                ELSE
                    ! Add Entry for this Parameter
                    N_ELMO_AVRG_OUT = N_ELMO_AVRG_OUT + 1

                    AVRG_PARS_OUT( N_ELMO_AVRG_OUT ) = AVRG_PARS( J )
                    AVRG_TYPE_OUT( N_ELMO_AVRG_OUT ) = ELMO_LIST( N )%VAR_TYPE 
                    AVRG_UNIT_OUT( N_ELMO_AVRG_OUT ) = ELMO_LIST( N )%UNIT     
                    AVRG_DESC_OUT( N_ELMO_AVRG_OUT ) = ELMO_LIST( N )%DESC     
                    IF ( M .EQ. 0 ) THEN
                       N_ELMO = N_ELMO + 1
                       USED_ELMO_ID( N_ELMO ) = ELMO_LIST(N)%ID
                       USED_ELMO_MODE( N_ELMO ) = 1
                       MAP_ID2USED( ELMO_LIST(N)%ID,1 ) = N_ELMO
                       MAP_AVRG2USED( N_ELMO_AVRG_OUT ) = N_ELMO
                    ELSE
                       P = INDEX1( AVRG_PARS_OUT( N_ELMO_AVRG_OUT ), 
     &                             N_ELMO_INST_OUT, INST_PARS_OUT ) 
                       MAP_AVRG2USED( N_ELMO_AVRG_OUT ) = MAP_INST2USED( P )
                    END IF   
                END IF 
            ELSEIF ( S .GT. 0 ) THEN
                ! Add Entry for CMAQ Model Species
                N_ELMO_AVRG_OUT = N_ELMO_AVRG_OUT + 1
                AVRG_PARS_OUT( N_ELMO_AVRG_OUT ) = AVRG_PARS( J )
                AVRG_TYPE_OUT( N_ELMO_AVRG_OUT ) = M3REAL 
                AVRG_UNIT_OUT( N_ELMO_AVRG_OUT ) = CGRID_CONC_UNIT(S)
                AVRG_DESC_OUT( N_ELMO_AVRG_OUT ) = 
     &                         'Average ' // CGRID_CONC_DESC(S)
                
                IF ( M .EQ. 0 ) THEN
                   N_ELMO = N_ELMO + 1
                   NTOT_ELMO_IDS = NTOT_ELMO_IDS + 1
                   USED_ELMO_ID( N_ELMO ) = NTOT_ELMO_IDS
                   USED_CGRID_ID( N_ELMO ) = S
                   USED_ELMO_MODE( N_ELMO ) = 1
                   MAP_ID2USED( NTOT_ELMO_IDS,1 ) = N_ELMO
                   MAP_AVRG2USED( N_ELMO_AVRG_OUT ) = N_ELMO
                ELSE
                   P = INDEX1( AVRG_PARS_OUT( N_ELMO_AVRG_OUT ), 
     &                         N_ELMO_INST_OUT, INST_PARS_OUT ) 
                   MAP_AVRG2USED( N_ELMO_AVRG_OUT ) = MAP_INST2USED( P )
                END IF   
             ELSEIF ( F .GT. 0 ) THEN
                ! Add Entry for Chemical Family
                N_ELMO_AVRG_OUT = N_ELMO_AVRG_OUT + 1
                AVRG_PARS_OUT( N_ELMO_AVRG_OUT ) = AVRG_PARS( J )
                AVRG_TYPE_OUT( N_ELMO_AVRG_OUT ) = M3REAL 
                AVRG_UNIT_OUT( N_ELMO_AVRG_OUT ) = 'ppmV for Gas. ug m-3 for PM'     
                AVRG_DESC_OUT( N_ELMO_AVRG_OUT ) = 'Chemical Family. See Definition in Control File.' 
                
                IF ( M .EQ. 0 ) THEN
                   N_ELMO = N_ELMO + 1
                   NTOT_ELMO_IDS = NTOT_ELMO_IDS + 1
                   USED_ELMO_ID( N_ELMO ) = NTOT_ELMO_IDS
                   USED_FAM_ID( N_ELMO ) = F
                   USED_ELMO_MODE( N_ELMO ) = 1
                   MAP_ID2USED( NTOT_ELMO_IDS,1 ) = N_ELMO
                   MAP_AVRG2USED( N_ELMO_AVRG_OUT ) = N_ELMO
                ELSE
                   P = INDEX1( AVRG_PARS_OUT( N_ELMO_AVRG_OUT ), 
     &                         N_ELMO_INST_OUT, INST_PARS_OUT ) 
                   MAP_AVRG2USED( N_ELMO_AVRG_OUT ) = MAP_INST2USED( P )
                END IF   
             ELSE
                XMSG = 'An Average ELMO Parameter '//TRIM(AVRG_PARS(J))//
     &                 ' has been requested that does not exist.'
                CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
             END IF
         END DO
         ALLOCATE( ELMO_AVRG( NCOLS,NROWS,NLAY_ELMO_AVRG,
     &                          N_ELMO_AVRG_OUT ), STAT=IOS )
         CALL CHECKMEM( IOS, 'ELMO_AVRG',PNAME )
         ELMO_AVRG = 0.0
      END IF
      
      ! Allocate Local ELMO Array
      ALLOCATE( ELMO_LOCAL( N_ELMO ), STAT=IOS )
      CALL CHECKMEM( IOS, 'ELMO_LOCAL',PNAME )
      
      ALLOCATE( L_CALC( N_ELMO ), STAT=IOS )
      CALL CHECKMEM( IOS, 'L_CALC',PNAME )

      ! Allocate local image of CGRID so it can be used each time step
      ALLOCATE( CONC_VEC( N_CGRID_SPC ), STAT=IOS )
      CALL CHECKMEM( IOS, 'CONC_VEC',PNAME )

      ! Map Diagnostic Variables to Model Species and Coefficients
      CALL MAP_ELMO_COEFFS


      END SUBROUTINE MAP_ELMO

!-------------------------------------------------------------------------
      SUBROUTINE MAP_ELMO_COEFFS
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------
      USE CGRID_SPCS, ONLY : CGRID_NAME
      USE UTIL_FAMILY_MODULE, ONLY : CHEMFAMILYNUM, CHEMFAMILYMEMBERS
      USE AERO_DATA, ONLY : ascat_na_fac, asoil_na_fac, acors_na_fac,
     &                      ascat_mg_fac, asoil_mg_fac, acors_mg_fac,
     &                      ascat_k_fac,  asoil_k_fac,  acors_k_fac,
     &                      ascat_ca_fac, asoil_ca_fac, acors_ca_fac,
     &                                    asoil_fe_fac, acors_fe_fac,
     &                                    asoil_mn_fac, acors_mn_fac

      IMPLICIT NONE

      INTEGER IDG, N_VARS, IELMO, S, F, IV

      ALLOCATE( ELMO_COEFFS( NTOT_ELMO_IDS ) )
      ELMO_COEFFS( : )%L_MAPPED = .FALSE.
      
      ! Save the number of ELMO parameters that should be output. In the
      ! course of mapping various parameters, it may be necessary to add
      ! some used parameters to N_ELMO that are intermediates to the desired
      ! parameters. For example, PMF_MASS must be calculated in order to
      ! then calculate AUNSP1_IMPVIJ
      N_ELMO_OUT = N_ELMO
                
      ! Loop through all available ELMO_LIST Parameters
      IELMO = 0
      DO WHILE ( IELMO .LE. N_ELMO_OUT )
          IELMO = IELMO + 1
          IDG = USED_ELMO_ID( IELMO )
          
          SELECT CASE ( IDG )
             
             !!!! Coarse-Mode Parameters !!!!  

             ! Map PMC_SO4 - Coarse-Mode Sulfate
             CASE ( ID_PMC_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
 
             ! Map PMC_NO3 - Coarse-Mode Nitrate
             CASE ( ID_PMC_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map PMC_NH4 - Coarse-Mode Ammonium
             CASE ( ID_PMC_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map PMC_CL - Coarse-Mode Chlorine
             CASE ( ID_PMC_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             ! Map PMC_NA - Coarse-Mode Sodium
             CASE ( ID_PMC_NA ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', real(ascat_na_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_na_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_na_fac) )
 
             ! Map PMC_MG - Coarse-Mode Magnesium
             CASE ( ID_PMC_MG ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', real(ascat_mg_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_mg_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_mg_fac) )
 
             ! Map PMC_K - Coarse-Mode Potassium
             CASE ( ID_PMC_K ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', real(ascat_k_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_k_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_k_fac) )

             ! Map PMC_CA - Coarse-Mode Calcium
             CASE ( ID_PMC_CA ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'COARSE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', real(ascat_ca_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_ca_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_ca_fac) )

             
             !!!! Fine-Mode Parameters !!!!  

             ! Map PMF_SO4 - Fine-Mode Sulfate
             CASE ( ID_PMF_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
 
             ! Map PMF_NO3 - Fine-Mode Nitrate
             CASE ( ID_PMF_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map PMF_NH4 - Fine-Mode Ammonium
             CASE ( ID_PMF_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS , 'FINE')
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map PMF_CL - Fine-Mode Chloride
             CASE ( ID_PMF_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             ! Map PMF_NA - Fine-Mode Sodium
             CASE ( ID_PMF_NA ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
 
             ! Map PMF_EC - Fine-Mode Elemental Carbon                                   
             CASE ( ID_PMF_EC )                                                         
               N_VARS = 1                                                               
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )                           
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AEC', 1.0 )                  
                                                                                        
             ! Map PMF_FE - Fine-Mode Iron
             CASE ( ID_PMF_FE ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AFE', 1.0 )
 
             ! Map PMF_AL - Fine-Mode Aluminum
             CASE ( ID_PMF_AL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AAL', 1.0 )
 
             ! Map PMF_SI - Fine-Mode Silicon
             CASE ( ID_PMF_SI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASI', 1.0 )
 
             ! Map PMF_TI - Fine-Mode Titanium
             CASE ( ID_PMF_TI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ATI', 1.0 )
 
             ! Map PMF_CA - Fine-Mode Calcium
             CASE ( ID_PMF_CA ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACA', 1.0 )
 
             ! Map PMF_MG - Fine-Mode Magnesium
             CASE ( ID_PMF_MG ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMG', 1.0 )
 
             ! Map PMF_K - Fine-Mode Potassium
             CASE ( ID_PMF_K ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AK', 1.0 )
 
             ! Map PMF_MN - Fine-Mode Manganese
             CASE ( ID_PMF_MN ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMN', 1.0 )
 
             ! Map PMF_H2O - Fine-Mode Particle Water           
             CASE ( ID_PMF_H2O )                               
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AH2O', 1.0/19.0 )
 
             ! Map SOIL_IMPV - IMPROVE reconstruction of Soil PM
             CASE ( ID_SOIL_IMPV )
               N_VARS = 5
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ACCUMULATION' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AAL', 2.20 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASI', 2.49 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACA', 1.63 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'AFE', 2.42 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AERO', 'ATI', 1.94 )
 
             ! Map PMF_UNSP1 - Retrieve Fine-Mode Unspeciated PM based 
             !   on reconstructed SOIL PM from IMPROVE method and including 
             !   NCOM in the unspeciated portion
             CASE ( ID_PMF_UNSP1 )
               N_VARS = 9
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AGG',  'PMF_MASS', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASO4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ANO3', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ANH4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AERO', 'ACL', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'AERO', 'ANA', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 7, 'AERO', 'AEC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 8, 'AGG',  'PMF_OC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 9, 'AGG',  'PMF_SOILIMPV', -1.0 )          
 
             ! Map PMF_UNSP2 - Retrieve Fine-Mode Unspeciated PM based 
             !   on reconstructed SOIL PM from IMPROVE method and EXCLUDING 
             !   NCOM in the unspeciated portion
             CASE ( ID_PMF_UNSP2 )
               N_VARS = 9
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AGG',  'PMF_MASS', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASO4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ANO3', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ANH4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AERO', 'ACL', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'AERO', 'ANA', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 7, 'AERO', 'AEC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 8, 'AGG',  'PMF_OA', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 9, 'AGG',  'PMF_SOILIMPV', -1.0 )          

             ! Map PMF_HP - Fine-Mode Hydronium Ion
             CASE ( ID_PMF_HP ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AH3OP', 1.0/19.0 )
 
             ! Map PMF_NCOM - Fine-Mode Non-Carbon Organic Mass in POA
             CASE ( ID_PMF_NCOM ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AGG', 'PMF_OA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AGG', 'PMF_OC',-1.0 )
 
             ! Map PMF_CLDGLY - Fine-Mode SOA from glyoxal and methylglyoxal
             CASE ( ID_PMF_CLDGLY ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AORGC', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'AGLY', 1.0 )
 
             ! Map PMF_ISOP - Fine-Mode Isoprene SOA excluding IEPOX SOA
             CASE ( ID_PMF_ISOP ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AISO1', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'AISO2', 1.0 )
 
             ! Map PMF_IEPOX - Fine-Mode IEPOX SOA
             CASE ( ID_PMF_IEPOX ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AISO3', 1.0 )
 
             ! Map PMF_MTNSOA - Fine-Mode Monoterpene Nitrate SOA
             CASE ( ID_PMF_MTNSOA ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMTNO3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'AMTHYD', 1.0 )
 
             ! Map PMF_MTSOA - Fine-Mode Monoterpene SOA excluding
             ! Nitrates
             CASE ( ID_PMF_MTSOA ) 
               N_VARS = 6
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMT1', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'AMT2', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'AMT3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'AMT4', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AERO', 'AMT5', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'AERO', 'AMT6', 1.0 )
 
             ! Map PMF_BENAPY - Fine-Mode Benzo-a-Pyrene
             CASE ( ID_PMF_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'FINE' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ABENAPY', 1.0 )
 
             ! Map GAS_BENAPY - Gas-Phase Benzo-a-Pyrene
             CASE ( ID_GAS_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'GAS', 'BENAPY',8701.7 ) ! Scale by 1000*MW_BENAPY/MW_Air
             
             ! Map GAS_NH3 - Gas-Phase Benzo-a-Pyrene
             CASE ( ID_GAS_NH3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'GAS', 'NH3',587.0 ) ! Scale by 1000*MW_BENAPY/MW_Air
 
             ! Map GAS_HNO3 - Gas-Phase Benzo-a-Pyrene
             CASE ( ID_GAS_HNO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'GAS', 'HNO3',2175.6 ) ! Scale by 1000*MW_BENAPY/MW_Air
 
             
             !!!! AMS Parameters !!!!  

             ! Map AMS_SO4 - AMS-Measure Sulfate
             CASE ( ID_AMS_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PMAMS' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
             
             ! Map AMS_NO3 - AMS-Measured Nitrate
             CASE ( ID_AMS_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PMAMS' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map AMS_NH4 - AMS-Measured Ammonium
             CASE ( ID_AMS_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PMAMS' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map AMS_CL - AMS-Measured Chloride
             CASE ( ID_AMS_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PMAMS' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             
             !!!! PM1.0 Parameters !!!!  

             ! Map PM1_SO4 - PM1 Sulfate
             CASE ( ID_PM1_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
 
             ! Map PM1_NO3 - PM1 Nitrate
             CASE ( ID_PM1_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map PM1_NH4 - PM1 Ammonium
             CASE ( ID_PM1_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map PM1_CL - PM1 Chloride
             CASE ( ID_PM1_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             ! Map PM1_NA - PM1 Sodium
             CASE ( ID_PM1_NA ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_na_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_na_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_na_fac) )
 
             ! Map PM1_EC - PM1 Elemental Carbon
             CASE ( ID_PM1_EC ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AEC', 1.0 )
 
             ! Map PM1_MG - PM1 Magnesium
             CASE ( ID_PM1_MG ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMG', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_mg_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_mg_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_mg_fac) )
 
             ! Map PM1_K - PM1 Potassium
             CASE ( ID_PM1_K ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AK', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_k_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_k_fac) )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_k_fac) )
 
             ! Map PM1_CA - PM1 Calcium
             CASE ( ID_PM1_CA ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_ca_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_ca_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_ca_fac ))
 
             ! Map PM1_Other - PM1 Other 
             CASE ( ID_PM1_OT ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AOTHR', 1.0 )
 
             ! Map PM1_FE - PM1 Iron
             CASE ( ID_PM1_FE ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AFE', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_fe_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_fe_fac ))
 
             ! Map PM1_SI - PM1 Silicon
             CASE ( ID_PM1_SI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASI', 1.0 )
 
             ! Map PM1_TI - PM1 Titanium
             CASE ( ID_PM1_TI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ATI', 1.0 )
 
             ! Map PM1_MN - PM1 Manganese
             CASE ( ID_PM1_MN ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMN', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_mn_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_mn_fac ))
 
             ! Map PM1_AL - PM1 Aluminum
             CASE ( ID_PM1_AL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AAL', 1.0 )
  
             ! Map PM1_Soil - PM1.0 Reconstructed Soil for the fine
             ! mode + explicit soil for the coarse mode
             CASE ( ID_PM1_SOIL ) 
               N_VARS = 6
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASOIL', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'AAL', 2.20 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASI', 2.49 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACA', 1.63 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AERO', 'AFE', 2.42 )
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'AERO', 'ATI', 1.94 )
   
             ! Map PM1.0 Unspeciated PM based on reconstructed
             ! SOIL PM from IMPROVE method and including NCOM in the
             ! unspeciated portion
             CASE ( ID_PM1_UNSP1 )
               N_VARS = 9
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AGG',  'PM1', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASO4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ANO3', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ANH4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AERO', 'ACL', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'AERO', 'ANA', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 7, 'AERO', 'AEC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 8, 'AGG',  'PM1_OC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 9, 'AGG',  'PM1_SOIL', -1.0 )          
  
             ! Map PM1_UN - PM1.0 Unspeciated Coarse Mass
             CASE ( ID_PM1_UN ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', 1.0 - real( ascat_na_fac + ascat_mg_fac +  
     &                                                                         ascat_k_fac  + ascat_ca_fac  )) 
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   1.0 - real( asoil_na_fac + asoil_mg_fac +
     &                                                                         asoil_k_fac  + asoil_ca_fac +
     &                                                                         asoil_mn_fac + asoil_fe_fac  ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   1.0 - real( acors_na_fac + acors_mg_fac +
     &                                                                         acors_k_fac  + acors_ca_fac +
     &                                                                         acors_mn_fac + acors_fe_fac  ))
              
             ! Map PM1_HP - PM1.0 Hydronium Ion
             CASE ( ID_PM1_HP ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM1' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AH3OP', 1.0/19.0 ) 
             
             !!!! PM2.5 Parameters !!!!  

             ! Map PM25_SO4 - PM2.5 Sulfate
             CASE ( ID_PM25_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
 
             ! Map PM25_NO3 - PM2.5 Nitrate
             CASE ( ID_PM25_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map PM25_NH4 - PM2.5 Ammonium
             CASE ( ID_PM25_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map PM25_CL - PM2.5 Chloride
             CASE ( ID_PM25_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             ! Map PM25_NA - PM2.5 Sodium
             CASE ( ID_PM25_NA ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_na_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_na_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_na_fac ))
 
             ! Map PM25_EC - PM2.5 Elemental Carbon
             CASE ( ID_PM25_EC ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AEC', 1.0 )
 
             ! Map PM25_MG - PM2.5 Magnesium
             CASE ( ID_PM25_MG ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMG', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_mg_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_mg_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_mg_fac ))
 
             ! Map PM25_K - PM2.5 Potassium
             CASE ( ID_PM25_K ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AK', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_k_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_k_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_k_fac ))
 
             ! Map PM25_CA - PM2.5 Calcium
             CASE ( ID_PM25_CA ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_ca_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_ca_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_ca_fac ))
 
             ! Map PM25_Other - PM25 Other 
             CASE ( ID_PM25_OT ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AOTHR', 1.0 )
 
             ! Map PM25_FE - PM2.5 Iron
             CASE ( ID_PM25_FE ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AFE', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_fe_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_fe_fac ))
 
             ! Map PM25_SI - PM2.5 Silicon
             CASE ( ID_PM25_SI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASI', 1.0 )
 
             ! Map PM25_TI - PM2.5 Titanium
             CASE ( ID_PM25_TI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ATI', 1.0 )
 
             ! Map PM25_MN - PM2.5 Manganese
             CASE ( ID_PM25_MN ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMN', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   real(asoil_mn_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   real(acors_mn_fac ))
 
             ! Map PM25_AL - PM2.5 Aluminum
             CASE ( ID_PM25_AL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AAL', 1.0 )
 
             ! Map PM25_Soil - PM2.5 Reconstructed Soil for the fine
             ! mode + explicit soil for the coarse mode
             CASE ( ID_PM25_SOIL ) 
               N_VARS = 6
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASOIL', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'AAL', 2.20 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASI', 2.49 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACA', 1.63 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AERO', 'AFE', 2.42 )
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'AERO', 'ATI', 1.94 )
   
             ! Map PM2.5 Unspeciated PM based on reconstructed
             ! SOIL PM from IMPROVE method and including NCOM in the
             ! unspeciated portion
             CASE ( ID_PM25_UNSP1 )
               N_VARS = 9
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AGG',  'PM25', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASO4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ANO3', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ANH4', -1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AERO', 'ACL', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'AERO', 'ANA', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 7, 'AERO', 'AEC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 8, 'AGG',  'PM25_OC', -1.0 )          
               CALL SET_ELMO_COEFF_MAP( IDG, 9, 'AGG',  'PM25_SOIL', -1.0 )          
 
             ! Map PM25_UN - PM2.5 Unspeciated Coarse Mass
             CASE ( ID_PM25_UN ) 
               N_VARS = 3
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASEACAT', 1.0 - real( ascat_na_fac + ascat_mg_fac +  
     &                                                                         ascat_k_fac  + ascat_ca_fac  )) 
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASOIL',   1.0 - real( asoil_na_fac + asoil_mg_fac +
     &                                                                         asoil_k_fac  + asoil_ca_fac +
     &                                                                         asoil_mn_fac + asoil_fe_fac  ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ACORS',   1.0 - real( acors_na_fac + acors_mg_fac +
     &                                                                         acors_k_fac  + acors_ca_fac +
     &                                                                         acors_mn_fac + acors_fe_fac  ))
 
             ! Map PM25_HP - PM2.5 Hydronium Ion
             CASE ( ID_PM25_HP ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AH3OP', 1.0/19.0 )
 
             
             !!!! PM2.5 - 10.0 Parameters !!!!  

             ! Map PM25to10_SO4 - PM2.5-10.0 Sulfate
             CASE ( ID_PM25to10_SO4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ASO4', 1.0 )
 
             ! Map PM25to10_NO3 - PM2.5-10.0 Nitrate
             CASE ( ID_PM25to10_NO3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
 
             ! Map PM25to10_NH4 - PM2.5-10.0 Ammonium
             CASE ( ID_PM25to10_NH4 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANH4', 1.0 )
 
             ! Map PM25to10_CL - PM2.5-10.0 Chloride
             CASE ( ID_PM25to10_CL ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACL', 1.0 )
 
             ! Map PM25to10_NA - PM2.5-10.0 Sodium
             CASE ( ID_PM25to10_NA ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25TO10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANA', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ASEACAT', real(ascat_na_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ASOIL',   real(asoil_na_fac ))
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ACORS',   real(acors_na_fac ))
 
             !!!! Toxics !!!!  
 
             ! Map PM25_HDSL - HAP inventory PM2.5 diesel mass
             CASE ( ID_PM25_HDSL ) 
               N_VARS = 6
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ADE_OTHR', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ADE_EC', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ADE_OC', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ADE_SO4', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AERO', 'ADE_NO3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'AERO', 'ADE_CORS', 1.0 )
 
             ! Map PM25_HBE - HAP inventory Beryllium
             CASE ( ID_PM25_HBE ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ABE', 1.0 )
 
             ! Map PM25_HCD - HAP inventory Cadmium
             CASE ( ID_PM25_HCD ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACD', 1.0 )
 
             ! Map PM25_HCR3 - HAP inventory Chromium III
             CASE ( ID_PM25_HCR3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACR_III', 1.0 )
 
             ! Map PM25_HCR6 - HAP inventory Chromium VI
             CASE ( ID_PM25_HCR6 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACR_VI', 1.0 )
 
             ! Map PM25_HCR - HAP inventory Total Chromium
             CASE ( ID_PM25_HCR ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AGG', 'PM25_HCR3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AGG', 'PM25_HCR6', 1.0 )
 
             ! Map PM25_HPB - HAP inventory Lead
             CASE ( ID_PM25_HPB ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'APB', 1.0 )
 
             ! Map PM25_HMN - HAP inventory Manganese
             CASE ( ID_PM25_HMN ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMN_HAPS', 1.0 )
 
             ! Map PM25_HNI - HAP inventory Nickel
             CASE ( ID_PM25_HNI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANI', 1.0 )
 
             ! Map PM25_HAS - HAP inventory Arsenic
             CASE ( ID_PM25_HAS ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AAS', 1.0 )
 
             ! Map PM25_HG - Mercury
             CASE ( ID_PM25_HG ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'APHG', 1.0 )
 
             ! Map PM25_BENAPY - Benzo-a-Pyrene
             CASE ( ID_PM25_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM25' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ABENAPY', 1.0 )
 
             ! Map PM10_HDSL - HAP inventory PM2.5 diesel mass
             CASE ( ID_PM10_HDSL ) 
               N_VARS = 6
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ADE_OTHR', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AERO', 'ADE_EC', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'ADE_OC', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'ADE_SO4', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 5, 'AERO', 'ADE_NO3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 6, 'AERO', 'ADE_CORS', 1.0 )
 
             ! Map PM10_HBE - HAP inventory Beryllium
             CASE ( ID_PM10_HBE ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ABE', 1.0 )
 
             ! Map PM10_HCD - HAP inventory Cadmium
             CASE ( ID_PM10_HCD ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACD', 1.0 )
 
             ! Map PM10_HCR3 - HAP inventory Chromium III
             CASE ( ID_PM10_HCR3 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACR_III', 1.0 )
 
             ! Map PM10_HCR6 - HAP inventory Chromium VI
             CASE ( ID_PM10_HCR6 ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ACR_VI', 1.0 )
 
             ! Map PM10_HCR - HAP inventory Total Chromium
             CASE ( ID_PM10_HCR ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AGG', 'PM10_HCR3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'AGG', 'PM10_HCR6', 1.0 )
 
             ! Map PM10_HPB - HAP inventory Lead
             CASE ( ID_PM10_HPB ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'APB', 1.0 )
 
             ! Map PM10_HMN - HAP inventory Manganese
             CASE ( ID_PM10_HMN ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AMN_HAPS', 1.0 )
 
             ! Map PM10_HNI - HAP inventory Nickel
             CASE ( ID_PM10_HNI ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANI', 1.0 )
 
             ! Map PM10_HAS - HAP inventory Arsenic
             CASE ( ID_PM10_HAS ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'AAS', 1.0 )
 
             ! Map PM10_HG - Mercury
             CASE ( ID_PM10_HG ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'APHG', 1.0 )
 
             ! Map PM10_BENAPY - Benzo-a-Pyrene
             CASE ( ID_PM10_BENAPY ) 
               N_VARS = 1
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'PM10' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ABENAPY', 1.0 )
 
             !!!! Other Parameters !!!!  

             ! Map TNO3 - Total Inorganic Nitrate
             CASE ( ID_TNO3 ) 
               N_VARS = 2
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'GAS', 'HNO3', 2175.6 ) ! Scale by 1000*MW_HNO3/MW_Air
 
             ! Map TNO3TOT - Total Inorganic+Organic Nitrate
             CASE ( ID_TNO3TOT ) 
               N_VARS = 4
               CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL' )
               CALL SET_ELMO_COEFF_MAP( IDG, 1, 'AERO', 'ANO3', 1.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 2, 'GAS', 'HNO3', 2175.6 ) ! Scale by 1000*MW_HNO3/MW_Air
               CALL SET_ELMO_COEFF_MAP( IDG, 3, 'AERO', 'AISOPNN', 2.0*62.0/226.0 )
               CALL SET_ELMO_COEFF_MAP( IDG, 4, 'AERO', 'AMTNO3', 62.0/231.0 )
 
             ! Otherwise
             CASE DEFAULT
               ! If the IDG is less than N_ELMO_LIST, then do nothing
               ! because it is a parameter that would have been defined
               ! in one of the previous cases. If IDG is larger than
               ! N_ELMO_LIST, then look through CMAQ model species and
               ! Chemical Families to determine how to map it
               IF ( IDG .GT. N_ELMO_LIST ) THEN
                  S = USED_CGRID_ID( IELMO )
                  IF ( S .GT. 0 ) THEN
                     ! CMAQ Species
                     N_VARS = 1
                     CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL' )
                     CALL SET_ELMO_COEFF_MAP( IDG, 1,'SPC', CGRID_NAME(S), 1.0 )
                  END IF

                  F = USED_FAM_ID( IELMO)
                  IF ( F .GT. 0 ) THEN
                     ! User-Defined Family
                     N_VARS = CHEMFAMILYNUM( F )
                     CALL INIT_ELMO_COEFFS( IDG, N_VARS, 'ALL' )
                     DO IV = 1,N_VARS
                       CALL SET_ELMO_COEFF_MAP( IDG, IV, 'SPC',
     &                          CHEMFAMILYMEMBERS(F,IV), 1.0 )
                     END DO
                  END IF
               END IF
         END SELECT

      END DO
      END SUBROUTINE MAP_ELMO_COEFFS
 
!-------------------------------------------------------------------------
      SUBROUTINE INIT_ELMO_COEFFS( IDG, N_VARS, AEROTYPE_TMP )
!     This subroutine allocates and initalizes all of the elements of an 
!        index IDG within the ELMO_COEFFS structure
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
      CHARACTER(16), SAVE :: PNAME = 'INIT_ELMO_COEF'
      INTEGER ALLOCSTAT

      ! Initialize All Components of ELMO_COEFFS
      ELMO_COEFFS( IDG )%L_MAPPED = .TRUE.
      ELMO_COEFFS( IDG )%N_VARS = N_VARS
      ALLOCATE( ELMO_COEFFS( IDG )%L_MODE( N_MODE ),
     &          ELMO_COEFFS( IDG )%L_GAS ( N_VARS ),
     &          ELMO_COEFFS( IDG )%L_SPC ( N_VARS ),
     &          ELMO_COEFFS( IDG )%L_AGG ( N_VARS ),
     &          ELMO_COEFFS( IDG )%I_SPEC( N_VARS ),
     &          ELMO_COEFFS( IDG )%COEFF ( N_VARS ),
     &          STAT=ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
           XMSG = 'Failure allocating ELMO_COEFFS' 
           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      ELMO_COEFFS( IDG )%L_GAS  ( : )   = .FALSE.
      ELMO_COEFFS( IDG )%L_SPC  ( : )   = .FALSE.
      ELMO_COEFFS( IDG )%L_AGG  ( : )   = .FALSE.
      ELMO_COEFFS( IDG )%I_SPEC ( : )   = 0
      ELMO_COEFFS( IDG )%L_MODE ( : )   = .FALSE.
      ELMO_COEFFS( IDG )%COEFF  ( : )   = 0.
      ELMO_COEFFS( IDG )%INLET          = 0
      ELMO_COEFFS( IDG )%WETNESS        = IWET

      ! Specify the Aerosol Modes Needed and Inlet Type based on User
      ! Input through the AEROTYPE variable.
      AEROTYPE = AEROTYPE_TMP
      CALL UPCASE( AEROTYPE )
      SELECT CASE ( AEROTYPE )
         CASE ( 'ALL' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
         CASE ('NONE' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .FALSE.
         CASE ('ULTRAFINE' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%ULTRAFINE_MASK
         CASE ('FINE' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%FINE_MASK
         CASE ('COARSE' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%COARSE_MASK
         CASE ('NUCLEATION' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%NUC_MASK
         CASE ('AITKEN' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%AITKEN_MASK
         CASE ('ACCUMULATION' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = AEROMODE(:)%ACCUM_MASK
         CASE ('PM01' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPM01
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM1' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPM1
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM25' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPM25
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM10' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPM10
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('PM25TO10' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPM25to10
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE ('PMAMS' )
             ELMO_COEFFS( IDG )%L_MODE( : ) = .TRUE.
             ELMO_COEFFS( IDG )%INLET = IPMAMS
             ELMO_COEFFS( IDG )%WETNESS = IWET
         CASE DEFAULT
             XMSG = 'An aerosol descriptor (' // trim(AEROTYPE) // ') has been given ' //
     &              'for the mapping of a ELMO parameter. It is not recognized.'
             CALL M3EXIT( 'INIT_ELMO_COEFFS', 0, 0, XMSG, XSTAT1 )
      END SELECT


      END SUBROUTINE INIT_ELMO_COEFFS

!-------------------------------------------------------------------------
      SUBROUTINE SET_ELMO_COEFF_MAP( IDG, IVAR, TSPEC_TMP, CSPEC, COEFF )
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
      IF ( TSPEC .EQ. 'GAS' ) THEN 
          ! Find the location of CSPEC in CGRID
          ISPEC = INDEX1( CSPEC, N_CGRID_SPC, CGRID_NAME(:) )
 
          IF ( ISPEC .GT. 0 ) THEN
             ! Found the Index. Save it!
             ELMO_COEFFS( IDG )%I_SPEC( IVAR ) = ISPEC
             ELMO_COEFFS( IDG )%L_GAS( IVAR )  = .TRUE.
          ELSE
             ! Could not find the species in the CGRID Array. Print an
             ! Error and Warn
             XMSG = "A PM Diagnostic variable has been requested that relies " //
     &           "on gas species " // TRIM(CSPEC) // ". This species has not been " //
     &           "incorporated in the GC namelist though. Please check that your "//
     &           "GC_namelist is compatible with the PM diagnostics you have called "//
     &           "for in the Emmissions Control Namelist."
             !CALL LOG_MESSAGE( LOGDEV, XMSG )
             !CALL M3WARN( 'MAP_ELMO_COEFFS', 0, 0, '' )
             ELMO_COEFFS( IDG )%L_GAS( IVAR )  = .FALSE.
          END IF
      
      ELSE IF ( TSPEC .EQ. 'AGG' ) THEN 
          ! Map to Index on ELMO_LIST, not the ID column
          ELMO_COEFFS( IDG )%L_AGG( IVAR ) = .TRUE.
          ISPEC = INDEX1( CSPEC, N_ELMO_LIST, ELMO_LIST%NAME )
      
          IF ( ISPEC .GT. 0 ) THEN
             ! Found the Index. Save the ID for this entry on the ELMO_LIST
             ELMO_COEFFS( IDG )%I_SPEC( IVAR ) = ELMO_LIST( ISPEC )%ID
             
             ! Check if it is used and add it to the USED_ELMO_ID
             ! vector if not
             USED = .FALSE.
             DO JSPEC = 1,N_ELMO
                IF ( ELMO_LIST( ISPEC )%ID .EQ. USED_ELMO_ID( JSPEC ) ) USED = .TRUE.
             END DO
             IF ( .NOT. USED ) THEN
                 N_ELMO = N_ELMO + 1
                 USED_ELMO_ID( N_ELMO ) = ELMO_LIST( ISPEC )%ID
             END IF

          ELSE
             ! Could not find the species in the ELMO List. Print an
             ! Error and Crash
             XMSG = "ERROR: A PM Diagnostic variable has been requested that relies " //
     &           "on the diagnostic aggregate variable " // TRIM(CSPEC) // ". This variable "//
     &           "has not been incorporated in the ELMO List though. Please check that the "//
     &           "table in ELMO_DATA is compatible with the PM diagnostics you have defined "//
     &           "in ELMO_PROC."
             CALL LOG_MESSAGE( LOGDEV, XMSG )
             CALL M3EXIT( 'MAP_ELMO_COEFFS', 0, 0, '', XSTAT1 )
          END IF          
  
      ELSE IF ( TSPEC .EQ. 'SPC' ) THEN 
          ! Map to a CGRID Species. No Special Instructions
          ELMO_COEFFS( IDG )%L_SPC( IVAR ) = .TRUE.
          ISPEC = INDEX1( CSPEC, N_CGRID_SPC, CGRID_NAME )
          ELMO_COEFFS( IDG )%I_SPEC( IVAR ) = ISPEC

      ELSE
          ! Aerosol
          ISPEC = INDEX1( CSPEC, N_AEROSPC, AEROSPC(:)%BULKNAME )
      
          IF ( ISPEC .GT. 0 ) THEN
             ! Found the Index. Save it!
             ELMO_COEFFS( IDG )%I_SPEC( IVAR ) = ISPEC
          ELSE
             ! Could not find the species in the Aerosol Array. Print an
             ! Error and Warn
             XMSG = "A PM Diagnostic variable has been requested that relies " //
     &           "on the aerosol species "//TRIM(CSPEC)//". This species has not been " //
     &           "incorporated in the AE namelist though. Please check that your "//
     &           "AE_namelist is compatible with the PM diagnostics you have called "//
     &           "for in the Emmissions Control Namelist."
             !CALL LOG_MESSAGE( LOGDEV, XMSG )
             !CALL M3WARN( 'MAP_ELMO_COEFFS', 0, 0, '' )
          END IF
      END IF
      ELMO_COEFFS( IDG )%COEFF( IVAR ) = COEFF

      END SUBROUTINE SET_ELMO_COEFF_MAP

!-------------------------------------------------------------------------
      SUBROUTINE CALC_CONCOUT_AGG( IDG, VAL )
!     This subroutine sums aggregate PM diagnostic parameters using the
!         ELMO_COEFFS structure as a guide for summing aerosol
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
      LOGICAL L_CALC( N_ELMO )
      REAl    COEFF
      INTEGER IM, WETNESS, ISPEC, INLET
      LOGICAL LMODE
      REAL    FRAC( N_MODE )

      CHARACTER( 500 ) :: XMSG

      ! If this Aggregate has not been mapped already, then return an
      ! error and crash the model
      IF ( .NOT. ELMO_COEFFS( IDG )%L_MAPPED ) THEN
         ILIST = MAP_ID2LIST( IDG )  
         XMSG = 'ELMO Parameter ' // TRIM( ELMO_LIST( ILIST )%NAME ) //
     &          ' has not been mapped to a procedure within ELMO_PROC even ' //
     &          'though it does appear to be an acceptable parameter on the ' //
     &          'ELMO_LIST. If you have added this parameter yourself, make ' //
     &          'sure it is being mapped to an actual calculation in CALC_ELMO ' //
     &          'or MAP_ELMO_COEFFS. If this parameter is needed for a different ' //
     &          'aggregate parameter you requested, please add this parameter ' //
     &          'explicitly as an ELMO requested output.'
         CALL LOG_MESSAGE( LOGDEV, XMSG )
         CALL M3EXIT( 'CALC_CONCOUT_AGG', 0, 0, '', XSTAT1 )
      END IF

      ! Initialize value of aggregate parameter
      VAL = 0.0
      
      ! Retrieve size cutoff if necessary
      IF ( ELMO_COEFFS( IDG )%INLET .EQ. IPMAMS ) THEN
         ! Inlet is for AMS Colection
         DO IM = 1,N_MODE
           IF ( INLET_FRAC( IPMAMS,IM,IWET ) .LT. 0. ) 
     &        CALL AERO_AMS( WET_AERO_M3( IM ),  WET_AERO_M2( IM ),
     &                       MOMENT0_CONC( IM ), AEROSPC_CONC( AH2O_IDX,IM ),
     &                       WET_AERO_DENS( IM ),AEROSPC( AH2O_IDX )%DENSITY, 
     &                       INLET_FRAC( IPMAMS,IM,IWET ) )
           FRAC( IM ) = INLET_FRAC( IPMAMS,IM,IWET )
         END DO
      ELSE IF ( ELMO_COEFFS(IDG)%INLET .GT. 0 .AND.
     &          ELMO_COEFFS(IDG)%INLET .LE. N_INLET ) THEN
         ! Inlet is for a filter with a specific size-range. Call the
         ! AERO_INLET wrapper, GET_AERO_INLET.
         INLET = ELMO_COEFFS( IDG )%INLET
         WETNESS = ELMO_COEFFS( IDG )%WETNESS
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
      DO IVAR = 1,ELMO_COEFFS( IDG )%N_VARS
          ISPEC = ELMO_COEFFS( IDG )%I_SPEC( IVAR )
          COEFF = ELMO_COEFFS( IDG )%COEFF( IVAR )

          IF ( ISPEC .GT. 0 ) THEN
             ! First check if this is a Gas from CGRID. All species
             ! labeled as Gas are assumed to be converted to mass units
             ! using the air density AIRDENS and MW conversions lumped
             ! into COEFF.
             IF  ( ELMO_COEFFS( IDG )%L_GAS( IVAR ) ) THEN
                 VAL = VAL + CONC_VEC( ISPEC ) * COEFF * AIRDENS

             ! Next check for aggregate variable
             ELSE IF  ( ELMO_COEFFS( IDG )%L_AGG( IVAR ) ) THEN
                 CALL CALC_ELMO( ISPEC, 1, VAL2 )
                 VAL = VAL + VAL2 * COEFF

             ! Next check if this is a Species from CGRID that should be
             ! added as-is
             ELSE IF  ( ELMO_COEFFS( IDG )%L_SPC( IVAR ) ) THEN
                 VAL = VAL + CONC_VEC( ISPEC ) * COEFF

             ! Add Aerosol Components
             ELSE 

                DO IM = 1,N_MODE
                   ! If this mode is supposed to be added (LMODE) and the
                   ! species is not missing, then add it up
                   LMODE = ELMO_COEFFS( IDG )%L_MODE( IM )
                   IF ( LMODE .AND. ( .NOT. AERO_MISSING( ISPEC,IM ) ) ) 
     &                 VAL = VAL + AEROSPC_CONC( ISPEC, IM ) * COEFF * FRAC( IM )

                END DO
             END IF
          END IF

      END DO 

      END SUBROUTINE CALC_CONCOUT_AGG
 
!-------------------------------------------------------------------------
      RECURSIVE SUBROUTINE CALC_ELMO( IDG, IM, OUTVAL )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC,
     &                     AH2O_IDX, AORGH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS,
     &                     AEROMODE, DRY_AERO_DENS, APOC_IDX, APNCOM_IDX
      USE AEROMET_DATA, ONLY: AIRRH, PI, AIRTEMP, SRFTEMP, AIRPRES, DZ, CFRAC, PV, ZH
      USE CGRID_SPCS, ONLY : RHOJ_LOC
      USE SOA_DEFN, ONLY : N_OA, N_OA_NotTracers, OASPC, MAP_OAtoAERO
      USE RUNTIME_VARS, ONLY : PHOTDIAG
      USE ASX_DATA_MOD, ONLY : MET_DATA

      IMPLICIT NONE

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
      ! Parameters list, which is how ELMO_LOCAL is defined.
      IUSED = MAP_ID2USED( IDG, IM )

      ! First determine if this parameter has been mapped already. If it
      ! has, send it back
      IF ( L_CALC( IUSED ) ) THEN
          OUTVAL = ELMO_LOCAL( IUSED )
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
               OUTVAL = OUTVAL +
     &            SUM( AEROSPC_CONC( :,IMODE ),
     &                 MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &             - AEROSPC_CONC( AH2O_IDX,IMODE )
               IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &             - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3 
            END DO

         ! Retrieve Ultrafine Particle Mass
         CASE ( ID_PMU_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%ULTRAFINE_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Fine Particle Mass
         CASE ( ID_PMF_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%FINE_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER )
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Coarse Particle Mass
         CASE ( ID_PMC_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%COARSE_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Nucleation-Mode Particle Mass
         CASE ( ID_PMNUC_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%NUC_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Aitken-Mode Particle Mass
         CASE ( ID_PMAIT_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%AITKEN_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
            END DO

         ! Retrieve Accumution-Mode Particle Mass
         CASE ( ID_PMACC_MASS )
            DO IMODE = 1,N_MODE
               IF ( AEROMODE( IMODE )%ACCUM_MASK ) THEN
                    OUTVAL = OUTVAL +
     &                 SUM( AEROSPC_CONC( :,IMODE ),
     &                      MASK=.NOT.AEROSPC(:)%TRACER ) ! ug m-3
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE )
                    IF ( AORGH2O_IDX .GT. 0 ) OUTVAL = OUTVAL
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
               END IF
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
     &                  - AEROSPC_CONC( AH2O_IDX,IMODE ) )
     &                  * INLET_FRAC( IPMAMS,IMODE,IWET ) 
               IF ( AORGH2O_IDX .GT. 0 ) PMAMS = PMAMS
     &                  - AEROSPC_CONC( AORGH2O_IDX,IMODE ) ! ug m-3
     &                  * INLET_FRAC( IPMAMS,IMODE,IWET ) 
            END DO
            OUTVAL = PMAMS
         
         ! Calculate AHPMOLAL - Fine Mode Concentration of H+ in
         ! Particle Water
         CASE ( ID_PMF_HPM )
            ! Retrieve AHPLUSIJ
            CALL CALC_ELMO( ID_PMF_HP, 1, VAL )
            ! Retrieve AH2OIJ
            CALL CALC_ELMO( ID_PMF_H2O, 1, VAL2 )
            ! HPMOLAL = AHPLUSIJ / AH2OIJ * 1000.0 [mol L-1]
            OUTVAL = VAL / VAL2 * 1000.0

         ! Calculate PMF_PH - Fine-Mode pH
         CASE ( ID_PMF_PH )
            ! Retrieve HPMOLAL
            CALL CALC_ELMO( ID_PMF_HPM, 1, VAL )
            ! Retrieve AH2OIJ
            CALL CALC_ELMO( ID_PMF_H2O, 1, VAL2 )

            !PMF_PH = -1*LOG10(HPMOLAL)
#ifdef mpas
            OUTVAL = -9.999E36
#else
            OUTVAL = BADVAL3
#endif
            IF ( VAL2 .GT. 0.01 )
     &           OUTVAL = -1.0 * LOG10( VAL )

         ! Calculate POCIJ - Fine-Mode Primary Organic Carbon Mass
         CASE ( ID_PMF_POC )
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
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
            DO IOA = 1,N_OA_NotTracers
                ! Skip primary species and those with negative OM:OC 
                IF ( OASPC( IOA )%OMtoOC .GT. 0.0 .AND. .NOT. OASPC( IOA )%PRIMARY ) THEN  
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
            DO IOA = 1,N_OA_NotTracers
               ! Skip species with negative OM:OC
               IF ( OASPC( IOA )%OMtoOC .GT. 0.0 ) THEN
                  VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                        MASK = AEROMODE(:)%FINE_MASK ) 
     &                      / OASPC( IOA )%OMtoOC
               END IF
            END DO
            OUTVAL = VAL  

         ! Calculate PMF_POA - Fine-Mode Primary Organic Aerosol Mass
         CASE ( ID_PMF_POA )
            ! Sum up primary organic aerosol species 
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
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
            DO IOA = 1,N_OA_NotTracers
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
            DO IOA = 1,N_OA_NotTracers
                VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ),
     &                      MASK = AEROMODE(:)%FINE_MASK ) 
            END DO
            OUTVAL = VAL    
         
         ! Calculate PMF_ASOA - Fine-Mode Anthropogenic OA Mass
         CASE ( ID_PMF_ASOA )
            ! Sum up anthropogenic oa species 
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
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
            DO IOA = 1,N_OA_NotTracers 
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
            CALL CALC_ELMO( ID_PMF_OA, 1, VAL  )
            CALL CALC_ELMO( ID_PMF_OC, 1, VAL2 )
            OUTVAL = VAL / ( VAL2 + TINY( 0.0 ) )
            
         ! Calculate PMF_OtoC - Fine-Mode O:C 
         CASE ( ID_PMF_OtoC )
            CALL CALC_ELMO( ID_PMF_OA, 1, VAL  )

            VAL2 = 0.0
            DO IOA = 1,N_OA_NotTracers
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
                CALL GET_AERO_INLET( IPM1, IMODE, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
               IF ( OASPC( IOA )%OMtoOC .GE. 0.0 ) 
     &              VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:))
     &                         / OASPC( IOA )%OMtoOC
            END DO
            OUTVAL = VAL  
 
         ! Calculate PM1_OM - PM1.0 Organic Aerosol Mass
         CASE ( ID_PM1_OA )
            ! Save PM1 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM1, IMODE, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
               VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:))
            END DO
            OUTVAL = VAL  
 
         ! Calculate PM25_OC - PM2.5 Organic Carbon Mass
         CASE ( ID_PM25_OC )
            ! Save PM2.5 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM25, IMODE, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species normalized to
            ! just carbon mass
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
               IF ( OASPC( IOA )%OMtoOC .GE. 0.0 ) 
     &              VAL = VAL + SUM( AEROSPC_CONC( MAP_OAtoAERO(IOA),: ) * FRAC(:) )
     &                        / OASPC( IOA )%OMtoOC
            END DO
            OUTVAL = VAL  
 
         ! Calculate PM25_OM - PM2.5 Organic Aerosol Mass
         CASE ( ID_PM25_OA )
            ! Save PM2.5 Fraction for each mode
            DO IMODE = 1,N_MODE
                CALL GET_AERO_INLET( IPM25, IMODE, IWET, FRAC( IMODE ) ) 
            END DO
            ! Sum up primary organic aerosol species
            VAL = 0.0
            DO IOA = 1,N_OA_NotTracers
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
            CALL CALC_ELMO( ID_AMS_OA, 1, VAL  )

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
 
               DO IOA = 1,N_OA_NotTracers
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
 
         ! Calculate Aerosol Fraction of Benzo-a-Pyrene
         CASE ( ID_BENAPY_FAERO )
            CALL CALC_ELMO( ID_PMF_BENAPY, 1, VAL  )
            CALL CALC_ELMO( ID_GAS_BENAPY, 1, VAL2  )
            OUTVAL = VAL / ( VAL + VAL2 + TINY( 0.0 ) )

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

            CALL CALC_ELMO( ID_PMF_NO3, 1, PMF_NO3 )
            PMF_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PMF_NO3 )

            PMF_NH4_LOSS = PMF_NO3_LOSS * 18.0 / 62.0

            CALL CALC_ELMO( ID_PMF_NH4, 1, PMF_NH4 )
            CALL CALC_ELMO( ID_PMF_SO4, 1, PMF_SO4 ) 
            CALL CALC_ELMO( ID_PMF_MASS,1, PMF_MASS) 

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

            CALL CALC_ELMO( ID_PM25_NO3, 1, PM25_NO3 )
            PM25_NO3_LOSS = MIN( 745.7/SRFTEMP * K2, PM25_NO3 )

            PM25_NH4_LOSS = PM25_NO3_LOSS * 18.0 / 62.0

            CALL CALC_ELMO( ID_PM25_NH4, 1, PM25_NH4 )
            CALL CALC_ELMO( ID_PM25_SO4, 1, PM25_SO4 ) 
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
             IF ( L1 .EQ. 1 ) THEN
                 OUTVAL = ELMO_AOD_550( C1,R1 )
             ELSE
#ifdef mpas
            OUTVAL = -9.000E36
#else
            OUTVAL = AMISS3
#endif
             END IF

         ! Retrieve Aerosol Extinction at 550 nm
         CASE ( ID_PMEXT550 )
            OUTVAL = ELMO_EXT_550( C1,R1,L1 )

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

         ! Retrieve DZ
         CASE ( ID_DZ )
            OUTVAL = DZ

         ! Retrieve ZH
         CASE ( ID_ZH )
            OUTVAL = ZH

         ! Retrieve Cloud Fraction
         CASE ( ID_CFRAC )
            OUTVAL = CFRAC

         ! Retrieve Potential Vorticity
         CASE ( ID_PV )
            OUTVAL = PV

         ! Retrieve Air Density x Jacobian/MSFX2
         CASE ( ID_RHOJ )
            OUTVAL = CONC_VEC( RHOJ_LOC ) ! kg m-3

         ! Retrieve Air Density
         CASE ( ID_DENS )
            OUTVAL = MET_DATA%DENS( C1,R1,L1 ) ! kg m-3

         CASE DEFAULT
            ! For most parameters the ELMO_COEFF structure will
            ! define all of the mappings and coefficients needed for
            ! a particular summation. This can therefore be used in
            ! most cases. Parameters in this category are linear
            ! combinations of CMAQ species. For example, PMF_SO4 = 
            ! ASO4I + ASO4J.
            CALL CALC_CONCOUT_AGG( IDG, OUTVAL )

         END SELECT 
 
         ! Save the fact that this parameter has been calculated
         L_CALC( IUSED ) = .TRUE.
         ELMO_LOCAL( IUSED ) = OUTVAL
 

      END SUBROUTINE CALC_ELMO
          
!-------------------------------------------------------------------------
      SUBROUTINE LOAD_ELMO( C,R,L,CONC,WRITE_STEP, INIT_STEP, INIT_TIME )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC, DRY_AERO_DENS,
     &                     AH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS
      USE AEROMET_DATA, ONLY: AIRRH

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: C, R, L
      INTEGER IDG, IM, AL, IL, IUSED, IAVRG, IINST

      REAL, INTENT( IN ) :: CONC( : )

      LOGICAL, INTENT( IN ) :: WRITE_STEP
      LOGICAL, INTENT( IN ) :: INIT_STEP
      LOGICAL, INTENT( IN ) :: INIT_TIME

      ! Initalize Local Parameters
      C1                = C
      R1                = R
      L1                = L
      ELMO_LOCAL( : )   = 0.0
      INLET_FRAC(:,:,:) = -1.
      L_CALC            = .FALSE.
      CONC_VEC( : )     = CONC( : )

      ! Check to make sure the current layer is requested for diagnostic
      ! output.
      IF ( ( L.GE.INST_LAYER_BOT .OR. L.GE.AVRG_LAYER_BOT ) .AND.
     &     ( L.LE.INST_LAYER_TOP .OR. L.LE.AVRG_LAYER_TOP ) ) THEN
       
        ! Calculate All Aerosol Size Distribution Parameters
        CALL CALC_AERODIST_PARAMS( INIT_TIME )
      
        ! Loop Through Used Parameters List and Call ELMO Functions 
        ! to fill in Local Array. ELMO_LOCAL is the same length as the
        ! ELMO_LIST and is ordered by the ELMO ID numbers so that it
        ! can be used seamlessly with the CALC_ELMO internal order.
        DO IUSED = 1,N_ELMO_OUT
           IM  = USED_ELMO_MODE( IUSED ) ! Map to 
           IDG = USED_ELMO_ID( IUSED ) ! Map to Parameter ID numbers
           CALL CALC_ELMO( IDG, IM, ELMO_LOCAL( IUSED ) )
        END DO

        ! *** Aggregate Diagnostic Parameters in Average Array
        !     using trapezoidal rule
        IF ( AVRG_ACTIVE .AND. 
     &       L.GE.AVRG_LAYER_BOT .AND. L.LE.AVRG_LAYER_TOP ) THEN
           AL = L - AVRG_LAYER_BOT + 1
           DO  IAVRG = 1,N_ELMO_AVRG_OUT
              IF ( USED_ELMO_ID( MAP_AVRG2USED( IAVRG )) .EQ. ID_PMF_PH
     &             .AND. ELMO_LOCAL( MAP_AVRG2USED( IAVRG ) ) .LT. -100. ) THEN
                 ! Special Averageing for Particle pH
                 ! Add whatever the current average is right now.
                 ! Certainly this is problematic, particularly
                 ! at the beginning of an hour where ELMO_AVRG =
                 ! 0. It's best to either use the instantaneous
                 ! value or recalculate the average fine particle acidity
                 ! offline.
                 IF ( INIT_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = 3.5  ! Assume relatively acidic particles
                 ELSE IF ( WRITE_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
                 ELSE
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
     &                      + 2.0 * ELMO_AVRG( C,R,AL,IAVRG )
                 END IF
              ELSE
                 ! For all other variables, apply trapezoidal rule for
                 ! averaging.
                 IF ( INIT_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_LOCAL( MAP_AVRG2USED( IAVRG ) ) 
                 ELSE IF ( WRITE_STEP ) THEN
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
     &                                        + ELMO_LOCAL( MAP_AVRG2USED( IAVRG ) )
                 ELSE
                    ELMO_AVRG( C,R,AL,IAVRG ) = ELMO_AVRG( C,R,AL,IAVRG ) 
     &                      + 2.0 * ELMO_LOCAL( MAP_AVRG2USED( IAVRG ) )
                 END IF 
              END IF
           END DO
        END IF
       
        ! *** Populate Diagnostic Parameters to Instantaneous Array if this is a write step
        IF ( INST_ACTIVE .AND. ( INIT_TIME .OR. WRITE_STEP ) .AND.
     &       L.GE.INST_LAYER_BOT .AND. L.LE.INST_LAYER_TOP ) THEN
           IL = L - INST_LAYER_BOT + 1
           DO IINST = 1,N_ELMO_INST_OUT
              ELMO_INST( C,R,IL,IINST ) = ELMO_LOCAL( MAP_INST2USED( IINST ) )
           END DO
        END IF
 
      END IF
      END SUBROUTINE LOAD_ELMO

!-------------------------------------------------------------------------
      SUBROUTINE ELMO_DRIVER( CGRID, JDATE, JTIME, TSTEP, INIT_TIME )
!     This subroutine maps the PM diagnostic variables that the user has
!       requested to the entries in the ELMO_DATA table.
!-------------------------------------------------------------------------

      USE AERO_DATA, ONLY: AEROMODE_LNSG, DRY_AERO_DIAM, WET_AERO_DIAM,
     &                     DRY_AERO_M2, WET_AERO_M2, DRY_AERO_M3, 
     &                     WET_AERO_M3, N_MODE, MOMENT0_CONC, MOMENT2_CONC,
     &                     MOMENT3_CONC, WET_AERO_DENS, AEROSPC_CONC,
     &                     AH2O_IDX, AEROSPC, CALC_AERODIST_PARAMS,
     &                     Extract_Aero
      USE SOA_DEFN, ONLY : Extract_Soa
      USE AEROSOL_CHEMISTRY, ONLY: HETCHEM_RATES
      USE AEROMET_DATA, ONLY: AIRRH, AIRTEMP, AIRQV, AIRDENS, AIRPRES,
     &                        SRFTEMP, H2OVP, H2OSATVP, MWWAT, MWAIR,
     &                        CFRAC, DZ, PV, ZH
      use centralized_io_module, only : interpolate_var, pv_avail
      use RUNTIME_VARS
      Use phot_mod, Only: init_phot_shared 

#ifdef sens
      USE DDM3D_DEFN, ONLY : SENGRID
#endif 

      IMPLICIT NONE

      REAL, POINTER         :: CGRID( :,:,:,: ) ! Master concentration grid
      INTEGER, INTENT( IN ) :: JDATE, JTIME     ! Date and time inputs to 
                                                ! determine whether to write 
                                                ! concentrations
      INTEGER, INTENT( IN ) :: TSTEP(3)
      LOGICAL, INTENT( IN ) :: INIT_TIME        ! Is this the first time step 
                                                !   of the simulation
      LOGICAL :: INIT_STEP        ! Is the time step beginning

      ! Variable to set time step for writing visibility file
      INTEGER, SAVE :: WSTEP  = 0          ! local write counter
      LOGICAL, SAVE :: WRITE_STEP =.FALSE. ! local write flag

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
    
      ! Statement Function **************
      REAL, PARAMETER :: EPSWATER = MWWAT / MWAIR
      REAL ESATL ! arithmetic statement function for vapor pressure [Pa]
      REAL TT
      ! Coefficients for the equation, ESATL defining saturation vapor pressure
      REAL, PARAMETER :: AL = 610.94
      REAL, PARAMETER :: BL = 17.625
      REAL, PARAMETER :: CL = 243.04

      INTEGER C, R, L

      REAL, ALLOCATABLE, SAVE :: PRES(:,:,:), TA(:,:,:), TEMP2(:,:),
     &                           QV(:,:,:), DENS(:,:,:), CFRACS(:,:),
     &                           ZF(:,:,:), PVS(:,:,:), ZHS(:,:,:)

      ! values of AL, BL, and CL are from:
      ! Alduchov and Eskridge, "Improved Magnus Form Approximations of
      !                       Saturation Vapor Pressure,"
      !                       Jour. of Applied Meteorology, vol. 35,
      !                       pp 601-609, April, 1996.
      ESATL( TT ) = AL * EXP( BL * ( TT - 273.15 ) / ( TT - 273.15 + CL ) )
 

      IF ( FIRSTIME ) THEN
          FIRSTIME = .FALSE.
          ! Initialize Number of Steps Used for Calculating Average
          ELMO_NSTEP = 0.

          ALLOCATE( PRES( NCOLS, NROWS, NLAYS),
     &              TA( NCOLS, NROWS, NLAYS ),
     &              QV( NCOLS, NROWS, NLAYS ),
     &              DENS( NCOLS, NROWS, NLAYS ),
     &              CFRACS( NCOLS, NROWS ),
     &              ZF( NCOLS, NROWS, NLAYS ),
     &              ZHS( NCOLS, NROWS, NLAYS ),
     &              PVS( NCOLS, NROWS, NLAYS ),
     &              TEMP2( NCOLS, NROWS ) )
      END IF

      ! Determine if this is a write step
      WRITE_STEP = .FALSE.
      INIT_STEP  = .FALSE.
      IF ( ELMO_NSTEP .LT. 1.0 ) THEN
         WSTEP = 0
         INIT_STEP = .TRUE.
      ELSE
         WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
         IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) )
     &        WRITE_STEP = .TRUE.
      END IF
      ELMO_NSTEP = ELMO_NSTEP + 1.0

      ! Get Meteorological Variables

      ! pressure [Pa]
      call interpolate_var ('PRES', jdate, jtime, PRES)

      ! temperature [K]
      call interpolate_var ('TA', jdate, jtime, TA)       ! Grid Cell Temp
      call interpolate_var ('TEMP2',jdate, jtime, TEMP2 ) ! 2-m Temp

      ! specific humidity [g H2O/g air]
      call interpolate_var ('QV', jdate, jtime, QV)

      ! air density [kg/m3]
      call interpolate_var ('DENS', jdate, jtime, DENS)
      
      ! get cloud fraction if photolysis hasn not been called yet
      call interpolate_var ('CFRAC', jdate, jtime, CFRACS)

      ! retrieve all layer heights. This is the height of the top of the
      ! layers
      call interpolate_var ('ZF', jdate, jtime, ZF)
      
      ! retrieve potential vorticity
      if ( pv_avail ) then
           call interpolate_var ('PV', jdate, jtime, PVS)
      else
#ifdef mpas
            PVS = -9.000E36
#else
            PVS = AMISS3
#endif
      end if
      
      ! retrieve layer midpoint heights. This is a mass weighted
      ! coordinate so is not necessarily consistent with the distance 
      ! between the heights of the layer bottom and top.
      call interpolate_var ('ZH', jdate, jtime, ZHS)

      ! Calculate Heterogeneous Chemistry Rates
      CALL INIT_PHOT_SHARED()
      CALL HETCHEM_RATES( TA, PRES, QV, CGRID, DENS )

      ! Process PM Diagnostics for Base Model
      DO C = 1,NCOLS
      DO R = 1,NROWS
      DO L = 1,NLAYS
         ! Grid cell meteorological data.
         AIRTEMP  = TA   ( C,R,L )
         SRFTEMP  = TEMP2( C,R )     ! 2-meter temperature (K)
         AIRPRES  = PRES ( C,R,L )   ! Note pascals
         AIRQV    = QV   ( C,R,L )
         AIRDENS  = DENS ( C,R,L )
         H2OSATVP = ESATL( AIRTEMP )
         H2OVP    = AIRPRES * AIRQV / ( EPSWATER  + AIRQV )
         AIRRH    = MAX( 0.005, MIN( 0.99, H2OVP / H2OSATVP ) ) ! 0-1
         IF ( L .EQ. 1 ) THEN
             DZ   = ZF( C,R,L )
             CFRAC= CFRACS( C,R ) 
         ELSE
             DZ   = ZF( C,R,L ) - ZF( C,R,L-1 )
#ifdef mpas
            CFRAC = -9.000E36
#else
            CFRAC = AMISS3
#endif
         END IF
         ZH       = ZHS( C,R,L )
         PV       = PVS( C,R,L )
 
         ! Extract Aerosols
#ifdef sens
         CALL EXTRACT_AERO( CGRID( C,R,L,: ), .TRUE., SENGRID( C,R,L,:,: ), .TRUE. )
         CALL EXTRACT_SOA( CGRID( C,R,L,: ), SENGRID( C,R,L,:,: ), .TRUE. )
#else
         CALL EXTRACT_AERO( CGRID( C,R,L,: ), .TRUE. )
         CALL EXTRACT_SOA( CGRID( C,R,L,: ) )
#endif         
         ! Populate Diagnostic Arrays
         CALL LOAD_ELMO( C,R,L, CGRID(C,R,L,:), WRITE_STEP, INIT_STEP, INIT_TIME )

      END DO
      END DO
      END DO

      END SUBROUTINE ELMO_DRIVER

!-------------------------------------------------------------------------
      SUBROUTINE OPEN_ELMO ( JDATE, JTIME, TSTEP )
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
!   11 May 16 D. Wong: - Modified the code to retreive ELMO information from
!                        construct ELMO_SPC_RECORD stored in module cgrid_spcs
!                        to provide flexibility to handle AE6 and AE61
!                      - renamed the procedure name from OPDIAM to OPELMO
!-------------------------------------------------------------------------

      USE GRID_CONF           ! horizontal & vertical domain specifications

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

      !...Arguments:

        INTEGER, INTENT( IN ) :: JDATE      ! current model date, coded YYYYDDD
        INTEGER, INTENT( IN ) :: JTIME      ! current model time, coded HHMMSS
        INTEGER, INTENT( IN ) :: TSTEP      ! output time step

      !...Local variables:

        CHARACTER( 16 ), SAVE :: PNAME = 'OPEN_ELMO'
        CHARACTER( 96 ) :: XMSG = ' '

        INTEGER :: L          ! loop induction variables
        INTEGER :: MDATE, MTIME
#ifndef mpas
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

#endif
      ! Open Instantaneous PM Diagnostic File
      IF ( INST_ACTIVE ) THEN
        
#ifndef mpas
         ! Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_ELMO_1, FSRDWR3, PNAME ) ) THEN

            ! Set output file characteristics based on COORD.EXT and open the aerosol
            ! diagnostic file
            SDATE3D = JDATE
            STIME3D = JTIME
            !CALL NEXTIME ( SDATE3D, STIME3D, TSTEP3D ) !  start the next hour
         
            NLAYS3D = INST_LAYER_TOP - INST_LAYER_BOT + 1
            DO L = INST_LAYER_BOT, INST_LAYER_TOP + 1
               VGLVS3D( L ) = VGLVS_GD( L )
            END DO
         
            NVARS3D = N_ELMO_INST_OUT 
            VTYPE3D(1:NVARS3D) = INST_TYPE_OUT( 1:NVARS3D )
            VNAME3D(1:NVARS3D) = INST_PARS_OUT( 1:NVARS3D )
            UNITS3D(1:NVARS3D) = INST_UNIT_OUT( 1:NVARS3D )
            VDESC3D(1:NVARS3D) = INST_DESC_OUT( 1:NVARS3D )
         
            FDESC3D( 1 ) = 'Parameters Relevant for PM Formation and Distribution'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO
         
            ! Open the Instantaneous aerosol diagnostic file
            IF ( .NOT. OPEN3( CTM_ELMO_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// CTM_ELMO_1 // ' file'
               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF

         END IF
#endif
      END IF
 

      ! Open Average Output PM Diagnostic File
      IF ( AVRG_ACTIVE ) THEN
        
#ifndef mpas
         ! Try to open existing file for update
         IF ( .NOT. OPEN3( CTM_AELMO_1, FSRDWR3, PNAME ) ) THEN

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
         
            NVARS3D = N_ELMO_AVRG_OUT 
            VTYPE3D(1:NVARS3D) = AVRG_TYPE_OUT( 1:NVARS3D )
            VNAME3D(1:NVARS3D) = AVRG_PARS_OUT( 1:NVARS3D )
            UNITS3D(1:NVARS3D) = AVRG_UNIT_OUT( 1:NVARS3D )
            VDESC3D(1:NVARS3D) = AVRG_DESC_OUT( 1:NVARS3D )
         
            FDESC3D( 1 ) = 'Parameters Relevant for PM Formation and Distribution'
            DO L = 2, MXDESC3
               FDESC3D( L ) = ' '
            END DO
         
            ! Open the Instantaneous aerosol diagnostic file
            IF ( .NOT. OPEN3( CTM_AELMO_1, FSNEW3, PNAME ) ) THEN
               XMSG = 'Could not create '// CTM_AELMO_1 // ' file'
               CALL M3EXIT ( PNAME, SDATE3D, STIME3D, XMSG, XSTAT1 )
            END IF
 
         END IF
#endif
      END IF
 
      END SUBROUTINE OPEN_ELMO 

!-------------------------------------------------------------------------
      SUBROUTINE WRITE_ELMO ( JDATE, JTIME, TSTEP, INIT_TIME )
! Revision history
!   20 Feb - B. Murphy: Created
!-------------------------------------------------------------------------

      USE RUNTIME_VARS

      IMPLICIT NONE 

      INCLUDE SUBST_FILES_ID

      INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP(3)
      LOGICAL, INTENT( IN ) :: INIT_TIME
      INTEGER MDATE, MTIME
      CHARACTER( 300 ) XMSG

      CHARACTER( 16 ), SAVE :: PNAME = 'WRITE_ELMO'

C *** If IO Proceesor, then Write Data
         MDATE = JDATE
         MTIME = JTIME

C *** Write data to the scalar output file.
         IF ( INST_ACTIVE ) THEN
#ifndef mpas
            IF ( .NOT. WRITE3( CTM_ELMO_1, 
     &           ALLVAR3, MDATE, MTIME,
     &           ELMO_INST(:,:,:,:) ) ) THEN
               XMSG = 'Could not write ' // CTM_ELMO_1 // ' file'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF

            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                     'Timestep written to', CTM_ELMO_1,
     &                     'for date and time', MDATE, MTIME

#endif
         END IF
     
C *** Write data to the average aerosol diagnostic file.
         IF ( .NOT.INIT_TIME ) THEN
#ifndef mpas
           IF ( AVRG_ACTIVE ) THEN
            IF ( .NOT. END_TIME ) THEN   ! ending time timestamp
               CALL NEXTIME ( MDATE, MTIME, -TSTEP(1) )
            END IF
          
            IF ( .NOT. WRITE3( CTM_AELMO_1, 
     &             ALLVAR3, MDATE, MTIME, 
     &             ELMO_AVRG(:,:,:,:) / 2.0 / 
     &             (ELMO_NSTEP-1.0) ) ) THEN
               XMSG = 'Could not write ' // CTM_AELMO_1 // ' file'
               CALL M3EXIT ( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
            END IF
          
            WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &                       'Timestep written to', CTM_AELMO_1,
     &                       'for date and time', MDATE, MTIME
          
          
           END IF
#endif
           ELMO_NSTEP = 0.
         END IF
      RETURN 

      END SUBROUTINE WRITE_ELMO

      END MODULE ELMO_PROC
