
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

!------------------------------------------------------------------------!
! This module contains the definition of various Families that are       !
! useful for users wanting to scale inputs or otherwise modify the the   !
! options for their outputs.                                             !
!                                                                        !
! Revision History:                                                      !
!  25 Jun 2020 B. Murphy initial implementation                          !
!------------------------------------------------------------------------!

      module util_Family_module

      IMPLICIT NONE

      SAVE

      ! Define Chemical Family Variables
      Integer                     :: N_Chem_Fams = 0
      Integer                     :: Max_Chem_Fam_Members = 0
      Character( 32 ),Allocatable :: ChemFamilyName( : ) 
      Integer,Allocatable         :: ChemFamilyNum( : ) 
      Character( 32 ),Allocatable :: ChemFamilyMembers( :,: ) 

      ! Define Stream Family Variables
      Integer                     :: Desid_N_Stream_Fams = 0
      Integer                     :: Desid_Max_Stream_Fam_Members = 0
      Character( 32 ),Allocatable :: StreamFamilyName( : ) 
      Integer,Allocatable         :: StreamFamilyNum( : )
      Character( 32 ),Allocatable :: StreamFamilyMembers( :,: )

      ! Define Region Family Variables
      Integer                     :: Desid_N_Reg_Fams = 0
      Integer                     :: Desid_Max_Reg_Fam_Members = 0
      Character( 32 ),Allocatable :: RegionFamilyName( : )
      Integer,Allocatable         :: RegionFamilyNum( : )
      Character( 32 ),Allocatable :: RegionFamilyMembers( :,: )

      ! Other Variables
      Logical                     :: linit = .FALSE.

      contains

          
! ----------------------------------------------------------------------
      subroutine read_families
! ----------------------------------------------------------------------
!     Load definitions for families from the Control File to these 
!     globally available variables.
!     
! ----------------------------------------------------------------------

      use RUNTIME_VARS, only : DESID_CTRL, MISC_CTRL, logdev, log_message, 
     &                         log_subheading
      use UTILIO_DEFN
      use desid_param_module, only : Desid_Max_Reg
      use aero_data, only : n_aerospc, aerospc, n_mode, aero_missing
      
      IMPLICIT NONE
      
      ! Define Dummy Variables for Opening Emission Control Namelist
      CHARACTER( 300 ) :: XMSG
      INTEGER          :: FUNIT
      INTEGER          :: STAT, IFAM, INUM, IOST, IAER, JAER, IM
      CHARACTER( 200 ) :: TMPLINE


      ! Define Chemical Families           
      NAMELIST / Chemical_FamVars / N_Chem_Fams, Max_Chem_Fam_Members
      NAMELIST / ChemicalFamilies / ChemFamilyName, ChemFamilyMembers

      ! Define Stream Families
      NAMELIST / Desid_StreamFamVars / Desid_N_Stream_Fams, Desid_Max_Stream_Fam_Members
      NAMELIST / Desid_StreamFam     / StreamFamilyName, StreamFamilyMembers

      ! Define Region Families
      NAMELIST / Desid_RegionDefVars / Desid_Max_Reg, Desid_N_Reg_Fams,
     &                                 Desid_Max_Reg_Fam_Members
      NAMELIST / Desid_RegionFam / RegionFamilyName, RegionFamilyMembers
 
      CALL LOG_SUBHEADING( LOGDEV, "Reading Family Definitions from Control File" )

      ! Retrieve the Name of the Misc. Control File
      IF ( MISC_CTRL .EQ. "MISC_CTRL_NML" ) THEN
          XMSG = 'You have chosen not to indicate the location of a' //
     &              'Misc. Control namelist file. You must give a value ' //
     &              'for the MISC_CTRL variable in the CMAQ runscript.'
          CALL M3EXIT( 'READ_FAMILIES',0,0,XMSG,1 )
      END IF
 
      ! Open Miscellaneous Control Namelist File
      FUNIT = JUNIT()
      OPEN( FILE = MISC_CTRL, UNIT = FUNIT, STATUS = 'OLD',
     &      POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

      ! Check for Error in File Open Process
      IF ( STAT .NE. 0 ) THEN
          WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &           'miscellaneous control namelist file: ',TRIM( MISC_CTRL )
          CALL M3EXIT( 'READ_FAMILIES',0,0,XMSG,1 )
      END IF
 
      ! Read Number of Chemical Families
      REWIND( FUNIT )
      READ( NML = Chemical_FamVars, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .EQ. -1 ) THEN
          XMSG = 'Note: the Chemical_FamVars section of the Misc. Control ' //
     &           'Namelist is missing. Default values for this section will be ' //
     &           'assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          N_Chem_Fams          = 0
          Max_Chem_Fam_Members = 120
      ELSE IF ( STAT .NE. 0 ) THEN
          ! Read Error Detected for MISC_CTRL
          backspace( FUNIT )
          read( FUNIT, fmt='(A)' ) tmpline
          XMSG = 'ERROR: There was a syntax error reading the Chemical_FamVars '//
     &           'variable for use by the families module. Please check the format of '//
     &           'each line for syntax errors. The invalid line was likely: '
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE
          CALL M3EXIT ( 'Read_Families', 0, 0, 'CMAQ must Crash until you '//
     &                  'fix the Chemical Families', 1 ) 
 
      END IF
 
      ! Read Chemical Family Specification Section
      N_Chem_Fams = N_Chem_Fams + n_aerospc
      ALLOCATE( ChemFamilyName( N_Chem_Fams),
     &          ChemFamilyNum( N_Chem_Fams),
     &          ChemFamilyMembers( N_Chem_Fams, Max_Chem_Fam_Members ),
     &          STAT = IOST)
      CALL CHECKMEM( IOST, 'ChemFamilies', 'Read_Families' )
      ChemFamilyName = ''
      ChemFamilyMembers = ''

      REWIND( FUNIT )
      READ( NML = ChemicalFamilies, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .EQ. -1 ) THEN
          XMSG = 'Note: the ChemicalFamilies section of the Misc. Control ' //
     &           'Namelist is missing. Default values for this section will be ' //
     &           'assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          ChemFamilyName     = ''
          ChemFamilyMembers  = ''
      ELSE IF ( STAT .NE. 0 ) THEN
          ! Read Error Detected for RGN_NML
          backspace( FUNIT )
          read( FUNIT, fmt='(A)' ) tmpline
          XMSG = 'ERROR: There was a syntax error reading the ChemicalFamilies '//
     &           'variable for use by the DESID module. Please check the format of '//
     &           'each line for syntax errors. The invalid line was likely: '
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE
          CALL M3EXIT ( 'Read_Families', 0, 0, 'CMAQ must Crash until you '//
     &                  'fix the Chemical Families', 1 ) 
 
      END IF

      ! Add Aerosol Species to Chemical Family List if they aren't
      ! already there
      DO IAER = 1,N_AEROSPC
         JAER = INDEX1( AEROSPC(IAER)%BULKNAME,N_Chem_Fams,ChemFamilyName )
         IF ( JAER .EQ. 0 ) THEN
           ! Add Aerosol Bulkname to the end of the chemical family
           ! list
           DO IFAM = 1,N_Chem_Fams
             if ( ChemFamilyName(ifam) .eq. '' ) then
                  ChemFamilyName(ifam) = aerospc(iaer)%bulkname
                inum = 0
                do im = 1,n_mode
                  if ( .not.aero_missing(iaer,im) ) then
                     inum = inum + 1
                     ChemFamilyMembers(ifam,inum) = aerospc(iaer)%name(im)
                  end if
                end do
                exit
             end if
           END DO
         ELSE
           N_Chem_Fams = N_Chem_Fams - 1
         END IF
      END DO

      ! Reallocate Chemical Family Arrays in case N_Chem_Fams was
      ! shortened due to aerosol bulknames being already present in the
      ! user-defined family list.
      ChemFamilyName = ChemFamilyName(1:N_Chem_Fams)
      ChemFamilyMembers = ChemFamilyMembers(1:N_Chem_Fams,1:Max_Chem_Fam_Members)

      ! Populate and Error Check All Family and Member Names
      DO IFAM = 1,N_Chem_Fams
          IF ( ChemFamilyName(IFAM) .EQ. '' ) THEN
             WRITE( XMSG, '(A,I3,A,A,A)' ),'ERROR: The number of Chemical Families ',
     &              N_Chem_Fams, ' is larger than the number of fields ',
     &              'provided in the ChemFamilyName array. Reduce N_Chem_Fams ',
     &              'in the MISC_CTRL file or provide more Chemical Families.'
             CALL M3EXIT( 'Read_Families',0,0,XMSG,1 )
          END IF
          CALL UPCASE( ChemFamilyName( IFAM ) )
          ChemFamilyNum( IFAM ) = 0

          DO INUM = 1,Max_Chem_Fam_Members
              IF ( ChemFamilyMembers( IFAM,INUM ) .EQ. '' ) EXIT
              CALL UPCASE( ChemFamilyMembers( IFAM,INUM ) )
              ChemFamilyNum ( IFAM ) = INUM
          END DO
      END DO  
      CLOSE( UNIT = FUNIT )
 
      !!! Stream Families !!!
      ! Retrieve the Name of the DESID Control File
      IF ( DESID_CTRL .EQ. "DESID_CTRL_NML" ) THEN
          XMSG = 'You have chosen not to indicate the location of a' //
     &              'DESID Control namelist file. You must give a value ' //
     &              'for the DESID_CTRL variable in the CMAQ runscript.'
          CALL M3EXIT( 'READ_FAMILIES',0,0,XMSG,1 )
      END IF
 
      ! Open Emission Control Namelist File
      FUNIT = JUNIT()
      OPEN( FILE = DESID_CTRL, UNIT = FUNIT, STATUS = 'OLD',
     &      POSITION = 'REWIND', FORM='FORMATTED', IOSTAT = STAT )

      ! Check for Error in File Open Process
      IF ( STAT .NE. 0 ) THEN
          WRITE( XMSG, '(A,A,A)' ),'ERROR: Could not read ',
     &           'DESID control namelist file: ',TRIM( DESID_CTRL )
          CALL M3EXIT( 'READ_FAMILIES',0,0,XMSG,1 )
      END IF
 
      ! Read Stream Family Specification Section
      REWIND( FUNIT )
      READ( NML = Desid_StreamFamVars, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .EQ. -1 ) THEN
          XMSG = 'Note: the StreamFamilies section of the DESID Control ' //
     &           'Namelist is missing. Default values for this section will be ' //
     &           'assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          Desid_N_Stream_Fams  = 0
          Desid_Max_Stream_Fam_Members = 0
      ELSE IF ( STAT .NE. 0 ) THEN
          ! Read Error Detected for RGN_NML
          backspace( FUNIT )
          read( FUNIT, fmt='(A)' ) tmpline
          XMSG = 'ERROR: There was a syntax error reading the Desid_StreamFamVars '//
     &           'variable for use by the DESID module. Please check the format of '//
     &           'each line for syntax errors. The invalid line was likely: '
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE
          CALL M3EXIT ( 'Read_Families', 0, 0, 'CMAQ must Crash until you '//
     &                  'fix the Stream Families', 1 ) 
 
      END IF

      ALLOCATE( StreamFamilyName( Desid_N_Stream_Fams ),
     &          StreamFamilyNum( Desid_N_Stream_Fams ),
     &          StreamFamilyMembers( Desid_N_Stream_Fams, Desid_Max_Stream_Fam_Members ),
     &          STAT = IOST )
      CALL CHECKMEM( IOST, 'Stream Families', 'Read_Families' )
      StreamFamilyName = ''
      StreamFamilyMembers = ''

      ! Read Stream Family Specification Section
      REWIND( FUNIT )
      READ( NML = Desid_StreamFam, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .EQ. -1 ) THEN
          XMSG = 'Note: the StreamFamilies section of the DESID Control ' //
     &           'Namelist is missing. Default values for this section will be ' //
     &           'assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          StreamFamilyName     = ''
          StreamFamilyMembers  = ''
      ELSE IF ( STAT .NE. 0 ) THEN
          ! Read Error Detected for RGN_NML
          backspace( FUNIT )
          read( FUNIT, fmt='(A)' ) tmpline
          XMSG = 'ERROR: There was a syntax error reading the Desid_StreamFam '//
     &           'variable for use by the DESID module. Please check the format of '//
     &           'each line for syntax errors. The invalid line was likely: '
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE
          CALL M3EXIT ( 'Read_Families', 0, 0, 'CMAQ must Crash until you '//
     &                  'fix the Stream Families', 1 ) 
 
      END IF
 
      ! Capitalize All Family and Member Names
      DO IFAM = 1,Desid_N_Stream_Fams
          IF ( StreamFamilyName(IFAM) .EQ. '' ) THEN
             WRITE( XMSG, '(A,I3,A,A,A)' ),'ERROR: The number of Stream Families ',
     &              Desid_N_Stream_Fams, ' is larger than the number of fields ',
     &              'provided in StreamFamilyName array. Reduce Desid_N_Stream_Fams ',
     &              'in the DESID_CTRL file or provide more Stream Families.'
             CALL M3EXIT( 'Read_Families',0,0,XMSG,1 )
          END IF
          CALL UPCASE( StreamFamilyName( IFAM ) )
          StreamFamilyNum( IFAM ) = 0

          DO INUM = 1,Desid_Max_Stream_Fam_Members
              IF ( StreamFamilyMembers(IFAM,INUM) .EQ. '' ) EXIT
              CALL UPCASE( StreamFamilyMembers( IFAM,INUM ) )
              StreamFamilyNum( IFAM ) = INUM
          END DO
      END DO

 
      !!! Region Families !!!
      ! Read Region Family Specification Section
      REWIND( FUNIT )
      READ( NML = Desid_RegionDefVars, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .EQ. -1 ) THEN
          XMSG = 'Note: the Desid_Regoin_DefVars section of the DESID Control ' //
     &           'Namelist is missing. Default values for this section will be '//
     &           'assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          Desid_Max_Reg  = 0
          Desid_N_Reg_Fams  = 0
          Desid_Max_Reg_Fam_Members  = 0
      ELSE IF ( STAT .NE. 0 ) THEN
          ! Read Error Detected for RGN_NML
          backspace( FUNIT )
          read( FUNIT, fmt='(A)' ) tmpline
          XMSG = 'ERROR: There was a syntax error reading the RegionFamilies '//
     &           'variable for use by the DESID module. Please check the format of '//
     &           'each line for syntax errors. The invalid line was likely: '
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE
          CALL M3EXIT ( 'Read_Families', 0, 0, 'CMAQ must Crash until you '//
     &                  'fix the Region Families', 1 ) 
 
      END IF

      ALLOCATE( RegionFamilyName( Desid_N_Reg_Fams ),
     &          RegionFamilyNum( Desid_N_Reg_Fams ),
     &          RegionFamilyMembers( Desid_N_Reg_Fams, Desid_Max_Reg_Fam_Members ),
     &          STAT=IOST )
      CALL CHECKMEM( IOST, 'Region Families','Read Families' )
      RegionFamilyName = ''
      RegionFamilyMembers = ''

      REWIND( FUNIT )
      READ( NML = Desid_RegionFam, UNIT = FUNIT, IOSTAT=STAT )
      IF ( STAT .EQ. -1 ) THEN
          XMSG = 'Note: the Desid_RegionFam section of the DESID Control ' //
     &           'Namelist is missing. Default values for this section will be '//
     &           'assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          RegionFamilyName     = ''
          RegionFamilyMembers  = ''
      ELSE IF ( STAT .NE. 0 ) THEN
          ! Read Error Detected for RGN_NML
          backspace( FUNIT )
          read( FUNIT, fmt='(A)' ) tmpline
          XMSG = 'ERROR: There was a syntax error reading the RegionFamilies '//
     &           'variable for use by the DESID module. Please check the format of '//
     &           'each line for syntax errors. The invalid line was likely: '
          CALL LOG_MESSAGE( LOGDEV, ' ')
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          WRITE( LOGDEV, '(8x,A)' ) TMPLINE
          CALL M3EXIT ( 'Read_Families', 0, 0, 'CMAQ must Crash until you '//
     &                  'fix the Region Families', 1 ) 
 
      END IF
 
      ! Capitalize All Family and Member Names
      DO IFAM = 1,Desid_N_Reg_Fams
          IF ( RegionFamilyName(IFAM) .EQ. '' ) THEN
             WRITE( XMSG, '(A,I3,A,A,A)' ),'ERROR: The number of Region Families ',
     &              Desid_N_Reg_Fams, ' is larger than the number of fields ',
     &              'provided in RegionFamilyName array. Reduce Desid_N_Reg_Fams ',
     &              'in the DESID_CTRL file or provide more Region Families.'
             CALL M3EXIT( 'Read_Families',0,0,XMSG,1 )
          END IF
          CALL UPCASE( RegionFamilyName( IFAM ) )
          RegionFamilyNum( IFAM ) = 0

          DO INUM = 1,Desid_Max_Reg_Fam_Members
              IF ( RegionFamilyMembers( IFAM,INUM ) .EQ. '' ) EXIT
              CALL UPCASE( RegionFamilyMembers( IFAM,INUM ) )
              RegionFamilyNum( IFAM ) = INUM
          END DO
      END DO
 

      CLOSE( UNIT = FUNIT )

      CALL LOG_MESSAGE( LOGDEV, ' ' ) ! Add a buffer space in the log file

      end subroutine read_families

! ----------------------------------------------------------------------
      subroutine map_chem_families( species0, spec_vec, nvec, out_vec )
! ----------------------------------------------------------------------
!     Return a vector out_vec of size nvec, equal to the size of specvec.
!     Out_vec is a logical which maps the input species name "species" 
!     to specvec, expanding any chemical families or keywords.
! ----------------------------------------------------------------------

      USE AERO_DATA, ONLY: N_MODE, MODESUFF, AEROSPC, N_AEROSPC
      USE RUNTIME_VARS, ONLY: LOGDEV
      USE UTILIO_DEFN

      IMPLICIT NONE

      CHARACTER(16), INTENT(IN) :: SPECIES0
      INTEGER      , INTENT(IN) :: NVEC
      CHARACTER(16), INTENT(IN) :: SPEC_VEC( NVEC )
      LOGICAL      , INTENT(OUT):: OUT_VEC( NVEC )

      CHARACTER(16) :: SPECIES

      INTEGER IFAM, ICHEM, IDX, JDX, IM, KDX, NCHEM
      CHARACTER(16) :: CHEM_NAME( 150 ), SN
      CHARACTER(200) :: XMSG

      ! Initialize Emissions Species Array
      SPECIES = SPECIES0
      CALL UPCASE( SPECIES )
      OUT_VEC = .FALSE.


      ! Find Indices of Species Relevant for "species"
      IF ( SPECIES .EQ. 'ALL' ) THEN
         ! Expand to Apply to All Species
         OUT_VEC = .TRUE.
      ELSE     
         ! Determine if the Species Label Refers to A Family and if So, 
         ! Apply the Rule to all members of that Family
         IFAM = INDEX1( SPECIES, N_Chem_Fams, ChemFamilyName )
         IF ( IFAM .EQ. 0 ) THEN
            NCHEM = 1
            CHEM_NAME(1) = SPECIES
         ELSE
            NCHEM = ChemFamilyNum( IFAM )
            CHEM_NAME(1:NCHEM) = ChemFamilyMembers( IFAM,1:NCHEM )
         END IF
      
         DO ICHEM = 1,NCHEM
           ! Find the Specific Species this Rule Identifies
           IDX = INDEX1( CHEM_NAME( ICHEM ), NVEC, SPEC_VEC )
           JDX = INDEX1( CHEM_NAME( ICHEM ), N_AEROSPC,  AEROSPC( : )%BULKNAME )
           IF ( IDX .NE. 0 ) THEN
             OUT_VEC( IDX ) = .TRUE.
           ELSE IF ( JDX .NE. 0 ) THEN
             ! This is an aerosol species, and it is being
             ! identified with a bulk name (no mode suffix). 
             ! We need to allow for all possible DIFF_SPC with
             ! all used suffixes
             SN = CHEM_NAME( ICHEM )
             DO IM = 1,N_MODE
               KDX = INDEX1( TRIM( SN )//MODESUFF( IM ), NVEC, SPEC_VEC )
               IF ( KDX .NE. 0 ) OUT_VEC( KDX ) = .TRUE.
             END DO
           ELSE
             WRITE( LOGDEV, '(/,19A,A,1A,A,A,/,A,/,A,/,A,/,A,/,A,/,A)' ),
     &         'Attention: Species ',TRIM(SPECIES),':',TRIM(CHEM_NAME(ICHEM)),
     &         ' was used in the CMAQ Miscellaneous',
     &         ' Control Instructions but it is not a valid CMAQ ',
     &         'transported species or family. Please add it to one of the ',
     &         'input chemical namelists (ie. GC, AE, etc), if desired. If the ',
     &         'variable is on an emission file and not a CMAQ species, no ',
     &         'further action is required. Note that aerosol Number and Surface ',
     &         'Area Species are not valid for scaling.'
            END IF
         END DO
      END IF 
 
      end subroutine map_chem_families

      end module util_Family_module
