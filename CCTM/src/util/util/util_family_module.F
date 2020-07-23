
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
      Integer                     :: NChemFamilies = 0
      Character( 32 )             :: ChemFamilyName( 50 ) = ''
      Integer                     :: ChemFamilyNum( 50 ) = 0
      Character( 32 )             :: ChemFamilyMembers( 50,100 ) = ''

      ! Define Stream Family Variables
      Integer                     :: NStreamFamilies = 0
      Character( 32 )             :: StreamFamilyName( 50 ) = ''
      Integer                     :: StreamFamilyNum( 50 ) = 0
      Character( 32 )             :: StreamFamilyMembers( 50,100 ) = ''

      ! Define Process Family Variables
      Integer                     :: NProcessFamilies = 0
      Character( 32 )             :: ProcessFamilyName( 50 ) = ''
      Integer                     :: ProcessFamilyNum( 50 ) = 0
      Character( 32 )             :: ProcessFamilyMembers( 50,100 ) = ''

      ! Define Region Family Variables
      Integer                     :: NRegionFamilies = 0
      Character( 32 )             :: RegionFamilyName( 50 ) = ''
      Integer                     :: RegionFamilyNum( 50 ) = 0
      Character( 32 )             :: RegionFamilyMembers( 50,100 ) = ''

      ! Other Variables
      Logical                     :: linit = .FALSE.

      contains

          
! ----------------------------------------------------------------------
      subroutine init_families
! ----------------------------------------------------------------------
!     Initialize all variables needed for tracking families of chemical
!     species, emissions streams, processes and regions. These vairables
!     are useful for inputs and outputs.
! ----------------------------------------------------------------------

      IMPLICIT NONE

      if ( linit ) return
      linit = .TRUE.

      NChemFamilies = 0
      ChemFamilyName = ''
      ChemFamilyNum = 0
      ChemFamilyMembers = ''

      NStreamFamilies = 0
      StreamFamilyName = ''
      StreamFamilyNum = 0
      StreamFamilyMembers = ''

      NProcessFamilies = 0
      ProcessFamilyName = ''
      ProcessFamilyNum = 0
      ProcessFamilyMembers = ''

      NRegionFamilies = 0
      RegionFamilyName = ''
      RegionFamilyNum = 0
      RegionFamilyMembers = ''

      end subroutine init_families


! ----------------------------------------------------------------------
      subroutine read_families
! ----------------------------------------------------------------------
!     Load definitions for families from the Control File to these 
!     globally available variables.
!     
! ----------------------------------------------------------------------

      use RUNTIME_VARS, only : EMISSCTRL, logdev, log_message, log_subheading
      use UTILIO_DEFN
      
      IMPLICIT NONE
      
      ! Define Dummy Variables for Opening Emission Control Namelist
      CHARACTER( 300 ) :: EQNAME, XMSG
      INTEGER          :: EMCTRL_NML
      INTEGER          :: STAT, IFAM, INUM

      ! Define Chemical Families
      NAMELIST / ChemicalFamilies / NChemFamilies, ChemFamilyName, 
     &           ChemFamilyNum, ChemFamilyMembers

      ! Define Stream Families
      NAMELIST / StreamFamilies / NStreamFamilies, StreamFamilyName, 
     &           StreamFamilyNum, StreamFamilyMembers

      ! Define Region Families
      NAMELIST / RegionFamilies / NRegionFamilies, RegionFamilyName, 
     &           RegionFamilyNum, RegionFamilyMembers
 
      ! Define Process Families
      NAMELIST / ProcessFamilies / NProcessFamilies, ProcessFamilyName, 
     &           ProcessFamilyNum, ProcessFamilyMembers
 
      ! Initialize Familiy Arrays
      call init_families

      CALL LOG_SUBHEADING( LOGDEV, "Reading Family Definitions from Control File" )

      ! Retrieve the Name of the Emission Control File
      IF ( EMISSCTRL .EQ. "EMISSCTRL_NML" ) THEN
          XMSG = 'You have chosen not to indicate the location of an' //
     &           'Emission Control namelist file. Default settings ' //
     &           'will be assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
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
 
      ! Read Chemical Family Specification Section
      REWIND( EMCTRL_NML )
      READ( NML = ChemicalFamilies, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          XMSG = 'Warning! Something went wrong while reading the ' //
     &           'ChemicalFamilies section of the Emissions Control ' //
     &           'Namelist. Default values for this section will be ' //
     &           'assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          NChemFamilies      = 0
          ChemFamilyName     = ''
          ChemFamilyNum      = 0
          ChemFamilyMembers  = ''
      END IF
      ! Capitalize All Family and Member Names
      DO IFAM = 1,NChemFamilies
          CALL UPCASE( ChemFamilyName( IFAM ) )
          DO INUM = 1,ChemFamilyNum ( IFAM )
              CALL UPCASE( ChemFamilyMembers( IFAM,INUM ) )
          END DO
      END DO

      ! Read Stream Family Specification Section
      REWIND( EMCTRL_NML )
      READ( NML = StreamFamilies, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          XMSG = 'Warning! Something went wrong while reading the ' //
     &           'StreamFamilies section of the Emissions Control ' //
     &           'Namelist. Default values for this section will be ' //
     &           'assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          NStreamFamilies      = 0
          StreamFamilyName     = ''
          StreamFamilyNum      = 0
          StreamFamilyMembers  = ''
      END IF
      ! Capitalize All Family and Member Names
      DO IFAM = 1,NStreamFamilies
          CALL UPCASE( StreamFamilyName( IFAM ) )
          DO INUM = 1,StreamFamilyNum( INUM )
              CALL UPCASE( StreamFamilyMembers( IFAM,INUM ) )
          END DO
      END DO
 
      ! Read Region Family Specification Section
      REWIND( EMCTRL_NML )
      READ( NML = RegionFamilies, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          XMSG = 'Warning! Something went wrong while reading the ' //
     &           'RegionFamilies section of the Emissions Control ' //
     &           'Namelist. Default values for this section will be '//
     &           'assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          NRegionFamilies      = 0
          RegionFamilyName     = ''
          RegionFamilyNum      = 0
          RegionFamilyMembers  = ''
      END IF
      ! Capitalize All Family and Member Names
      DO IFAM = 1,NRegionFamilies
          CALL UPCASE( RegionFamilyName( IFAM ) )
          DO INUM = 1,RegionFamilyNum( INUM )
              CALL UPCASE( RegionFamilyMembers( IFAM,INUM ) )
          END DO
      END DO
 
      ! Read Process Family Specification Section
      REWIND( EMCTRL_NML )
      READ( NML = ProcessFamilies, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          XMSG = 'Warning! Something went wrong while reading the ' //
     &           'ProcessFamilies section of the Emissions Control '//
     &           'Namelist. Default values for this section will be '//
     &           'assumed.'
          CALL LOG_MESSAGE( LOGDEV, ' ' )
          CALL LOG_MESSAGE( LOGDEV, XMSG )
          NProcessFamilies      = 0
          ProcessFamilyName     = ''
          ProcessFamilyNum      = 0
          ProcessFamilyMembers  = ''
      END IF
      ! Capitalize All Family and Member Names
      DO IFAM = 1,NProcessFamilies
          CALL UPCASE( ProcessFamilyName( IFAM ) )
          DO INUM = 1,ProcessFamilyNum( INUM )
              CALL UPCASE( ProcessFamilyMembers( IFAM,INUM ) )
          END DO
      END DO
 
      CLOSE( UNIT = EMCTRL_NML )

      CALL LOG_MESSAGE( LOGDEV, ' ' ) ! Add a buffer space in the log file

      end subroutine read_families

! ----------------------------------------------------------------------
      subroutine map_chem_families( species0, spec_vec, nvec, out_vec )
! ----------------------------------------------------------------------
!     Return a vector outvec of size nvec, equal to the size of specvec.
!     Outvec is a logical which maps the input species name "species" 
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
         IFAM = INDEX1( SPECIES, NChemFamilies, ChemFamilyName )
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
             WRITE( LOGDEV, '(/,3A,/,A,/,A,/,A,/,A)' ),
     &         'Species ',TRIM(SPECIES),' was used in the Emissions',
     &         ' Control Instructions Namelist but it is not a valid CMAQ ',
     &         'transported species or family. Please add it to one of the ',
     &         'input chemical namelists (ie. GC, AE, etc). Note that aerosol',
     &         'Number and Surface Area Species are not valid for scaling.'
             XMSG = 'Error in Emissions Map Processing.'
             CALL M3EXIT( 'Map_Chem_Families', 0, 0, XMSG, XSTAT1 )
            END IF
         END DO
      END IF 
 
      end subroutine map_chem_families

      end module util_Family_module
