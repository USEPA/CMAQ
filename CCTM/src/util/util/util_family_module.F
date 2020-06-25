
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
      Character( 32 ),Allocatable :: ChemFamilyName( 50 ) = ''
      Integer, Allocatable        :: ChemFamilyNum( 50 ) = 0
      Character( 32 ),Allocatable :: ChemFamilyMembers( 50,100 ) = ''

      ! Define Stream Family Variables
      Integer                     :: NStreamFamilies = 0
      Character( 32 ),Allocatable :: StreamFamilyName( 50 ) = ''
      Integer, Allocatable        :: StreamFamilyNum( 50 ) = 0
      Character( 32 ),Allocatable :: StreamFamilyMembers( 50,100 ) = ''

      ! Define Process Family Variables
      Integer                     :: NProcessFamilies = 0
      Character( 32 ),Allocatable :: ProcessFamilyName( 50 ) = ''
      Integer, Allocatable        :: ProcessFamilyNum( 50 ) = 0
      Character( 32 ),Allocatable :: ProcessFamilyMembers( 50,100 ) = ''

      ! Define Region Family Variables
      Integer                     :: NRegionFamilies = 0
      Character( 32 ),Allocatable :: RegionFamilyName( 50 ) = ''
      Integer, Allocatable        :: RegionFamilyNum( 50 ) = 0
      Character( 32 ),Allocatable :: RegionFamilyMembers( 50,100 ) = ''

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

      NChemFamlies = 0
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

      use RUNTIME_VARS, only : EMISSCTRL, logdev
      
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
 
      ! Read Chemical Family Specification Section
      REWIND( EMCTRL_NML )
      READ( NML = ChemicalFamilies, UNIT = EMCTRL_NML, IOSTAT=STAT )
      IF ( STAT .NE. 0 ) THEN
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'ChemicalFamilies section of the Emissions Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
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
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'StreamFamilies section of the Emissions Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
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
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'RegionFamilies section of the Emissions Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
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
          WRITE( LOGDEV, "(5x,A,/,5x,A,/,5x,A,/,5x,A)" ),
     &           'Warning! Something went wrong while reading the ',
     &           'ProcessFamilies section of the Emissions Control ',
     &           'Namelist. Default values for this section will be ',
     &           'assumed.'
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
 

      end subroutine load_families

      end module util_Family_module
