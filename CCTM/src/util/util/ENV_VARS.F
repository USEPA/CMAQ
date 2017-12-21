
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

!....................................................................
!  The ENV_VARS module contains file unit identifiers for the log files,
!  input files, and the values of all environment variables.
!
!  It also contains routines for reading environment variables, and
!  opening input files.
!....................................................................

      MODULE ENV_VARS

      IMPLICIT NONE

      SAVE

      PUBLIC

      INTEGER :: OUTDEV    ! File Unit for Standard Output
      INTEGER :: LOGDEV    ! File Unit for Ascii Log File
      INTEGER :: DIAGDEV   ! File Unit For Ascii Diagnostic Output
      INTEGER :: MYPE = -1 ! Processor Number

      INTEGER, PARAMETER :: CTM_DIAG_LVL = 1
      
      INTEGER :: LOG_LINE_LENGTH = 80  ! Cut the log offs at this character if possible
      INTEGER :: LOG_MAJOR_TAB   = 5   ! Left tab for all text including headings
      INTEGER :: LOG_MINOR_TAB   = 2   ! Tab for indenting subsequent lines of text in
                                       ! a paragraph for instance.

      CONTAINS

!.........................................................................
      SUBROUTINE LOG_HEADING( FUNIT, CHEAD_IN )

!  Formats and writes a user-supplied heading to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ. The length of the input array is set at 80 because
!  we would like to try limiting lines to 80 characters and a heading
!  should probably just be one line.
!.........................................................................
      USE M3UTILIO

      IMPLICIT NONE

      INTEGER, INTENT( IN )           :: FUNIT
      CHARACTER( * ), INTENT( IN )    :: CHEAD_IN
      CHARACTER( len=: ), ALLOCATABLE :: CHEAD
      CHARACTER( 20 )                 :: FMT

      ! Capitalize the heading
      CHEAD = CHEAD_IN
      CALL UPCASE( CHEAD )

      ! Write the heading to the log file
      WRITE( FMT, '("(/,", I0, "x,A,A,A)")' ) LOG_MAJOR_TAB

      WRITE( FUNIT, FMT ),
     &       '$>----- ',TRIM( CHEAD ),' -----<$'

      END SUBROUTINE LOG_HEADING
      
!.........................................................................
      SUBROUTINE LOG_SUBHEADING( FUNIT, CHEAD )

!  Formats and writes a user-supplied sub-heading to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ. The length of the input array is set at 80 because
!  we would like to try limiting lines to 80 characters and a sub-heading
!  should probably just be one line.
!.........................................................................

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: FUNIT
      CHARACTER( * ), INTENT( IN ) :: CHEAD
      CHARACTER( 20 )       :: FMT

      ! Write the sub-heading to the log file
      WRITE( FMT, '("(/,", I0, "x,A,A,A)")' ) LOG_MAJOR_TAB
      WRITE( FUNIT, FMT ),'--- ',TRIM( CHEAD ),' ---'

      END SUBROUTINE LOG_SUBHEADING
      
!.........................................................................
      SUBROUTINE LOG_MESSAGE( FUNIT, CMSG_IN )

!  Formats and writes a user-supplied message to a specific log file.
!  This approach is intended to standardize the log files that are
!  created by CMAQ.
!.........................................................................
      USE M3UTILIO

      IMPLICIT NONE

      INTEGER, INTENT( IN ) :: FUNIT
      CHARACTER( * ), INTENT( IN ) :: CMSG_IN
      CHARACTER( len=: ), ALLOCATABLE :: CMSG
      CHARACTER( 20 )       :: FMT

      CHARACTER( LOG_LINE_LENGTH ) :: CTEMP
      INTEGER :: MSG_SIZE
      INTEGER :: NLINE1, NLINE2, NLINE, NTAB, LAST_SPACE

      CMSG = CMSG_IN

      ! Write the message to the log file, while trimming to 80
      ! characters (while accounting for tab-length)
      NLINE1 = LOG_LINE_LENGTH - LOG_MAJOR_TAB
      NLINE2 = LOG_LINE_LENGTH - LOG_MAJOR_TAB - LOG_MINOR_TAB
      NLINE  = NLINE1
      NTAB   = LOG_MAJOR_TAB
      
      ! Determine Length of Total Message
      MSG_SIZE = LEN_TRIM( CMSG )
      
      DO WHILE ( MSG_SIZE .GT. LOG_LINE_LENGTH )
         ! Isolate One Line of Text
         LAST_SPACE = INDEX( CMSG( 1:NLINE+1 ), " ", BACK=.TRUE. )
     
         CTEMP = CMSG( 1:LAST_SPACE-1 )

         WRITE( FMT, '("(", I0, "x,A)")' ) NTAB
         WRITE( FUNIT, FMT ), CTEMP

         CMSG = CMSG( LAST_SPACE+1:LEN( CMSG ) )
         MSG_SIZE = LEN_TRIM( CMSG )

         IF ( NTAB .EQ. LOG_MAJOR_TAB ) NTAB = NTAB + LOG_MINOR_TAB
         IF ( NLINE.EQ. NLINE1 ) NLINE = NLINE2
      END DO

      ! Write Last Line
      WRITE( FMT, '("(", I0, "x,A)")' ) NTAB
      WRITE( FUNIT, FMT ), TRIM( CMSG )


      END SUBROUTINE LOG_MESSAGE
      
 
      END MODULE ENV_VARS
