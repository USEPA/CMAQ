
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/emis/emis/BIOG_EMIS.F,v 1.5 2012/01/19 15:23:37 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE BIOG_EMIS

C-----------------------------------------------------------------------
C Function: biogenics emissions interface to the speciation profiles file

C Revision History:
C     ?? ??? ???? ?.?????: initial implementation
C     20 Sep 2007 J.Young: inline DSCSPROF, eliminate MODSPRO module
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN

C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER, SAVE                      :: MSPCS      ! no. of emitted species
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EMSPC( : ) ! emitted species names

C Mole and mass factors:
      REAL,            ALLOCATABLE, SAVE :: MLFAC( :,: ) ! mole factors
      REAL,            ALLOCATABLE, SAVE :: MSFAC( :,: ) ! mass factors

C----------------------------------------------------------------------
C Former include file  B3V12DIMS3.EXT
C dimensioning parameters for BEIS v3.12 

      INTEGER, PARAMETER :: NSEF     = 35 ! # of em class in Summer emis factor table
      INTEGER, PARAMETER :: NSEASONS = 2  ! number of seasons modeled (2 for now)
      INTEGER, PARAMETER :: NLAI     = 3  ! no. of different LAIs (ISOP and MBO)
      INTEGER, PARAMETER :: NSUMMER  = 1  ! summer flag
      INTEGER, PARAMETER :: NWINTER  = 2  ! winter flag

C Names for BEIS species, emitted model species
C DO NOT CHANGE THE ORDER OF THESE ARRAYS

      CHARACTER( 5 ) :: BIOTYPES( NSEF ) =
     &                          (/ 'ISOP ', 'MBO  ', 'METH ', 
     &                             'APIN ', 'BPIN ', 'D3CAR', 'DLIM ',
     &                             'CAMPH', 'MYRC ', 'ATERP', 'BPHE ', 
     &                             'SABI ', 'PCYM ', 'OCIM ', 'ATHU ',
     &                             'TRPO ', 'GTERP', 'ETHE ', 'PROPE',
     &                             'ETHO ', 'ACET ', 'HEXA ', 'HEXE ',
     &                             'HEXY ', 'FORM ', 'ACTAL', 'BUTE ',
     &                             'ETHA ', 'FORAC', 'ACTAC', 'BUTO ',
     &                             'CO   ', 'ORVOC', 'SESQT',
     &                             'NO   ' /)

      CHARACTER( 5 ) :: LAITYPES( NLAI ) =
     &                          (/ 'ISOP ', 'MBO  ', 'METH ' /)

      CHARACTER( 1 ) :: SEASON( NSEASONS ) =
     &                          (/ 'S', 'W' /)

C-----------------------------------------------------------------------

      CONTAINS
         FUNCTION BIOG_INIT() RESULT ( SUCCESS )

         USE UTILIO_DEFN

         IMPLICIT NONE

C Subroutine arguments:
         LOGICAL SUCCESS

C External Functions:
         INTEGER,        EXTERNAL :: GETFLINE

C Parameters:
C (A line from the profile file is a record.)
         INTEGER,        PARAMETER :: NSEGS = 6  ! # of potential line segments (fields)
         INTEGER,        PARAMETER :: MXLINES = 100 ! max lines for requested sppro name
         INTEGER,        PARAMETER :: MXSPPOL = 10  ! max no. spc per pollutant
         CHARACTER,      PARAMETER :: CINVHDR = '#' ! Indicator for inventory hdr fields
!        REAL,           PARAMETER :: GM2TON  = 1.0 / 907184.74  ! grams to tons

C unit number for speciation profiles file
         INTEGER :: RDEV

C Header definitions for NONHAP pollutants 
         CHARACTER( 7 ), PARAMETER :: HDRSTART = '/NONHAP' ! start of header
         CHARACTER( 5 ), PARAMETER :: HDREND   = '/END/'   ! end of header

         INTEGER :: MXSPEC    ! max no. of species per pol

C Table of species names per inventory pollutant
         CHARACTER( 16 ), ALLOCATABLE :: SPCNAMES( :,: )
C Table of mole-based units per inventory pollutant for all species
         CHARACTER( 16 ), ALLOCATABLE :: MOLUNITS( :,: )

C Arrays for getting pollutant-specific information from file
         INTEGER         :: NSPECA ( NSEF )     ! number of species per pollutant
         CHARACTER( 16 ) :: POLNAMA( NSEF )     ! unsorted pollutant names
         CHARACTER( 20 ) :: SEGMENT( NSEGS )    ! Segments of parsed lines
         INTEGER         :: INDX1A  ( MXLINES ) ! sorting index for SPECNMA
         CHARACTER( 16 ) :: SPECNMA ( MXLINES ) ! unsort spcs names
         CHARACTER( 16 ) :: TMPNAMES( MXSPPOL,NSEF ) ! unsort names per pollutant
         LOGICAL         :: LMOLAR  ( MXLINES ) ! true: moles conversion is not mass

         INTEGER         :: IPOS( MXSPPOL )   ! position in input pollutant list

C Local variables
         INTEGER        I, J, K, M, N ! counters and indices
         INTEGER        ICOUNT     ! tmp counter while populating SPCNAMES
         INTEGER        IOS        ! i/o status
         INTEGER        POL        ! pollutant counter
         INTEGER        IREC       ! record counter
         INTEGER        ISP        ! species names counter
         INTEGER        NIPOS      ! number of pollutant matches
         INTEGER        PNDX       ! position (from INDEX1) of pol in POLNAMA
         INTEGER        SNDX       ! position (from INDEX1) of pol in SPECNMA
         INTEGER        NDX        ! index position

         LOGICAL     :: INHEADER = .FALSE.   ! true: in file header

         CHARACTER( 256 ) :: LINE       ! buffer for profile record
         CHARACTER( 256 ) :: MESG       ! message buffer
         CHARACTER(   5 ) :: SPNPRF     ! record (line) speciation profile name
         CHARACTER(  16 ) :: POLNAM     ! record (line) pollutant name
         CHARACTER(  16 ) :: SPECNM     ! record (line) species name
         CHARACTER(   5 ) :: SPPRO      ! requested speciation profile name

         REAL                SPLTFAC, SDIV, SMFAC ! line speciation profile factors

         CHARACTER( 16 ) :: PNAME = 'BIOG_INIT'
#ifdef Verbose
         integer logdev   ! debug
#endif

C-----------------------------------------------------------------------

#ifdef Verbose
         logdev = init3()   ! debug
#endif
         SUCCESS = .TRUE.

C Open speciation profiles file
         RDEV = PROMPTFFILE(
     &            'Enter logical name for Speciation Profiles file',
     &            .TRUE., .TRUE., 'GSPRO', PNAME )
C                 rdonly  fmtted

C Get the speciation profile to use
         MESG = 'Speciation profile to use for biogenics'
         CALL ENVSTR( 'BIOG_SPRO', MESG, 'B3V10', SPPRO, IOS )

C Scan speciation profiles file to get all of the pollutant-species combinations
C that are valid for the pollutants in the inventory. The species names are sorted
C in alphebetical order for each pollutant, and the pollutants are in the same order
C as BIOTYPES. Also retrieve the maximum number of species per pollutant and the
C maximum number of profile entries per pollutant.

C Initialize species count per pollutant and flag for indicating true molar
C conversions (NOTE - for some pollutants like PM10, there are no mole-based
C factor and outputs should be in units of gm/mole in the mole-base speciation
C matrix)
         NSPECA   = 0        ! array
         POLNAMA  = ' '      ! array
         TMPNAMES = ' '      ! array
         LMOLAR   = .FALSE.  ! array

C Read through input file to determine the total number of pollutants in the input
C file, to determine the number of profiles per pollutant, to store the unique
C species names, and to store the units for mass-based and mole-based conversions
         ICOUNT = 1
         POL    = 0
         ISP    = 0
         IREC   = 0
         DO
            READ( RDEV, 93000, END=1999, IOSTAT=IOS ) LINE
            IREC = IREC + 1
            IF ( IOS .GT. 0 ) THEN
               WRITE( MESG, 94010 )
     &               'I/O error', IOS, 'reading speciation profile ' //
     &               'file at line', IREC
               CALL M3MESG( MESG )
               SUCCESS = .FALSE.; RETURN
            END IF

C Skip blank and comment lines
            IF ( LINE .EQ. ' ' .OR. LINE( 1:1 ) .EQ. CINVHDR ) CYCLE

C Skip all lines until the end of the header...
C Check for header start
            NDX = INDEX( LINE, HDRSTART )
            IF ( NDX .GT. 0 ) INHEADER = .TRUE.

            NDX = INDEX( LINE, HDREND )
            IF ( INHEADER ) THEN
               IF ( NDX .GT. 0 ) INHEADER = .FALSE.
               CYCLE
            ELSE IF ( NDX .GT. 0 ) THEN
               WRITE( MESG,94010 ) 'Header end found before header ' //
     &                             'started at line', IREC
               CALL M3MESG( MESG )
               SUCCESS = .FALSE.; RETURN
            END IF

C Skip lines that don't reference the requested speciation profile name
            IF ( LINE( 1:1 ) .EQ. '"' ) THEN
               IF ( LINE( 2:6 ) .NE. SPPRO ) CYCLE
            ELSE
               IF ( LINE( 1:5 ) .NE. SPPRO ) CYCLE
            END IF

C Separate the line of data (record) into the segments (parse the record fields)
            CALL PARSLINE( LINE, NSEGS, SEGMENT )

C Left-justify character strings and convert factors to reals
            SPNPRF  = ADJUSTL ( SEGMENT( 1 ) )
            POLNAM  = ADJUSTL ( SEGMENT( 2 ) )
            SPECNM  = ADJUSTL ( SEGMENT( 3 ) )
            SPLTFAC = STR2REAL( SEGMENT( 4 ) )
            SDIV    = STR2REAL( SEGMENT( 5 ) )
            SMFAC   = STR2REAL( SEGMENT( 6 ) )

C Check width of character fields of fixed length
            N = LEN_TRIM( SPNPRF )
            IF ( N .GT. 5 ) THEN
               WRITE( MESG,94010 ) 'ERROR: Speciation profile code ' //
     &                 'exceeds max width of 5 characters at line', IREC
               CALL M3MESG( MESG )
               SUCCESS = .FALSE.; RETURN
            END IF

            N = LEN_TRIM( POLNAM )
            IF ( N .GT. 16 ) THEN
               WRITE( MESG,94010 ) 'ERROR: Pollutant name ' //
     &                'exceeds max characters of 16 at line', IREC
               CALL M3MESG( MESG )
               SUCCESS = .FALSE.; RETURN
            END IF

            N = LEN_TRIM( SPECNM )
            IF ( N .GT. 16 ) THEN
               WRITE( MESG,94010 ) 'ERROR: Species name ' //
     &                 'exceeds max characters of 16 at line', IREC
               CALL M3MESG( MESG )
               SUCCESS = .FALSE.; RETURN
            END IF

C Make sure divisor factor is not zero
            IF ( SDIV .EQ. 0.0 ) THEN
               WRITE( MESG,94010 ) 'ERROR: Zero divisor found at line ', IREC
               CALL M3MESG( MESG )
               SUCCESS = .FALSE.; RETURN
            END IF

C Search for pollutant in list of valid names, and go to the end of the loop if
C none found (skip entry).  Record number and position of all matches.
            M    = 0
            IPOS = 0   ! array
            DO N = 1, NSEF
               IF ( POLNAM .EQ. BIOTYPES( N ) ) THEN
                  M = M + 1
                  IF ( M .LE. MXSPPOL ) THEN
                     IPOS( M ) = N
                  ELSE      ! Max of 10 profile pollutant names per biotype
                     MESG = 'ERROR: Exceeded max pollutant names' //
     &                      'per biotype in ' // PNAME
                     CALL M3MSG2( MESG )
                     MESG = 'Quitting'
                     CALL M3MESG( MESG )
                     SUCCESS = .FALSE.; RETURN
                  END IF
               END IF
            END DO
            NIPOS = M

            IF ( MAXVAL( IPOS ) .EQ. 0 ) CYCLE

C Build unique pollutant name list (POLNAMA) from list of all profile file pollutants
            PNDX = INDEX1( POLNAM, POL, POLNAMA )

            IF ( PNDX .LE. 0 ) THEN     ! if current POLNAM is not in POLNAMA, then
               POL = POL + 1            ! increment counter, and
               POLNAMA( POL ) = POLNAM  ! add POLNAM to POLNAMA
            END IF

C Build unique species name list (SPECNMA) from list of profile file species names
            SNDX = INDEX1( SPECNM, ISP, SPECNMA )

            IF ( SNDX .LE. 0 ) THEN     ! if current SPECNM is not in SPECNMA, then
               ISP = ISP + 1
               INDX1A ( ISP ) = ISP     ! add to index
               SPECNMA( ISP ) = SPECNM  ! add SPECNM to SPECNMA

C If mole-based = mass based, then use molar transform
               IF ( SPLTFAC / SDIV .NE. SMFAC ) LMOLAR( ISP ) = .TRUE.

            END IF

C Check if species is already stored for current pollutant, and if not, increment
C species-per-pollutant counter and add species to list.
            DO M = 1, NIPOS
               K   = NSPECA( IPOS( M ) )
               NDX = INDEX1( SPECNM, K, TMPNAMES( 1,IPOS( M ) ) )
               IF ( NDX .LE. 0 ) THEN
                  K = K + 1
                  IF ( K .LE. MXSPPOL ) THEN
                     TMPNAMES( K,IPOS( M ) ) = SPECNM
                  ELSE
                     MESG = 'ERROR: Exceeded TMPNAMES dimension'
                     CALL M3MESG( MESG )
                     SUCCESS = .FALSE.; RETURN
                  END IF
                  NSPECA( IPOS( M ) ) = K
               END IF
            END DO

         END DO   ! infinite read loop

1999     CONTINUE ! end reading speciation profile input lines

         IF ( POL .EQ. 0 ) THEN
            MESG = 'ERROR: No pollutants found in speciation ' //
     &             'profiles that match the inventory!'
            CALL M3MESG( MESG )
            SUCCESS = .FALSE.; RETURN
         END IF

         IF ( ISP .EQ. 0 ) THEN
            MESG = 'ERROR: No species found in speciation profile!'
            CALL M3MESG( MESG )
            SUCCESS = .FALSE.; RETURN
         END IF

C max number of species per pollutant
         MXSPEC  = MAXVAL( NSPECA )

C Allocate memory for species names array and units to use for mole-based
C transformations.
         ALLOCATE( SPCNAMES( MXSPEC,NSEF ), STAT=IOS )
         CALL CHECKMEM( IOS, 'SPCNAMES', PNAME )
         ALLOCATE( MOLUNITS( MXSPEC,NSEF ), STAT=IOS )
         CALL CHECKMEM( IOS, 'MOLUNITS', PNAME )

         SPCNAMES = ' '   ! array
         MOLUNITS = ' '   ! array

C Sort master species names
         CALL SORTIC( ISP, INDX1A, SPECNMA )  ! sort on INDX1A

C Cycle through count of all valid pollutants (NSEF) and all species associated
C with these pollutants (ISP).  Check if species is valid for the current pollutant,
C and if so, store in the output species name list.
         DO I = 1, NSEF
            ICOUNT = 0
            DO J = 1, ISP
C Process species in sorted order
               K = INDX1A( J )
C Find species in list of valid species per pollutant
               NDX = INDEX1( SPECNMA( K ), NSPECA( I ), TMPNAMES( 1,I ) )
               IF ( NDX .GT. 0 ) THEN
                  ICOUNT = ICOUNT + 1
                  SPCNAMES( ICOUNT, I ) = SPECNMA( K )
C When the species does not have molar factors, store the molar units as mass units
                  IF ( LMOLAR( K ) ) THEN
                     MOLUNITS( ICOUNT, I ) = 'moles/ton'
                  ELSE
                     MOLUNITS( ICOUNT, I ) = 'g/ton'
                  END IF
               END IF
            END DO
         END DO

C Reposition sequential file for second pass

         REWIND( RDEV )

         ALLOCATE( EMSPC( MXLINES ), STAT=IOS )
         CALL CHECKMEM( IOS, 'EMSPC', PNAME )

         EMSPC = ' '   ! array initialization

#ifdef Verbose
         write( logdev,* ) '    Biogenic emissions species:'
         write( logdev,* ) '    Pol POLNAMES  Spc SPCNAMES         Mspcs EMSPC'
#endif
C Find emitted species names
         MSPCS = 0
         DO POL = 1, NSEF
            DO ISP = 1, MXSPEC
               IF ( SPCNAMES( ISP,POL ) .EQ. ' ' ) CYCLE
               NDX = INDEX1 ( SPCNAMES( ISP,POL ), MSPCS, EMSPC ) 
               IF ( NDX .EQ. 0 ) THEN
                  MSPCS = MSPCS + 1
                  EMSPC( MSPCS ) = SPCNAMES( ISP,POL )
#ifdef Verbose
                  write( logdev,'( 5X, I3, 1X, A5, 5X, I3, 1X, A16, I4, 3X, A16 )' )
     &                   pol, biotypes( pol ), isp, spcnames( isp,pol ), mspcs,
     &                   emspc( mspcs )
               else
                  write( logdev,'( 5X, I3, 1X, A5, 5X, I3, 1X, A16 )' )
     &                   pol, biotypes( pol ), isp, spcnames( isp,pol )
#endif
               END IF
            END DO
         END DO

C Allocate memory for storing mole- and mass-based factors
         ALLOCATE( MLFAC( MSPCS,NSEF ), STAT=IOS )
         CALL CHECKMEM( IOS, 'MLFAC', PNAME )
         ALLOCATE( MSFAC( MSPCS,NSEF ), STAT=IOS )
         CALL CHECKMEM( IOS, 'MSFAC', PNAME )
         MLFAC = 0.0   ! array
         MSFAC = 0.0   ! array

C Read through input file to determine the total number of pollutants in the
C input file, to determine the number of profiles per pollutant, to store the
C unique species names, and to store the units for mass-based and mole-based
C conversions
         IREC = 0
         DO
            READ( RDEV, 93000, END=2999, IOSTAT=IOS ) LINE
            IREC = IREC + 1
            IF ( IOS .GT. 0 ) THEN
               WRITE( MESG, 94010 )
     &              'I/O error', IOS, 'reading speciation profile ' //
     &              'file at line', IREC
               CALL M3MESG( MESG )
               SUCCESS = .FALSE.; RETURN
            END IF

C Skip blank and comment lines
            IF ( LINE .EQ. ' ' .OR. LINE( 1:1 ) .EQ. CINVHDR ) CYCLE

C Skip lines that don't reference the requested speciation profile name
            IF ( LINE( 1:1 ) .EQ. '"' ) THEN
               IF ( LINE( 2:6 ) .NE. SPPRO ) CYCLE
            ELSE
               IF ( LINE( 1:5 ) .NE. SPPRO ) CYCLE
            END IF

C Separate the line of data into each part
            CALL PARSLINE( LINE, NSEGS, SEGMENT )

C Left-justify character strings and convert factors to reals
            SPNPRF  = ADJUSTL ( SEGMENT( 1 ) )
            POLNAM  = ADJUSTL ( SEGMENT( 2 ) )
            SPECNM  = ADJUSTL ( SEGMENT( 3 ) )
            SPLTFAC = STR2REAL( SEGMENT( 4 ) )
            SDIV    = STR2REAL( SEGMENT( 5 ) )
            SMFAC   = STR2REAL( SEGMENT( 6 ) )

C Search for pollutant in list of valid names, and go to the end of the loop if
C not found (skip entry)
            PNDX = INDEX1( POLNAM, NSEF, BIOTYPES )
            IF ( PNDX .EQ. 0 ) CYCLE
            SNDX = INDEX1( SPECNM, MSPCS, EMSPC )
            IF ( SNDX .GT. 0 ) THEN
               MLFAC( SNDX,PNDX ) = SPLTFAC / SDIV
!              MSFAC( SNDX,PNDX ) = SMFAC * GM2TON
               MSFAC( SNDX,PNDX ) = SMFAC
            END IF

         END DO   ! infinite read loop

2999     CONTINUE ! end reading speciation profile input lines

         RETURN

C-----------------------------------------------------------------------

93000    FORMAT( A )
94010    FORMAT( 10( A, :, I8, :, 1X ) )

         END FUNCTION BIOG_INIT

      END MODULE BIOG_EMIS
