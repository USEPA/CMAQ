
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

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE BIOG_EMIS

C-----------------------------------------------------------------------
C Function: biogenics emissions interface to the speciation profiles file

C Revision History:
C     ?? ??? ???? ?.?????: initial implementation
C     20 Sep 2007 J.Young: inline DSCSPROF, eliminate MODSPRO module
C     16 Feb 2011 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C     16 Aug 2018 G. Sarwar: updated for CB6R3M_AE7_KMTBR
C     01 Feb 2019 D. Wong: Implemented centralized I/O approach, and 
C                          created a new module biog_emis_param_module 
C                          (model_data_module.F) to hold some of information
C                          originally stored here to avoid cyclic dependence
C     10 Mar 2021 G. Sarwar: Changed CB6R3M_AE7_KMTBR to CB6R3M_AE7_AQ
C     10 June 2021 G. Sarwar: Replacd CB6R3M_AE7_AQ with CB6R5M_AE7_AQ
C      Xiaoyang Chen at Northeastern University notified that CMAQ is not generating any monoterpene
C             emissions when in-line option is enabled with RACM2
C     07 June 2021 G. Sarwar: replaced "B10RD" with "B3V10" for RACM2
C     30 Sept 2021 G. Sarwar: added CB6R5_AE7_AQ
C     04 Oct  2021 G. Sarwar: added CB6R5HAP_AE7_AQ
C     14 Sep  2023 G. Sarwar: added CRACMM2
C-----------------------------------------------------------------------
      USE RUNTIME_VARS
      USE DESID_VARS
      USE biog_emis_param_module

      IMPLICIT NONE

      INTEGER, SAVE                      :: MSPCS      ! no. of emitted species
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: EMSPC( : ) ! emitted species names

C Mole and mass factors:
      REAL,            ALLOCATABLE, SAVE :: MLFAC( :,: ) ! mole factors
      REAL,            ALLOCATABLE, SAVE :: MSFAC( :,: ) ! mass factors

C-----------------------------------------------------------------------

      CONTAINS
         FUNCTION BIOG_INIT() RESULT ( SUCCESS )
      
         USE RXNS_DATA, ONLY : MECHNAME
         USE UTILIO_DEFN
         USE CGRID_SPCS, ONLY : N_CGRID_SPC, CGRID_NAME, CGRID_MW
#ifdef mpas
         USE util_module, only : PROMPTFFILE, index1, str2real, sortic
#endif

         IMPLICIT NONE

C Subroutine arguments:
         LOGICAL SUCCESS

C Parameters:
C (A line from the profile file is a record.)
         INTEGER,        PARAMETER :: NSEGS = 6  ! # of potential line segments (fields)
         INTEGER,        PARAMETER :: MXLINES = 100 ! max lines for requested sppro name
         INTEGER,        PARAMETER :: MXSPPOL = 10  ! max no. spc per pollutant
         INTEGER,        PARAMETER :: NMECHS  = 21  ! dimension for number of mechanisms considered
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
         INTEGER        NDX, INDX  ! index position

         LOGICAL     :: INHEADER = .FALSE.   ! true: in file header

         CHARACTER( 256 ) :: LINE       ! buffer for profile record
         CHARACTER( 256 ) :: MESG       ! message buffer
         CHARACTER(  16 ) :: SPNPRF     ! record (line) speciation profile name
         CHARACTER(  16 ) :: POLNAM     ! record (line) pollutant name
         CHARACTER(  16 ) :: SPECNM     ! record (line) species name
         CHARACTER(  16 ) :: FILE_SPPRO ! label read from gspro

         REAL                SPLTFAC, SDIV, SMFAC ! line speciation profile factors
 
         TYPE BIOG_MECH_TYPE
              CHARACTER( 32 ) :: CHEMMECH
              CHARACTER( 16 ) :: BIOGMECH
         END TYPE BIOG_MECH_TYPE

         TYPE( BIOG_MECH_TYPE ) :: BIOG_MECH_MAP( NMECHS ) = (/
     &         BIOG_MECH_TYPE( 'CB05E51_AE6_AQ         ','B10C5   '),
     &         BIOG_MECH_TYPE( 'CB05EH51_AE6_AQ        ','B10C5   '),
     &         BIOG_MECH_TYPE( 'CB05MP51_AE6_AQ        ','B10C5   '),
     &         BIOG_MECH_TYPE( 'CB05TUCL51_AE6_AQ      ','B10C5   '),
     &         BIOG_MECH_TYPE( 'CB6R3_AE6_AQ           ','B10C6   '),
     &         BIOG_MECH_TYPE( 'CB6MP_AE6_AQ           ','B10C6   '),
     &         BIOG_MECH_TYPE( 'CB6R5HAP_AE7_AQ        ','B10C6AE7'),
     &         BIOG_MECH_TYPE( 'CB6R3_AE7_AQ           ','B10C6AE7'),     
     &         BIOG_MECH_TYPE( 'CB6R5_AE7_AQ           ','B10C6AE7'),     
     &         BIOG_MECH_TYPE( 'CB6R3_AE7_AQKMT2       ','B10C6AE7'),     
     &         BIOG_MECH_TYPE( 'CB6R5M_AE7_AQ          ','B10C6AE7'),
     &         BIOG_MECH_TYPE( 'RACM2_AE6_AQ           ','B3V10   '),
     &         BIOG_MECH_TYPE( 'SAPRC07TB_AE6_AQ       ','B10SP   '),
     &         BIOG_MECH_TYPE( 'SAPRC07TC_AE6_AQ       ','B10SP   '),
     &         BIOG_MECH_TYPE( 'SAPRC07TIC_AE6I_AQ     ','B10SP   '),
     &         BIOG_MECH_TYPE( 'SAPRC07TIC_AE6I_AQKMTI ','B10SP   '),
     &         BIOG_MECH_TYPE( 'SAPRC07TIC_AE7I_AQ     ','B10SP   '),
     &         BIOG_MECH_TYPE( 'SAPRC07TIC_AE7I_AQKMT2 ','B10SP   '),
     &         BIOG_MECH_TYPE( 'CRACMM2                ','CRACMM2 '),
     &         BIOG_MECH_TYPE( 'CRACMM1_AQ             ','CRACMM1 '),
     &         BIOG_MECH_TYPE( 'CRACMM1AMORE_AQ        ','CRACMM1 ') /)
 
         CHARACTER( 16 ) :: PNAME = 'BIOG_INIT'

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

C Open speciation profiles file
         RDEV = PROMPTFFILE(
     &            'Enter logical name for Speciation Profiles file',
     &            .TRUE., .TRUE., 'GSPRO', PNAME )

         IF ( SPPRO .EQ. 'DEFAULT' ) THEN 
            INDX = INDEX1( MECHNAME, NMECHS, BIOG_MECH_MAP%CHEMMECH )
            SPPRO = BIOG_MECH_MAP( INDX )%BIOGMECH
            WRITE( LOGDEV, '(5X,A,A,A)' ), 'Accessing compatible biogenic ',
     &             'emissions mechanism: ',TRIM( SPPRO )
         END IF

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

C Skip lines that dont reference the requested speciation profile name
            IF ( LINE( 1:1 ) .EQ. '"' ) THEN
               FILE_SPPRO = LINE( 2: INDEX( LINE,'"' )-1 )
               IF ( FILE_SPPRO .NE. SPPRO ) CYCLE 
            ELSE
               FILE_SPPRO = LINE( 1: INDEX( LINE,';' )-1 )
               IF ( FILE_SPPRO .NE. SPPRO ) CYCLE 
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
            IF ( N .GT. 16 ) THEN
               WRITE( MESG,94010 ) 'ERROR: Speciation profile code ' //
     &                'exceeds max width of 16 characters at line', IREC
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
#ifdef mpas
               NDX = INDEX1( SPECNM, K, TMPNAMES( :,IPOS( M ) ) )
#else
               NDX = INDEX1( SPECNM, K, TMPNAMES( 1,IPOS( M ) ) )
#endif
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
#ifdef mpas
               NDX = INDEX1( SPECNMA( K ), NSPECA( I ), TMPNAMES( :,I ) )
#else
               NDX = INDEX1( SPECNMA( K ), NSPECA( I ), TMPNAMES( 1,I ) )
#endif
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
C Find emitted CMAQ species names
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

C Save Species names in global array for mapping emissions
         DESID_EMVAR( IBIOSRM )%LEN = MSPCS
         ALLOCATE( DESID_EMVAR( IBIOSRM )%ARRY ( MSPCS ) )
         ALLOCATE( DESID_EMVAR( IBIOSRM )%UNITS( MSPCS ) )
         ALLOCATE( DESID_EMVAR( IBIOSRM )%MW   ( MSPCS ) )
         ALLOCATE( DESID_EMVAR( IBIOSRM )%USED ( MSPCS ) )
         ALLOCATE( DESID_EMVAR( IBIOSRM )%CONV ( MSPCS ) )
         ALLOCATE( DESID_EMVAR( IBIOSRM )%BASIS( MSPCS ) )
         ALLOCATE( DESID_EMVAR( IBIOSRM )%LAREA( MSPCS ) )
         ALLOCATE( DESID_EMVAR( IBIOSRM )%LAREAADJ( MSPCS ) )

         DESID_EMVAR( IBIOSRM )%ARRY  = EMSPC
         DESID_EMVAR( IBIOSRM )%UNITS = 'MOLES/S'
         DESID_EMVAR( IBIOSRM )%MW    = 1.0
         DESID_EMVAR( IBIOSRM )%USED  = .FALSE.
         DESID_EMVAR( IBIOSRM )%CONV  = 1.0
         DESID_EMVAR( IBIOSRM )%BASIS = 'MOLE'
         DESID_EMVAR( IBIOSRM )%LAREA = .FALSE.
         DESID_EMVAR( IBIOSRM )%LAREAADJ = .FALSE.

         ! Populate the Molecular Weight Field by Matching Gas Species
         ! to the CMAQ mechanism species
         DO ISP = 1,MSPCS
             INDX = INDEX1( EMSPC( ISP ), N_CGRID_SPC, CGRID_NAME )
             IF ( INDX .NE. 0 ) THEN
                 DESID_EMVAR( IBIOSRM )%MW( ISP ) = CGRID_MW( INDX )
             ELSE
                 WRITE( MESG, '(A,A,A,A)' ) 'WARNING: BEIS emission species ',
     &               EMSPC( ISP ),' is not found in the Gas-Phase Mechanism. ',
     &               'The emission variable molecular weight will be set to 1.0'
                 CALL LOG_MESSAGE( LOGDEV, MESG )
                 DESID_EMVAR( IBIOSRM )%MW( ISP ) = 1.0
             END IF

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

C Skip lines that dont reference the requested speciation profile name
            IF ( LINE( 1:1 ) .EQ. '"' ) THEN
               FILE_SPPRO = LINE( 2: INDEX( LINE,'"' )-1 )
               IF ( FILE_SPPRO .NE. SPPRO ) CYCLE 
            ELSE
               FILE_SPPRO = LINE( 1: ( INDEX( LINE,';' )-1 ) )
               IF ( FILE_SPPRO .NE. SPPRO ) CYCLE 
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
