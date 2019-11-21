#ifdef sens
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
C $Header: /project/work/rep/CCTM/src/init/yamo_ddm3d/sinput.F,v 1.1.1.1 2009/09/29 13:49:13 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE SINPUT( STDATE, STTIME )

C--------------------------------------------------------------------
C Function: SINPUT
C   Reads from the sensitivity input file to determine the desired
C   number of sensitivity parameters, their type (e.g., sens to emissions
C   or sens to init conditions) and associated species (IPARM)
C   
C Adapted by Daniel Cohan, Jan 2002, from code written for MAQSIP by 
C   Amir Hakami
C Updated for compatibility with Intel compiler, Jan. 2004, which
C   involved major changes to define variables as pointers.
C   REGIONS file is now read within sinput rather than in rdregion,
C   and SENS.EXT and REGIONS.EXT files are no longer needed.
C   Changed MAXDATES to NDAYS.
C Updated Aug 2006 for reaction rate sensitivities.
C   11 Aug 2011 S.L. Napelenok: restructured emissions options for inline
C   20 Nov 2012 S.L. Napelenok: update for cmaq 5.0.1
C   21 Oct 2015 S.L. Napelenok: update for cmaq 5.1.1
C                               shorten SEN_PAR to only 3 characters due
C                               to long species names
C   20 Nov 2019 S.L. Napelenok: update for cmaq 5.3.1
C                               remove MY_N*; clean-up variable declarations;
C                               update regional and emissions tream logic;
C
C
C--------------------------------------------------------------------

      USE UTILIO_DEFN
      USE DDM3D_DEFN
      USE HGRD_DEFN, ONLY: NCOLS, NROWS, GL_NCOLS, GL_NROWS, COLSX_PE, ROWSX_PE, MYPE
      USE VGRD_DEFN, ONLY: NLAYS
      USE RXNS_DATA, ONLY: NRXNS

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID                 ! file name parameters

      CHARACTER( 80 ) :: TXT, TXT2, EMISTYPE
      INTEGER      STDATE                    ! starting date,    format YYYYDDD
      INTEGER      STTIME                    ! starting time,    format HHMMSS
      INTEGER      NSPC( NPMAX )             ! number of species chosen
                                             ! for that sensitivity parameter
      INTEGER      SPCORDER( NPMAX, NSPCSD ) ! holds the species numbers
                                             ! in the order in which they
                                             ! are specified in the input file

      REAL AMOUNT                            ! used to read in emission rate
      INTEGER HOUR                           ! counter over the hours
      INTEGER LAYER                          ! used to read in layer
      INTEGER RXN                            ! used to read in reaction number
      INTEGER TIME1, TIME2                   ! used to read in begin & end time
      INTEGER DATE                           ! used to read in desired date
      INTEGER NDATE                          ! counter over the dates
      INTEGER NUMDATES                       ! number of dates for that sens
      INTEGER LASTDATE                       ! STDATE + NDAYS;
                                             ! note that this may be gt the actual
                                             ! enddate of the simulation

      INTEGER      NPSEN                     ! num of sens parameters
      INTEGER      JVUNIT
      INTEGER      NUM                       ! spc indx num returned by
                                             ! SEN_NUM or REAC_NUM
      INTEGER      P, LVL, ROW, COL
      INTEGER      GCOL, GROW, GLVL
      INTEGER      COLMIN, COLMAX, ROWMIN, ROWMAX, LVLMIN, LVLMAX

      INTEGER      NREG, NGCELLS, NCORNERS, NCIRCLES
      REAL         GRIDSIZE
      INTEGER      C_CENTER, R_CENTER, C_MIN, C_MAX, R_MIN, R_MAX
      REAL         MINRAD, MAXRAD
      INTEGER      C_DIST, R_DIST
      REAL         TOT_DIST
      INTEGER      R, R2
      INTEGER      GCELL, CORNER, CIRCLE
      INTEGER SPC, S

      CHARACTER( 255) :: INFLNAME          ! input file name
      CHARACTER( 16 ) :: SEN_INPUT = 'SEN_INPUT       '  ! input file name
      CHARACTER( 1  ) :: BLANK  = ' '
      CHARACTER( 2  ) :: BLANK2 = '  '
      CHARACTER( 3  ) :: BLANK3 = '   '
      CHARACTER( 10 ) :: BLANK10= '          '
      CHARACTER( 1  ) :: JUNK
      CHARACTER( 1  ) :: JUNKA
      CHARACTER( 1  ) :: JUNKB, JUNKC, JUNKD
      CHARACTER( 2  ) :: JUNK2
      CHARACTER( 16 ), SAVE :: PNAME = 'SINPUT'
      CHARACTER( 120) :: XMSG = ' '

      INTEGER      ALLOCSTAT

      CHARACTER( 80 )     :: VARDESC       ! environment variable description
      INTEGER      STATUS                  ! ENVYN status

C Variables used for regional sensitivity calculations
      REAL, ALLOCATABLE :: RGNS ( :,:,: )
      CHARACTER( 16 ), ALLOCATABLE :: REGNNAME ( : )
      INTEGER REGNTIME, REGNDATE, NREGIONS
      INTEGER     GXOFF, GYOFF              ! global origin offset from file
      INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
      INTEGER     LOGDEV
      INTEGER I

C Variables used for hddm-3d
      INTEGER      CHECK

      INTERFACE
         SUBROUTINE SEN_NUM( TEMP, NUM )
            CHARACTER( * ), INTENT( IN ) :: TEMP
            INTEGER, INTENT( OUT )       :: NUM
         END SUBROUTINE SEN_NUM
         SUBROUTINE REAC_NUM( TEMP, NUM )
            CHARACTER( * ), INTENT( IN ) :: TEMP
            INTEGER, INTENT( OUT )       :: NUM
         END SUBROUTINE REAC_NUM
      END INTERFACE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc

      LOGDEV = INIT3 ()

C Initialize IPT, IPARM, IREGION, IRXN to 0; 
C SEN_PAR to blank
      GRID_FILE = ' '
      PT3D_FILE = ' '
      GRID_NUM = 0
      PT3D_NUM = 0

      IPT = 0
      NSPC = 0
      SEN_PAR = BLANK
      IRXN = 0
      IPARM = 0
      IAMOUNT = 0.0
      IDATE = 0
      ITIME( :,1 ) = 0
      ITIME( :,2 ) = 240000
      ILAYER = 1
      IREGION  = 0.0

      S_NSTREAMS( : ) = 0
      S_STREAMLBL( :,: ) = 'empty' 
      

      NPSEN = 0

C Open input file SEN_INPUT
      CALL NAMEVAL ( SEN_INPUT, INFLNAME )
      JVUNIT = JUNIT( )

      OPEN ( UNIT = JVUNIT, FILE=INFLNAME, STATUS = 'OLD') 

      ! Read in name of sensitivity
      READ(JVUNIT,'(A80)',END=999) TXT
      IF ( TXT( :1 ) .EQ. BLANK ) THEN
          XMSG = 'Expecting sensitivity name. ' //
     &           'Cannot begin with a space.'
          CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
      END IF

  20  CONTINUE
      NPSEN = NPSEN + 1

      IF ( NPSEN .GT. NPMAX ) THEN
        XMSG = 'NPSEN exceeds NPMAX.  Change NPMAX in run script.'
        CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
      END IF

      SEN_PAR( NPSEN ) = TXT( 1:3 ) 

      ! Read in the type of sensitivity 
      ! (only INIT, EMIS, BOUN, RATE, HIGH, and PVO3 implemented so far)
      READ( JVUNIT, '(A80)', END = 999 ) TXT
c       IF ( TXT( :1 ) .NE. BLANK ) THEN
c         XMSG = 'Need one space before specifying the sentype.'
c         CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
c       END IF

CCCCCCC Sensitivity to Emissions CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF ( INDEX( TXT, 'EMIS' ) .NE. 0 ) THEN
          IPT( NPSEN ) = 3
  30      CONTINUE

          ! Check if an emissions label has been specified. Otherwise, assume total.
          READ( JVUNIT, '(A80)', END = 999 ) EMISTYPE

          IF ( INDEX(EMISTYPE,'SPECIES' .NE. 0 )  THEN ! assume all streams
            S_NSTREAMS( NPSEN ) = 99
            GOTO 35 ! 
          END IF

          IF ( INDEX(EMISTYPE,'TOTA') .NE. 0 ) THEN
            S_NSTREAMS( NPSEN ) = 99 ! total emissions - all streams
            GOTO 30
          ELSE
            S_NSTREAMS( NPSEN ) = 1 + COUNT(TRANSFER(EMISTYPE, 'A', LEN(EMISTYPE)) == "," )
            READ(EMISTYPE,*) S_STREAMLBL( NPSEN, 1:S_NSTREAMS( NPSEN ) )
          END IF

CCCCCCC Sensitivity to vorticity CCCCCCCCCCCCCCCCCCCCC
        ELSE IF ( INDEX( TXT, 'PVO3' ) .NE. 0 ) THEN
          IPT( NPSEN ) = 6

          READ( JVUNIT, '(A80)', END = 999 ) TXT

          IF ( TXT ( :8 ) .NE. ' SPECIES' ) THEN
            XMSG = 'Expecting header SPECIES ' //
     &             '(allcap, 1 leading space).'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

CCCCCCC Sensitivity to initial conditions CCCCCCCCCCCCCCCCCCCCC
        ELSE IF ( TXT( 2:5 ) .EQ. 'INIT' ) THEN
          IPT( NPSEN ) = 1 

          READ( JVUNIT, '(A80)', END = 999 ) TXT

          IF ( TXT ( :8 ) .NE. ' SPECIES' ) THEN
            XMSG = 'Expecting header SPECIES ' //
     &             '(allcap, 1 leading space).'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF  

CCCCCCC Sensitivity to boundary conditions CCCCCCCCCCCCCCCCCC
        ELSE IF ( TXT( 2:5 ) .EQ. 'BOUN' ) THEN
          IPT( NPSEN ) = 2
          
          READ( JVUNIT, '(A80)', END = 999 ) TXT

          IF ( TXT ( :8 ) .NE. ' SPECIES' ) THEN
            XMSG = 'Expecting header SPECIES ' //
     &             '(allcap, 1 leading space).'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

CCCCCCC Higher-order sensitivity CCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ELSE IF ( TXT( 2:5 ) .EQ. 'HIGH' ) THEN

          IF ( .NOT. HIGH ) THEN
             XMSG = 'hddm-3d option not set'
             CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

          IPT( NPSEN ) = 4

          CHECK = 0

          ! Read the first part of the 2nd order derivative
          ! and match it with an existing parameter
          READ( JVUNIT, '(A80)', END = 999 ) TXT

          ! Set IPARM( NPSEN, P ) to 1 for the P matching the
          ! existing parameter 
          DO P = 1, NPSEN - 1
            IF ( TXT( 2:4 ) .EQ. SEN_PAR( P ) ) THEN
              IPARM( NPSEN, P ) = 1
              CHECK = CHECK + 1
            END IF
          END DO

          IF ( CHECK .NE. 1 ) THEN
            XMSG = 'Could not match sensitivity name ' //
     &             TXT( 2:9 ) //
     &             ' for higher-order sens.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

          CHECK = 0

          ! Read the second part of the 2nd order derivative
          ! and match it with an existing parameter
          READ( JVUNIT, '(A80)', END = 999 ) TXT

          ! Set IPARM( NPSEN, P ) to 2 if this is different
          ! from the first part of the derivative 
          ! ( e.g., d2(O3)/{d(NOx)d(VOC)} )
          ! and to 3 if it is the same parameter (e.g., d2(O3)/d(NOx)2)
          DO P = 1, NPSEN - 1
            IF ( TXT( 2:4 ) .EQ. SEN_PAR( P ) ) THEN
              IPARM( NPSEN, P ) = IPARM( NPSEN, P ) + 2
              CHECK = CHECK + 1
            END IF
          END DO

          IF ( CHECK .NE. 1 ) THEN
            XMSG = 'Could not match sensitivity name ' //
     &             TXT( 2:9 ) //
     &             ' for higher-order sens.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

 32       CONTINUE

          READ( JVUNIT, '(A80)', END = 999 ) TXT

          IF ( TXT( :10 ) .EQ. BLANK10 ) GOTO 32
          IF ( TXT( :3 ) .EQ. 'END' ) GOTO 999
          IF ( TXT( :1 ) .NE. BLANK ) THEN
            GOTO 20
          ELSE
            XMSG = 'Unexpected text ' // TXT
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

CCCCCCC Sensitivity to a chemical reaction rate constant CCCCC
        ELSE IF ( TXT( 2:5 ) .EQ. 'RATE' ) THEN
          IPT( NPSEN ) = 5

          READ( JVUNIT, '(A80)', END = 999 ) TXT

          IF ( TXT ( :9 ) .NE. ' REACTION' ) THEN
            XMSG = 'Expecting header REACTION ' //
     &             '(allcap, 1 leading space).'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

          ! Which reaction?
          READ( JVUNIT, '(A80)', END = 999 ) TXT
          IF ( TXT( :2 ) .NE. BLANK2 ) THEN
             XMSG = 'Expected two blanks before reaction name.'
             CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

 33       CONTINUE 

          ! REAC_NUM finds the reaction number corresponding to the input name
          CALL REAC_NUM( TXT( 3:18 ), NUM )
          IRXN( NPSEN, NUM ) = 1

 34       CONTINUE

          ! Check if there is more than 1 reaction
          READ( JVUNIT, '(A80)', END = 999 ) TXT
          IF ( TXT( :10 ) .EQ. BLANK10 ) GOTO 34
          IF ( TXT( :2 ) .EQ. BLANK2 ) THEN
            GOTO 33 ! Another reaction
          ELSE
            GOTO 50 ! Not another reaction, so check for
                    ! other specifications or new sens parameter
          END IF

CCCCCCC Unrecognized or non-implemented sens type CCCCCCCCCCCC
        ELSE
          XMSG = 'Sentype not recognized: ' // TXT //
     &           '. Use EMIS, INIT, BOUN, RATE, HIGH, or PVO3 '
          CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )

        END IF  ! Finished setting sentype

 35     CONTINUE

CCCCCCC Which species? CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        READ( JVUNIT, '(A80)', END = 999 ) TXT

        IF ( TXT( :2 ) .NE. BLANK2 ) THEN
          XMSG = 'Need two leading spaces before a species.'
          CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
        END IF

        IF ( TXT( :5 ) .EQ. '  ALL' ) THEN
          DO SPC = 1, NSPCSD
            IPARM( NPSEN, SPC ) = 1
            SPCORDER( NPSEN, SPC ) = SPC
          END DO
          GOTO 45
        END IF

 40     CONTINUE

        CALL SEN_NUM( TXT( 3:10 ), NUM ) 
        IPARM( NPSEN, NUM ) = 1
        NSPC( NPSEN ) = NSPC( NPSEN ) + 1
        SPCORDER( NPSEN, NSPC( NPSEN ) ) = NUM

 45     CONTINUE
 
        READ( JVUNIT, '(A80)', END = 999 ) TXT

        IF ( TXT( :10 ) .EQ. BLANK10 ) GOTO 45

        IF ( TXT( :2 ) .EQ. BLANK2 ) GOTO 40 ! Another species

CCCCCCC Specify emission amount? CCCCCCCCCCCCCCCCCCCCCCCCC

        IF ( TXT( :7 ) .EQ. ' AMOUNT' ) THEN
          ! The 25th slot is a flag indicating amount has been specified
          DO SPC = 1, NSPCSD
            IAMOUNT ( NPSEN, SPC, 25 ) = 1.0 * IPARM( NPSEN, SPC )
          END DO

          IF ( IPT( NPSEN ) .NE. 3 ) THEN
            XMSG = 'Amount specification only implemented ' //
     &             'for emissions so far.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

          IF ( S_NSTREAMS( NPSEN ) .NE. 99 ) THEN
            XMSG = 'Amount can only be specified for total emissions.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

          DO S = 1, NSPC( NPSEN )
            SPC = SPCORDER( NPSEN, S )
            READ( JVUNIT, '( a2, f8.2 )', END = 999 )
     &          TXT, AMOUNT

            IF ( TXT( :2 ) .NE. BLANK2 ) THEN
              XMSG = 'Expected two blanks before specifying amount.'
              CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
            END IF

            IF ( AMOUNT .GE. 0 ) THEN
              IAMOUNT( NPSEN, SPC, 1:24 ) = AMOUNT
            ELSE
              DO HOUR = 1, 24
                READ( JVUNIT, '( a2, f8.2 )', END = 999 )
     &            TXT, AMOUNT
                IF ( TXT( :2 ) .NE. BLANK2 ) THEN
                  XMSG = 'Expected two blanks before specifying amount.'
                  CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
                END IF
                IF ( AMOUNT .LT. 0 ) THEN
                  XMSG = 'Amount must be non-negative.'
                  CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
                ELSE
                  IAMOUNT( NPSEN, SPC, HOUR ) = AMOUNT
                END IF
              END DO
            END IF

          END DO  ! S

 48       CONTINUE

          READ( JVUNIT, '(A80)', END = 999 ) TXT

          IF ( TXT( :10 ) .EQ. BLANK10 ) GOTO 48

          ! Which layer?
          IF ( TXT( :7 ) .EQ. '  LAYER' ) THEN
            READ( JVUNIT, '( a3, i3 )', END = 999 )
     &          TXT, LAYER
            IF ( TXT( :3 ) .NE. BLANK3 ) THEN
              XMSG = 'Expected three blanks before layer number.'
              CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
            END IF
            IF ( ( LAYER .LT. 1 ) .OR. ( LAYER .GT. NLAYS ) ) THEN
              XMSG = 'Layer out of range.'
              CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
            ELSE
              ! Set the layer to 1, all others to 0
              ILAYER( NPSEN, 1:NLAYS ) = 0
              ILAYER( NPSEN, LAYER ) = 1
            ENDIF
 49         CONTINUE
            READ( JVUNIT, '(A80)', END = 999 ) TXT
            IF ( TXT( :10 ) .EQ. BLANK10 ) GOTO 49
          ELSE
            ! If layer not specified, assume ground-level emissions
            ILAYER( NPSEN, 1 ) = 1
            ILAYER( NPSEN, 2:NLAYS ) = 0
          END IF

        END IF

CCCCCCC What date(s)? CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 50     CONTINUE

        IF ( TXT( :5 ) .EQ. ' DATE' ) THEN
          IF ( IPT( NPSEN ) .EQ. 1 ) THEN
            XMSG = 'Cannot specify date for initial condition.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

          READ( JVUNIT, '( a2, i7 )', END = 999 )
     &          TXT, NUMDATES

          IF ( TXT( :2 ) .NE. BLANK2 ) THEN
            XMSG = 'Expected two blanks before specifying date.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF
          IF ( ( NUMDATES .LT. 1 ) .OR.
     &         ( NUMDATES .GT. NDAYS ) ) THEN
            XMSG = 'Number of dates out of bounds.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

          LASTDATE = STDATE + NDAYS

          DO NDATE = 1, NUMDATES
            READ( JVUNIT, '( a2, i7 )', END = 999 ) TXT, DATE
            IF ( ( DATE .LT. STDATE ) .OR.
     &           ( DATE .GT. LASTDATE ) ) THEN
              XMSG = 'Date out of bounds.'
              CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
            ELSE
              IDATE( NPSEN, 1 + DATE - STDATE ) = 1
            END IF
          END DO

 52       CONTINUE

          READ( JVUNIT, '(A80)', END = 999 ) TXT

          IF ( TXT( :10) .EQ. BLANK10 ) GOTO 52

        ELSE
          ! If DATES keyword not specified, set all to 1
          IDATE( NPSEN, 1:NDAYS ) = 1
        ENDIF

CCCCCCC What time? CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        IF ( TXT( :5 ) .EQ. ' TIME' ) THEN
          IF ( IPT( NPSEN ) .EQ. 1 ) THEN
            XMSG = 'Cannot specify time for initial condition.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

          READ( JVUNIT, '(a2,i6,a1,i6)', END = 999 )
     &          TXT, TIME1, JUNK, TIME2

          IF ( TXT( :2 ) .NE. BLANK2 ) THEN
            XMSG = 'Expected two blanks before specifying time.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF
          IF ( JUNK .NE. BLANK ) THEN
            XMSG = 'Expected a space between begin time & end time.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF
          IF ( ( TIME1 .LT. 0 ) .OR. ( TIME1 .GT. 240000 ) ) THEN
            XMSG = 'Begin time must be between 0 and 240000.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF
          IF ( ( TIME2 .LT. 0 ) .OR. ( TIME2 .GT. 240000 ) ) THEN
            XMSG = 'End time must be between 0 and 240000.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

          ITIME( NPSEN, 1 ) = TIME1
          ITIME( NPSEN, 2 ) = TIME2

 55       CONTINUE

          READ( JVUNIT, '(A80)', END = 999 ) TXT

          IF ( TXT( :10) .EQ. BLANK10 ) GOTO 55

        END IF

CCCCCCC Which region(s) ? CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ! If true, then region and gridcell not specified, 
        ! so set as domainwide and go to next sensitivity
        IF ( TXT( :1 ) .NE. BLANK ) THEN
          DO LVL = 1, NLAYS
            DO ROW = 1, NROWS
              DO COL = 1, NCOLS
                IREGION( COL, ROW, LVL, NPSEN ) = 1.0
              END DO
            END DO
          END DO
          IF ( TXT( :3 ) .EQ. 'END' ) THEN
            GOTO 999
          ELSE
            ! New sensitivity
            GOTO 20 
          END IF
        END IF

        IF ( TXT( :7 ) .EQ. ' REGION' ) THEN

          IF ( RGN ) THEN
             READ( JVUNIT, '(a2,i2)' ) TXT, NREG
             IF ( TXT .NE. BLANK2 ) THEN
               XMSG = 'Expected two spaces before number of regions.'
               CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
             END IF
             ! 99 is the code for all regions (domainwide)
             IF ( NREG .EQ. 99 ) THEN
               DO LVL = 1, NLAYS
                 DO ROW = 1, NROWS
                   DO COL = 1, NCOLS
                     IREGION( COL, ROW, LVL, NPSEN ) = 1.0
                   END DO
                 END DO
               END DO
             ELSE IF ( NREG .GT. NREGIONS ) THEN
               XMSG = 'Number of regions requested ' //
     &                'exceeds total number of regions ' //
     &                'available.'
               CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
             ELSE
               DO R = 1, NREG
                 READ( JVUNIT, '(A80)' ) TXT
                 IF ( TXT( :2 ) .NE. BLANK2 ) THEN
                   XMSG = 'Expected 2 leading blanks ' //
     &                    'before region name.'
                   CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
                 END IF
                 DO R2 = 1, NREGIONS
                   IF ( TXT( 3:18 ) .EQ. REGNNAME( R2 ) ) THEN
                     DO LVL = 1, NLAYS
                       DO ROW = 1, NROWS
                         DO COL = 1, NCOLS
                           IREGION( COL, ROW, LVL, NPSEN ) =
     &                     MIN( (IREGION( COL, ROW, LVL, NPSEN ) +
     &                        RGNS( COL, ROW, R2 ) ), 1.0 )
                         END DO
                       END DO
                     END DO
                     GOTO 60
                   ELSE IF ( R2 .EQ. NREGIONS ) THEN
                     XMSG = 'Could not identify region ' //
     &                      TXT( 3:18 ) //
     &                      ' from region list.'
                     CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
                   END IF
                 END DO  ! loop over NREGIONS
 60              CONTINUE
               END DO  ! loop over NREG
             END IF

 65          CONTINUE
             READ( JVUNIT, '(A80)' ) TXT

             IF ( TXT( :10 ) .EQ. BLANK10 ) GOTO 65
             IF ( TXT( :3 ) .EQ. 'END' ) GOTO 999
             IF ( TXT( :1 ) .NE. BLANK ) GOTO 20
             IF ( TXT( :8 ) .EQ. ' GRIDCEL' ) GOTO 100
             IF ( TXT( :8 ) .EQ. ' CORNERS' ) GOTO 130
             IF ( TXT( :8 ) .EQ. ' CIRCLES' ) GOTO 140
             XMSG = 'Could not interpret entry after RGNS.'
             CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )

          ELSE

             XMSG = 'Regions option is not set.'
             CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

        END IF  ! TXT .EQ. REGION

        IF ( TXT( :8 ) .EQ. ' GRIDCEL' ) THEN
          GOTO 100
        END IF
        IF ( TXT( :8 ) .EQ. ' CORNERS' ) THEN
          GOTO 130
        END IF
        IF ( TXT( :8 ) .EQ. ' CIRCLES' ) THEN
          GOTO 140
        ELSE
          XMSG = 'Subheading ' // TXT // ' not recognized.'
          CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
        END IF

CCCCCCC Which gridcell(s)? CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 100    CONTINUE
        ! How many gridcell(s) will be input?
        READ( JVUNIT, * ) NGCELLS

        IF ( NGCELLS .LT. 1 ) THEN
          XMSG = 'Expected at least one gridcell.'
          CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
        END IF

        DO GCELL = 1, NGCELLS
          READ( JVUNIT , *) GCOL, GROW, GLVL
C global grid tests first
          IF ( .NOT.(( GCOL .GE. 1 .AND. GCOL .LE. GL_NCOLS )
     &        .AND.(GROW .GE. 1 .AND. GROW .LE. GL_NROWS )) ) THEN
             XMSG = 'Gridcell column or row out of global bounds.'
             CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF
CTON translate the row & col limits from global to PE-specific values 
C using the info from module HGRD_DEFN
          GCOL = GCOL - COLSX_PE(1,MYPE+1) + 1
          GROW = GROW - ROWSX_PE(1,MYPE+1) + 1
          IF ( ( GCOL .GE. 1 .AND. GCOL .LE. NCOLS )
     &        .AND.(GROW .GE. 1 .AND. GROW .LE. NROWS ) ) THEN
            IF ( GLVL .GE. 1 .AND. GLVL .LE. NLAYS ) THEN
              IREGION( GCOL, GROW, GLVL, NPSEN ) = 1.0
            ELSE IF (GLVL .EQ. 99 ) THEN  ! 99 means all layers
              DO LVL = 1, NLAYS
                IREGION ( GCOL, GROW, LVL, NPSEN ) = 1.0
              END DO
            ELSE
              XMSG = 'Gridcell level out of bounds.'
              CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
            END IF
          END IF
        END DO ! Loop over NGCELLS

 105    CONTINUE

        READ( JVUNIT, '(A80)' ) TXT

        IF ( TXT( :10 ) .EQ. BLANK10 ) GOTO 105
        IF ( TXT( :3 ) .EQ. 'END' ) GOTO 999
        IF ( TXT( :1 ) .NE. BLANK ) GOTO 20
        IF ( TXT( :8 ) .EQ. ' CORNERS' ) GOTO 130
        IF ( TXT( :8 ) .EQ. ' CIRCLES' ) GOTO 140
        XMSG = 'Could not interpret entry after GRIDCELL.'
        CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )

CCCCCCC Which corner-defined zones? CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 130    CONTINUE

        ! How many corner-defined zones?
        READ( JVUNIT, * ) NCORNERS

        IF ( NCORNERS .LT. 1 ) THEN
          XMSG = 'Expected at least one corner region.'
          CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
        END IF

        DO CORNER = 1, NCORNERS
          READ( JVUNIT, * )
     &                COLMIN, COLMAX,  
     &                ROWMIN, ROWMAX, 
     &                LVLMIN, LVLMAX  
          IF ( ( COLMIN .GT. COLMAX ) .OR. ( ROWMIN .GT. ROWMAX ) ) THEN
            XMSG = 'Minimum column and row must be less than maximum.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF
CTON This test must be against global #cols and rows not local to PE
          IF ( ( COLMAX .GT. GL_NCOLS ) .OR. ( ROWMAX .GT. GL_NROWS ) ) THEN
            XMSG = 'Maximum column or row out of global bounds.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF
          IF ( ( LVLMIN .GT. LVLMAX ) .OR. ( LVLMAX .GT. NLAYS ) ) THEN
            XMSG = 'Unacceptable level numbers for corner boundaries.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF 

CTON translate the row & col limits from global to PE-specific values 
C using the info from module HGRD_DEFN
          COLMIN = COLMIN - COLSX_PE(1,MYPE+1) + 1
          COLMAX = COLMAX - COLSX_PE(1,MYPE+1) + 1
          ROWMIN = ROWMIN - ROWSX_PE(1,MYPE+1) + 1
          ROWMAX = ROWMAX - ROWSX_PE(1,MYPE+1) + 1
C if the box is completely outside this PE's range do nothing
          IF(.NOT.(COLMAX .LT. 1 .OR. COLMIN .GT. NCOLS
     &    .OR. ROWMAX .LT. 1 .OR. ROWMIN .GT. NROWS))THEN
C  yes, there is some overlap. Crop it to fit
             COLMIN = MAX(COLMIN,1)
             COLMAX = MIN(COLMAX,NCOLS)
             ROWMIN = MAX(ROWMIN,1)
             ROWMAX = MIN(ROWMAX,NROWS)
             DO COL = COLMIN, COLMAX
                DO ROW = ROWMIN, ROWMAX
                   DO LVL = LVLMIN, LVLMAX
                      IREGION( COL, ROW, LVL, NPSEN ) = 1.0
                   END DO
                END DO
             END DO
          END IF
   
        END DO 

 135    CONTINUE

        READ( JVUNIT, '(A80)' ) TXT

        IF ( TXT( :10 ) .EQ. BLANK10 ) GOTO 135
        IF ( TXT( :3 ) .EQ. 'END' ) GOTO 999
        IF ( TXT( :1 ) .NE. BLANK ) GOTO 20
        IF ( TXT( :8 ) .EQ. ' CIRCLES' ) GOTO 140
        XMSG = 'Could not interpret entry after GRIDCELL.'
        CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )   

CCCCCCC End CORNERS CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCC Which Circle zones? CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 140    CONTINUE

        ! How many circle-defined zones?
        READ( JVUNIT, * ) NCIRCLES

        ! What is the grid resolution (e.g., 12km)?
        READ( JVUNIT, * ) GRIDSIZE

        IF ( NCIRCLES .LT. 1 ) THEN
          XMSG = 'Expected at least one circle region.'
          CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
        END IF

        DO CIRCLE = 1, NCIRCLES
          ! Read coordinates of circle's center and 
          ! the minimum and maximum radii
          READ( JVUNIT, * ) C_CENTER, R_CENTER, MINRAD, MAXRAD

          IF ( ( MINRAD .LT. 0.0 ) .OR. 
     &         ( MAXRAD .LT. MINRAD ) ) THEN
            XMSG = 'Radii lengths unacceptable.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF
           
          C_MIN = C_CENTER - FLOOR( MAXRAD / GRIDSIZE )
          C_MAX = C_CENTER + FLOOR( MAXRAD / GRIDSIZE )
          R_MIN = R_CENTER - FLOOR( MAXRAD / GRIDSIZE )
          R_MAX = R_CENTER + FLOOR( MAXRAD / GRIDSIZE )
          IF ( ( C_MIN .LT. 1 ) .OR. 
     &         ( C_MAX .GT. GL_NCOLS ) ) THEN
            XMSG = 'Circle columns out of global bounds.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF
          IF ( ( R_MIN .LT. 1 ) .OR.
     &         ( R_MAX .GT. GL_NROWS ) ) THEN
            XMSG = 'Circle rows out of global bounds.'
            CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
          END IF

          ! A cell is within the circle-zone iff the distance between
          ! its center and the circle center is .ge. the minimum radius
          ! and .le. the maximum radius. Thus, the zone is in the shape
          ! of a circle (if minimum radius = 0) or ring.
CTON transform centers to local PE-specific col & row numbers. Does not matter
C if they are <1 or >NCOLS or >NROWS as they are used to calculate a distance
          C_CENTER = C_CENTER - COLSX_PE(1,MYPE+1) + 1
          R_CENTER = R_CENTER - ROWSX_PE(1,MYPE+1) + 1
          DO COL = 1, NCOLS
            C_DIST = COL - C_CENTER
            DO ROW = 1, NROWS
              R_DIST = ROW - R_CENTER
              TOT_DIST = GRIDSIZE *
     &                   ( ( C_DIST**2.0 + R_DIST**2.0 ) ** 0.5 )
              IF ( ( TOT_DIST .GE. MINRAD ) .AND.
     &             ( TOT_DIST .LE. MAXRAD ) ) THEN
                DO LVL = 1, NLAYS
                  IREGION( COL, ROW, LVL, NPSEN ) = 1.0
                END DO
              END IF
            END DO
          END DO

        END DO  ! Loop over NCIRCLES

CCCCCCC End CIRCLES CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

 150    CONTINUE
        READ( JVUNIT, '(A80)' ) TXT
        IF ( TXT( :10 ) .EQ. BLANK10 ) GOTO 150
        IF ( TXT( 1:3 ) .EQ. 'END' ) THEN
          GOTO 999
        ELSE IF ( TXT( :1 ) .NE. BLANK )  THEN
          GOTO 20
        ELSE
          XMSG = 'Text not recognized. Expecting ' //
     &           'END, a new sensitivity name, ' //
     &           'or a blank line.'
          CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
        END IF

  999 CONTINUE

      ! Sens parameter input file must contain exact number
      ! of sensitivity parameters indicated by NPMAX in runscript.
      IF ( NPSEN .NE. NPMAX ) THEN
        XMSG = 'Wrong number of sens specified. Does not match NPMAX.'
        CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
      END IF

      Write(LOGDEV,*) " "
      Write(LOGDEV,*) "Sensitivity parameters in this simulation:"
      Write(LOGDEV,*) "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="
      Write(LOGDEV,'(a2,1x,a4,1x,a4,1x,a10)') "#","name","type"

      Do NP = 1, NPMAX
        Write(LOGDEV,'(i2,1x,a4,1x,i4.4,9(1x,i2.2))') NP,SEN_PAR(NP),IPT(NP)
      End Do
      Write(LOGDEV,*) " "

c Calculate 2nd order mapping

      IHIGH = 0
      If ( HIGH ) then

         Do NP = 1, NPMAX
            If ( IPT(NP) .Eq. 4 ) Then
               Do ROW = 1, NP - 1
                  If ( IPARM(NP,ROW) .eq. 1 ) Then
                     IHIGH(NP,1) = ROW
                  Else If ( IPARM(NP,ROW) .eq. 2 ) Then
                     IHIGH(NP,2) = ROW
                  Else If ( IPARM(NP,ROW) .eq. 3 ) Then
                     IHIGH(NP,1) = ROW
                     IHIGH(NP,2) = ROW
                  End If
               End Do
               If ( IHIGH(NP,1) .Eq. 0 .Or. IHIGH(NP,2) .Eq. 0 ) Then
                  XMSG = 'Required 1st order parameters not found'
     &                   // ' for parameter = > ' // SEN_PAR(NP)
                  CALL M3EXIT( 'sinput', 0 , 0, XMSG, XSTAT2 )
               End If
            End If
         End Do
c     Else
c        XMSG = 'DDM-3D HIGH option not enabled - check runscript'
c        CALL M3EXIT ( 'sinput', 0, 0, XMSG, XSTAT2 )
      End If

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE SEN_NUM(TEMP, NUM )

C------------------------------------------------------------
C Function: Assigns to NUM the species number corresponding
C           to the input string "TEMP"
C------------------------------------------------------------ 
      USE CGRID_SPCS
c     USE SUBST_MODULES         ! stenex
      USE UTILIO_DEFN

      IMPLICIT NONE  

c     INCLUDE SUBST_GC_SPC
c     INCLUDE SUBST_AE_SPC      ! aerosol species table
c     INCLUDE SUBST_NR_SPC      ! non-reactive species table
c     INCLUDE SUBST_TR_SPC      ! tracer species table
      INCLUDE SUBST_FILES_ID  ! file name parameters
c     INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
c     INCLUDE SUBST_IOFDESC   ! file header data structure
c     INCLUDE SUBST_IODECL    ! I/O definitions and declarations

      CHARACTER*8 TEMP
      CHARACTER*16 TEMP2
      INTEGER NUM, ISP

      CHARACTER*16 PNAME
      DATA         PNAME   / 'SEN_NUM' /
      SAVE         PNAME
      CHARACTER*120 XMSG
      DATA          XMSG / ' ' /
C
      NUM = 0
      DO 10 ISP = 1, N_GC_SPC
         TEMP2 = GC_SPC(ISP)
        IF(TEMP .EQ. TEMP2(:8))THEN
             NUM = ISP
             RETURN
         ENDIF
   10 CONTINUE
      DO 20 ISP = 1, N_AE_SPC
         TEMP2 = AE_SPC( ISP )
         IF ( TEMP .EQ. TEMP2(:8)) THEN
           NUM = N_GC_SPC + 1 + ISP
           RETURN
         ENDIF
   20 CONTINUE
      DO 30 ISP = 1, N_NR_SPC
         TEMP2 = NR_SPC( ISP )
         IF ( TEMP .EQ. TEMP2(:8)) THEN
           NUM = N_GC_SPC + 1 + N_AE_SPC + ISP
           RETURN
         ENDIF
   30 CONTINUE

      IF(NUM .EQ. 0) THEN
         XMSG = 'Sensitivity parameter ' // TEMP //
     &          'not found in species name array'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      ENDIF

      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE REAC_NUM( TEMP, NUM )

C------------------------------------------------------------
C Function: Assigns to NUM the reaction number corresponding
C           to the input string "TEMP"
C------------------------------------------------------------

c     USE SUBST_MODULES         ! stenex
      USE UTILIO_DEFN

      USE RXNS_DATA, Only: NRXNS, RXLABEL

      IMPLICIT NONE

c     INCLUDE SUBST_RXCMMN    ! Mechanism reaction common block
      INCLUDE SUBST_FILES_ID  ! file name parameters
c     INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
c     INCLUDE SUBST_IOFDESC   ! file header data structure
c     INCLUDE SUBST_IODECL    ! I/O definitions and declarations

      CHARACTER*16 TEMP
      CHARACTER*16 TEMP2
      INTEGER NUM, RXN

      CHARACTER*16 PNAME
      DATA         PNAME   / 'REAC_NUM' /
      SAVE         PNAME
      CHARACTER*120 XMSG
      DATA          XMSG / ' ' /

CCCC
      NUM = 0
      DO RXN = 1, NRXNS
        TEMP2 = RXLABEL(RXN)
        IF(TEMP .EQ. TEMP2) THEN
             NUM = RXN
             RETURN
        ENDIF
      END DO

      IF(NUM .EQ. 0) THEN
         XMSG = 'Sensitivity parameter ' // TEMP //
     &          'not found in RXLABEL array'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      ENDIF

      RETURN
      END

#endif
