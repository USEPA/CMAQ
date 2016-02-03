
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

c     SUBROUTINE SINPUT( STDATE, STTIME, NPMAX, IPT, IPARM,
c    &                   ISPLIT,  
c    &                   GRID_FILE, GRID_NUM, PT3D_FILE, PT3D_NUM,
c    &                   IAMOUNT, ILAYER, NDAYS, IDATE,
c    &                   ITIME, IREGION, IRXN, SEN_PAR )

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
C--------------------------------------------------------------------

C
C  variables 
C     NPMAX   - Max # of sens parameters allowed
C     IPT     - Type of sensitivity (e.g. IPT=3 for sens. to emission) 
C     IPARM   - ARRAY FOR PARAMETER TYPE AND SPECIES ORDER # 
C     ISPLIT  - Type of emission (total, area, biogenic, mobile, 
C                                 point, non-road)
C     IAMOUNT - Amount by which emissions are perturbed in a sens param,
C     ILAYER  - Layers included in sens parameter
C     IDATE and ITIME - Dates and times included in sens parameter,
C               if not all dates & times are included
C     IREGION - Region of emission (1 = domainwide)
C     IRXN - Which chemical reaction for reaction rate sensitivities
C     SEN_PAR - Names of sensitivity parameters


*
**** REMEMBER TO SET ERROR MESSAGES USING M3ERR
*
!     USE HGRD_DEFN             ! horizontal domain specifications
c     USE PCGRID_DEFN
!     USE CGRID_SPCS   ! CGRID species number and offsets
c     USE SUBST_MODULES         ! stenex
      USE UTILIO_DEFN
      USE DDM3D_DEFN
      USE HGRD_DEFN, ONLY: NCOLS, NROWS, GL_NCOLS, GL_NROWS, MY_NCOLS, MY_NROWS, COLSX_PE, ROWSX_PE, MYPE
      USE VGRD_DEFN, ONLY: NLAYS

      IMPLICIT NONE

!     INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters
c     INCLUDE SUBST_GC_SPC
c     INCLUDE SUBST_AE_SPC      ! aerosol species table
c     INCLUDE SUBST_NR_SPC      ! non-reactive species table
c     INCLUDE SUBST_TR_SPC      ! tracer species table
      INCLUDE SUBST_FILES_ID  ! file name parameters
c     INCLUDE SUBST_IOPARMS   ! I/O parameters definitions
c     INCLUDE SUBST_IOFDESC   ! file header data structure
c     INCLUDE SUBST_IODECL    ! I/O definitions and declarations
      INCLUDE SUBST_RXCMMN    ! Mechanism reaction common block

      CHARACTER( 80 ) :: TXT, TXT2, EMISTYPE
      INTEGER      STDATE       ! starting date,    format YYYYDDD
      INTEGER      STTIME       ! starting time,    format HHMMSS
c     INTEGER, POINTER :: IPT( : )
!      INTEGER      IPT( NPMAX )         ! Type of sensitivity
                                             ! 1 = INIT; 2 = BOUN;
                                             ! 3 = EMIS; 4 = HIGH
                                             ! 5 = RATE
c     INTEGER       MXSPCS              ! Number of species in CGRID
c     PARAMETER   ( MXSPCS = N_GC_SPCD
c    &                     + N_AE_SPC
c    &                     + N_NR_SPC
c    &                     + N_TR_SPC )
c     INTEGER, POINTER :: IPARM( :,: )
!      INTEGER      IPARM( NPMAX, MXSPCS )    ! set to 1 if sens to that species
                                              ! set to 0 otherwise
c     INTEGER NPMAX            ! Max number of sens parameters allowed
c     INTEGER NDAYS            ! No of dates in model run
      INTEGER      NSPC( NPMAX )             ! number of species chosen
                                             ! for that sensitivity parameter
      INTEGER      SPCORDER( NPMAX, NSPCSD ) ! holds the species numbers
                                             ! in the order in which they
                                             ! are specified in the input file
c     INTEGER, POINTER :: ISPLIT( :,: )
!      INTEGER      ISPLIT( NPMAX, 6 )        ! 1 = total; 2 = area; 3 = bio
                                             ! 4 = mobile; 5 = point;
                                             ! 6 = non-road
                                        ! SLN - changed 15Aug2011 to ( NPMAX, 4 )
                                        ! 1 = total; 2 = 3D file; 3 = beis; 4 = inline

c     CHARACTER( 16 ), POINTER :: GRID_FILE( :,: )  ! ( NPMAX, 9 )  
c     INTEGER, POINTER ::  GRID_NUM( : )
c     CHARACTER( 16 ), POINTER :: PT3D_FILE( :,:,: )  ! ( NPMAX, 2, 9 )  
c     INTEGER, POINTER ::  PT3D_NUM( : )

      REAL AMOUNT                       ! used to read in emission rate
c     REAL, POINTER :: IAMOUNT( :,:,: )
!      REAL IAMOUNT ( NPMAX, MXSPCS, 25 )     ! emission rate during the hour
                                        ! the 25th slot is 1 if emis amounts specified,
                                        ! 0 otherwise
      INTEGER HOUR                      ! counter over the hours
c     INTEGER, POINTER :: ILAYER( :,: )
!     INTEGER ILAYER ( NPMAX, NLAYS )   ! 1 if layer's emis included, 0 else
                                        ! only used if emis amount specified,
                                        ! otherwise set to 1 for all layers
      INTEGER LAYER                     ! used to read in layer

      ! IRXN( NPMAX, NRXNS ) = 1 if sens to that reaction rate;
      !                      = 0 otherwise
c     INTEGER, POINTER :: IRXN( :,: )
      INTEGER RXN                       ! used to read in reaction number

      INTEGER TIME1, TIME2              ! used to read in begin & end time
c     INTEGER, POINTER :: ITIME( :,: )
!      INTEGER ITIME ( NPMAX, 2 )        ! begin time & end time for each sens
                                        ! default is 0, 240000
                                        ! Note: to span midnight, begin time
                                        ! may be larger than endtime
      INTEGER DATE                      ! used to read in desired date
      INTEGER NDATE                     ! counter over the dates
      INTEGER NUMDATES                  ! number of dates for that sens
      INTEGER LASTDATE                  ! STDATE + NDAYS;
                                        ! note that this may be gt the actual
                                        ! enddate of the simulation
c     INTEGER, POINTER :: IDATE( :,: )
!     INTEGER IDATE ( NPMAX, NDAYS ) ! set to 1 if that date is included
!     REAL IREGION( SENCOLS, SENROWS, NLAYS, NPMAX )
c     REAL, POINTER ::  IREGION( :,:,:,: )
      INTEGER      NPSEN                     ! num of sens parameters
      INTEGER      JVUNIT
c     INTEGER      JUNIT
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
c     CHARACTER( 8 ), POINTER :: SEN_PAR( : )
!      CHARACTER( 8  ) :: SEN_PAR( NPMAX )  ! name of the sensitivity 
                                           ! (eg, EMIS_NO)
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

c     LOGICAL, EXTERNAL   :: ENVYN         ! get environment variable as boolean
      CHARACTER( 80 )     :: VARDESC       ! environment variable description
      INTEGER      STATUS                  ! ENVYN status

C Variables used for regional sensitivity calculations
c     CHARACTER( 16 ), SAVE  :: DDM3D_RGN = 'DDM3D_RGN'
c     LOGICAL, SAVE          :: RGN
      REAL, ALLOCATABLE :: RGNS ( :,:,: )
      CHARACTER( 16 ), ALLOCATABLE :: REGNNAME ( : )
      INTEGER REGNTIME, REGNDATE, NREGIONS
      INTEGER     GXOFF, GYOFF              ! global origin offset from file
      INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
      INTEGER     LOGDEV
      INTEGER I

C Variables used for hddm-3d
c     CHARACTER( 16 ), SAVE  :: DDM3D_HIGH = 'DDM3D_HIGH'
c     LOGICAL, SAVE          :: HIGH
      INTEGER      CHECK

C Variables used for split emissions files
c     CHARACTER( 16 ), SAVE  :: DDM3D_ES = 'DDM3D_ES'
c     LOGICAL, SAVE          :: ES

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

c check environmental variables

c     RGN = .FALSE.         ! default
c     VARDESC = 'Flag for using sensitivity regions file' 
c     RGN = ENVYN( DDM3D_RGN, VARDESC, RGN, STATUS )
c     IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
c     IF ( STATUS .EQ. 1 ) THEN
c        XMSG = 'Environment variable improperly formatted'
c        CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT2 )
c     ELSE IF ( STATUS .EQ. -1 ) THEN
c        XMSG =
c    &       'Environment variable set, but empty ... Using default:'
c        WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, STTIME
c     ELSE IF ( STATUS .EQ. -2 ) THEN
c        XMSG = 'Environment variable not set ... Using default:'
c        WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, STTIME
c     END IF

c     HIGH = .FALSE.         ! default
c     VARDESC ='Flag for computing higher order sensitivities (hddm-3d)'
c     HIGH = ENVYN( DDM3D_HIGH, VARDESC, HIGH, STATUS )
c     IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
c     IF ( STATUS .EQ. 1 ) THEN
c        XMSG = 'Environment variable improperly formatted'
c        CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT2 )
c     ELSE IF ( STATUS .EQ. -1 ) THEN
c        XMSG =
c    &       'Environment variable set, but empty ... Using default:'
c        WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, STTIME
c     ELSE IF ( STATUS .EQ. -2 ) THEN
c        XMSG = 'Environment variable not set ... Using default:'
c        WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, STTIME
c     END IF

c     ES = .FALSE.         ! default
c     VARDESC = 'Flag for using split emissions files'
c     ES = ENVYN( DDM3D_ES, VARDESC, ES, STATUS )
c     IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
c     IF ( STATUS .EQ. 1 ) THEN
c        XMSG = 'Environment variable improperly formatted'
c        CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT2 )
c     ELSE IF ( STATUS .EQ. -1 ) THEN
c        XMSG =
c    &       'Environment variable set, but empty ... Using default:'
c        WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, STTIME
c     ELSE IF ( STATUS .EQ. -2 ) THEN
c        XMSG = 'Environment variable not set ... Using default:'
c        WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, STTIME
c     END IF


      IF ( RGN ) THEN ! READ REGIONS FILE 

C Get domain decomp info from the emissions file
         CALL SUBHFILE ( REGIONS_1, GXOFF, GYOFF,
     &                   STARTCOL, ENDCOL, STARTROW, ENDROW )

C Open input file REGIONS_1
         IF ( .NOT. OPEN3( REGIONS_1, FSREAD3, PNAME ) ) THEN
           XMSG = 'Could not open '// REGIONS_1 // ' file'
           CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
         END IF


         IF ( .NOT. DESC3( REGIONS_1 ) ) THEN
            XMSG = 'Could not get ' // REGIONS_1 // ' file description'
            CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
         END IF

         NREGIONS = NVARS3D
         REGNDATE = SDATE3D
         REGNTIME = STIME3D

C Allocate arrays to hold region names and locations
         ALLOCATE( REGNNAME( NREGIONS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
             XMSG = 'Failure allocating REGNNAME'
             CALL M3EXIT ( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( RGNS(NCOLS,NROWS,NREGIONS), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating RGNS'
            CALL M3EXIT ( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
         END IF

         RGNS = 0.0

C Read data from regions file into arry RGNS
         DO I = 1, NREGIONS
            REGNNAME( I ) = VNAME3D( I )
            IF ( .NOT. INTERPX( REGIONS_1, REGNNAME(I), PNAME,
     &           STARTCOL, ENDCOL, STARTROW, ENDROW, 1, 1,
     &           REGNDATE, REGNTIME, RGNS( 1,1,I) ) )
     &           CALL M3ERR( PNAME, REGNDATE, REGNTIME,
     &           'Could not read ' // REGNNAME(I) //
     &           'from ' // REGIONS_1 // ' file',
     &           .TRUE. )
         END DO

C Close regions file
         IF ( .NOT. CLOSE3( REGIONS_1 ) ) THEN
           XMSG = 'Could not close ' // REGIONS_1
           CALL M3EXIT( PNAME, REGNDATE, REGNTIME, XMSG, XSTAT1 )
         END IF

      END IF

C Initialize IPT, IPARM, IREGION, IRXN to 0; 
C SEN_PAR to blank
C ISPLIT to total emissions
      ISPLIT = 0
      ISPLIT( :, 1 ) = 1
      GRID_FILE = ' '
      PT3D_FILE = ' '
      GRID_NUM = 0
      PT3D_NUM = 0
      DO P = 1, NPMAX
        IPT( P ) = 0
        NSPC( P ) = 0
        SEN_PAR( P ) = ' '
c       ISPLIT( P, 1 ) = 1
c       ISPLIT( P, 2:6 ) = 0
        DO RXN = 1, NRXNS
          IRXN( P, RXN ) = 0
        END DO
        DO SPC = 1, NSPCSD
          IPARM( P, SPC ) = 0
          IAMOUNT( P, SPC, 1:25 ) = 0.0
        END DO
        IDATE( P, 1:NDAYS ) = 0
        ITIME( P, 1 ) = 0
        ITIME( P, 2 ) = 240000
        ILAYER( P, 1:NLAYS ) = 1
        DO LVL = 1, NLAYS
          DO ROW = 1, NROWS
            DO COL = 1, NCOLS
              IREGION( COL, ROW, LVL, P )  = 0.0
            END DO
          END DO
        END DO
      END DO

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
        XMSG = 'NPSEN exceeds NPMAX.  Too many sens specified.'
        CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
      END IF

      SEN_PAR( NPSEN ) = TXT( 1:8 ) 

      ! Read in the type of sensitivity 
      ! (only INIT, EMIS, BOUN implemented so far)
      READ( JVUNIT, '(A80)', END = 999 ) TXT
        IF ( TXT( :1 ) .NE. BLANK ) THEN
          XMSG = 'Need one space before specifying the sentype.'
          CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
        END IF

CCCCCCC Sensitivity to Emissions CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF ( TXT( 2:5 ) .EQ. 'EMIS' ) THEN
          IPT( NPSEN ) = 3
  30      CONTINUE

          ! Check if a type of emissions (area, bio, ...)
          ! has been specified. Otherwise, assume total.
          READ( JVUNIT, '(A80)', END = 999 ) EMISTYPE

          IF ( EMISTYPE( :8 ) .EQ. ' SPECIES' ) GOTO 35 

          IF ( EMISTYPE( :2 ) .NE. BLANK2 ) THEN
            XMSG = 'Expecting SPECIES ' //
     &             '(one leading space, allcap) or ' //
     &             'an emission file type (GRID, PT3D, ' //
     &             'BEIS, TOTA; 2 leading spaces)'
            CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
          END IF

          IF ( ES ) THEN
             IF ( EMISTYPE( 3:6 ) .EQ. 'TOTA' ) THEN
                ISPLIT( NPSEN, 1 ) = 1
                GOTO 30
c            ELSE IF ( EMISTYPE( 3:6 ) .EQ. 'AREA' ) THEN
c               ISPLIT( NPSEN, 2 ) = 1
c               ISPLIT( NPSEN, 1 ) = 0
c               GOTO 30
c            ELSE IF ( EMISTYPE( 3:6 ) .EQ. 'BIOG' ) THEN
c               ISPLIT( NPSEN, 3 ) = 1
c               ISPLIT( NPSEN, 1 ) = 0
c               GOTO 30 
c            ELSE IF ( EMISTYPE( 3:6 ) .EQ. 'MOBI' ) THEN
c               ISPLIT( NPSEN, 4 ) = 1
c               ISPLIT( NPSEN, 1 ) = 0
c               GOTO 30
c            ELSE IF ( EMISTYPE( 3:6 ) .EQ. 'POIN' ) THEN
c               ISPLIT( NPSEN, 5 ) = 1
c               ISPLIT( NPSEN, 1 ) = 0
c               GOTO 30
c            ELSE IF ( EMISTYPE( 3:6 ) .EQ. 'NORD' ) THEN
c               ISPLIT( NPSEN, 6 ) = 1
c               ISPLIT( NPSEN, 1 ) = 0
c               GOTO 30
             ELSE IF ( EMISTYPE( 3:6 ) .EQ. 'GRID' ) THEN
                ISPLIT( NPSEN, 2 ) = 1
                ISPLIT( NPSEN, 1 ) = 0
                READ( JVUNIT, * ) NGCELLS
                IF ( NGCELLS .LT. 1 .OR. NGCELLS .GT. 9 ) THEN
                  XMSG = 'Expected between 1 and 9 filenames.'
                  CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
                END IF
                GRID_NUM( NPSEN ) = NGCELLS
                DO I = 1, NGCELLS
                  READ( JVUNIT, '(A80)', END = 999 ) TXT
                  IF ( TXT( :2 ) .NE. BLANK2 ) THEN
                    XMSG = 'Expecting FILENAME ' //
     &               '(2 leading space)'
                    CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
                  END IF
                  GRID_FILE( NPSEN, I ) = TXT(3:18)
                END DO
                GOTO 30
             ELSE IF ( EMISTYPE( 3:6 ) .EQ. 'BEIS' ) THEN
                ISPLIT( NPSEN, 3 ) = 1
                ISPLIT( NPSEN, 1 ) = 0
                GOTO 30
             ELSE IF ( EMISTYPE( 3:6 ) .EQ. 'PT3D' ) THEN
                ISPLIT( NPSEN, 4 ) = 1
                ISPLIT( NPSEN, 1 ) = 0
                READ( JVUNIT, * ) NGCELLS
                IF ( NGCELLS .LT. 1 .OR. NGCELLS .GT. 9 ) THEN
                  XMSG = 'Expected between 1 and 9 filenames.'
                  CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
                END IF
                PT3D_NUM( NPSEN ) = NGCELLS
                DO I = 1, NGCELLS
                  READ( JVUNIT, '(A80)', END = 999 ) TXT
                  IF ( TXT( :2 ) .NE. BLANK2 ) THEN
                    XMSG = 'Expecting STK_GRP ' //
     &               '(3 leading space)'
                    CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
                  END IF
                  PT3D_FILE( NPSEN, 1, I ) = TXT(3:18)
                  READ( JVUNIT, '(A80)', END = 999 ) TXT
                  IF ( TXT( :2 ) .NE. BLANK2 ) THEN
                    XMSG = 'Expecting STK_EMIS ' //
     &               '(3 leading space)'
                    CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
                  END IF
                  PT3D_FILE( NPSEN, 2, I ) = TXT(3:18)
                END DO
                GOTO 30
             ELSE
                XMSG = 'Emissions type not recognized.' // EMISTYPE( 3:6 )
                CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
             END IF
          ELSE
             IF ( EMISTYPE( 3:6 ) .EQ. 'TOTA' ) THEN
                ISPLIT( NPSEN, 1 ) = 1
                GOTO 30
             ELSE
                XMSG = 'Split Emissions option  is not set, so must ' //
     &             'choose emistype TOTA or not specify.'
                CALL M3EXIT( PNAME, STDATE, STTIME, XMSG, XSTAT1 )
             END IF
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
            IF ( TXT( 2:9 ) .EQ. SEN_PAR( P ) ) THEN
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
            IF ( TXT( 2:9 ) .EQ. SEN_PAR( P ) ) THEN
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
          XMSG = 'Sentype not recognized: ' // TXT( 2:5 ) //
     &           '. Use EMIS, INIT, BOUN, RATE, or HIGH. '
          CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )

        END IF  ! Finished setting sentype

 35     CONTINUE

CCCCCCC Which species? CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        READ( JVUNIT, '(A80)', END = 999 ) TXT

        IF ( TXT( :2 ) .NE. BLANK2 ) THEN
          XMSG = 'Need two leading spaces before a species.'
          CALL M3EXIT( PNAME,STDATE,STTIME,XMSG,XSTAT1 )
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

          IF ( ISPLIT( NPSEN, 1 ) .NE. 1 ) THEN
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
          IF(.NOT.(COLMAX .LT. 1 .OR. COLMIN .GT. MY_NCOLS
     &    .OR. ROWMAX .LT. 1 .OR. ROWMIN .GT. MY_NROWS))THEN
C  yes, there is some overlap. Crop it to fit
             COLMIN = MAX(COLMIN,1)
             COLMAX = MIN(COLMAX,MY_NCOLS)
             ROWMIN = MAX(ROWMIN,1)
             ROWMAX = MIN(ROWMAX,MY_NROWS)
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
      Write(LOGDEV,'(a2,1x,a8,1x,a4,1x,a10)') "#","name","type"

      Do NP = 1, NPMAX
        Write(LOGDEV,'(i2,1x,a8,1x,i4.4,9(1x,i2.2))') NP,SEN_PAR(NP),IPT(NP)
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
      USE SUBST_MODULES         ! stenex
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

      USE SUBST_MODULES         ! stenex
      USE UTILIO_DEFN

      IMPLICIT NONE

      INCLUDE SUBST_RXCMMN    ! Mechanism reaction common block
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

