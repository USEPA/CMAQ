#ifdef sens


C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      Module DDM3D_DEFN

C-----------------------------------------------------------------------
C   8 Nov 12 S.L.Napelenok: initial version
C  15 Oct 15 S.L.Napelenok: ddm-3d implementation for cmaq5.1.1
C  29 Jun 16 S.L.Napelenok: update for cmaq5.2, include SPCNAME here
C  20 Nov 19 S.L.Napelenok: update for cmaq 5.3.1
C  
C-----------------------------------------------------------------------
C
C  variables
C     NPMAX   - Max # of sens parameters allowed
C     IPT     - Type of sensitivity:
C                   1 = INIT; 2 = BOUN;
C                   3 = EMIS; 4 = HIGH
C                   5 = RATE; 6 = PVO3
C     IPARM   - ARRAY FOR PARAMETER TYPE AND SPECIES ORDER #
C                   set to 1 if sens to that species, 0 otherwise
C     IAMOUNT - Amount by which emissions are perturbed in a sens param,
C                   ( NPMAX, MXSPCS, 25 ), the 25th slot is 1 if emis
C                   amounts specified, 0 otherwise
C     ILAYER  - Layers included in sens parameter
C                   ( NPMAX, NLAYS ), 1 if layer's emis included, 0
C                   otherwise, only used if emis amount specified,
C                   otherwise set to 1 for all layers
C     IDATE and ITIME - Dates and times included in sens parameter,
C               if not all dates & times are included; Note: to span
C               midnight, begin time may be larger than endtime
C     IREGION - Region of emission (1 = domainwide)
C     IRXN - Which chemical reaction for reaction rate sensitivities
C                   1 if sens to that reaction rate; 0 otherwise
C     SEN_PAR - Names of sensitivity parameters
C     NDAYS - No of dates in model run


      
      Use CGRID_SPCS, Only: NSPCSD, N_GC_SPC ! CGRID species number and offsets

      Implicit None

      Real, Save, Pointer              :: SENGRID( :,:,:,:,: )
      Real, Save, Pointer              :: SENGRID_TEMP( :,:,:,: )

      Real, Save, Allocatable          :: SAGRID( :,:,:,:,: )
      Integer, Save                    :: NPMAX = 1

      Integer, Save                    :: NDAYS = 1

      Integer, Allocatable, Save, Target :: TGT_IPT( : )
      Integer, Pointer :: IPT (:)
      Integer, Allocatable, Save, Target :: TGT_IPARM( :,: )
      Integer, Pointer :: IPARM ( :,: )
      CHARACTER( 16 ), Allocatable, Save, Target :: TGT_GRID_FILE( :,: )
      CHARACTER( 16 ), POINTER :: GRID_FILE( :,: )  ! ( NPMAX, 9 )  
      Integer, Allocatable, Save, Target :: TGT_GRID_NUM( : )
      Integer, Pointer ::  GRID_NUM( : )      

      CHARACTER( 16 ), Allocatable, Save, Target :: TGT_PT3D_FILE(:,:,:)
      CHARACTER( 16 ), POINTER :: PT3D_FILE( :,:,: )  ! ( NPMAX, 2, 9 )  
      Integer, Allocatable, Save, Target :: TGT_PT3D_NUM( : )
      Integer, Pointer ::  PT3D_NUM( : )

      REAL, Allocatable, Save, Target :: TGT_IAMOUNT( :,:,: )
      REAL, POINTER :: IAMOUNT( :,:,: )
      Integer, Allocatable, Save, Target :: TGT_ILAYER( :,: )
      Integer, Pointer :: ILAYER( :,: )

      Integer, Allocatable, Save, Target :: TGT_IRXN( :,: )
      Integer, Pointer :: IRXN( :,: )

      Integer, Allocatable, Save, Target :: TGT_IDATE( :,: )
      Integer, Pointer :: IDATE( :,: )
      Integer, Allocatable, Save, Target :: TGT_ITIME( :,: )
      Integer, Pointer :: ITIME( :,: )
      Real, Allocatable, Save, Target :: TGT_IREGION( :,:,:,:)
      Real, Pointer :: IREGION( :,:,:,: )
      Character( 8 ), Allocatable, Save, Target :: TGT_SENPAR( : )
      Character( 8 ), Pointer :: SEN_PAR( : )

      Real, Allocatable, Save ::  BSEN( :,:,: ) ! boundary sens - equivalent to BCON in hadvypmm.F

      Logical, Save           :: RST  ! Flag for using restart file
      Logical, Save           :: BCS  ! Flag for reading in boundary sensitivities
      Logical, Save           :: HIGH ! Flag for higher order sensitivities
c     Logical, Save           :: RGN  ! Flag for using regions files
c     Logical, Save           :: ES   ! Flag for pre-merged emissions

      Integer, Save           :: STARTDATE ! copy from STDATE and save for ddm to use 
      Integer                 :: DATENUM   ! 1 + jdate - stdate

      Integer NP, SENNUM

      Integer, Allocatable, Save, Target :: TGT_IHIGH( :,: )
      Integer, Pointer :: IHIGH ( :,: ) ! 2nd order mapping; IHIGH(NP,1) is 1st term; IHIGH(NP,2) is 2nd term

      CHARACTER( 16 ) :: SPCNAME

c Cloud module variables
      REAL( 8 ), ALLOCATABLE, SAVE       :: S_CONDEP( :,:,:,: )  ! sens of conv wdep
      REAL( 8 ), ALLOCATABLE, SAVE       :: S_TOTDEP( :,:,:,: )  ! sens of tot dep
      REAL( 4 ), ALLOCATABLE, SAVE       :: S_DEPWRT( :,:,: )    ! wdep write array

      REAL( 8 ), ALLOCATABLE, SAVE       :: S_POLC ( :,: )       ! sens of incloud conc (mol/mol) 
      REAL( 8 ), ALLOCATABLE, SAVE       :: S_CEND ( :,: )       ! sens of ending conc (mol/mol)
      REAL( 8 ), ALLOCATABLE, SAVE       :: S_REMOV( :,: )       ! sens of moles/m2 or mm*mol/lit scavenged
      REAL( 8 ), ALLOCATABLE, SAVE       :: S_REMOVAC( : )       ! sens of variable storing H+ deposition

      REAL,      ALLOCATABLE, SAVE       :: S_HPWDEP ( : )       ! hydrogen wet dep (mm mol/liter)
      REAL( 8 ), ALLOCATABLE, SAVE       :: S_GAS    ( :,: )     ! gas phase conc (mol/mol)
      REAL( 8 ), ALLOCATABLE, SAVE       :: S_GASWDEP( :,: )     ! gas phase wet dep array (mm mol/liter)
      REAL( 8 ), ALLOCATABLE, SAVE       :: S_AEROSOL( :,:,: )   ! aerosol conc (mol/mol)
      REAL( 8 ), ALLOCATABLE, SAVE       :: S_AERWDEP( :,:,: )   ! aerosol wet dep array (mm mol/liter)

      REAL( 8 ), ALLOCATABLE, SAVE       :: S_CCR     ( :,:,: )
      REAL( 8 ), ALLOCATABLE, SAVE       :: S_CBELOW  ( :,: )

      Character( 16 ), Allocatable, Save :: S_WETDEPSPC( : )     ! List of variable names in sens wet dep file

      REAL( 8 ), ALLOCATABLE, SAVE :: S_CONC    ( :,:,: )
      REAL( 8 ), ALLOCATABLE, SAVE :: S_BMOL    ( :,: )   ! moles/m2 species below cloud
      REAL( 8 ), ALLOCATABLE, SAVE :: S_CBASE0  ( :,: )   ! initial ave trace gas mix rat below cld
      REAL( 8 ), ALLOCATABLE, SAVE :: S_CBASEF  ( :,: )   ! final ave trac gas mix rat blw cld (moles/mole)
      REAL( 8 ), ALLOCATABLE, SAVE :: S_BCLDWT  ( :,:,: ) ! below cloud weighting function
      REAL( 8 ), ALLOCATABLE, SAVE :: S_INCLOUD ( :,:,: ) ! fin. in cloud conc. after mix and chem moles/mole
      REAL( 8 ), ALLOCATABLE, SAVE :: S_OUTCLOUD( :,:,: ) ! fin. outside cld conc. "   "   "   " moles/mole
      REAL( 8 ), ALLOCATABLE, SAVE :: S_PCLD    ( :,:,: ) ! moles sp/mole air in cloud
      REAL( 8 ) S_CONDIS
      REAL( 8 ) DDM3D_CONCMINL                            ! the conc small number is too small even with real(8)!!!!

c VDIFF module variables
      REAL, ALLOCATABLE, SAVE :: SNGRD( :,:,:,:,: )        ! sengrid replacement
      REAL, ALLOCATABLE, SAVE :: S_DDEP( :,:,:,: )         ! ddep sens accumulator
      REAL, ALLOCATABLE, SAVE :: SENS( :,:,: )             ! secondary SENGRID expression
      REAL, ALLOCATABLE, SAVE :: S_EMIS( :,:,: )           ! stores SVDEMIS*DTS
      REAL, ALLOCATABLE, SAVE :: S_DD ( :,:,: )            ! DD for sens 
      REAL, ALLOCATABLE, SAVE :: S_UU ( :,:,: )            ! UU for sens
      REAL, ALLOCATABLE, SAVE :: S_DDBF( :,: )             ! incremental SENDDEP
      REAL, ALLOCATABLE, SAVE :: S_POL( :,: )
      REAL S_DELC
      REAL, ALLOCATABLE, SAVE :: S_PLDV_HONO( : )

c Emissisons module variables
      Integer, Allocatable, Save         :: S_NSTREAMS( : )           ! Number of emissions streams per parameter
      Character( 32 ), Allocatable, Save :: S_STREAMLBL( :,: )        ! Emissions stream names per parameter (max = 99)
      Integer, Allocatable, Save         :: S_NRGN( : )               ! Number of emissions streams per parameter
      Character( 16 ), Allocatable, Save :: S_RGNLBL( :,: )           ! Emissions stream names per parameter (max = 99)
      Character( 16 ), Allocatable, Save :: S_SPCLIST( :,: )          ! User-specified species list
      REAL,    ALLOCATABLE               :: SVDEMIS_DIFF( :,:,:,:,: ) ! sensitivity emissions array mapped to diffused species
      INTEGER, ALLOCATABLE, SAVE         :: SENS_PER_STREAM( : )      ! number of senstivity parameters using the numbered stream
      INTEGER, ALLOCATABLE, SAVE         :: STREAM_TO_SENS( :,: )     ! mapping

c DEPV_DEF/m3dry variables
      REAL,    ALLOCATABLE, SAVE :: S_PVD( :,:,:,: )
      REAL,    ALLOCATABLE, SAVE :: S_CGRIDL1( :,:,:,: )
c     REAL,    ALLOCATABLE, SAVE :: S_CMP ( :,:,:,: )
      REAL,    ALLOCATABLE, SAVE :: S_PLDV( :,:,:,: )

c I/O variables
      Integer, Save         :: N_ASENS_VARS = 0

      Contains

C-----------------------------------------------------------------------
      Subroutine INIT_DDM3D

      Use UTILIO_DEFN
      Use HGRD_DEFN, Only: NCOLS, NROWS
      Use VGRD_DEFN, Only: NLAYS

      Use RXNS_DATA, Only: NRXNS

      Implicit None

c     Include SUBST_RXCMMN

c     Integer STDATE, STTIME  ! Starting date/time

      Character( 16 ) :: PNAME = 'INIT_DDM3D'
      Character( 16 ) :: CTM_NPMAX  = 'CTM_NPMAX'
      Character( 16 ) :: CTM_STTIME = 'CTM_STTIME'
      Character( 16 ) :: CTM_RUNLEN = 'CTM_RUNLEN'
      Character( 16 ) :: DDM3D_RST  = 'DDM3D_RST'   ! switch for sens restart file
      Character( 16 ) :: DDM3D_BCS  = 'DDM3D_BCS'   ! switch for reading boundary sensitivities
      Character( 16 ) :: DDM3D_HIGH = 'DDM3D_HIGH'  ! switch for higher order sensitivities
c     Character( 16 ) :: DDM3D_RGN  = 'DDM3D_RGN'   ! switch for using regions files
c     Character( 16 ) :: DDM3D_ES   = 'DDM3D_ES'    ! switch for pre-merged emissions

      Integer RUNLEN, BGNTIME

      Integer STATUS
      Character( 80 ) :: VARDESC
      Integer LOGDEV       ! FORTRAN unit number for log file

      Character( 96 ) :: XMSG = ' '
      Integer ALLOCSTAT

      LOGDEV = INIT3 ()

c     STARTDATE = STDATE

c Determine number of parameters
      VARDESC = 'Max number of sens parameters.'
      NPMAX = ENVINT( CTM_NPMAX, VARDESC, NPMAX, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         XMSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         XMSG = 'Envt variable set, but empty...Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, 0
      ELSE IF ( STATUS .EQ. -2 ) THEN
         XMSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, 0
      END IF

C Use the begin time and run length to determine how many
C dates are in the model run, for use in dimensioning IDATE
      BGNTIME = 000000         ! default
      VARDESC = 'Scenario Starting Time (HHMMSS)'
      BGNTIME = ENVINT( CTM_STTIME, VARDESC, BGNTIME, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         XMSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         XMSG = 'Envirnmt variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, BGNTIME
      ELSE IF ( STATUS .EQ. -2 ) THEN
         XMSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, BGNTIME
      END IF

      RUNLEN = 240000         ! default
      VARDESC = 'Scenario Run Duration (HHMMSS)'
      RUNLEN = ENVINT( CTM_RUNLEN, VARDESC, RUNLEN, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         XMSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         XMSG = 'Envirnmt variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, BGNTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
         XMSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, BGNTIME
      END IF

      NDAYS = 1 + FLOOR( REAL( BGNTIME + RUNLEN ) / 240000.0 )

c Allocate sensitivy arrays

c IREGION
      ALLOCATE   ( TGT_IREGION( NCOLS,NROWS,NLAYS,NPMAX ),
     &             STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_IREGION'
         CALL M3EXIT ( 'TGT_IREGION', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_IREGION = 0.0
      IREGION => TGT_IREGION

c IPT
      ALLOCATE   ( TGT_IPT( NPMAX ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_IPT'
         CALL M3EXIT ( 'TGT_IPT', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_IPT = 0
      IPT => TGT_IPT

c IPARM
      ALLOCATE   ( TGT_IPARM( NPMAX, NSPCSD ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_IPARM'
         CALL M3EXIT ( 'TGT_IPARM', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_IPARM = 0
      IPARM => TGT_IPARM

c GRID_FILE
      ALLOCATE   ( TGT_GRID_FILE( NPMAX, 9 ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_GRID_FILE'
         CALL M3EXIT ( 'TGT_GRID_FILE', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_GRID_FILE = ' '
      GRID_FILE => TGT_GRID_FILE

c PT3D_FILE
      ALLOCATE   ( TGT_PT3D_FILE( NPMAX, 2, 9 ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_PT3D_FILE'
         CALL M3EXIT ( 'TGT_PT3D_FILE', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_PT3D_FILE = ' '
      PT3D_FILE => TGT_PT3D_FILE

c GRID_NUM
      ALLOCATE   ( TGT_GRID_NUM( NPMAX ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_GRID_NUM'
         CALL M3EXIT ( 'TGT_GRID_NUM', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_GRID_NUM = 0
      GRID_NUM => TGT_GRID_NUM

c PT3D_NUM
      ALLOCATE   ( TGT_PT3D_NUM( NPMAX ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating TGT_PT3D_NUM'
               CALL M3EXIT ( 'TGT_PT3D_NUM', 0, 0, XMSG, XSTAT2 )
               END IF
      TGT_PT3D_NUM = 0
      PT3D_NUM => TGT_PT3D_NUM

c IAMOUNT
      ALLOCATE   ( TGT_IAMOUNT( NPMAX, NSPCSD, 25 ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_IAMOUNT'
         CALL M3EXIT ( 'TGT_IAMOUNT', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_IAMOUNT = 0.0
      IAMOUNT => TGT_IAMOUNT

c ILAYER
      ALLOCATE   ( TGT_ILAYER( NPMAX, NLAYS), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_ILAYER'
         CALL M3EXIT ( 'TGT_ILAYER', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_ILAYER = 0
      ILAYER => TGT_ILAYER

c IRXN
      ALLOCATE   ( TGT_IRXN( NPMAX,NRXNS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_IRXN'
         CALL M3EXIT ( 'TGT_IRXN', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_IRXN = 0
      IRXN => TGT_IRXN

c IDATE
      ALLOCATE   ( TGT_IDATE( NPMAX, NDAYS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_IDATE'
         CALL M3EXIT ( 'TGT_IDATE', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_IDATE = 0
      IDATE => TGT_IDATE

c ITIME
      ALLOCATE   ( TGT_ITIME( NPMAX, 2 ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_ITIME'
         CALL M3EXIT ( 'TGT_ITIME', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_ITIME = 0
      ITIME => TGT_ITIME

c SENPAR
      ALLOCATE   ( TGT_SENPAR( NPMAX ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_SENPAR'
         CALL M3EXIT ( 'TGT_SENPAR', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_SENPAR = ' '
      SEN_PAR => TGT_SENPAR

c Using restart file?
      RST = .TRUE.         ! default
      VARDESC = 'Flag for using sensitivity restart files'
      RST = ENVYN( DDM3D_RST, VARDESC, RST, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         XMSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         XMSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, A16, L5)' ) XMSG, DDM3D_RST, RST
      ELSE IF ( STATUS .EQ. -2 ) THEN
         XMSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, A16, L5)' ) XMSG, DDM3D_RST, RST
      END IF

c Reading sensitivity boundaries
      BCS = .FALSE.         ! default
      VARDESC='Flag for using sensitivities BCs'
      BCS = ENVYN( DDM3D_BCS, VARDESC, BCS, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         XMSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         XMSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, A16, L5)' ) XMSG, DDM3D_BCS, BCS
      ELSE IF ( STATUS .EQ. -2 ) THEN
         XMSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, A16, L5)' ) XMSG, DDM3D_BCS, BCS
      END IF

c Computing higher order sensitivities
      HIGH = .FALSE.         ! default
      VARDESC='Flag for computing higher order sensitivities'
      HIGH = ENVYN( DDM3D_HIGH, VARDESC, HIGH, STATUS )
      IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
      IF ( STATUS .EQ. 1 ) THEN
         XMSG = 'Environment variable improperly formatted'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      ELSE IF ( STATUS .EQ. -1 ) THEN
         XMSG = 'Environment variable set, but empty ... Using default:'
         WRITE( LOGDEV, '(5X, A, A16, L5)' ) XMSG, DDM3D_HIGH, HIGH
      ELSE IF ( STATUS .EQ. -2 ) THEN
         XMSG = 'Environment variable not set ... Using default:'
         WRITE( LOGDEV, '(5X, A, A16, L5)' ) XMSG, DDM3D_HIGH, HIGH
      END IF

c     ALLOCATE( YCDDM (N_GC_SPC),  STAT = ALLOCSTAT )
c     IF ( ALLOCSTAT .NE. 0 ) THEN
c        XMSG = 'Failure allocating YCDDM'
c        CALL M3EXIT ( 'YCDDM', 0, 0, XMSG, XSTAT2 )
c     END IF

c IHIGH
      ALLOCATE   ( TGT_IHIGH( NPMAX, 2 ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating TGT_IHIGH'
         CALL M3EXIT ( 'TGT_IHIGH', 0, 0, XMSG, XSTAT2 )
      END IF
      TGT_IHIGH = 0
      IHIGH => TGT_IHIGH

c Emissions
      ALLOCATE ( S_NSTREAMS( NPMAX ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating S_NSTREAMS(NPMAX)'
         CALL M3EXIT ( 'INIT_DDM3D', 0, 0, XMSG, XSTAT2 )
      END IF
      S_NSTREAMS = 0

      ALLOCATE ( S_STREAMLBL( NPMAX, 99 ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating S_STREAMLBL(NPMAX,99)'
         CALL M3EXIT ( 'INIT_DDM3D', 0, 0, XMSG, XSTAT2 )
      END IF
      S_STREAMLBL = 'empty'

      ALLOCATE ( S_NRGN( NPMAX ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating S_NRGN(NPMAX)'
         CALL M3EXIT ( 'INIT_DDM3D', 0, 0, XMSG, XSTAT2 )
      END IF
      S_NRGN = 0

      ALLOCATE ( S_RGNLBL( NPMAX,99 ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating S_RGNLBL(NPMAX,99)'
         CALL M3EXIT ( 'INIT_DDM3D', 0, 0, XMSG, XSTAT2 )
      END IF
      S_RGNLBL = 'empty'

      ALLOCATE ( S_SPCLIST( NPMAX, 999 ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         XMSG = 'Failure allocating S_SPCLIST( NPMAX, 999 )'
         CALL M3EXIT ( 'INIT_DDM3D', 0, 0, XMSG, XSTAT2 )
      END IF
      S_SPCLIST = ''


c Get sensitivity parameter information from control file
      CALL SINPUT( 0, 0 )

      Return
      End Subroutine INIT_DDM3D

C-----------------------------------------------------------------------
      Subroutine CKTIME( JDATE,JTIME,PRM,TIMEFLAG )

C-----------------------------------------------------------------------
C Function:
C   CKTIME determines whether the current model time, JTIME, is
C   within the time range ITIME specified in sensinput.dat.
C   Returns TIMEFLAG as TRUE or FALSE.
C   Called by advection and diffusion routines in sensitivity analysis.
C Revision History:
C   Created July 02 Daniel Cohan
C 15th March 2006: SRTonse, LBL, if time period straddles midnight
C then both dates need to be set as true and only the hours connected
C to the straddling period are set with TIMEFLAG TRUE
C 18 Jun 13 S.Napelenok: ddm-3d implementation for cmaq5.0.1;
C                        moved into this module from a separate file
C-----------------------------------------------------------------------

      Use UTILIO_DEFN

      Implicit None

      Integer, Intent(In)  :: JDATE       ! current date
      Integer, Intent(In)  :: JTIME       ! current model time, coded HHMMSS
      Integer, Intent(In)  :: PRM         ! number of current sensitivity parameter
      Logical, Intent(Out) :: TIMEFLAG    ! check for valide date/time

      Integer              :: NAD, I


      TIMEFLAG = .False.

C time period does not straddle midnight
      IF ( ( JTIME .GE. ITIME ( PRM,1 )) .AND. (JTIME .LE. ITIME(PRM,2 ))) THEN
        TIMEFLAG = .TRUE.

C time period straddles midnight
      ELSE IF ( ITIME ( PRM,1 ) .GT. ITIME ( PRM,2 ) ) THEN
C count number of active dates set
        NAD = 0
        DO I = 1,NDAYS
           IF(IDATE(PRM,I) .EQ. 1)NAD = NAD + 1
        END DO
C if all dates set true do not bother to test on date, test on time only
        IF(NAD .EQ. NDAYS)THEN
           TIMEFLAG =
     &     (JTIME .GE. ITIME(PRM,1) .AND. JTIME .LE. 240000) .OR.
     &     (JTIME .LE. ITIME(PRM,2) .AND. JTIME .GE. 0 )
        ELSE
C test on date, and only pick times that straddle 2 active dates
           I =  1 + JDATE - STARTDATE     !date index in IDATE array
           TIMEFLAG =
     &     (JTIME.GE.ITIME(PRM,1) .AND. JTIME .LE. 240000
     &     .AND. IDATE(PRM,I).EQ.1 .AND. IDATE(PRM,I+1).EQ.1)
     &     .OR.
     &     (JTIME.LE.ITIME(PRM,2) .AND. JTIME .GE. 0
     &     .AND. IDATE(PRM,I-1).EQ.1 .AND. IDATE(PRM,I).EQ.1)
        END IF
      END IF

      Return
      End Subroutine CKTIME

C-----------------------------------------------------------------------

      End Module DDM3D_DEFN
#endif
