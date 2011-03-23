
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
C $Header: /project/yoj/arc/CCTM/src/gas/smvgear/grinit.F,v 1.1 2011/03/23 21:13:16 sjr Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C @(#)grinit.F  1.1 /project/mod3/CMAQ/src/chem/smvgear/SCCS/s.grinit.F 07 Jul 1 997 12:45:26

      SUBROUTINE GRINIT  

C**********************************************************************
C
C  FUNCTION: To initialize Gear chemistry solver variables and to group 
C            cells into blocks
C
C  PRECONDITIONS: None                                       
C                                                                      
C  RETURN VALUES: All values stored in common blocks                 
C                                                                      
C  KEY SUBROUTINES/FUNCTIONS CALLED: None
C
C  REVISION HISTORY: Prototype created By Jerry Gipson, June, 1995.
C
C                    Revised 3/14/96 by Jerry Gipson to conform to
C                      Models-3 minimum IOV configuration. 
C                    Revised December 1996 by Jerry Gipson to conform
C                      to the Models-3 interim CTM that includes emissions
C                      in chemistry.
C                    Revised April 1997 to conform to Models-3 beta
C                      version
C                    Revised May 1997 to optionally get Gear tolerances
C                      from environment variables
C                    Modified June, 1997 by Jerry Gipson to be consistent
C                      with beta CTM
C                    Modified September, 1997 by Jerry Gipson to be
C                      consistent with the targetted CTM
C                    Mod for unicode by Jeff, Feb. 1999
C                    16 Aug 01 J.Young: Use HGRD_DEFN; Use GRVARS
C                    31 Jan 05 J.Young: dyn alloc - establish both horizontal
C                    & vertical domain specifications in one module (GRID_CONF)
C                    28 Jun 10 J.Young: remove unnecessary modules and includes
C                    30 Jun 10 J.Young: convert for Namelist redesign; move all
C                    local include file variables into GRVARS module
C***********************************************************************

      USE GRVARS                ! inherits GRID_CONF

      IMPLICIT NONE
      
C.....INCLUDES: None

C.....ARGUMENTS: None

C.....PARAMETERS: None

C.....EXTERNAL FUNCTIONS:
      INTEGER, EXTERNAL :: JUNIT,   ! Get an unused unit number
     &                     INIT3    ! Get the log unit number
      
      REAL,    EXTERNAL :: ENVREAL  ! Get a REAL from an environment variable
      
C.....SAVED VARIABLES: None

C.....LOCAL VARIABLES:
      CHARACTER( 80 ) :: VARDESC   ! Description of environment variable 
      CHARACTER( 96 ) :: MSG       ! Error message
      CHARACTER( 16 ) :: PNAME = 'GRINIT'     ! Program Name
      CHARACTER( 16 ) :: GEAR_RTOL = 'GEAR_RTOL' ! Name of env variable for RTOL
      CHARACTER( 16 ) :: GEAR_ATOL = 'GEAR_ATOL' ! Name of env variable for ATOL
            
      INTEGER COL            ! Column number index
      INTEGER IAVGSIZE       ! Average number of cells per block
      INTEGER IDUMMY         ! Dummy integer variable 
      INTEGER IERRST         ! Ouput error status number
      INTEGER LEV            ! Level number index
      INTEGER LOGDEV         ! Unit number of output log
!     INTEGER OFFSET         ! Pointer for start cell number in a block
      INTEGER NBLK           ! Block number index
      INTEGER NCOUNT         ! Counter for number of cells for grid
      INTEGER NOXYZ          ! Total number of cells for grid
      INTEGER ROW            ! Row number index
      INTEGER STATUS         ! Status code for functions

      REAL ATOL              ! Absolute tolerance parameter
      REAL DEFTOL            ! Default tolerance
      REAL RTOL              ! Relative tolerance parameter
C***********************************************************************
      
      LOGDEV = INIT3( )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set control parameters for output report options
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LDEBUG = .FALSE.
      IF( LDEBUG ) THEN
         IRUNBUG  = 1 
         NSTEPOUT = 1 
         NSUBOUT  = 1 
         NPDOUT   = 1
         DBGCOL   = 10
         DBGROW   = 10
         DBGLEV   = 1
         IUNDBG   = JUNIT( )  
         OPEN( UNIT = IUNDBG, FILE = 'DEBUG.OUT' )        
      ENDIF
      
      LDUMPBLK = .FALSE.
      IF( LDUMPBLK ) THEN
         IRUNBLK = 1
         IBLKBLK = 1
         IUNBIC  = JUNIT( )
         OPEN( UNIT = IUNBIC, FILE = 'ICBLKOUT.OUT', 
     &         FORM = 'UNFORMATTED' )
      ENDIF
      
      LDUMPCELL = .FALSE.
      IF( LDUMPCELL ) THEN
         IRUNCELL = 1
         IBLKCELL = 1
         INUMCELL = 1
         IUNCIC   = JUNIT( )
         OPEN( UNIT = IUNCIC, FILE= 'ICCELLOUT.OUT' )
      ENDIF
      
      LTRACE = .FALSE.
      IF( LTRACE ) THEN
         IRUNTRC1 = 1
         IRUNTRC2 = 2
         IBLKTRC  = 1
         IUNTRC   = JUNIT( ) 
         OPEN( UNIT = IUNTRC, FILE = 'TRACE.OUT' )
         WRITE( IUNTRC, 93020 ) IBLKTRC
         WRITE( IUNTRC, 93040 )
      ENDIF
      
      LPERFSMRY = .FALSE.
      IF( LPERFSMRY ) THEN
        IUNPERF = JUNIT( )
        OPEN( UNIT = IUNPERF, FILE= 'PERFSMRY.OUT',
     &        FORM = 'UNFORMATTED')   
      ENDIF
      
      LCELLCONC = .FALSE.
      IF( LCELLCONC ) THEN
         RUNMIN   = 0.0
         IRUNPRO1 = 1
         IRUNPRO2 = 4
         CROWOUT  = 18 
         CCOLOUT  = 18
         CLEVOUT  = 1
         IUNCOUT  = JUNIT( ) 
         OPEN( UNIT = IUNCOUT, FILE = 'PROFILE.OUT', 
     &         FORM = 'UNFORMATTED')
      ENDIF
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set some constants for the Gear solver
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      VARDESC = 'Absolute tolerance for SMVGEAR'
      DEFTOL = 1.0E-09
      ATOL = ENVREAL( GEAR_ATOL, VARDESC, DEFTOL, STATUS )
      IF ( STATUS .EQ. 1 ) THEN
         MSG = 'Environment variable' // GEAR_ATOL //
     &         ' improperly formatted'
         CALL M3EXIT( PNAME, 0, 0, MSG, 0 )
      ELSEIF ( STATUS .LT. 0 ) THEN
         ATOL = 1.0E-09
      ENDIF
      
      VARDESC = 'Relative tolerance for SMVGEAR'
      DEFTOL = 1.0E-03
      RTOL = ENVREAL( GEAR_RTOL, VARDESC, DEFTOL, STATUS )
      IF ( STATUS .EQ. 1 ) THEN
         MSG = 'Environment variable' // GEAR_RTOL //
     &         ' improperly formatted'
         CALL M3EXIT( PNAME, 0, 0, MSG, 0 )
      ELSEIF ( STATUS .LT. 0 ) THEN
         RTOL = 1.0E-03
      ENDIF

      LREORDER       = .TRUE.
      HMAXDAY( NCS ) = 15.0
      HMAXNIT        = 5.0
      HMIN           = 1.0E-10
      YLOW   ( NCS ) = ATOL
      ERRMAX ( NCS ) = RTOL
      FRACDEC        = 0.25
      CONCMIN        = 1.0E-30
      ZBOUND         = 1.0E-30

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get number of cells in grid and store i,j,k indices of cells in
c  sequential order
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NCOUNT = 0
      DO LEV = 1, NLAYS
         DO COL = 1, MY_NCOLS
            DO ROW = 1, MY_NROWS
               NCOUNT = NCOUNT + 1
               CROW( NCOUNT ) = ROW
               CCOL( NCOUNT ) = COL
               CLEV( NCOUNT ) = LEV
            END DO
         END DO
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Define block structure for grid; stop if maxblks exceeded
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NOXYZ = NCOUNT
      NBLKS = 1 + ( NOXYZ - 1 ) / BLKSIZE
      IF( NBLKS .GT. MXBLKS ) THEN
         WRITE( MSG, 92020 ) NBLKS, MXBLKS
         IDUMMY = 0
         IERRST = 2
         CALL M3EXIT( PNAME, IDUMMY, IDUMMY, MSG, IERRST )
      ENDIF

      IAVGSIZE = 1 + ( NOXYZ - 1 ) / NBLKS
      IAVGSIZE = MIN( IAVGSIZE, BLKSIZE )
      OFFSET = 0

      DO NBLK = 1, NBLKS
         IF( NBLK .LT. NBLKS ) THEN
            BLKCNO( NBLK ) = OFFSET
            BLKLEN( NBLK ) = IAVGSIZE
            OFFSET = OFFSET + IAVGSIZE   ! updates in GRVARS module for use in
         ELSE                            ! GRDRIVER and GRSMVGEAR
            BLKCNO( NBLK ) = OFFSET
            BLKLEN( NBLK ) = NOXYZ - ( ( NBLK-1 ) * IAVGSIZE )
         ENDIF
      END DO

      RETURN
      
C********************** FORMAT STATEMENTS ******************************      
92020 FORMAT( 1X, 'ERROR: MAXIMUM NUMBER OF BLOCKS EXCEEDED',
     &            ' FOR GRID', 'NBLKS=', I3, 1X, ' MAXBLKS=',
     &            I3, '  CHANGE GRPARMS.EXT' )
93020 FORMAT( 1X, 'TRACE FOR BLOCK = ', I3 )               
93040 FORMAT( 1X, 'RUN STP# NFE NJE ITR CFL EFL QUSD     TIME',
     &             '      H USED       HRATIO' )            

      END
         
