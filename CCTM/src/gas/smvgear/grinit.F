
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
C                    29 Mar 11 S.Roselle: Use UTILIO_DEFN for external functions
C***********************************************************************
      USE RUNTIME_VARS
      USE GRVARS                ! inherits GRID_CONF
      USE UTILIO_DEFN

      IMPLICIT NONE
      
C.....INCLUDES: None

C.....ARGUMENTS: None

C.....PARAMETERS: None

C.....EXTERNAL FUNCTIONS: None
      
C.....SAVED VARIABLES: None

C.....LOCAL VARIABLES:
      CHARACTER( 80 ) :: VARDESC   ! Description of environment variable 
      CHARACTER( 96 ) :: MSG       ! Error message
      CHARACTER( 16 ) :: PNAME = 'GRINIT'     ! Program Name
            
      INTEGER COL            ! Column number index
      INTEGER IAVGSIZE       ! Average number of cells per block
      INTEGER IDUMMY         ! Dummy integer variable 
      INTEGER IERRST         ! Ouput error status number
      INTEGER LEV            ! Level number index
      INTEGER NBLK           ! Block number index
      INTEGER NCOUNT         ! Counter for number of cells for grid
      INTEGER NOXYZ          ! Total number of cells for grid
      INTEGER ROW            ! Row number index
      INTEGER STATUS         ! Status code for functions

C***********************************************************************
      
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set control parameters for output report options
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
#ifdef debug_gear
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
#endif
   
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set some constants for the Gear solver
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      LREORDER       = .TRUE.
      HMAXDAY( NCS ) = 15.0D0
      HMAXNIT        = 5.0D0
      HMIN           = 1.0D-10
      YLOW   ( NCS ) = REAL( GEAR_MIN_ATOL, 8 )
      ERRMAX ( NCS ) = REAL( GEAR_RTOL, 8 )
      FRACDEC        = 0.25D0
      CONCMIN        = 1.0D-30
      ZBOUND         = 1.0D-30

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Get number of cells in grid and store i,j,k indices of cells in
c  sequential order
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NCOUNT = 0
      DO LEV = 1, NLAYS
         DO COL = 1, NCOLS
            DO ROW = 1, NROWS
               NCOUNT = NCOUNT + 1
               CROW( NCOUNT ) = ROW
               CCOL( NCOUNT ) = COL
               CLEV( NCOUNT ) = LEV
               IF( ROW .EQ. 1 .AND. COL .EQ. 1 .AND. LEV .EQ. 1 )NCELL1 = NCOUNT
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
         
