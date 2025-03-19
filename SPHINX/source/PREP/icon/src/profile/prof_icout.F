
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
C $Header: /project/yoj/arc/ICON/src/profile/prof_icout.F,v 1.2 2011/10/21 16:41:56 yoj Exp $ 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


      SUBROUTINE PROF_ICOUT ( LOGUNIT, PROF_FL_NAME )

C***********************************************************************
 
C  Function: Reads the input profile concentration file and opens and
C            writes the output IC file
              
C  Preconditions: None
  
C  Key Subroutines/Functions Called: OPN_IC_FILE
C                                    PROF_VINTERP   
 
C  Revision History:
C  Prototype created by Jerry Gipson, January, 1998
C  Modified April 1998 by JG to remove ppb option for 
C     input profile file
C  Modified 4/22/99 by JG to provide default treatment
C     for missing SDATE and STIME
C  Modified 04/20/00 by David Wong, LM
C     -- enable parallelization
C  01/24/02 Steve Howard (Jeff Young) - dynamic allocation
C  12/13/04 J.Young: vert dyn alloc - Use VGRD_DEFN
C  06 Jun 11 J.Young: Replaced I/O API include files with UTILIO_DEFN
C  21 May 12 J.Young: Replaced IC_PARMS include file with an F90 module
C  02 Sep 18 C.Hogrefe and S.Roselle: Adapted to read new profile format;
C                       removed species mapping
C  06 Nov 18 S.Roselle: Replaced UTILIO_DEFN with M3UTILIO
 
C***********************************************************************

      USE HGRD_DEFN   ! Module to store and load the horizontal grid variables
      USE VGRD_DEFN   ! vertical layer specifications
      USE M3UTILIO    ! IOAPI module
      USE IC_PARMS    ! ICON parameters

      IMPLICIT NONE     


C Arguments: 
      INTEGER, INTENT( IN ) :: LOGUNIT        ! Unit number for output log
      CHARACTER( 256 ), INTENT( IN ):: PROF_FL_NAME        ! Input profile file name

C Parameters: none

C Local Variables:
      CHARACTER( 80 ) :: MSG = ' '            ! Log message
      CHARACTER( 16 ) :: PNAME = 'PROF_ICOUT' ! Program Name
      CHARACTER( 16 ) :: VNAME                ! Species name on CTM conc file
      CHARACTER( 16 ), ALLOCATABLE :: IC_FNAME( : )  ! Logical names of IC Output file(s)

      CHARACTER(1000) :: RECORD
      CHARACTER(16), ALLOCATABLE :: VNAME_PROF_TEMP(:)
      CHARACTER(16), ALLOCATABLE :: VUNIT_PROF_TEMP(:)
      CHARACTER(16), ALLOCATABLE :: PROF_SP_NAME( : ) ! Profile species names
      CHARACTER(16), ALLOCATABLE :: UNITS_IN( : ) ! Units for CTM species
      CHARACTER(80), ALLOCATABLE :: VDESC_IN( : ) ! Variable description for CTM species

      INTEGER NSPCS_IN      ! Total No. of species in input file(s)
      INTEGER NLAYS_IN      ! No. of layers in input file
      INTEGER C, R          ! Column, Row loop indices
      INTEGER FLN           ! IC output file number
      INTEGER L             ! Layer loop index
      INTEGER N             ! Loop indices for species
      INTEGER PFILE         ! Unit number of profile file   
      INTEGER SDATE         ! Date for IC Output
      INTEGER STIME         ! Time for IC output
      INTEGER TSTEP         ! Timestep for IC Output

      INTEGER I, K, NVARS, NFIELDS
      INTEGER IOSTATUS      ! Status code
      INTEGER ALLOCSTAT     ! Status returned from array allocation

      INTEGER, ALLOCATABLE :: VTYPE_IN( : ) ! variable type for CTM species

      LOGICAL  LNEG          ! Flag for negative concentrations

      REAL, ALLOCATABLE :: ZH_IN( : )        ! Input layer heights
      REAL, ALLOCATABLE :: COUT( :,:,: )     ! Output IC conc
      REAL, ALLOCATABLE :: INPROF( :,: )     ! Input conc profiles
      REAL, ALLOCATABLE :: VIPROF( :,:,:,: ) ! Vert. interp. profiles
                            
      REAL, ALLOCATABLE :: VAR_PROF_TEMP( :,: )

      INTERFACE

         SUBROUTINE PROF_VINTERP( LOGUNIT, NSPCS_IN, NLAYS_IN, ZH_IN,
     &                            CONCIN, CONCOUT )
            INTEGER, INTENT( IN ) :: LOGUNIT
            INTEGER, INTENT( IN ) :: NSPCS_IN
            INTEGER, INTENT( IN ) :: NLAYS_IN
            REAL, INTENT( IN )  :: ZH_IN( : )
            REAL, INTENT( IN )  :: CONCIN( :,: )
            REAL, INTENT( OUT ) :: CONCOUT( :,:,:,: )
         END SUBROUTINE PROF_VINTERP

         SUBROUTINE OPN_IC_FILE ( LOGUNIT, SDATE, STIME, TSTEP, NSPCS_OUT,
     &                            SPNAME_OUT, VTYPE_OUT, UNITS_OUT,
     &                            VDESC_OUT, IC_FNAME, RINDX )
            CHARACTER( 16 ), INTENT( OUT ) :: IC_FNAME( : )
            CHARACTER( 16 ), INTENT( IN )  :: SPNAME_OUT( : )
            CHARACTER( 16 ), INTENT( IN )  :: UNITS_OUT( : )
            CHARACTER( 80 ), INTENT( IN )  :: VDESC_OUT( : )
            INTEGER, INTENT( IN ) :: LOGUNIT
            INTEGER, INTENT( IN ) :: NSPCS_OUT
            INTEGER, INTENT( IN ) :: RINDX
            INTEGER, INTENT( IN ) :: SDATE
            INTEGER, INTENT( IN ) :: STIME
            INTEGER, INTENT( IN ) :: TSTEP
            INTEGER, INTENT( IN ) :: VTYPE_OUT( : )
         END SUBROUTINE OPN_IC_FILE

      END INTERFACE

C***********************************************************************

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Read the input profile file data  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PFILE = JUNIT()

      OPEN( UNIT = PFILE, FILE = PROF_FL_NAME, ERR = 999 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  read comment lines
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DO 
         READ( PFILE,'(A)',IOSTAT = IOSTATUS ) RECORD
         IF ( IOSTATUS .NE. 0 ) THEN
            MSG = 'Error reading profile file'
            CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
         END IF
         IF ( RECORD(1:1) .NE. '#' ) EXIT
      END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  done reading comment lines, now parse column names
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      NFIELDS = 1
      DO I = 1, LEN( RECORD )
         IF ( RECORD(I:I) .EQ. ',' ) NFIELDS = NFIELDS + 1
      END DO
      NLAYS_IN = NFIELDS - 2 ! FIELDS 1 AND 2 ARE NAME AND UNITS

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  allocate arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      ALLOCATE( VAR_PROF_TEMP( MX_INFL_SP, NLAYS_IN ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating VAR_PROF_TEMP'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( INPROF( NLAYS_IN, MX_INFL_SP ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating INPROF'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( ZH_IN( NLAYS_IN ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating ZH_IN'
         CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( VNAME_PROF_TEMP( MX_INFL_SP),
     &          VUNIT_PROF_TEMP( MX_INFL_SP),
     &          PROF_SP_NAME( MX_INFL_SP ),
     &          STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating VNAME_PROF_TEMP, VUNIT_PROF_TEMP, PROF_SP_NAME'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( UNITS_IN( MX_INFL_SP ),
     &          VDESC_IN( MX_INFL_SP ),
     &          VTYPE_IN( MX_INFL_SP ),
     &          STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating UNITS_IN, VDESC_IN, VTYPE_IN'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      ALLOCATE( IC_FNAME( MXCTMS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating IC_FNAME'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  read data fields
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      NVARS = 1
      DO 
         READ( PFILE,*,IOSTAT = IOSTATUS )
     &      VNAME_PROF_TEMP( NVARS ),
     &      VUNIT_PROF_TEMP( NVARS ),
     &    ( VAR_PROF_TEMP( NVARS,K ), K=1,NLAYS_IN )

         IF ( IOSTATUS .NE. 0 ) THEN
            IF ( NVARS .EQ. 1 ) THEN    ! error in first data line
               MSG = 'Error: no species data in profile file'
               CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
            ELSE
               EXIT
            END IF
         END IF
         NVARS = NVARS + 1
      END DO
      NVARS = NVARS - 1

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  load data into species arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      NSPCS_IN = 0
      INPROF = 0.0
      DO N = 1, NVARS
         IF ( N .EQ. INDEX1( 'PRES', NVARS, VNAME_PROF_TEMP ) ) THEN
         	  CYCLE
         ELSE IF ( N .EQ. INDEX1( 'ZH', NVARS, VNAME_PROF_TEMP ) ) THEN
            DO L = 1, NLAYS_IN
               ZH_IN( L ) = VAR_PROF_TEMP( N,L )
            END DO
         ELSE IF ( N .EQ. INDEX1( 'ZF', NVARS, VNAME_PROF_TEMP ) ) THEN
         	  CYCLE
         ELSE IF ( N .EQ. INDEX1( 'WVEL', NVARS, VNAME_PROF_TEMP ) ) THEN
         	  CYCLE
         ELSE
            NSPCS_IN = NSPCS_IN + 1
            PROF_SP_NAME( NSPCS_IN ) = VNAME_PROF_TEMP( N )
            UNITS_IN( NSPCS_IN )     = VUNIT_PROF_TEMP( N )
            VTYPE_IN( NSPCS_IN )     = M3REAL
            VDESC_IN( NSPCS_IN )     = VNAME_PROF_TEMP( N )
            DO L = 1, NLAYS_IN
               INPROF( L,NSPCS_IN ) = VAR_PROF_TEMP( N,L )
            END DO
         END IF
      END DO

      ALLOCATE( VIPROF( NCOLS, NROWS, NLAYS, NSPCS_IN ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating VIPROF'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  vertical interpolation
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      CALL PROF_VINTERP ( LOGUNIT, NSPCS_IN, NLAYS_IN, ZH_IN, INPROF, VIPROF )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Open IC output file
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SDATE = 0
      STIME = 0
      TSTEP = 0
      CALL OPN_IC_FILE ( LOGUNIT, SDATE, STIME, TSTEP, NSPCS_IN,
     &                   PROF_SP_NAME, VTYPE_IN, UNITS_IN, VDESC_IN,
     &                   IC_FNAME, 1 )

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Write the output IC concentrations
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ALLOCATE( COUT( NCOLS, NROWS, NLAYS ), STAT = ALLOCSTAT )
      IF ( ALLOCSTAT .NE. 0 ) THEN
         MSG = 'Failure allocating COUT'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT1 )
      END IF

      LNEG = .FALSE.

      DO N = 1, NSPCS_IN

         FLN = ( N - 1 ) / MXVARS3 + 1
         VNAME = PROF_SP_NAME( N )
           
         DO L = 1, NLAYS
            DO R = 1, NROWS
               DO C = 1, NCOLS
                  COUT( C,R,L ) = VIPROF( C,R,L,N )
               END DO
            END DO
         END DO
       
         IF ( .NOT. WRITE3( IC_FNAME( FLN ), VNAME, SDATE, STIME,
     &                     COUT( 1,1,1 ) ) ) THEN
            MSG =  'Could not WRITE species ' //  VNAME // 
     &             'to file ' // IC_FNAME( FLN ) 
            CALL M3EXIT ( PNAME, SDATE, STIME, MSG, XSTAT2 )
         END IF

      END DO

      IF ( LNEG ) THEN
         MSG = 'Negative ICs output'
         CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )
      END IF

      RETURN

999   CONTINUE 

      MSG = 'Could not open file ' // PROF_FL_NAME
      CALL M3EXIT ( PNAME, 0, 0, MSG, XSTAT2 )

C************************* FORMAT STATEMENTS ***************************

94000 FORMAT( 1X )

      END
