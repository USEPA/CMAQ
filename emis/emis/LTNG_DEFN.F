!C***********************************************************************
!C   Portions of Models-3/CMAQ software were developed or based on      *
!C   information from various groups: Federal Government employees,     *
!C   contractors working on a United States Government contract, and    *
!C   non-Federal sources (including research institutions).  These      *
!C   research institutions have given the Government permission to      *
!C   use, prepare derivative works, and distribute copies of their      *
!C   work in Models-3/CMAQ to the public and to permit others to do     *
!C   so.  EPA therefore grants similar permissions for use of the       *
!C   Models-3/CMAQ software, but users are requested to provide copies  *
!C   of derivative works to the Government without restrictions as to   *
!C   use by others.  Users are responsible for acquiring their own      *
!C   copies of commercial software associated with Models-3/CMAQ and    *
!C   for complying with vendor requirements.  Software copyrights by    *
!C   the MCNC Environmental Modeling Center are used with their         *
!C   permissions subject to the above restrictions.                     *
!C***********************************************************************

!C RCS file, release, date & time of last delta, author, state, [and locker]
!C $Header: /project/yoj/arc/CCTM/src/emis/emis/LTNG_DEFN.F,v 1.1 2011/04/27 17:19:04 sjr Exp $

!C what(1) key, module and SID; SCCS file; date and time of last delta:
!C %W% %P% %G% %U%

!C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE LTNG_DEFN

!C----------------------------------------------------------------------
!C Function: production of NO from lightning

!C Revision History:
!C     1 Jul 2010: Jeff Young
!C     1 Jan 2011: Rob Pinder: added support for online calculation of 
!C                 lightning NO from convective precip and NLDN data
!C    10 Mar 2011: Jeff Young: code revisions
!C    11 Apr 2011: S.Roselle: replaced I/O API include files with UTILIO_DEFN
!C----------------------------------------------------------------------

      IMPLICIT NONE

      LOGICAL, SAVE :: LTNG_NO          ! flag for lightning NO emissions
      INTEGER, SAVE :: LTNG_MAP         ! map to GC NO
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_LT( :,:,: )   ! lightning emis

      PUBLIC LTNG_NO, LTNG_MAP, VDEMIS_LT, LTNG_INIT, GET_LTNG
      PRIVATE

!C lightning emis species name
      CHARACTER( 16 ), PARAMETER :: LTSPC = 'NO'

      LOGICAL, SAVE :: LPARAM     ! flag to use lightning NO parameters file
      LOGICAL, SAVE :: LTNGDIAG   ! flag to turn on lightning NO diagnostics
      INTEGER, SAVE :: LTLYRS     ! no. of emissions layers
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_DIAG( :,:,: ) ! lightning NO diagnostic

!C file name for off line lightning NO input; set to "InLine" for inline production
      CHARACTER( 16 ), SAVE :: LNAME
!C file name for lightning NO diagnostic output
      CHARACTER( 16 ), SAVE :: DNAME = 'LTNGOUT'
!C file name for inline lightning parameters, (only used if LNAME = "InLine")
      CHARACTER( 16 ), SAVE :: ANAME
!C file name for met file for inline lightning
      CHARACTER( 16 ), SAVE :: MNAME

!C allocate these if LNAME = 'InLine'
      REAL,    ALLOCATABLE, SAVE :: COLUMN_LTNG_NO( :,: ) ! column total NO
      REAL,    ALLOCATABLE, SAVE :: LTNG_PRSFC    ( :,: ) ! surface pressure
      REAL,    ALLOCATABLE, SAVE :: LTNG_RC       ( :,: ) ! convective rainfall
!     REAL,    ALLOCATABLE, SAVE :: LTNG_CLDT     ( :,: ) ! cloud top
!C allocate these if LPARAM
      REAL,    ALLOCATABLE, SAVE :: STRIKE_FACTOR ( :,: ) ! flashes per unit precip
      REAL,    ALLOCATABLE, SAVE :: LRATIO        ( :,: ) ! ratio derived from NLDN data
      REAL,    ALLOCATABLE, SAVE :: OCEAN_MASK    ( :,: ) ! reduce offshore strikes
      REAL,    ALLOCATABLE, SAVE :: ICCG          ( :,: ) ! intercloud strikes per cloud to ground strike
      REAL,    ALLOCATABLE, SAVE :: MOLSNFLASH    ( :,: ) ! moles N per flash

!C    scenario time/date needed for diagnostic output
      INTEGER, SAVE :: SDATE     ! scenario start date
      INTEGER, SAVE :: STIME     ! scenario start time

      INTEGER, SAVE :: NSTEPS    ! run duration
      INTEGER, SAVE :: NTICS = 0 ! no. of substeps within an output tstep
      INTEGER, SAVE :: LDATE     ! test date to update emissions diag avg
      INTEGER, SAVE :: LTIME     ! test time to update emissions diag avg
      INTEGER, SAVE :: LOGDEV

!C Vertical coord values
      REAL,    ALLOCATABLE, SAVE :: VGLVSLT( : )

      INTEGER, SAVE :: GXOFF, GYOFF   ! global origin offset from file
      INTEGER, SAVE :: STRTCOLLT3, ENDCOLLT3, STRTROWLT3, ENDROWLT3

      CONTAINS

!C======================================================================
!C Initialize lightning routines
 
         FUNCTION LTNG_INIT ( JDATE, JTIME, TSTEP, EMLAYS ) RESULT ( SUCCESS )

         USE GRID_CONF ! horizontal & vertical domain specifications
         USE CGRID_SPCS         ! CGRID mechanism species
         USE UTILIO_DEFN

         IMPLICIT NONE

!C Includes:
         INCLUDE SUBST_CONST     ! constants

!C Arguments:
C         INTEGER, INTENT( IN ) :: JDATE, JTIME, TSTEP
C         INTEGER, INTENT( IN ) :: EMLAYS     ! number of emissions layers
         INTEGER :: JDATE, JTIME, TSTEP
         INTEGER :: EMLAYS     ! number of emissions layers

         LOGICAL SUCCESS

!C External Functions:
         INTEGER, EXTERNAL :: SETUP_LOGDEV
         LOGICAL, EXTERNAL :: CHKGRID

!C Local Variables:
         CHARACTER( 16 ), SAVE :: CTM_LTNG_NO = 'CTM_LTNG_NO'  ! env var flag
         CHARACTER( 16 )       :: PNAME = 'LTNG_INIT'
         CHARACTER( 80 )       :: VARDESC   ! env variable description
         CHARACTER( 120 )      :: XMSG = ' '

         LOGICAL LTNGPARAM           ! env var to use lightning NO parameters file
         INTEGER LSPCS               ! no. of lightning species
         REAL    CONVEM              ! conversion for emissions rates to Kg/s
         INTEGER J, K, L, V, STATUS
         LOGICAL OK

         INTEGER :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
!-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         LOGDEV = SETUP_LOGDEV()

!C Read in scenario time/date
         SDATE = ENVINT( 'CTM_STDATE', 'Scenario Start (YYYYJJJ)', 0, STATUS )
         STIME = ENVINT( 'CTM_STTIME', 'Scenario Start (HHMMSS)', 0, STATUS )
         NSTEPS = ENVINT( 'CTM_RUNLEN', 'Scenario Duration (HHMMSS)', 24, STATUS )
         NSTEPS = NSTEPS / TSTEP           ! initscen guarantees divisibility

!C Lightning NO production
!C Set LTNG_NO to Y or T to turn on lightning NO production

         VARDESC = 'Flag for lightning emissions'
         LTNG_NO = ENVYN( CTM_LTNG_NO, VARDESC, .FALSE., STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            XMSG = 'Env. variable set, but empty: Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF

         IF ( .NOT. LTNG_NO ) RETURN

         VARDESC = 'lightning NO file name or InLine flag'
         CALL ENVSTR( 'LTNGNO', VARDESC, 'LTNGNO', LNAME, STATUS )
         IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( STATUS .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         ELSE IF ( STATUS .EQ. -1 ) THEN
            XMSG = 'Env. variable set, but empty: Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         ELSE IF ( STATUS .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF

!C Is lightning NO production inline, or from a file?
         IF ( LNAME .EQ. "InLine" ) THEN  ! inline lightning NO production
            XMSG = 'Using in-line lightning NO production'
            CALL M3MSG2( XMSG )

!C Is there a file specifying iccg, ocean, and other parameters?
            VARDESC = 'Use parameter file'
            LPARAM = ENVYN( 'LTNGPARAM', VARDESC, .FALSE., STATUS )
            IF ( STATUS .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
            IF ( STATUS .EQ. 1 ) THEN
               XMSG = 'Environment variable improperly formatted'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            ELSE IF ( STATUS .EQ. -1 ) THEN
               XMSG = 'Env. variable set, but empty: Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            ELSE IF ( STATUS .EQ. -2 ) THEN
               XMSG = 'Environment variable not set ... Using default:'
               WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
            END IF

            ALLOCATE( COLUMN_LTNG_NO( NCOLS,NROWS ),
     &                LTNG_PRSFC    ( NCOLS,NROWS ),
     &                LTNG_RC       ( NCOLS,NROWS ), STAT = STATUS )
!    &                LTNG_CLDT     ( NCOLS,NROWS ),
            IF ( STATUS .NE. 0 ) THEN
               XMSG = 'COLUMN_LTNG_NO, LTNG_PRSFC, LTNG_RC, or LTNG_CLDT'
     &              // '  memory allocation failed'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            IF ( LPARAM ) THEN
               XMSG = 'Using lightning NO parameters file'
               CALL M3MSG2( XMSG )
!C Open lightning parameters file?
               ANAME = PROMPTMFILE(
     &                    'Enter name for lightning parameters file',
     &                    FSREAD3, 'LTNGPARM_FILE', PNAME )
!C Read description of normalized emissions file
!              IF ( .NOT. DESC3( ANAME ) ) THEN
!                 XMSG = 'Could not get description of file "'
!    &                 // TRIM( ANAME ) // '"'
!                 CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
!                 SUCCESS = .FALSE.; RETURN
!              END IF

!C Check grid definition (initialize if first call)
               OK = CHKGRID( ANAME )
            
               ALLOCATE( LRATIO       ( NCOLS,NROWS ),
     &                   ICCG         ( NCOLS,NROWS ),
     &                   OCEAN_MASK   ( NCOLS,NROWS ),
     &                   MOLSNFLASH   ( NCOLS,NROWS ),
     &                   STRIKE_FACTOR( NCOLS,NROWS ), STAT = STATUS )
               IF ( STATUS .NE. 0 ) THEN
                  XMSG = 'LRATIO, ICCG, OCEAN_MASK, MOLSNFLASH or STRIKE_FACTOR' 
     &                 // '  memory allocation failed'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

!C Get domain window info for the lightning parameters file
               CALL SUBHFILE ( ANAME, GXOFF, GYOFF,
     &                         STRTCOLLT3, ENDCOLLT3, STRTROWLT3, ENDROWLT3 )

               IF ( .NOT. INTERPX( ANAME, "LTratio", PNAME,
     &                             STRTCOLLT3,ENDCOLLT3, STRTROWLT3,ENDROWLT3, 1,1,
     &                             JDATE, JTIME, LRATIO ) ) THEN
                  XMSG = 'Could not read LTratio from' // TRIM( ANAME )
               END IF

               IF ( .NOT. INTERPX( ANAME, "ICCG", PNAME,
     &                             STRTCOLLT3,ENDCOLLT3, STRTROWLT3,ENDROWLT3, 1,1,
     &                             JDATE, JTIME, ICCG ) ) THEN
                  XMSG = 'Could not read ICCG from' // TRIM( ANAME )
               END IF

               IF ( .NOT. INTERPX( ANAME, "OCNMASK", PNAME,
     &                             STRTCOLLT3,ENDCOLLT3, STRTROWLT3,ENDROWLT3, 1,1,
     &                             JDATE, JTIME, OCEAN_MASK ) ) THEN
                  XMSG = 'Could not read OCNMASK from' // TRIM( ANAME ) // '"'
               END IF
              
               IF ( .NOT. INTERPX( ANAME, "MOLSN", PNAME,
     &                             STRTCOLLT3,ENDCOLLT3, STRTROWLT3,ENDROWLT3, 1,1,
     &                             JDATE, JTIME, MOLSNFLASH ) ) THEN
                  XMSG = 'Could not read MOLSN from' // TRIM( ANAME )
               END IF
              
               IF ( .NOT. INTERPX( ANAME, "STRKCNT", PNAME,
     &                             STRTCOLLT3,ENDCOLLT3, STRTROWLT3,ENDROWLT3, 1,1,
     &                             JDATE, JTIME, STRIKE_FACTOR ) ) THEN
                  XMSG = 'Could not read STRKCNT from' // TRIM( ANAME )
               END IF

            END IF   ! LPARAM

!C Set up domain based on met file
!C Open met file
            MNAME = PROMPTMFILE( 
     &              'Enter name for gridded met input file',
     &              FSREAD3, 'MET_CRO_3D', PNAME )
!           write(*,*) "LTNG INIT MNAME", MNAME

!C Read description of domain (there may be a better way to get this info)
            IF ( .NOT. DESC3( MNAME ) ) THEN
               XMSG = 'Could not get description of file "'
     &              // TRIM( MNAME ) // '"'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

!C Check grid definition (initialize if first call)
            OK = CHKGRID( MNAME )
            
!C Get domain window info for met_cro_3d file
            CALL SUBHFILE ( MNAME, GXOFF, GYOFF,
     &              STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

!C Set up vertical layers
            ALLOCATE( VGLVSLT( 0:EMLAYS ), STAT = STATUS )
            CALL CHECKMEM( STATUS, 'VGLVSLT', PNAME )

!C Store local layer information
            DO L = EMLAYS, 0, -1
                  VGLVSLT( L ) = VGLVS3D( L+1 )
                  WRITE( LOGDEV,'(5X, A, I3, A, F11.7)' ) 'VGLVSLT(', L, ' ):', VGLVSLT( L )
            END DO

!C Lightning Diagnostic file?
            LTNGDIAG = ENVYN( 'LTNGDIAG', 
     &              'Write lightning NO production diagnostic file?',
     &              .FALSE., STATUS )
           
            IF ( LTNGDIAG ) THEN
!C Build description for, and open lightning diagnostic file
!C (all but variables-table and horizontal domain in description is borrowed from MNAME)
               LDATE = SDATE; LTIME = STIME
               CALL NEXTIME( LDATE, LTIME, TSTEP )
               SDATE3D = LDATE
               STIME3D = LTIME
               TSTEP3D = TSTEP
               NLAYS3D = EMLAYS
               NVARS3D = 1

               VNAME3D( 1 ) = LTSPC
               VDESC3D( 1 ) = 'hourly average NO produced from lightning'
               VTYPE3D( 1 ) = M3REAL
               UNITS3D( 1 ) = 'mol/s'

               FDESC3D = ' '   ! array assignment
               FDESC3D( 1 ) = 'Gridded lightning NO production from CMAQ'
               FDESC3D( 2 ) = '/from/ ' // PNAME
               FDESC3D( 3 ) = '/Version/ CMAQ'

!C Open output file (mol/s)
               IF ( MYPE .EQ. 0 ) THEN
                  IF ( .NOT. OPEN3( DNAME, FSNEW3, PNAME ) ) THEN
                     XMSG = 'Could not open ' // TRIM( DNAME )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
                  END IF
               END IF

               ALLOCATE( VDEMIS_DIAG( NCOLS,NROWS,EMLAYS ), STAT = STATUS )
               IF ( STATUS .NE. 0 ) THEN
                  XMSG = 'VDEMIS_DIAG memory allocation failed'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF
               VDEMIS_DIAG = 0.0   ! array assignment

            END IF   ! LTNGDIAG
            
         ELSE   ! lightning emissions off line

!C Lightning NO production from an input file
            CALL M3MSG2( 'Using lightning NO production from a file' )

!C Open lightning emissions input file
            LNAME = PROMPTMFILE(
     &              'Enter name for lightning emissions input file',
     &              FSREAD3, 'LTNGNO', PNAME )

!C Get description of emissions file
            IF ( .NOT. DESC3( LNAME ) ) THEN
               XMSG = 'Could not get description of file "'
     &              // TRIM( LNAME ) // '"'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

!C Check grid definition (intialize, if first call)
            OK = CHKGRID( LNAME )
            
!C Get domain decomp info for the lightning emissions file
            CALL SUBHFILE ( LNAME, GXOFF, GYOFF,
     &                      STRTCOLLT3, ENDCOLLT3, STRTROWLT3, ENDROWLT3 )

         END IF   ! lightning NO production inline, or from a file

         LTLYRS = EMLAYS

!C Lightning to gas-phase species map

         LTNG_MAP = INDEX1( LTSPC, N_GC_EMIS, GC_EMIS )
         IF ( LTNG_MAP .EQ. 0 ) THEN
            XMSG = TRIM( LTSPC ) // ' not found in GC_EMIS table'
            CALL M3EXIT
         END IF

         ALLOCATE( VDEMIS_LT( NCOLS,NROWS,LTLYRS ), STAT = STATUS )
         IF ( STATUS .NE. 0 ) THEN
            XMSG = 'VDEMIS_LT memory allocation failed'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         RETURN

         END FUNCTION LTNG_INIT

!C======================================================================
!C Get NO produced from lightning in VDEMIS_LT

         SUBROUTINE GET_LTNG ( JDATE, JTIME, TSTEP )

         USE GRID_CONF             ! horizontal & vertical domain specifications
         USE UTILIO_DEFN

         IMPLICIT NONE

!     need to change this to set this path in the Makefile
!        include "/usr/local/intel/mkl/include/mkl_vml.f77"
         INCLUDE SUBST_CONST    ! constants

         INTEGER JDATE, JTIME, TSTEP( 2 )

         REAL,   PARAMETER :: CONVPA = 1.0E-2  ! convert Pa to hPa
!        REAL,   PARAMETER :: WK = 8.0         ! shape parameter for weibull distribution
!        REAL,   PARAMETER :: WLAMBDA = 700.0  ! scale parameter for weibull distribution
         REAL,   PARAMETER :: WMU = 350.0      ! mean
         REAL,   PARAMETER :: WSIGMA = 200.0   ! standard deviation
         REAL,   PARAMETER :: W2MU = 600.0     ! mean
         REAL,   PARAMETER :: W2SIGMA = 50.0   ! standard deviation
         REAL,   PARAMETER :: SQRT2 = 1.414213562731

         INTEGER COL, ROW, LAY ! iterator variables

         REAL    PCALC    ! pressure level for NO vertical distribution (hPa)
         REAL    BOTTOM   ! pressure at bottom of grid cell (hPa)
         REAL    TOP      ! pressure at top of grid cell (hPa)        
         REAL    BOTTOM_FRAC, TOP_FRAC ! their difference is the fraction of lightning NO in this grid cell
         REAL    BOTTOM_FRAC2, TOP_FRAC2
         REAL    SUM_FRAC ! stores the sum of vertical fractions to re-normalize the column
         REAL    WEIGHT ! used to normalize emissions to total amount
         REAL    inErfB, inErfT !  nputs to error funciton calculation
         REAL    outErfB, outErfT !  utputs from error funciton calculation
         REAL :: LTEMIS( LTLYRS )

         LOGICAL, SAVE :: LASTTIC   ! true: last sync step this output tstep
         REAL    DIVFAC   ! averaging factor for diagnostic file

         CHARACTER( 16 ) :: MNAME
         CHARACTER( 16 ) :: PNAME = 'GET_LTNG'
         CHARACTER( 120 ) :: XMSG = ' '

         INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2

C statement function for ERF approximation
         REAL :: ERF                ! ERF approx. statement function
         REAL :: X                  ! dummy argument for ERF
         ERF( X ) = SIGN( 1.0, X ) * SQRT( 1.0 - EXP( -4.0 * X * X / PI ) )

!-----------------------------------------------------------------------

         IF ( LNAME .EQ. "InLine" ) THEN
!C case of inline lightning NO production

!C initialize output array
            VDEMIS_LT = 0.0

            WRITE( LOGDEV,'(5X, A)' ) 'InLine LNOx production'

!C Open met file
            MNAME = PROMPTMFILE( 
     &                    'Enter name for gridded met input file',
     &                    FSREAD3, 'MET_CRO_2D', PNAME )

!C Get domain window info for met_cro_2d file
            CALL SUBHFILE ( MNAME, GXOFF, GYOFF,
     &              STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )

            IF ( .NOT. INTERPX( MNAME, "RC", PNAME,  ! convective rain in cm
     &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                          JDATE, JTIME, LTNG_RC ) ) THEN
               XMSG = 'Could not read RC from ' // TRIM( MNAME )
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )     
            END IF

            IF ( .NOT. INTERPX( MNAME, "PRSFC", PNAME,  ! surface pressure in Pa
     &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                          JDATE, JTIME, LTNG_PRSFC ) ) THEN
               XMSG = 'Could not read PRSFC from ' // TRIM( MNAME )
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
            END IF

!           IF ( .NOT. INTERPX( MNAME, "CLDT", PNAME,  ! cloud top in meters
!    &                          STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
!    &                          JDATE, JTIME, LTNG_CLDT ) ) THEN
!              XMSG = 'Could not read CLDT from ' // TRIM( MNAME )
!              CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
!           END IF

!C Iterate over each grid cell and distribute lightning NO vertically

            IF ( LPARAM ) THEN

               DO ROW = 1, MY_NROWS 
                  DO COL = 1, MY_NCOLS
                     COLUMN_LTNG_NO( COL,ROW ) = LTNG_RC( COL,ROW )
     &               * STRIKE_FACTOR( COL,ROW )  ! flashes per unit precip
     &               * LRATIO( COL,ROW )         ! ratio derived from NLDN data
     &               * OCEAN_MASK( COL,ROW )     ! reduce offshore strikes
     &               * ( 1.0 + ICCG( COL,ROW ) ) ! intercloud strikes per cloud to ground strike
     &               *   MOLSNFLASH( COL,ROW )   ! moles of N per flash
     &                 / ( 60.0 * 60.0 )         ! get time units right

!                   write( logdev,'( 2i4, 7f9.4 )' ) col,row,
!      &                 ltng_rc( col,row ),
!      &                 strike_factor( col,row ), lratio( col,row ),
!      &                 ocean_mask( col,row ), iccg( col,row ), 
!      &                 molsnflash( col,row )
                  END DO
               END DO

            ELSE

               DO ROW = 1, MY_NROWS 
                  DO COL = 1, MY_NCOLS
                     COLUMN_LTNG_NO( COL,ROW ) = LTNG_RC( COL,ROW )
     &               * 147.0 * ( XCELL_GD * YCELL_GD / 36000.0**2 )   ! 147 flahses, but adjust depending on horizontal grid resolution
     &               *  500.0 * 4.0 / ( 60.0 * 60.0 ) ! 500 moles N per flash * 3 intercloud strikes per ground strike
                  END DO
               END DO

            END IF
                 
            VDEMIS_LT = 0.0   ! array assignment

            DO ROW = 1, MY_NROWS 
               DO COL = 1, MY_NCOLS

!C check to see if there are lightning strikes for this grid cell
!C only calculate lightning for cloud top greater than 6500 meters
!                 write( logdev,* ) col, row, column_ltng_no( col,row ),
!    &                              ltng_cldt( col,row )
!                 IF ( COLUMN_LTNG_NO( COL,ROW ) .GT. 0.0  .AND.
!    &                 LTNG_CLDT( COL,ROW ) .GT. 6500.0 )  THEN

                  IF ( COLUMN_LTNG_NO( COL,ROW ) .LE. 0.0 ) CYCLE
                  SUM_FRAC = 0.0  
                  LTEMIS = 0.0   ! array assignment

                  DO LAY = 1, LTLYRS

!C Get pressures: Use SIGMA values and surface pres. p=sigma*(psfc-ptop)+ptop
                     BOTTOM = ( VGLVSLT( LAY-1 )
     &                      * ( LTNG_PRSFC( COL,ROW ) - VGTOP_GD )
     &                      + VGTOP_GD ) * CONVPA
!                           write( logdev,* ) "bottom: ", bottom
                     TOP    = ( VGLVSLT( LAY )
     &                      * ( LTNG_PRSFC( COL,ROW ) - VGTOP_GD )
     &                      + VGTOP_GD ) * CONVPA
!                           write( logdev,* ) "top: ", top

!C Find the bottom and top of each layer, and calculate the fraction 
!C of the column emissions for that layer
!C Use normal distribution, mean = wmu, standard deviation = wsigma
                     inErfB = ( BOTTOM - WMU ) / ( WSIGMA * SQRT2 )
                     inErfT = ( TOP - WMU ) / ( WSIGMA * SQRT2 )
                     outErfB = ERF( inErfB )
                     outErfT = ERF( inErfT )
                     BOTTOM_FRAC = 0.5 * ( 1.0 + outErfB )
                     TOP_FRAC    = 0.5 * ( 1.0 + outErfT )

!C Find the bottom and top of each layer, and calculate the fraction
!C of the column emissions for that layer
!C use normal distribution, mean = wmu, standard deviation = wsigma
                     inErfB = ( BOTTOM - W2MU ) / ( W2SIGMA * SQRT2 )
                     inErfT = ( TOP - W2MU ) / ( W2SIGMA * SQRT2 )
                     outErfB = ERF( inErfB )
                     outErfT = ERF( inErfT )
                     BOTTOM_FRAC2 = 0.5 * ( 1.0 + outErfB )
                     TOP_FRAC2    = 0.5 * ( 1.0 + outErfT )

!C Add weighted contribution to this level
                     WEIGHT = ( BOTTOM_FRAC - TOP_FRAC )
     &                      + ( BOTTOM_FRAC2 - TOP_FRAC2 ) * 0.2

                     LTEMIS( LAY ) = WEIGHT * COLUMN_LTNG_NO( COL,ROW )

!C Sum weights in order to normalize to 1
                     SUM_FRAC = SUM_FRAC + WEIGHT

!                    write( logdev,* ) col, row, lay,
!     &                                bottom_frac, top_frac,
!     &                                bottom_frac2, top_frac2, sum_frac,
!     &                                column_ltng_no( col,row )

!C If emissions are less than 0, generate an error message in the log
                     IF ( LTEMIS( LAY ) .LT. 0.0 ) THEN
                        WRITE( LOGDEV,* ) LTEMIS( LAY ),
     &                                    COLUMN_LTNG_NO( COL,ROW ),
     &                                    BOTTOM_FRAC, TOP_FRAC
                        XMSG = '*** Ltng NO emis is less than zero'
                        CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                     END IF

!                    write( logdev,* ) "VDEMIS_LT ", VDEMIS_LT( 1,COL,ROW,LAY )

                  END DO   ! end layers loop 

                  DO LAY = 1, LTLYRS
!C Re-normalize, in some cases area under the error function is not 1
                     VDEMIS_LT( COL,ROW,LAY ) = LTEMIS( LAY ) / SUM_FRAC
!                    write( logdev,* ) col, row, lay,
!     &                                vdemis_lt( col,row,lay )
                  END DO        ! layers renormalized

               END DO   ! columns
            END DO   ! rows

!C Write lightning NO to the diagnostic file

            IF ( LTNGDIAG ) THEN

               VDEMIS_DIAG = VDEMIS_DIAG + VDEMIS_LT   ! array assignment
               NTICS = NTICS + 1
               LASTTIC = NTICS .EQ. TIME2SEC( TSTEP( 1 ) ) / TIME2SEC( TSTEP( 2 ) )
      
               IF ( LASTTIC ) THEN   ! time to write out

                  DIVFAC = 1.0 / FLOAT( NTICS )
                  VDEMIS_DIAG = VDEMIS_DIAG * DIVFAC   ! array assignment

                  IF ( .NOT. WRITE3( DNAME, LTSPC, LDATE, LTIME, VDEMIS_DIAG ) )  THEN
                     XMSG = 'Could not write to ' // TRIM( DNAME )
                     CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  ELSE
                     WRITE( LOGDEV,94040 )
     &                    'Timestep written to', TRIM( DNAME ),
     &                    'for date and time', LDATE, LTIME
                  END IF

                  VDEMIS_DIAG = 0.0   ! array assignment
                  NTICS = 0.0
                  CALL NEXTIME( LDATE, LTIME, TSTEP( 1 ) )

               END IF ! LASTTIC

            END IF  ! diagnostics turned on

         ELSE  ! LTNGO is not "InLine", but instead specifies a file

!C Read in lightning NO production from an input file
            VDEMIS_LT = 0.0   ! array assignment

            IF ( .NOT. INTERPX( LNAME, LTSPC, PNAME,
     &                          STRTCOLLT3,ENDCOLLT3, STRTROWLT3,ENDROWLT3, 1,LTLYRS,
     &                          JDATE, JTIME, VDEMIS_LT ) ) THEN
               XMSG = 'Could not read NO' //
     &                ' from file "' // TRIM( LNAME ) // '"'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
!!!!! comment this out to avoid problems reading in a specific time / date from LNO file
               
            END IF

!           DO ROW = 1, MY_NROWS 
!              DO COL = 1, MY_NCOLS
!                 DO LAY = 1, LTLYRS
!                    IF ( VDEMIS_LT( COL,ROW,LAY ) .gt. 0.0 ) THEN
!                       write( logdev,* ) col, row, lay,
!     &                                   VDEMIS_LT( COL,ROW,LAY )
!                    END IF
!                 END DO
!              END DO
!           END DO
                
         END IF  ! end lightning NO production inline or from a file
               
         write( logdev,* ) 'VDEMIS_LT sum: ', jdate, jtime, sum( vdemis_lt )

!C Save date
!        LDATE = JDATE

!        IF ( IHR .LT. NSTEPS ) THEN
!           IF ( LASTTIC ) THEN
!              NTICS = 0
!           END IF
!           RETURN
!        ELSE IF ( NTICS .LT. TIME2SEC( TSTEP( 1 ) ) / TIME2SEC( TSTEP( 2 ) ) ) THEN
!           RETURN
!        END IF

         RETURN

C------------------  Format  Statements   ------------------------------

94040 FORMAT( /5X, 3( A, :, 1X ), I8, ":", I6.6 )
        
         END SUBROUTINE GET_LTNG
      
      END MODULE LTNG_DEFN

