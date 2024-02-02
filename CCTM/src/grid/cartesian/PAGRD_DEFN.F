
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
      MODULE PAGRD_DEFN

C process analysis variables that are dependent on NCOLS, NROWS

C Preconditions: HGRD_INIT() called from PAR_INIT, which is called from DRIVER

C Revision History:
C   J.Young 17 Aug 01: create
C   D.Wong, J.Young Oct 01: set dimensions for DELC and CSAV correctly for
C   subdomain; IRROUT must have full domain because of SE_DATA_COPY in pa_output
C   J.Young 31 Jan 05: dyn alloc - establish both horizontal & vertical domain
C   specifications in one module
C   J.Young 22 Dec 05: move proc. analy. stuff from HGRD_DEFN to here. create
C   arrays COLSZ_PE and ROWSZ_PE for proc analy, which are equivalent to
C   COLSX_PE and ROWSX_PE
C   J.Young  8 Jul 10: minor mods
C   S. Roselle 29 Mar 11: Replaced I/O API include files with UTILIO_DEFN
C   J.Young 14 Sep 11: allow default subdomain values
C   J.Young 28 Jun 16: use PA_DEFN module intead of include files
C   J.Young 16 Sep 16: update for inline procan
C   C. Nolte, S. Roselle Sep 18: replace M3UTILIO with UTILIO_DEFN
C   D.Dwong 01 Feb 19: removed all MY_N clauses
C.......................................................................
      USE RUNTIME_VARS

      IMPLICIT NONE

C Total number of columns, rows and layers for PA output
      INTEGER, SAVE :: PACOLS, PAROWS, PALEVS

C Starting and ending column of local PA grid in this processor
      INTEGER, SAVE :: MY_BEGCOL, MY_ENDCOL
C Starting and ending row of local PA grid in this processor
      INTEGER, SAVE :: MY_BEGROW, MY_ENDROW
C Starting and ending layer of local PA grid in this processor
      INTEGER, SAVE :: MY_BEGLEV, MY_ENDLEV

C Starting and ending column of local IRR grid in this processor
      INTEGER, SAVE :: MY_IRR_BEGCOL, MY_IRR_ENDCOL
C Starting and ending row of local IRR grid in this processor
      INTEGER, SAVE :: MY_IRR_BEGROW, MY_IRR_ENDROW
C Starting and ending layer of local IRR grid in this processor
      INTEGER, SAVE :: MY_IRR_BEGLEV, MY_IRR_ENDLEV

C Number of columns and rows of local PA/IRR grid in this processor
      INTEGER, SAVE :: MY_PACOLS, MY_PAROWS

C column and row range for each processor
      INTEGER, ALLOCATABLE, SAVE :: COLSZ_PE( :,: ), ROWSZ_PE( :,: )

C Conc difference for a science process
      REAL, ALLOCATABLE, SAVE :: DELC( :,:,:,: )

C Array for outputting IRR results
      REAL, ALLOCATABLE, SAVE :: IRROUT( :,:,:,: )

C Buffer for writing IRR results
      REAL, ALLOCATABLE, SAVE :: DBUFF( :,:,: )

      CONTAINS
         FUNCTION PAGRD_INIT ( RANK ) RESULT ( SUCCESS )

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE UTILIO_DEFN
#ifndef mpas
#ifdef parallel
         USE SE_MODULES          ! stenex (using SE_UTIL_MODULE)
#else
         USE NOOP_MODULES        ! stenex (using NOOP_UTIL_MODULE)
#endif
#endif
         USE PA_DEFN             ! Process Anaylsis control and data variables

         INTEGER, INTENT( IN ) :: RANK
         LOGICAL SUCCESS

         INTEGER IRCOLS
         INTEGER IRROWS
         INTEGER IRLEVS

         INTEGER, ALLOCATABLE :: NCOLS_PE( : )  ! Column range for each PE
         INTEGER, ALLOCATABLE :: NROWS_PE( : )  ! Row range for each PE

         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER ALST
         INTEGER X1F0  ! dimension homotopy for parallel or serial
         CHARACTER( 16 ) :: PNAME = 'PAGRD_INIT'
         CHARACTER( 96 ) :: XMSG = ' '

C-----------------------------------------------------------------------

C This function is expected to be called only once - at startup

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            SUCCESS = .TRUE.

            IF ( LIPR .OR. LIRR ) THEN

C Retrieve the process analysis subdomain dimensions:
               IF ( PA_ENDCOL .EQ. 0 ) PA_ENDCOL = GL_NCOLS
               IF ( PA_ENDROW .EQ. 0 ) PA_ENDROW = GL_NROWS
               IF ( PA_ENDLEV .EQ. 0 ) PA_ENDLEV = NLAYS

               IF ( PA_BEGCOL .LT. 1 .OR. PA_ENDCOL .GT. GL_NCOLS .OR.
     &              PA_BEGROW .LT. 1 .OR. PA_ENDROW .GT. GL_NROWS .OR.
     &              PA_BEGLEV .LT. 1 .OR. PA_ENDLEV .GT. NLAYS ) THEN
                  XMSG = 'Process Analysis domain inconsistent with CTM domain' 
                  CALL M3WARN ( PNAME, 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

#ifndef mpas
C Determine IPR subgrid mapping with data redistribution
               CALL SUBST_SUBGRID_INDEX
     &             ( PA_BEGCOL, PA_ENDCOL, PA_BEGROW, PA_ENDROW,
     &               PA_BEGLEV, PA_ENDLEV,
     &               MY_BEGCOL, MY_ENDCOL, MY_BEGROW, MY_ENDROW,
     &               MY_BEGLEV, MY_ENDLEV, MY_PACOLS, MY_PAROWS )

               PACOLS = PA_ENDCOL - PA_BEGCOL + 1
               PAROWS = PA_ENDROW - PA_BEGROW + 1
               PALEVS = PA_ENDLEV - PA_BEGLEV + 1
#endif

               ALLOCATE ( NCOLS_PE( NPROCS ),
     &                    NROWS_PE( NPROCS ), STAT = ALST )
               IF ( ALST .NE. 0 ) THEN
                  XMSG = '*** PA_NCOLS_PE or PA_NROWS_PE Memory allocation failed'
                  CALL M3WARN ( PNAME, 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

               ALLOCATE ( COLSZ_PE( 2,NPROCS ),
     &                    ROWSZ_PE( 2,NPROCS ), STAT = ALST )
               IF ( ALST .NE. 0 ) THEN
                  XMSG = '*** COLSX_PE or ROWSX_PE Memory allocation failed'
                  CALL M3WARN ( PNAME, 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

C Construct the processor-to-subdomain map for IPR domain
               CALL SUBHDOMAIN( NPROCS, NPCOL, NPROW, PACOLS, PAROWS,
     &                          NCOLS_PE, NROWS_PE, COLSZ_PE, ROWSZ_PE )

               MY_PACOLS = NCOLS_PE( RANK+1 )
               MY_PAROWS = NROWS_PE( RANK+1 )

               IF ( LIPR ) THEN

                  ALLOCATE ( DELC( MY_PACOLS,MY_PAROWS,PALEVS,NIPRVAR ),
     &                       STAT = ALST )
                  IF ( ALST .NE. 0 ) THEN
                     XMSG = '*** DELC Memory allocation failed'
                     CALL M3WARN ( PNAME, 0, 0, XMSG )
                     SUCCESS = .FALSE.; RETURN
                  END IF

               END IF

               IF ( LIRR ) THEN

C Determine IRR subgrid mapping without data redistribution
#ifndef mpas
                  CALL SUBST_SUBGRID_INDEX
     &                ( PA_BEGCOL, PA_ENDCOL, PA_BEGROW, PA_ENDROW,
     &                  PA_BEGLEV, PA_ENDLEV,
     &                  MY_IRR_BEGCOL, MY_IRR_ENDCOL, MY_IRR_BEGROW,
     &                  MY_IRR_ENDROW, MY_IRR_BEGLEV, MY_IRR_ENDLEV, 'N' )
#endif

                  X1F0 = ( NCOLS / GL_NCOLS ) * ( NROWS / GL_NROWS )
                  IRCOLS = X1F0 * PACOLS + ( 1 - X1F0 ) * NCOLS
                  IRROWS = X1F0 * PAROWS + ( 1 - X1F0 ) * NROWS
                  IRLEVS = PALEVS

                  ALLOCATE ( IRROUT( IRCOLS,IRROWS,IRLEVS,NIRRVAR ),
     &                       STAT = ALST )
                  IF ( ALST .NE. 0 ) THEN
                     XMSG = '*** IRROUT Memory allocation failed'
                     CALL M3WARN ( PNAME, 0, 0, XMSG )
                     SUCCESS = .FALSE.; RETURN
                  END IF

                  ALLOCATE ( DBUFF( MY_PACOLS,MY_PAROWS,PALEVS ), STAT = ALST )
                  IF ( ALST .NE. 0 ) THEN
                     XMSG = 'ERROR allocating DBUFF'
                     CALL M3WARN ( PNAME, 0, 0, XMSG )
                     SUCCESS = .FALSE.; RETURN
                  ENDIF

               END IF

            ELSE

               PA_BEGCOL = 1
               PA_ENDCOL = 1
               PA_BEGROW = 1
               PA_ENDROW = 1
               PA_BEGLEV = 1
               PA_ENDLEV = 1

               XMSG = 'No Process Analysis for this run'
               CALL M3WARN ( PNAME, 0, 0, XMSG )
               SUCCESS = .TRUE.; RETURN

            END IF

         ELSE   ! called more than once

            XMSG = 'Horizontal domain dependent variables already allocated'
            CALL M3WARN ( PNAME, 0, 0, XMSG )
            SUCCESS = .FALSE.; RETURN

         END IF   ! FIRSTIME

         RETURN
         END FUNCTION PAGRD_INIT

      END MODULE PAGRD_DEFN
