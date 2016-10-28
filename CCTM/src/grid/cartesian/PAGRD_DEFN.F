
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
C $Header: /project/yoj/arc/CCTM/src/grid/cartesian/PAGRD_DEFN.F,v 1.4 2011/10/21 16:11:18 yoj Exp $

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
C.......................................................................

      IMPLICIT NONE

C Starting and ending column for PA output
      INTEGER, SAVE :: PA_BEGCOL, PA_ENDCOL
C Starting and ending row for PA output
      INTEGER, SAVE :: PA_BEGROW, PA_ENDROW
C Starting and ending layer for PA output
      INTEGER, SAVE :: PA_BEGLEV, PA_ENDLEV

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

C previous Conc in a science process
      REAL, ALLOCATABLE, SAVE :: CSAV( :,:,:,: )

C Array for outputting IRR results
      REAL, ALLOCATABLE, SAVE :: IRROUT( :,:,:,: )

C Buffer for writing IRR results
      REAL, ALLOCATABLE, SAVE :: DBUFF( :,:,: )

      CONTAINS
         FUNCTION PAGRD_INIT ( NPROCS, RANK ) RESULT ( SUCCESS )

         USE GRID_CONF           ! horizontal & vertical domain specifications
         USE M3UTILIO            ! i/o-api
#ifdef parallel
         USE SE_MODULES          ! stenex (using SE_UTIL_MODULE)
#else
         USE NOOP_MODULES        ! stenex (using NOOP_UTIL_MODULE)
#endif
         USE PA_DEFN             ! Process Anaylsis control and data variables

         INTEGER, INTENT( IN ) :: NPROCS  ! total worker processors
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
         CHARACTER( 32 ) :: PA_BCOL_ECOL = 'PA_BCOL_ECOL'
         CHARACTER( 32 ) :: PA_BROW_EROW = 'PA_BROW_EROW'
         CHARACTER( 32 ) :: PA_BLEV_ELEV = 'PA_BLEV_ELEV'
         CHARACTER( 16 ) :: V_LIST( 2 )
         INTEGER NV

         INTERFACE
            SUBROUTINE GET_ENVLIST ( ENV_VAR, NVARS, VAL_LIST )
               CHARACTER( * ),  INTENT ( IN )  :: ENV_VAR
               INTEGER,         INTENT ( OUT ) :: NVARS
               CHARACTER( 16 ), INTENT ( OUT ) :: VAL_LIST( : )
            END SUBROUTINE GET_ENVLIST
         END INTERFACE

C-----------------------------------------------------------------------

C This function is expected to be called only once - at startup

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            SUCCESS = .TRUE.

            IF ( LIPR .OR. LIRR ) THEN

C Retrieve the process analysis subdomain dimensions:

               CALL GET_ENVLIST ( PA_BCOL_ECOL, NV, V_LIST )
               IF ( NV .LE. 0 ) THEN   ! default to GL_NCOLS
                  PA_BEGCOL = 1; PA_ENDCOL = GL_NCOLS
               ELSE IF ( NV .NE. 2 ) THEN
                  XMSG = 'Environment variable error for ' // PA_BCOL_ECOL
                  CALL M3WARN ( PNAME, 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
               ELSE
                  READ( V_LIST( 1 ), '( I4 )' ) PA_BEGCOL
                  READ( V_LIST( 2 ), '( I4 )' ) PA_ENDCOL
               END IF

               CALL GET_ENVLIST ( PA_BROW_EROW, NV, V_LIST )
               IF ( NV .LE. 0 ) THEN   ! default to GL_NCOLS
                  PA_BEGROW = 1; PA_ENDROW = GL_NROWS
               ELSE IF ( NV .NE. 2 ) THEN
                  XMSG = 'Environment variable error for ' // PA_BROW_EROW
                  CALL M3WARN ( PNAME, 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
               ELSE
                  READ( V_LIST( 1 ), '( I4 )' ) PA_BEGROW
                  READ( V_LIST( 2 ), '( I4 )' ) PA_ENDROW
               END IF

               CALL GET_ENVLIST ( PA_BLEV_ELEV, NV, V_LIST )
               IF ( NV .LE. 0 ) THEN   ! default to GL_NCOLS
                  PA_BEGLEV = 1; PA_ENDLEV = NLAYS
               ELSE IF ( NV .NE. 2 ) THEN
                  XMSG = 'Environment variable error for ' // PA_BLEV_ELEV
                  CALL M3WARN ( PNAME, 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
               ELSE
                  READ( V_LIST( 1 ), '( I4 )' ) PA_BEGLEV
                  READ( V_LIST( 2 ), '( I4 )' ) PA_ENDLEV
               END IF

               IF ( PA_BEGCOL .LT. 1 .OR. PA_ENDCOL .GT. GL_NCOLS .OR.
     &              PA_BEGROW .LT. 1 .OR. PA_ENDROW .GT. GL_NROWS .OR.
     &              PA_BEGLEV .LT. 1 .OR. PA_ENDLEV .GT. NLAYS ) THEN
                  XMSG = 'Process Analysis domain inconsistent with CTM domain' 
                  CALL M3WARN ( PNAME, 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

C Determine IPR subgrid mapping with data redistribution
               CALL SUBST_SUBGRID_INDEX
     &             ( PA_BEGCOL, PA_ENDCOL, PA_BEGROW, PA_ENDROW,
     &               PA_BEGLEV, PA_ENDLEV,
     &               MY_BEGCOL, MY_ENDCOL, MY_BEGROW, MY_ENDROW,
     &               MY_BEGLEV, MY_ENDLEV, MY_PACOLS, MY_PAROWS )

!              write( *,* ) ' se_subgrid_index - MY_BEGCOL: ', MY_BEGCOL
!              write( *,* ) ' se_subgrid_index - MY_ENDCOL: ', MY_ENDCOL
!              write( *,* ) ' se_subgrid_index - MY_BEGROW: ', MY_BEGROW
!              write( *,* ) ' se_subgrid_index - MY_ENDROW: ', MY_ENDROW
!              write( *,* ) ' se_subgrid_index - MY_BEGLEV: ', MY_BEGLEV
!              write( *,* ) ' se_subgrid_index - MY_ENDLEV: ', MY_ENDLEV
!              write( *,* ) ' se_subgrid_index - MY_PACOLS: ', MY_PACOLS
!              write( *,* ) ' se_subgrid_index - MY_PAROWS: ', MY_PAROWS

C NOTE: (CMAQ Forecast model version)
C       SE_SUBGRID_INDEX doesn't know about the "writers" (I/O) partition.
C       Therefore, COLLECT, which is in that partition won't have MY_PACOLS
C       and MY_PAROWS. COLLECT needs these to determine a size for the message
C       buffer that receives grid data from the worker processors. Use
C       SUBHDOMAIN (below).

               PACOLS = PA_ENDCOL - PA_BEGCOL + 1
               PAROWS = PA_ENDROW - PA_BEGROW + 1
               PALEVS = PA_ENDLEV - PA_BEGLEV + 1

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

!              write( *,* ) ' subhdomain - MY_PACOLS: ', MY_PACOLS
!              write( *,* ) ' subhdomain - MY_PAROWS: ', MY_PAROWS
!              do i = 1, NPROCS
!                 write( *,* ) ' I, PA_NCOLS_PE: ', i, NCOLS_PE( i )
!                 write( *,* ) ' I, PA_NROWS_PE: ', i, NROWS_PE( i )
!                 write( *,* ) ' I, COLSZ_PE( 1,I ): ', i, COLSZ_PE( 1,i )
!                 write( *,* ) ' I, COLSZ_PE( 2,I ): ', i, COLSZ_PE( 2,i )
!                 write( *,* ) ' I, ROWSZ_PE( 1,I ): ', i, ROWSZ_PE( 1,i )
!                 write( *,* ) ' I, ROWSZ_PE( 2,I ): ', i, ROWSZ_PE( 2,i )
!              end do

               IF ( LIPR ) THEN

                  ALLOCATE ( DELC( MY_PACOLS,MY_PAROWS,PALEVS,NIPRVAR ),
     &                       STAT = ALST )
                  IF ( ALST .NE. 0 ) THEN
                     XMSG = '*** DELC Memory allocation failed'
                     CALL M3WARN ( PNAME, 0, 0, XMSG )
                     SUCCESS = .FALSE.; RETURN
                  END IF

                  ALLOCATE ( CSAV( MY_PACOLS,MY_PAROWS,PALEVS,NCSAVE ),
     &                       STAT = ALST )
                  IF ( ALST .NE. 0 ) THEN
                     XMSG = '*** CSAV Memory allocation failed'
                     CALL M3WARN ( PNAME, 0, 0, XMSG )
                     SUCCESS = .FALSE.; RETURN
                  END IF

               END IF

               IF ( LIRR ) THEN

C Determine IRR subgrid mapping without data redistribution
                  CALL SUBST_SUBGRID_INDEX
     &                ( PA_BEGCOL, PA_ENDCOL, PA_BEGROW, PA_ENDROW,
     &                  PA_BEGLEV, PA_ENDLEV,
     &                  MY_IRR_BEGCOL, MY_IRR_ENDCOL, MY_IRR_BEGROW,
     &                  MY_IRR_ENDROW, MY_IRR_BEGLEV, MY_IRR_ENDLEV, 'N' )

                  X1F0 = ( MY_NCOLS / GL_NCOLS ) * ( MY_NROWS / GL_NROWS )
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
