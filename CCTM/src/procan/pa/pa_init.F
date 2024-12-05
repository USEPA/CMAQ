
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

      SUBROUTINE PA_INIT( CGRID, SDATE, STIME, TSTEP )
 
C-----------------------------------------------------------------------
 
C Function: Create the Process Analysis ouput files and initialize
C           Process Analysis arrays           
 
C Preconditions: None
 
C Key Subroutines/Functions Called: SUBST_PA_INDEX, SUBST_IRR_INDEX,
C                                   PA_MKHDR, SUBST_4D_DATA_COPY
 
C  Revision History:
C  Prototype created by Jerry Gipson, July, 1996
C  allow env var for file names Jeff, Dec, 1996
C  Changed CGOUT dimension parameter from NSPCSD to NSPCS -- Jerry Gipson, May, 1997.
C  Modified May, 1997 by Jerry Gipson to be consistent with beta CTM
C  Modified June 1997 by Jerry Gipson to change CGOUT to CGRID       
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Modified Jun, 1998 by Jerry Gipson to add PA domain error check
C  Modified 1/19/99 by David Wong at LM:
C                      -- add four include files because of new PA_CMN.EXT
C                      -- modify PA_INDEX function call parameter list
C                      -- add DATA_COPY function call to redistribute PA grid
C  Modified 2/26/99 by David Wong at LM:
C                      -- remove SUBST_AE_SPC, SUBST_NR_SPC, SUBST_TR_SPC,
C                         three .EXT files
C                      -- call SUBST_IRR_INDEX to determine row, column, and
C                         level loop indexes which are associated with IRR
C                         calculations
C                      -- renamed DATA_COPY function name
C                      -- use ifdef statement to distinguish parallel
C                         implementation of IRR calculation which does not 
C                         shift to the origin
C  Modified 4/12/00 by Jerry Gipson to correct units for aerosol IPRs writen to IO/API file
C  Modified 1 Jul 2000 by Jeff Young to allow for file updating - added PA_MKHDR
C  Modified 22 Nov 00 by J.Young: Dave Wong`s f90 stenex SUBGRID_INDEX and DATA_COPY
C  6 Oct 01 J.Young: dyn alloc - generic CGRID declaration; Use PAGRD_DEFN,
C                                which uses HGRD_DEFN; new pario
C  Modified 10 Oct 01 by David Wong
C                      -- let other processors open PA output files after
C                         processor has created them.
C                      -- used a new formula to compute number of IPR and IRR 
C                         output files
C  31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                                 domain specifications in one module (GRID_CONF)
C  16 Jan 06 J.Young: move PAGRD_INIT to par_init
C   1 Apr 09 J.Young: remove cruft
C   8 Jul 10 J.Young: restructure
C  23 Jul 10 D.Wong:  move first OPEN3 try inside IF ( MYPE .EQ. 0 ) clause
C  16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C  11 May 11 D.Wong: incorporated twoway model implementation
C  16 Sep 16 J.Young: update for inline procan
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE PAGRD_DEFN            ! PA horiz domain specs
      USE UTILIO_DEFN
#ifndef mpas
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_UTIL_MODULE, SE_DATA_COPY_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_UTIL_MODULE, NOOP_DATA_COPY_MODULE)
#endif
#endif
      USE PA_DEFN               ! Process Anaylsis control and data variables

      IMPLICIT NONE 

C..Includes:
      INCLUDE SUBST_FILES_ID    ! file name parameters

C..Arguments:
      REAL, POINTER :: CGRID( :,:,:,: )
      INTEGER, INTENT( IN ) :: SDATE      !  starting date, format YYYYDDD
      INTEGER, INTENT( IN ) :: STIME      !  starting time, format HHMMSS
      INTEGER, INTENT( IN ) :: TSTEP( 3 ) ! time step vector (HHMMSS)
                             ! TSTEP(1) = local output step
                             ! TSTEP(2) = sciproc sync. step (chem)
                             ! TSTEP(3) = twoway model time step w.r.t. wrf time
                             !            step and wrf/cmaq call frequency

C..Parameters:

C..External Functions: None

C..Local Variables:
      CHARACTER(  16 ) :: ENV_DFLT  ! Environment variable default value
      CHARACTER(  80 ) :: ENV_DESC  ! Environment variab;e description
      CHARACTER(  16 ) :: OUTFNAME  ! Assigned IPR or IRR output file name
      CHARACTER(  16 ) :: PNAME = 'PA_INIT' ! Routine Name
      CHARACTER( 128 ) :: XMSG = ' '
      CHARACTER( 256 ) :: RET_VAL   ! Returned value of environment variable

      LOGICAL LSTOP     ! Flag to stop because a PA file not assigned
 
      INTEGER C         ! Loop index for columns
      INTEGER R         ! Loop index for rows
      INTEGER L         ! Loop index for layers
      INTEGER PC        ! Index for PA output column
      INTEGER PL        ! Index for PA output level
      INTEGER PR        ! Index for PA output row
      INTEGER ICG       ! Index for species in cgout array
      INTEGER NIRR      ! Loop index for IRR outputs
      INTEGER NFL       ! Loop index for no. of files
      INTEGER NOUT      ! Loop index for process monitoring outputs
      INTEGER NUMFLS    ! Number of PA or IRR output files needed
      INTEGER STATUS    ! Status Code

C-----------------------------------------------------------------------

      LSTOP = .FALSE.         

#ifndef mpas
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Open the Integrated Process Rates output file(s) if needed
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF ( LIPR ) THEN

C..compute the number of files needed

         NUMFLS = ( NIPRVAR - 1 ) / MXVARS3 + 1

C..open each one 
         DO NFL = 1, NUMFLS

            IF ( NFL .EQ. 1 ) THEN
               OUTFNAME = CTM_IPR_1
            ELSE IF ( NFL .EQ. 2 ) THEN
               OUTFNAME = CTM_IPR_2
            ELSE IF ( NFL .EQ. 3 ) THEN
               OUTFNAME = CTM_IPR_3
            ELSE IF ( NFL .GT. 3 ) THEN
               XMSG = 'Maximum number of IPR output files exceeded'
               CALL M3EXIT( PNAME, SDATE, STIME, XMSG, XSTAT1 )
            END IF

            ENV_DESC = 'IPR Output file ' // OUTFNAME 
            ENV_DFLT = ' '       
            XMSG = 'IPR output file ' // TRIM( OUTFNAME ) // ' not assigned'
            CALL ENVSTR( OUTFNAME, ENV_DESC, ENV_DFLT, RET_VAL, STATUS)
            IF ( STATUS .NE. 0 ) CALL M3EXIT( PNAME, SDATE, STIME, XMSG, XSTAT1 )

            IF ( MYPE .EQ. 0 ) THEN

C..try to open existing file for update
               IF ( .NOT. OPEN3( OUTFNAME, FSRDWR3, PNAME ) ) THEN

                  XMSG = 'Could not open ' // TRIM( OUTFNAME )
     &                 // ' file for update - try to open new'
                  CALL M3MESG( XMSG )

                  CALL PA_MKHDR( 'IPR', NFL, OUTFNAME, SDATE, STIME, TSTEP )

                  IF ( OPEN3( OUTFNAME, FSNEW3, PNAME ) ) THEN   ! open new
                     XMSG = 'Opened Integrated Reaction Rate Output File: '
     &                    // OUTFNAME
                     CALL M3MESG( XMSG )
                  ELSE
                     XMSG = 'Could not open Integrated Process Rate Output File: ' 
     &                   // OUTFNAME
                     CALL M3EXIT( PNAME, SDATE, STIME, XMSG, XSTAT1 )
                  END IF

               END IF

            END IF

            CALL SUBST_BARRIER

         END DO   ! NUMFLS

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Initialize arrays
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         DELC = 0.0

      END IF   ! LIPR

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Open the Integrated Reaction Rates output file(s) if needed
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF ( LIRR ) THEN

C..compute the number of files needed

         NUMFLS = ( NIRRVAR - 1 ) / MXVARS3 + 1

C..open each one
         DO NFL = 1, NUMFLS

            IF ( NFL .EQ. 1 ) THEN
               OUTFNAME = CTM_IRR_1
            ELSE IF ( NFL .EQ. 2 ) THEN
               OUTFNAME = CTM_IRR_2
            ELSE IF ( NFL .EQ. 3 ) THEN
               OUTFNAME = CTM_IRR_3
            ELSE IF ( NFL .GT. 3 ) THEN
               XMSG = 'Maximum number of IRR output files exceeded'
               CALL M3EXIT(PNAME, SDATE, STIME, XMSG, XSTAT2 )
            END IF

            ENV_DESC = 'IRR Output file ' // OUTFNAME 
            ENV_DFLT = ' '       
            CALL ENVSTR( OUTFNAME, ENV_DESC, ENV_DFLT, RET_VAL, STATUS )
            XMSG = 'IRR output file ' // TRIM( OUTFNAME ) // ' not assigned'
            IF ( STATUS .NE. 0 ) CALL M3EXIT( PNAME, SDATE, STIME, XMSG, XSTAT1 )

            IF ( MYPE .EQ. 0 ) THEN

C..try to open existing file for update
               IF ( .NOT. OPEN3( OUTFNAME, FSRDWR3, PNAME ) ) THEN

                  XMSG = 'Could not open ' // TRIM( OUTFNAME )
     &                 // ' file for update - try to open new'
                  CALL M3MESG( XMSG )

                  CALL PA_MKHDR( 'IRR', NFL, OUTFNAME, SDATE, STIME, TSTEP )

                  IF ( OPEN3( OUTFNAME, FSNEW3, PNAME ) ) THEN   ! open new
                     XMSG = 'Opened Integrated Reaction Rate Output File: '
     &                    // OUTFNAME
                     CALL M3MESG( XMSG )
                  ELSE
                     XMSG = 'Could not open Integrated Reaction Rate Output'
     &                    // ' File ' // OUTFNAME
                     CALL M3EXIT( PNAME, SDATE, STIME, XMSG, XSTAT1 )
                  END IF

               END IF

            END IF

            CALL SUBST_BARRIER

         END DO   ! NFL

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Initialize arrays
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         IRROUT = 0.0

      END IF   !   LIRR
#endif

      RETURN
      END
