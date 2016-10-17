
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
C $Header: /project/yoj/arc/CCTM/src/procan/pa/pa_irr_ctl.F,v 1.4 2011/10/21 16:11:33 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PA_IRR_SETUP( NBLKS, LIRRBLK, LCELL, BLKLEN,
     &                         CCOL, CROW, CLEV )

C-----------------------------------------------------------------------
C Function: Set up for and update the integrated reaction rates 
C           computed by the subroutine pa_irr
             
C Preconditions: None
 
C Key Subroutines/Functions Called: None
 
C Revision History
C  Prototype created by Jerry Gipson, November, 1996
C  Modified Sept, 1997 by Jerry Gipson to be consistent with targeted CTM
C  Modified 1/19/99 by David Wong at LM:
C                      -- add four include files because of new PA_CMN.EXT
C  Modified 2/26/99 by David Wong at LM:
C                      -- remove SUBST_AE_SPC, SUBST_NR_SPC, SUBST_TR_SPC, 
C                         three .EXT files
C                      -- use ifdef parallel to distinguish the need of
C                         shifting to origin to perform calculation in serial 
C                         but not in parallel
C                      -- change BEG* and END* to corresponding 
C                         MY_IRR_BEG*, and MY_IRR_END*, respectively
C   30 Mar 01 J.Young: Use PAGRD_DEFN, which uses HGRD_DEFN; eliminate
C                      BLKPRM.EXT; Use GRVARS
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module (GRID_CONF)
C   21 Jun 10 J.Young: convert for Namelist redesign
C   16 Sep 16 J.Young: update for inline procan (IRR)
C-----------------------------------------------------------------------

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE PA_DEFN               ! Process Anaylsis control and data variables
      USE PAGRD_DEFN            ! PA horiz domain specs

      IMPLICIT NONE 

C..Includes: None

C..Arguments: 
      INTEGER NBLKS               ! Number of blocks in modeling domain
!     LOGICAL LIRRBLK( MXBLKS )   ! Flag for doing IRR calculation for block
!     INTEGER LCELL  ( MXBLKS )   ! Offset to 1st cell in each block
!     INTEGER BLKLEN ( MXBLKS )   ! Number of cells in each block
!     INTEGER CCOL   ( MXCELLS )  ! Column pointer for cell
!     INTEGER CLEV   ( MXCELLS )  ! Level pointer for cell
!     INTEGER CROW   ( MXCELLS )  ! Row pointer for cell

      LOGICAL, INTENT( OUT ) :: LIRRBLK( : )     ! Flag for doing IRR
      INTEGER, INTENT(  IN ) :: BLKLEN ( : )     ! Number of cells in each
      INTEGER, INTENT(  IN ) :: LCELL  ( : )     ! Offset to 1st cell in each
      INTEGER, INTENT(  IN ) :: CCOL   ( : )     ! Column pointer for cell
      INTEGER, INTENT(  IN ) :: CROW   ( : )     ! Row pointer for cell
      INTEGER, INTENT(  IN ) :: CLEV   ( : )     ! Level pointer for cell

C..Additional or other Arguments for ENTRY's
      INTEGER, INTENT(  IN ) :: NUMCELLS         ! Number of cells in one block
      INTEGER, INTENT(  IN ) :: OFFSET           ! Offset to 1st cell in a block
      LOGICAL, INTENT( OUT ) :: LIRRFLAG         ! Flag for doing IRR for one block
!     INTEGER                   NORDCELL( MXCELLS )
      INTEGER, INTENT(  IN ) :: NORDCELL( : )    ! Cell number of ordered cells in a block
      INTEGER, INTENT( INOUT ) :: NIRRCLS        ! No. of cells in block for IRR cells
!     INTEGER                  IRRCELL( BLKSIZE )
      INTEGER, INTENT( INOUT ) :: IRRCELL( : )   ! Cell No. of an IRR cell

C..Parameters: None

C..External Functions: None
 
C..Saved Local Variables:
!?    LOGICAL, SAVE :: LIRRCELL( BLKSIZE )  ! Flag to do IRR for a cell

C..Scratch Local Variables:
      INTEGER BLK            ! Loop index for blocks
      INTEGER CELL           ! Counter of cells in a block
      INTEGER COL, ROW, LEV  ! Column, row, and level indices
      INTEGER ECL            ! Ending cell no. of block
      INTEGER ICL            ! Loop index for cells in a block
      INTEGER NCELL          ! Loop index for cells in a block
      INTEGER PC, PR, PL     ! Column, row, and level indices for PA outputs
      INTEGER NIRR           ! Loop index for IRR variables
      INTEGER SCL            ! Starting cell no. of block
C-----------------------------------------------------------------------

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Flag all blocks that will need IRR/MB calculations
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      DO BLK = 1, NBLKS
         SCL = LCELL( BLK ) + 1
         ECL = LCELL( BLK ) + BLKLEN( BLK )
         DO ICL = SCL, ECL
            COL = CCOL( ICL )
            ROW = CROW( ICL )
            LEV = CLEV( ICL )
            IF ( COL .GE. MY_IRR_BEGCOL .AND. COL .LE. MY_IRR_ENDCOL .AND.
     &           ROW .GE. MY_IRR_BEGROW .AND. ROW .LE. MY_IRR_ENDROW .AND.
     &           LEV .GE. MY_IRR_BEGLEV .AND. LEV .LE. MY_IRR_ENDLEV ) 
     &           LIRRBLK( BLK ) = .TRUE.
         END DO
      END DO 
      RETURN

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Check one block of cells to see if it has a cell that needs IRR
c  calculations for the case in which when the cell composition of the
c  block can change
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ENTRY PA_IRR_CKBLK ( NUMCELLS, LIRRFLAG, OFFSET, CCOL,
     &                     CROW, CLEV, NORDCELL, NIRRCLS, IRRCELL )
      NIRRCLS = 0
      DO NCELL = 1, NUMCELLS
         ICL = NORDCELL( OFFSET + NCELL )
         COL = CCOL( ICL )
         ROW = CROW( ICL )
         LEV = CLEV( ICL )
!?       LIRRCELL( NCELL ) = .FALSE.
         IF ( COL .GE. MY_IRR_BEGCOL .AND. COL .LE. MY_IRR_ENDCOL .AND.
     &       ROW .GE. MY_IRR_BEGROW .AND. ROW .LE. MY_IRR_ENDROW .AND.
     &       LEV .GE. MY_IRR_BEGLEV .AND. LEV .LE. MY_IRR_ENDLEV ) THEN
            LIRRFLAG = .TRUE.
!?          LIRRCELL( NCELL ) = .TRUE.
            NIRRCLS = NIRRCLS + 1
            IRRCELL( NIRRCLS ) = NCELL
         END IF
      END DO
      RETURN            
 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Initialize IRRBLK to zero
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ENTRY PA_IRR_BLKSTRT ( NUMCELLS )
      DO NIRR = 1, NIRRVAR
         DO CELL = 1, NUMCELLS
            IRRBLK( CELL,NIRR ) = 0.0
         END DO
      END DO
      RETURN

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Update the IRROUT array after a block is finished for the case
c  in which the cell composition of the block is fixed
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ENTRY PA_IRR_BLKENDF( OFFSET, NUMCELLS, CCOL, CROW, CLEV )

      SCL = OFFSET + 1
      ECL = OFFSET + NUMCELLS
      CELL = 0
      DO ICL = SCL, ECL
         COL = CCOL( ICL )
         ROW = CROW( ICL )
         LEV = CLEV( ICL )
         CELL = CELL + 1
         IF ( COL .GE. MY_IRR_BEGCOL .AND. COL .LE. MY_IRR_ENDCOL .AND.
     &        ROW .GE. MY_IRR_BEGROW .AND. ROW .LE. MY_IRR_ENDROW .AND.
     &        LEV .GE. MY_IRR_BEGLEV .AND. LEV .LE. MY_IRR_ENDLEV ) THEN

C -- in serial case, calculations are shifted to the origin but not in
C    parallel implementation

#ifdef parallel
            PC = COL
            PR = ROW
            PL = LEV
#else
            PC = COL - MY_IRR_BEGCOL + 1
            PR = ROW - MY_IRR_BEGROW + 1
            PL = LEV - MY_IRR_BEGLEV + 1
#endif
            DO NIRR = 1, NIRRVAR
               IRROUT( PC,PR,PL,NIRR ) = IRROUT( PC,PR,PL,NIRR )
     &                                 + IRRBLK( CELL,NIRR )      
            END DO
         END IF
      END DO

      RETURN

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Update the IRROUT array after a block is finished for the case
c  in which the cell composition of the block can change
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      ENTRY PA_IRR_BLKENDC ( OFFSET, CCOL, CROW, CLEV,
     &                       NORDCELL, NIRRCLS, IRRCELL )

      DO NCELL = 1, NIRRCLS
         CELL = IRRCELL( NCELL )
         ICL = NORDCELL( OFFSET + CELL )
         COL = CCOL( ICL )
         ROW = CROW( ICL )
         LEV = CLEV( ICL )

C -- in serial case, calculations are shifted to the origin but not in
C    parallel implementation

#ifdef parallel
         PC = COL
         PR = ROW
         PL = LEV
#else
         PC = COL - MY_IRR_BEGCOL + 1
         PR = ROW - MY_IRR_BEGROW + 1
         PL = LEV - MY_IRR_BEGLEV + 1
#endif

         DO NIRR = 1, NIRRVAR
            IRROUT( PC,PR,PL,NIRR ) = IRROUT( PC,PR,PL,NIRR )
     &                              + IRRBLK( NCELL,NIRR )      
         END DO
      END DO

      RETURN

      END
