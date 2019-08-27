
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
      MODULE PCGRID_DEFN

C-----------------------------------------------------------------------
C Function:
C   Create CGRID target, PCGRID. CGRID becomes a pointer to enable a one-
C   cell ghost region around CGRID for PinG

C Preconditions:
C   Horizontal domain extents must be set (subroutine PAR_INIT -> HGRD_DEFN)
C   Number of species in the species groups must be available (include files
C   in CGRID_SPCS)
C   This module is referenced in driver and sciproc. driver must call HGRD_INIT,
C   CGRID_MAP, and PAR_INIT prior to calling PCGRID_INIT (below).

C Subroutines and functions called:

C Revision history:
C    Apr 00 - Al, David, Jeff - initial implementation
C    Dec 00 - Jeff - add CGRID_MAP "method"
C    Jan 01 - Jeff - remove CGRID_MAP "method" - now in module CGRID_SPCS
C    Nov 01 - Jeff - correct pointer reference for ping
C    Dec 04 - Jeff - dyn alloc - USE VGRD_DEFN
C    Jan 05 - Jeff - dyn alloc - establish both horizontal & vertical
C                    domain specifications in one module
C 21 Jun 10 J.Young: convert for Namelist redesign
C   29 Mar 11 S.Roselle: Replaced I/O API include files with UTILIO_DEFN

C-----------------------------------------------------------------------

      USE GRID_CONF                ! horizontal & vertical domain specifications
      USE CGRID_SPCS, ONLY: NSPCSD  ! CGRID mechanism species

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE, TARGET :: PCGRID( :,:,:,: )

      CONTAINS
         FUNCTION PCGRID_INIT () RESULT ( SUCCESS )

         USE UTILIO_DEFN

!        INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters

         LOGICAL :: SUCCESS
         INTEGER :: ALLOCSTAT
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 120 ) :: XMSG = ' '

C PCGRID extents
         INTEGER :: LCOL
         INTEGER :: HCOL
         INTEGER :: LROW
         INTEGER :: HROW

         SUCCESS = .TRUE.

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.

#ifdef parallel
            LCOL = 1 - NTHIK
            HCOL = NCOLS + NTHIK
            LROW = 1 - NTHIK
            HROW = NROWS + NTHIK
#else
            LCOL = 1
            HCOL = NCOLS
            LROW = 1
            HROW = NROWS
#endif

            ALLOCATE ( PCGRID( LCOL:HCOL,LROW:HROW,NLAYS,NSPCSD ),
     &                 STAT = ALLOCSTAT )

            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating PCGRID'
               CALL M3WARN ( 'PCGRID_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
               END IF

            PCGRID = 0.0
!           PCGRID = -999.999

            ELSE
            XMSG = 'PCGRID already ALLOCATED'
            CALL M3WARN ( 'PCGRID_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

          RETURN
          END FUNCTION PCGRID_INIT

      END MODULE PCGRID_DEFN
