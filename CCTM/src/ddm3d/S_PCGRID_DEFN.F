
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
C $Header: /project/work/rep/CCTM/src/driver/yamo_ddm3d/S_PCGRID_DEFN.F,v 1.1.1.1 2009/09/29 12:58:45 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE S_PCGRID_DEFN

C-----------------------------------------------------------------------
C Function:
C   Create SENGRID target, S_PCGRID. 

C Preconditions:
C   Horizontal domain extents must be set (subroutine PAR_INIT -> HGRD_DEFN)
C   Number of species in the species groups must be available (include files
C   in CGRID_SPCS)
C   This module is referenced in driver and sciproc. driver must call HGRD_INIT,
C   CGRID_MAP, and PAR_INIT prior to calling PCGRID_INIT (below).

C Subroutines and functions called:

C Revision history:
C   Apr 00 - Al, David, Jeff - initial implementation
C   Dec 00 - Jeff - add CGRID_MAP "method"
C   Jan 01 - Jeff - remove CGRID_MAP "method" - now in module CGRID_SPCS
C   Nov 01 - Jeff - correct pointer reference for ping
C   Jul 02 - D. Cohan - created this based on PCGRID_DEFN, for use
C                       in sensitivity analysis
C   Jan 04 - D. Cohan - modified for compatibility with Intel compiler
C   Jul 06 - D. Cohan - modified to reflect changes in PCGRID_DEFN.F:
C   Dec 04 - Jeff - dyn alloc - USE VGRD_DEFN
C   Jan 05 - Jeff - dyn alloc - establish both horizontal & vertical
C                   domain specifications in one module
C   Nov 12 - S.L. Napelenok - update for cmaq 5.0
C   Oct 15 - S.L. Napelenok - update for cmaq 5.1.1
C-----------------------------------------------------------------------

      USE GRID_CONF                ! horizontal & vertical domain specifications
      USE CGRID_SPCS, ONLY: NSPCSD ! CGRID species number and offsets

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE, TARGET :: S_PCGRID( :,:,:,:,: )
      REAL, ALLOCATABLE, SAVE, TARGET :: S_PCGRID_TEMP( :,:,:,: )

      CONTAINS
         FUNCTION S_PCGRID_IN ( NPMAX ) RESULT ( SUCCESS )

         USE UTILIO_DEFN

         INTEGER :: NPMAX
         LOGICAL :: SUCCESS
         INTEGER :: ALLOCSTAT
         INTEGER, SAVE :: LOGDEV
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
            LOGDEV = INIT3()

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

            ALLOCATE 
     &         ( S_PCGRID( LCOL:HCOL,LROW:HROW,NLAYS,NPMAX,NSPCSD ),
     &           S_PCGRID_TEMP(  LCOL:HCOL,LROW:HROW,NLAYS,NSPCSD ),
     &           STAT = ALLOCSTAT )

            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating S_PCGRID or S_PCGRID_TEMP'
               CALL M3WARN ( 'S_PCGRID_IN', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
               END IF

            S_PCGRID = 0.0
!           PCGRID = -999.999

            ELSE
            XMSG = 'S_PCGRID or S_PCGRID_TEMP already ALLOCATED'
            CALL M3WARN ( 'S_PCGRID_IN', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

          RETURN
          END FUNCTION S_PCGRID_IN

      END MODULE S_PCGRID_DEFN
