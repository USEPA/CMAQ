
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
C $Header: /project/yoj/arc/CCTM/src/par/par_noop/par_init_noop.F,v 1.2 2011/10/21 16:11:26 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

      SUBROUTINE PAR_INIT( COLROW, NSPCS, CLOCK, IERROR )
C.....................................................................
C
C  PURPOSE:    No-operation version of parallel initialization, for
C              running in serial configuration. Local processor
C              dimensions are set to the one-processor values.
C              
C
C  REVISION HISTORY: 
C       Original version 10/1998 by Al Bourgeois for merging parallel
C                and serial codes.
C       Modified 01/1998 by Al Bourgeois to fix subscripts in arrays 
C                ROWSX_PE and COLSX_PE. 
C       Modified 11/2001 by Jeff Young for dynamic alloc version
C       Modified 31/01/05 J.Young: dyn alloc - establish both horizontal &
C                vertical domain specifications in one module
C                16 Jan 06 J.Young: possible PA grid init
C       Modified 03/29/11 S.Roselle: Replaced I/O API include files with
C                UTILIO_DEFN
C
C
C  ARGUMENT LIST DESCRIPTION:
C  IN:
C     CHARACTER( 2 ), INTENT( IN ) :: COLROW ! columns/rows arg list check
C     INTEGER NSPCS             ! Number of species in CGRID.
C
C  OUT:
C     REAL    CLOCK             ! Wall-clock time (sec) at MPI initialization.
C     INTEGER IERROR            ! Error code.
C
C
C  LOCAL VARIABLE DESCRIPTION:  see below
C
C  CALLS:  None 
C
C........................................................................

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE PA_DEFN               ! PROCAN flags
      USE PAGRD_DEFN            ! PA horiz domain specs
      USE UTILIO_DEFN

      IMPLICIT  NONE

C Include Files

C Arguments

      CHARACTER( 2 ), INTENT( IN ) :: COLROW ! columns/rows arg list check
      INTEGER, INTENT( IN )        :: NSPCS  ! Number of species in CGRID
      REAL, INTENT( OUT ) :: CLOCK ! Wall-clock time (sec) at MPI initialization
      INTEGER, INTENT( OUT )       :: IERROR ! Error code

C Local Variables

      CHARACTER( 96 ) :: MSG

C Set error code to zero.
      IERROR = 0

      NPROCS = 1
      MYPE = 0
      CLOCK = 0

C Set up horizontal domain and calculate processor-to-subdomain maps
C and define vertical layer structure (in module GRID_CONF)

      IF ( .NOT. GRID_INIT ( NPROCS, MYPE ) ) THEN
         MSG = '*** Failure defining domain configuration'
         CALL M3WARN ( 'PAR_INIT', 0, 0, MSG )
         IERROR = 1
         RETURN
      END IF

C Set up horizontal domain and calculate processor-to-subdomain maps for
C process analysis, if required

      IF ( LIPR .OR. LIRR ) THEN
         IF ( .NOT. PAGRD_INIT( MYPE ) ) THEN
            MSG = '*** Failure defining PA domain configuration'
            CALL M3WARN ( 'PAR_INIT', 0, 0, MSG )
            IERROR = 1
            RETURN
         END IF
      END IF

      RETURN
      END
