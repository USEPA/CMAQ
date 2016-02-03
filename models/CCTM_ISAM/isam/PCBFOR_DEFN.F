
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
C $Header: /work/MOD3EVAL/krt/CMAQv5.0/models/CCTM/src/grid/cartesian/PCBFOR_DEFN.F,v 1.1.1.1 2012/01/27 20:25:19 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE PCBFOR_DEFN

C-----------------------------------------------------------------------
C20140428   Has function PCBFOR_INIT
C      
C-----------------------------------------------------------------------

      USE GRID_CONF                ! horizontal & vertical domain specifications
      USE CGRID_SPCS, ONLY: NSPCSD  ! CGRID mechanism species

      IMPLICIT NONE

      REAL, ALLOCATABLE, SAVE, TARGET :: PCBFOR( :,:,:,: )

      CONTAINS
         FUNCTION PCBFOR_INIT () RESULT ( SUCCESS )

C-----------------------------------------------------------------------
C20140428   Create CBFOR target, PCBFOR; just like CGRID on PCGRID.
C           CBFOR backs up CGRID before the latter gets changed.
C            Called by driver.F
C      
C-----------------------------------------------------------------------

         USE UTILIO_DEFN

!        INCLUDE SUBST_VGRD_ID     ! vertical dimensioning parameters

         LOGICAL :: SUCCESS
         INTEGER :: ALLOCSTAT
         INTEGER, SAVE :: LOGDEV
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 120 ) :: XMSG = ' '

C PCBFOR extents
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

            ALLOCATE ( PCBFOR( LCOL:HCOL,LROW:HROW,NLAYS,NSPCSD ),
     &                 STAT = ALLOCSTAT )

            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating PCBFOR'
               CALL M3WARN ( 'PCBFOR_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
               END IF

            PCBFOR = 0.0
!           PCBFOR = -999.999

            ELSE
            XMSG = 'PCBFOR already ALLOCATED'
            CALL M3WARN ( 'PCBFOR_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

          RETURN
          END FUNCTION PCBFOR_INIT

      END MODULE PCBFOR_DEFN
