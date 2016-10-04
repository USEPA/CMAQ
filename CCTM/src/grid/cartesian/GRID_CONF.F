
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
C $Header: /project/yoj/arc/CCTM/src/grid/cartesian/GRID_CONF.F,v 1.2 2011/10/21 16:11:16 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE GRID_CONF

C Define the horizontal grid and vertical layer structure by inheriting
C HGRD_DEFN and VGRD_DEFN. In addition, set up the computational blocking
C parameters.
C VGRD_DEFN replaces COORD.EXT and VGRD.EXT
C Revision History: Jeff Young 31 Jen 05: created
C.......................................................................

      USE HGRD_DEFN             ! horizontal domain specifications
      USE VGRD_DEFN             ! vertical layer specifications

      IMPLICIT NONE

!     INTEGER, PARAMETER :: BLKSIZE = 500
      INTEGER, PARAMETER :: BLKSIZE = 50
      INTEGER, SAVE :: MXCELLS
      INTEGER, SAVE :: MXBLKS

      CONTAINS

         FUNCTION GRID_INIT ( NPROCS, MYID ) RESULT ( SUCCESS )

         INTEGER :: NPROCS
         INTEGER :: MYID
         LOGICAL :: SUCCESS

         INTEGER, SAVE :: LOGDEV
         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         CHARACTER( 96 ) :: MSG = ' '

         INTEGER, EXTERNAL :: SETUP_LOGDEV

C-----------------------------------------------------------------------

C This function is expected to be called only once - at startup

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            LOGDEV = SETUP_LOGDEV()
            SUCCESS = .TRUE.

C Set up horizontal domain and calculate processor-to-subdomain maps
            IF ( .NOT. HGRD_INIT ( NPROCS, MYID ) ) THEN
               MSG = '*** Failure defining horizontal domain'
               CALL M3WARN ( 'GRID_INIT', 0, 0, MSG )
               SUCCESS = .FALSE.
               RETURN
               END IF

C define vertical layer structure
            IF ( .NOT. VGRD_INIT() ) THEN
               MSG = '*** Failure defining vertical structure'
               CALL M3WARN ( 'GRID_INIT', 0, 0, MSG )
               SUCCESS = .FALSE.
               RETURN
               END IF

            MXCELLS = NCOLS * NROWS * NLAYS
            MXBLKS  = 1 + ( MXCELLS - 1 ) / BLKSIZE

            ELSE
            MSG = 'Domain configuration already defined'
            CALL M3WARN ( 'GRID_INIT', 0, 0, MSG )
            SUCCESS = .FALSE.; RETURN

            END IF   ! FIRSTIME

         RETURN
         END FUNCTION GRID_INIT

      END MODULE GRID_CONF


