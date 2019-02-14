
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
C $Header: /project/yoj/arc/ICON/src/common/VGRD_DEFN.F,v 1.2 2011/10/21 16:41:51 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      MODULE VGRD_DEFN

C Define the vertical layer structure (replaces COORD.EXT and VGRD.EXT)
C Revision History: Jeff Young: 6 Dec 04: created
C                              26 Jul 06: comments in line with PARMS3.EXT
C                   S. Roselle 29 Mar 11: Replaced I/O API include files
C                                         with UTILIO_DEFN
C                   S.Roselle  02 Nov 18: Use MET_CRO_3D_FIN to get layer
C                                         structure instead of requiring
C                                         a separate LAYER_FILE
C                   S.Roselle  06 Nov 18: Replaced UTILIO_DEFN with M3UTILIO
C.......................................................................

      IMPLICIT NONE

      INTEGER, SAVE :: NLAYS = 0

      REAL, ALLOCATABLE, SAVE :: VGLVS_GD( : )  ! vert layer surface values
      REAL, ALLOCATABLE, SAVE :: X3FACE_GD( : ) ! mono. incr. vert coord values
                                                ! = 1 - VGLVS_GD
      CHARACTER( 16 ), SAVE :: GDNAME_GD

C The definitions and declarations in this F90 Module follow those
C given in the I/O-API include file FDESC3C.EXT and replace similar
C definitions. This F90 Module is compatible with FDESC3C.EXT.

C VGTYP_GD:
C The vertical grid type:
C  1: VGSGPH3 : hydrostatic sigma-P
C  2: VGSGPN3 : non-h sigma-P
C  3: VGSIGZ3 : sigma-Z
C  4: VGPRES3 : pressure (mb)
C  5: VGZVAL3 : Z (m) (above sea lvl)
C  6: VGHVAL3 : H (m) (above ground)
C  7: VGWRFEM : WRF mass-core sigma
C  8: VGWRFNM : WRF NMM
C  9: IMISS3 for vertical coordinates not stored in VGLVSD
C            (e.g., temporally or spatially changing vertical coordinates)

      INTEGER, SAVE :: VGTYP_GD

C VGTPUN_GD:
C The units of the vertical coordinate top.

      CHARACTER( 16 ), SAVE :: VGTPUN_GD

C VGTOP_GD:
C The value for the model top used in the definition of the sigma coordinate
C systems in the VGTPUN_GD units.

      REAL, SAVE :: VGTOP_GD

C For sigma-P, the relationship between pressure levels P and sigma-P is
C given by:
C    sigma-P = ( P - VGTOP_GD ) / (P_srf - VGTOP_GD ),
C    where P_srf is the surface pressure.

C VGLVUN_GD:
C The units of the vertical coordinate surface values,

      CHARACTER( 16 ) :: VGLVUN_GD

C-----------------------------------------------------------------------

      CONTAINS

         FUNCTION VGRD_INIT ( ) RESULT ( SUCCESS )

            USE M3UTILIO   ! IOAPI module

            IMPLICIT NONE

            LOGICAL :: SUCCESS

!           integer, save :: logdev
            LOGICAL, SAVE :: FIRSTIME = .TRUE.
            CHARACTER( 96 ) :: XMSG = ' '
            CHARACTER( 16 ) :: LAYER_FILE = 'MET_CRO_3D_FIN'

            INTEGER L, ALLOCSTAT

C This function is expected to be called only once - at startup

            IF ( FIRSTIME ) THEN
               FIRSTIME = .FALSE.
!              logdev = init3()
               SUCCESS = .TRUE.

C get vert layer definition from LAYER_FILE

               IF ( .NOT. OPEN3( LAYER_FILE, FSREAD3, 'VGRD_INIT' ) ) THEN
                  XMSG = 'Could not open '// LAYER_FILE
                  CALL M3WARN ( 'VGRD_INIT', 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

               IF ( .NOT. DESC3( LAYER_FILE ) ) THEN
                  XMSG = 'Could not get ' // LAYER_FILE // ' file description'
                  CALL M3WARN ( 'VGRD_INIT', 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

               GDNAME_GD = GDNAM3D

               NLAYS = NLAYS3D

               VGTYP_GD  = VGTYP3D
               VGTOP_GD  = VGTOP3D
               VGTPUN_GD = 'Pa'
               VGLVUN_GD = 'none'

               ALLOCATE ( VGLVS_GD( NLAYS + 1 ), STAT = ALLOCSTAT )
               ALLOCATE ( X3FACE_GD( 0:NLAYS ), STAT = ALLOCSTAT )
               IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating VGLVS_GD, X3FACE_GD'
                  CALL M3WARN ( 'VGRD_INIT', 0, 0, XMSG )
                  SUCCESS = .FALSE.; RETURN
               END IF

               DO L = 1, NLAYS + 1
                  VGLVS_GD( L ) = VGLVS3D( L )
               END DO

C set layer layer thickenesses

               DO L = 0, NLAYS
                  X3FACE_GD( L ) = 1.0 - VGLVS_GD( L + 1 )
               END DO

            ELSE
               XMSG = 'Vertical layer structure already defined'
               CALL M3WARN ( 'VGRD_INIT', 0, 0, XMSG )
               SUCCESS = .FALSE.; RETURN

            END IF   ! FIRSTIME

         RETURN
         END FUNCTION VGRD_INIT

      END MODULE VGRD_DEFN

C  21   VGLVS_GD   X3FACE_GD
C  
C   1     1.0       0.0
C   2     0.995     0.005
C   3     0.99      0.01
C   4     0.985     0.015
C   5     0.98      0.02
C   6     0.97      0.03
C   7     0.96      0.04
C   8     0.945     0.055
C   9     0.93      0.07
C  10     0.91      0.09
C  11     0.89      0.11
C  12     0.865     0.135
C  13     0.84      0.16
C  14     0.78      0.22
C  15     0.7       0.3
C  16     0.6       0.4
C  17     0.5       0.5
C  18     0.4       0.6
C  19     0.3       0.7
C  20     0.2       0.8
C  21     0.1       0.9
C  22     0.0       1.0
