
!-----------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in    !
!  continuous development by various groups and is based on information !
!  from these groups: Federal Government employees, contractors working !
!  within a United States Government contract, and non-Federal sources  !
!  including research institutions.  These groups give the Government   !
!  permission to use, prepare derivative works of, and distribute copies!
!  of their work in the CMAQ system to the public and to permit others  !
!  to do so.  The United States Environmental Protection Agency         !
!  therefore grants similar permission to use the CMAQ system software, !
!  but users are requested to provide copies of derivative works or     !
!  products designed to operate in the CMAQ system to the United States !
!  Government without restrictions as to use by others.  Software       !
!  that is used with the CMAQ system but distributed under the GNU      !
!  General Public License or the GNU Lesser General Public License is   !
!  subject to their copyright restrictions.                             !
!-----------------------------------------------------------------------!

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /home/sjr/cvs2git/TOOLS/src/sitecmp/module_grid.F,v 1.4 2011/10/21 14:41:32 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C*************************************************************************
C
C  MODULE:  defines a grid class
C             
C*************************************************************************
      MODULE GRID_DATA

      TYPE GRID

         INTEGER  :: NCOLS
         INTEGER  :: NROWS
         INTEGER  :: GDTYP

         REAL*8  ::  P_ALP
         REAL*8  ::  P_BET
         REAL*8  ::  P_GAM
         REAL*8  ::  XCENT
         REAL*8  ::  YCENT
         REAL*8  ::  XORIG
         REAL*8  ::  YORIG
         REAL*8  ::  XCELL
         REAL*8  ::  YCELL

      END TYPE GRID

      TYPE ( GRID ) M3GRID

      CONTAINS

         SUBROUTINE GET_M3GRID
C*************************************************************************
C
C  FUNCTION:  To set grid data
C             
C*************************************************************************
         USE M3FILES
         USE M3UTILIO

         IMPLICIT NONE 

C..ARGUMENTS: None

C..PARAMETERS: None

C..EXTERNAL FUNCTIONS: None

C..SAVED LOCAL VARIABLES: None

C..SCRATCH LOCAL VARIABLES:
         CHARACTER*16    PNAME        ! Program Name

         INTEGER   N                  ! Loop index
         INTEGER   STATUS             ! Status code

C**********************************************************************
         DATA  PNAME       / 'GET_GRID'  /

         IF ( .NOT. DESC3( M3_FLNAME( 1 ) ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                  'Could not get ' // M3_FLNAME( 1 ) //
     &                  ' file description', XSTAT1 )
         ENDIF

         M3GRID % NCOLS = NCOLS3D
c         M3GRID % NROWS = NCOLS3D
         M3GRID % NROWS = NROWS3D
         M3GRID % GDTYP = GDTYP3D

         M3GRID % P_ALP = P_ALP3D
         M3GRID % P_BET = P_BET3D
         M3GRID % P_GAM = P_GAM3D
         M3GRID % XCENT = XCENT3D
         M3GRID % YCENT = YCENT3D
         M3GRID % XORIG = XORIG3D
         M3GRID % YORIG = YORIG3D
         M3GRID % XCELL = XCELL3D
         M3GRID % YCELL = YCELL3D

         RETURN

         END SUBROUTINE GET_M3GRID

      END MODULE GRID_DATA
