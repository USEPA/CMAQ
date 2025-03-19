
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
C $Header: /project/yoj/arc/CCTM/src/biog/beis3/chkgrid.F,v 1.5 2011/10/21 16:10:17 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      FUNCTION CHKGRID( FNAME ) RESULT( SUCCESS )

C-----------------------------------------------------------------------
 
C  Description:
C    Check the grid information and against the first file referenced, and
C    update for next check
 
C  Preconditions:
C    FDESC3 common already loaded from call to DESC3 before calling this routine
 
C  Subroutines and Functions Called:
 
C  Revision History:
C    Oct 2007 Jeff: Make a function and simplify for inline BEIS in CMAQ
C    Feb 2011 S.Roselle: Replaced I/O API include files with UTILIO_DEFN
C    10 May 2011 D.Wong: incorporated twoway model implementation
 
C-----------------------------------------------------------------------
C Modified from:

C Project Title: EDSS Tools Library
C File: @(#)$Id: chkgrid.F,v 1.5 2011/10/21 16:10:17 yoj Exp $
C COPYRIGHT (C) 2004, Environmental Modeling for Policy Development
C All Rights Reserved
C Carolina Environmental Program
C University of North Carolina at Chapel Hill
C 137 E. Franklin St., CB# 6116
C Chapel Hill, NC 27599-6116
C smoke@unc.edu
C Pathname: $Source: /project/yoj/arc/CCTM/src/biog/beis3/chkgrid.F,v $
C Last updated: $Date: 2011/10/21 16:10:17 $
C-----------------------------------------------------------------------

      USE UTILIO_DEFN

      IMPLICIT NONE

C Includes:

C Arguments:
      CHARACTER( * ), INTENT( IN )  :: FNAME   ! File name
      LOGICAL                       :: SUCCESS ! true: comparison succeeded

C External Functions:

C Parameters:
      CHARACTER( 10 ), PARAMETER :: BLANK10  = ' '

C Local variables:
      CHARACTER( 16 ), SAVE :: GRDNM = ' ' ! grid nam
      INTEGER,   SAVE :: GDTYP  = -1       ! i/o api grid type code
      INTEGER,   SAVE :: NCOLS  = 0        ! number of columns in grid
      INTEGER,   SAVE :: NROWS  = 0        ! number of rows in grid
      REAL( 8 ), SAVE :: XORIG  = 0.D0     ! x-origin of grid
      REAL( 8 ), SAVE :: YORIG  = 0.D0     ! y-origin of grid
      REAL( 8 ), SAVE :: XCENT  = 0.D0     ! x-center of projection
      REAL( 8 ), SAVE :: YCENT  = 0.D0     ! y-center of projection
      REAL( 8 ), SAVE :: XCELL  = 0.D0     ! x-dim of cells
      REAL( 8 ), SAVE :: YCELL  = 0.D0     ! y-dim of cells
      REAL( 8 ), SAVE :: P_ALP  = 0.D0     ! projection alpha
      REAL( 8 ), SAVE :: P_BET  = 0.D0     ! projection beta
      REAL( 8 ), SAVE :: P_GAM  = 0.D0     ! projection gamma
      LOGICAL,   SAVE :: OFFLAG = .FALSE.  ! true: subgrid offset has been set
      INTEGER,   SAVE :: XOFF   = 0        ! subgrid offset (x-sub = x - xoff)
      INTEGER,   SAVE :: YOFF   = 0        ! subgrid offset
      INTEGER,   SAVE :: XOFF_A = 0        ! tmp subgrid offset (x-sub = x - xoff)
      INTEGER,   SAVE :: YOFF_A = 0        ! tmp subgrid offset
!     INTEGER       L       ! length of file description
      INTEGER       NC                     ! test number of columns
      INTEGER       NR                     ! test number of rows
      INTEGER       XO                     ! test x-offset  
      INTEGER       YO                     ! test y-offset  

      REAL( 8 )     :: CHK_X   ! test val for checking subgrid even with grid
      REAL( 8 )     :: CHK_Y   ! test val for checking subgrid even with grid

      LOGICAL, SAVE :: GINIT  = .FALSE. ! true: grid settings have been initialized

      CHARACTER(  12 ) :: FILETYPE = 'gridded file'
      CHARACTER( 300 ) :: MESG     ! message buffer

      CHARACTER( 16 ) :: PNAME = 'CHKGRID' ! procedure name

C-----------------------------------------------------------------------

      SUCCESS = .TRUE.

#ifndef mpas
      NC = NCOLS3D
      NR = NROWS3D

C If grid information has already been initialized, then compare existing to this file
      IF ( GINIT ) THEN

#ifndef twoway
C Check settings that must be consistent for exact grid match
         IF ( NC .NE. NCOLS            .OR.
     &        NR .NE. NROWS            .OR.
     &        DBLERR( XORIG3D, XORIG ) .OR.
     &        DBLERR( YORIG3D, YORIG ) ) THEN
            SUCCESS = .TRUE.
            MESG = 'WARNING: Columns, rows, x-origin, or ' //
     &             'y-origin for ' // FNAME // ' in ' //
     &             CRLF() // BLANK10 // FILETYPE // 
     &             ' are inconsistent with GRID NAME: ' // 
     &             TRIM( GRDNM )
            CALL M3MSG2( MESG ) 
         END IF
#endif

         XOFF = 0
         YOFF = 0

C Check settings that must be consistent for grids and subgrids
         IF ( GDTYP3D .NE. GDTYP        .OR.
     &        DBLERR( XCELL3D, XCELL  ) .OR.
     &        DBLERR( YCELL3D, YCELL  ) .OR.
     &        DBLERR( XCENT3D, XCENT  ) .OR.
     &        DBLERR( YCENT3D, YCENT  ) .OR.
     &        DBLERR( P_ALP3D, P_ALP  ) .OR.
     &        DBLERR( P_BET3D, P_BET  ) .OR.
     &        DBLERR( P_GAM3D, P_GAM  ) ) THEN
            SUCCESS = .FALSE.
            MESG = 'ERROR: Grid type, cell sizes, or ' //
     &             'grid projection for ' // FNAME // ' in '//
     &              CRLF() // BLANK10 // FILETYPE // 
     &             ' are inconsistent with with GRID NAME: ' 
     &             // TRIM( GRDNM )
            CALL M3MSG2( MESG ) 
         END IF

C Ensure that origins are compatible with each other by making sure they line up
C based on the cell sizes
         CHK_X  = ( XORIG3D - XORIG ) / XCELL
         CHK_X  = CHK_X - INT( CHK_X )
         CHK_Y  = ( YORIG3D - YORIG ) / YCELL
         CHK_Y  = CHK_Y - INT( CHK_Y )
         IF ( DBLERR( CHK_X, 0.D0 ) .OR.
     &        DBLERR( CHK_Y, 0.D0 ) ) THEN
            SUCCESS = .FALSE.
            MESG = 'ERROR: Grid origins not compatible ' //
     &             'between ' // FNAME // ' in ' // 
     &             CRLF() // BLANK10 // FILETYPE // 
     &             ' and set by GRID NAME: ' // TRIM( GRDNM )
            CALL M3MSG2( MESG ) 
         END IF

C If offset has been set, then check to ensure its the same
         IF ( OFFLAG ) THEN

C If file has different origin from the subgrid...
            IF ( XORIG3D .NE. XORIG .OR. 
     &           YORIG3D .NE. YORIG ) THEN

               XO = INT( ( XORIG3D - XORIG ) / XCELL )
               YO = INT( ( YORIG3D - YORIG ) / YCELL )
               IF ( XOFF .NE. XO .OR.
     &              YOFF .NE. YO ) THEN
                  SUCCESS = .FALSE.
                  MESG = 'WARNING: Subgrid offset for ' //
     &                   FNAME // ' in ' // CRLF() // BLANK10// 
     &                   FILETYPE // 'is ' //
     &                  'inconsistent with with GRID NAME: ' 
     &                   // TRIM( GRDNM )
                  CALL M3MSG2( MESG ) 
               END IF

C If file has same origin as subgrid
            ELSE



C Check that current subgrid is the same as the previous subgrid
               IF ( NC .NE. NCOLS            .OR.
     &              NR .NE. NROWS            .OR.
     &              DBLERR( XORIG3D, XORIG ) .OR.
     &              DBLERR( YORIG3D, YORIG ) ) THEN
                  SUCCESS = .FALSE.
                  MESG = 'WARNING: Columns, rows, x-origin, ' //
     &                   'or y-origin for ' // FNAME // ' in ' 
     &                   // CRLF() // BLANK10 // FILETYPE // 
     &                   'are inconsistent with values from ' // 
     &                   TRIM( GRDNM)
                  CALL M3MSG2( MESG ) 
               END IF

            END IF

C If offset for final subgrid hasn't been set yet...
         ELSE

C Compute possible offset from upper right hand corner, and if there is one, set flag
!?          XOFF_A = INT( ( XORIG   + NCOLS * XCELL   )
!?   &                  - ( XORIG3D +    NC * XCELL3D ) ) / XCELL
!?          YOFF_A = INT( ( YORIG   + NROWS * YCELL   )
!?   &                  - ( YORIG3D +    NR * YCELL3D ) ) / YCELL

C Compute possible offset from origin, and if so, set flag
            XOFF_A = INT( ( XORIG3D - XORIG ) / XCELL )
            YOFF_A = INT( ( YORIG3D - YORIG ) / YCELL )
               
C Reset origin and number of cells to latest grid
            GRDNM = GDNAM3D

C Store grid and offset parameters
            XOFF = XOFF_A
            YOFF = YOFF_A
            IF ( XOFF .NE. 0 .OR. YOFF .NE. 0 ) OFFLAG = .TRUE.
            XORIG = XORIG3D
            YORIG = YORIG3D
            NCOLS = NC
            NROWS = NR

         END IF

C Store initial grid information
      ELSE

         GINIT = .TRUE.
         GRDNM = GDNAM3D
         GDTYP = GDTYP3D
         P_ALP = P_ALP3D
         P_BET = P_BET3D
         P_GAM = P_GAM3D
         XCENT = XCENT3D
         YCENT = YCENT3D
         XORIG = XORIG3D
         YORIG = YORIG3D
         XCELL = XCELL3D
         YCELL = YCELL3D
         NCOLS = NC
         NROWS = NR

         MESG = 'NOTE: Grid settings initialized using ' // 
     &          TRIM( FNAME ) // ' in ' // CRLF() // BLANK10 // 
     &          FILETYPE // ' GRID NAME: ' // TRIM( GRDNM )

         CALL M3MSG2( MESG )

      END IF

      IF ( .NOT. SUCCESS ) THEN

         MESG = 'ERROR: Grid parameters for ' // FNAME // ' in ' //
     &          CRLF() // BLANK10 // FILETYPE //
     &          ' are inconsistent with GRID NAME: ' // TRIM( GRDNM )
         CALL M3MSG2( MESG )

      END IF
#endif

      RETURN

      CONTAINS

         LOGICAL FUNCTION DBLERR( PD, QD )

         IMPLICIT NONE

         REAL( 8 ), INTENT( IN ) :: PD
         REAL( 8 ), INTENT( IN ) :: QD

         DBLERR = ( ( PD - QD )*( PD - QD ) .GT. 1.0D-12 * ( PD*PD + QD*QD + 1.0D-5 ) )

         RETURN

         END FUNCTION DBLERR

      END

