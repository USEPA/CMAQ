
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
C $Header: /project/yoj/arc/CCTM/src/util/util/subhfile.F,v 1.2 2011/04/01 15:41:57 sjr Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE SUBHFILE ( FNAME, GXOFF, GYOFF,
     &                      STRTCOL, ENDCOL, STRTROW, ENDROW )

C returns local starting and ending column and row indices for file FNAME

      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN

      IMPLICIT NONE
 
C Includes:
 
      INCLUDE SUBST_FILES_ID    ! file name parameters

C Arguments:

      CHARACTER( 16 ), INTENT( IN )  :: FNAME
      INTEGER,         INTENT( OUT ) :: GXOFF ! X global origin offset from file (.ge. 0)
      INTEGER,         INTENT( OUT ) :: GYOFF ! Y global origin offset from file (.ge. 0)
      INTEGER,         INTENT( OUT ) :: STRTCOL ! local processor start colum in file
      INTEGER,         INTENT( OUT ) ::  ENDCOL ! local processor end colum in file
      INTEGER,         INTENT( OUT ) :: STRTROW ! local processor start row in file
      INTEGER,         INTENT( OUT ) ::  ENDROW ! local processor end row in file

C External Functions:
  
C Parameters

      REAL( 8 ), PARAMETER :: HALF = 0.5D+00
      REAL( 8 ), PARAMETER :: ONE  = 1.0D+00
      REAL( 8 ), PARAMETER :: TEN  = 1.0D+01
      REAL( 8 ), PARAMETER :: ONEK = 1.0D+03
      REAL( 8 ), PARAMETER :: TENK = 1.0D+04
      REAL( 8 ), PARAMETER :: TOL  = ONE / ONEK
      REAL( 8 ), PARAMETER :: THOU = 1.0D-03
      REAL( 8 ), PARAMETER :: MIN_DOUBLE = 1.0D-08

C local variables:

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: LOGDEV
      INTEGER       :: INDX                 ! because mype starts at 0
      CHARACTER( 16 ) :: PNAME = 'SubhFile_Cell'
      CHARACTER( 16 ) :: BNAME
      CHARACTER( 96 ) :: XMSG = ' '

      REAL( 8 ), SAVE :: XORIG_B, YORIG_B
      REAL( 8 ), SAVE :: XCENT_B, YCENT_B
      REAL( 8 ), SAVE :: XCELL_B, YCELL_B
      REAL( 8 ), SAVE :: XORIG_C, YORIG_C  ! from GRIDDESC, in cell coord
      REAL( 8 ), SAVE :: XORIG_F, YORIG_F  ! from file, in cell coord

      INTEGER DOTFILE
      REAL( 8 ) :: RELOFFX, RELOFFY

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.
         LOGDEV = INIT3 ()

C open cross file for subsequent comparison

         BNAME = TRIM( GRID_CRO_2D )

         IF ( .NOT. OPEN3( BNAME, FSREAD3, PNAME ) ) THEN
            XMSG = 'Could not open '// BNAME
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
            END IF

         IF ( .NOT. DESC3( BNAME ) ) THEN
            XMSG = 'Could not get ' // BNAME // ' file description'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            END IF

C set base header data

         XORIG_B = XORIG3D
         YORIG_B = YORIG3D
         XCENT_B = XCENT3D
         YCENT_B = YCENT3D
         XCELL_B = XCELL3D
         YCELL_B = YCELL3D

C Scale resolution

         XCELL_B = FLOAT( NINT( TENK * XCELL_B ) ) / TENK
         YCELL_B = FLOAT( NINT( TENK * YCELL_B ) ) / TENK

C Convert to grid cell coord and truncate

         XORIG_B = XORIG_B / XCELL_B
         XORIG_B = FLOAT( NINT( ONEK * XORIG_B ) ) / ONEK
         YORIG_B = YORIG_B / YCELL_B
         YORIG_B = FLOAT( NINT( ONEK * YORIG_B ) ) / ONEK

         XORIG_C = XORIG_GD / XCELL_GD
         XORIG_C = FLOAT( NINT( ONEK * XORIG_C ) ) / ONEK
         YORIG_C = YORIG_GD / YCELL_GD
         YORIG_C = FLOAT( NINT( ONEK * YORIG_C ) ) / ONEK

      END IF   ! FIRSTIME

C open existing file for readonly access

      IF ( .NOT. OPEN3( FNAME, FSREAD3, PNAME ) ) THEN
         XMSG = 'Could not open '// FNAME
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
      END IF

      IF ( .NOT. DESC3( FNAME ) ) THEN
         XMSG = 'Could not get ' // TRIM( FNAME )
     &        // ' file description'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

C check some header data against the reference file in this met set -
C mother grid center and grid cell resolution

      IF ( ABS( XCENT3D - XCENT_B ) .GT.
     &     TOL * ABS( XCENT3D + XCENT_B + TOL ) .OR.
     &     ABS( YCENT3D - YCENT_B ) .GT.
     &     TOL * ABS( YCENT3D + YCENT_B + TOL ) .OR.
     &     ABS( XCELL3D - XCELL_B ) .GT. TOL * XCELL3D .OR.
     &     ABS( YCELL3D - YCELL_B ) .GT. TOL * YCELL3D ) THEN
         WRITE( LOGDEV,2003 ) XCENT_B, XCENT3D, YCENT_B, YCENT3D,
     &                        XCELL_B, XCELL3D, YCELL_B, YCELL3D
2003     FORMAT(/ 5X, 'XCENT_B:', F20.12, 2X, 'XCENT3D (file):', F20.12 
     &          / 5X, 'YCENT_B:', F20.12, 2X, 'YCENT3D (file):', F20.12
     &          / 5X, 'XCELL_B:', F20.12, 2X, 'XCELL3D (file):', F20.12 
     &          / 5X, 'YCELL_B:', F20.12, 2X, 'YCELL3D (file):', F20.12 )

         XMSG = 'File header inconsistent with GRID_CRO_2D'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

C check some header data against GRIDDESC

      IF ( ABS( XCENT3D - XCENT_GD ) .GT.
     &     TOL * ABS( XCENT3D + XCENT_GD + TOL ) .OR.
     &     ABS( YCENT3D - YCENT_GD ) .GT.
     &     TOL * ABS( YCENT3D + YCENT_GD + TOL ) .OR.
     &     ABS( XCELL3D - XCELL_GD ) .GT. TOL * XCELL3D .OR.
     &     ABS( YCELL3D - YCELL_GD ) .GT. TOL * YCELL3D ) THEN
         WRITE( LOGDEV,2003 ) XCENT_GD, XCENT3D, YCENT_GD, YCENT3D,
     &                        XCELL_GD, XCELL3D, YCELL_GD, YCELL3D

         XMSG = 'File header inconsistent with GRIDDESC'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

C Convert to grid cell coord and truncate

      XORIG_F = XORIG3D / XCELL3D
      XORIG_F = FLOAT( NINT( ONEK * XORIG_F ) ) / ONEK
      YORIG_F = YORIG3D / YCELL3D
      YORIG_F = FLOAT( NINT( ONEK * YORIG_F ) ) / ONEK
 
C Check if file (cross or dot) is "lined up"

      RELOFFX = THOU * NINT( ONEK * ( XORIG_B - XORIG_F ) )
      RELOFFX = RELOFFX - ONE * NINT( RELOFFX )

      IF ( ABS( RELOFFX ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFX ) .LT. HALF - MIN_DOUBLE .OR.
     &        ABS( RELOFFX ) .GT. HALF + MIN_DOUBLE ) THEN
            WRITE( LOGDEV,* ) '    RELOFFX: ', RELOFFX
            WRITE( LOGDEV,* ) '    XORIG_B, XORIG_F: ', XORIG_B, XORIG_F
            XMSG = 'Inconsistent Base/File Xorig'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            END IF
      END IF

      RELOFFY = THOU * NINT( ONEK * ( YORIG_B - YORIG_F ) )
      RELOFFY = RELOFFY - ONE * NINT( RELOFFY )

      IF ( ABS( RELOFFY ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFY ) .LT. HALF - MIN_DOUBLE .OR.
     &        ABS( RELOFFY ) .GT. HALF + MIN_DOUBLE ) THEN
            WRITE( LOGDEV,* ) '    RELOFFY: ', RELOFFY
            WRITE( LOGDEV,* ) '    YORIG_B, YORIG_F: ', YORIG_B, YORIG_F
            XMSG = 'Inconsistent Base/File Yorig'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            END IF
      END IF
      IF ( ABS( RELOFFX - RELOFFY ) .GT. MIN_DOUBLE ) THEN
         XMSG = 'Inconsistent X- and Y-resolution (file vs. file)'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

C check the file against the processor setup (COORD.EXT), and get the offsets

      DOTFILE = 0
      RELOFFX = THOU * NINT( ONEK * ( XORIG_C - XORIG_F ) )
      RELOFFX = RELOFFX - ONE * NINT( RELOFFX )

      IF ( ABS( RELOFFX ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFX ) .LT. HALF - MIN_DOUBLE .OR.
     &        ABS( RELOFFX ) .GT. HALF + MIN_DOUBLE ) THEN
            WRITE( LOGDEV,* ) '    RELOFFX: ', RELOFFX
            WRITE( LOGDEV,* ) '    XORIG_GD, XORIG_F: ', XORIG_C, XORIG_F
            XMSG = 'File Xorig inconsistent with GRIDDESC'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            END IF
      END IF

      RELOFFY = THOU * NINT( ONEK * ( YORIG_C - YORIG_F ) )
      RELOFFY = RELOFFY - ONE * NINT( RELOFFY )

      IF ( ABS( RELOFFY ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFY ) .LT. HALF - MIN_DOUBLE .OR.
     &        ABS( RELOFFY ) .GT. HALF + MIN_DOUBLE ) THEN
            WRITE( LOGDEV,* ) '    RELOFFY: ', RELOFFY
            WRITE( LOGDEV,* ) '    YORIG_GD, YORIG_F: ', YORIG_C, YORIG_F
            XMSG = 'File Yorig inconsistent with GRIDDESC'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            ELSE
            DOTFILE = 1
            END IF
      END IF
      IF ( ABS( RELOFFX - RELOFFY ) .GT. MIN_DOUBLE ) THEN
         XMSG = 'Inconsistent X- and Y-resolution (file vs. model)'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

      INDX = MYPE + 1
      GXOFF   = XORIG_C - XORIG_F
      STRTCOL = GXOFF + COLSX_PE( 1,INDX )
      ENDCOL  = STRTCOL + MY_NCOLS - 1 + DOTFILE
      GYOFF   = YORIG_C - YORIG_F
      STRTROW = GYOFF + ROWSX_PE( 1,INDX )
      ENDROW  = STRTROW + MY_NROWS - 1 + DOTFILE
      IF ( GXOFF .LT. 0 .OR. GYOFF .LT. 0 ) THEN
         XMSG = 'Model domain is outside file domain'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

!     WRITE( LOGDEV,* ) ' '
!     WRITE( LOGDEV,* ) ' File: ', FNAME
!     WRITE( LOGDEV,* ) '    StartCol: ', STRTCOL
!     WRITE( LOGDEV,* ) '      EndCol: ', ENDCOL
!     WRITE( LOGDEV,* ) '    StartRow: ', STRTROW
!     WRITE( LOGDEV,* ) '      EndRow: ', ENDROW

      RETURN
      END SUBROUTINE SUBHFILE
