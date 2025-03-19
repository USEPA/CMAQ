
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
      SUBROUTINE SUBHFILE ( FNAME, GXOFF, GYOFF,
     &                      STRTCOL, ENDCOL, STRTROW, ENDROW )

C 11 May 11 D.Wong: incorporated twoway model implementation
C 10 Mar 14 D.Wong: turned off GXOFF and GYOFF check for the twoway model
C                   since METCRO is one grid cell bigger than the other
C                   files to avoid M3EXIT issue
C  1 Feb 19 David Wong: removed all MY_N clauses
C 23 Mar 21 David Wong: modified code to accommodate a flexibility to allow each input
C                       can have different XORIG and YORIG settings than the simulation
C                       domain if it can be overlapped with the simulation domain
C                       perfectly w.r.t. domain resolution

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
      INTEGER       :: INDX                 ! because mype starts at 0
      CHARACTER( 16 ) :: PNAME = 'SubhFile_Cell'
      CHARACTER( 16 ) :: BNAME
      CHARACTER( 96 ) :: XMSG = ' '

      INTEGER       :: LOC_STRTCOL, LOC_ENDCOL, LOC_STRTROW, LOC_ENDROW

      REAL( 8 ), SAVE :: XORIG_B, YORIG_B
      REAL( 8 ), SAVE :: XCENT_B, YCENT_B
      REAL( 8 ), SAVE :: XCELL_B, YCELL_B
      REAL( 8 ), SAVE :: XORIG_C, YORIG_C  ! from GRIDDESC, in cell coord
      REAL( 8 ), SAVE :: XORIG_F, YORIG_F  ! from file, in cell coord

      INTEGER DOTFILE
      REAL( 8 ) :: RELOFFX, RELOFFY

C-----------------------------------------------------------------------

#ifndef mpas
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

C open cross file for subsequent comparison

!        BNAME = TRIM( GRID_CRO_2D )

!        IF ( .NOT. OPEN3( BNAME, FSREAD3, PNAME ) ) THEN
!           XMSG = 'Could not open '// BNAME
!           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT1 )
!           END IF

!        IF ( .NOT. DESC3( BNAME ) ) THEN
!           XMSG = 'Could not get ' // BNAME // ' file description'
!           CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
!           END IF

C set base header data

         XORIG_B = XORIG3D
!        YORIG_B = YORIG3D
!        XCENT_B = XCENT3D
!        YCENT_B = YCENT3D
!        XCELL_B = XCELL3D
!        YCELL_B = YCELL3D

C Scale resolution

!        XCELL_B = REAL( IDNINT( TENK * XCELL_B ), 8 ) / TENK
!        YCELL_B = REAL( IDNINT( TENK * YCELL_B ), 8 ) / TENK

C Convert to grid cell coord and truncate

!        XORIG_B = XORIG_B / XCELL_B
!        XORIG_B = REAL( IDNINT( ONEK * XORIG_B ), 8 ) / ONEK
!        YORIG_B = YORIG_B / YCELL_B
!        YORIG_B = REAL( IDNINT( ONEK * YORIG_B ), 8 ) / ONEK

         XORIG_C = XORIG_GD / XCELL_GD
         XORIG_C = REAL( IDNINT( ONEK * XORIG_C ), 8 ) / ONEK
         YORIG_C = YORIG_GD / YCELL_GD
         YORIG_C = REAL( IDNINT( ONEK * YORIG_C ), 8 ) / ONEK

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

!     IF ( ABS( XCENT3D - XCENT_B ) .GT.
!    &     TOL * ABS( XCENT3D + XCENT_B + TOL ) .OR.
!    &     ABS( YCENT3D - YCENT_B ) .GT.
!    &     TOL * ABS( YCENT3D + YCENT_B + TOL ) .OR.
!    &     ABS( XCELL3D - XCELL_B ) .GT. TOL * XCELL3D .OR.
!    &     ABS( YCELL3D - YCELL_B ) .GT. TOL * YCELL3D ) THEN
!        WRITE( LOGDEV,2003 ) XCENT_B, XCENT3D, YCENT_B, YCENT3D,
!    &                        XCELL_B, XCELL3D, YCELL_B, YCELL3D
!2003     FORMAT(/ 5X, 'XCENT_B:', F20.12, 2X, 'XCENT3D (file):', F20.12 
!    &          / 5X, 'YCENT_B:', F20.12, 2X, 'YCENT3D (file):', F20.12
!    &          / 5X, 'XCELL_B:', F20.12, 2X, 'XCELL3D (file):', F20.12 
!    &          / 5X, 'YCELL_B:', F20.12, 2X, 'YCELL3D (file):', F20.12 )

!        XMSG = 'File header inconsistent with GRID_CRO_2D'
!#ifndef twoway
!        CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
!#endif
!     END IF

C check some header data against GRIDDESC

      IF ( ABS( XCENT3D - XCENT_GD ) .GT.
     &     TOL * ABS( XCENT3D + XCENT_GD + TOL ) .OR.
     &     ABS( YCENT3D - YCENT_GD ) .GT.
     &     TOL * ABS( YCENT3D + YCENT_GD + TOL ) .OR.
     &     ABS( XCELL3D - XCELL_GD ) .GT. TOL * XCELL3D .OR.
     &     ABS( YCELL3D - YCELL_GD ) .GT. TOL * YCELL3D ) THEN
         WRITE( LOGDEV,2003 ) XCENT_GD, XCENT3D, YCENT_GD, YCENT3D,
     &                        XCELL_GD, XCELL3D, YCELL_GD, YCELL3D
 2003     FORMAT(/ 5X, 'XCENT_B:', F20.12, 2X, 'XCENT3D (file):', F20.12 
     &          / 5X, 'YCENT_B:', F20.12, 2X, 'YCENT3D (file):', F20.12
     &          / 5X, 'XCELL_B:', F20.12, 2X, 'XCELL3D (file):', F20.12 
     &          / 5X, 'YCELL_B:', F20.12, 2X, 'YCELL3D (file):', F20.12 )

         XMSG = 'File header inconsistent with GRIDDESC'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF

C Convert to grid cell coord and truncate

      XORIG_F = XORIG3D / XCELL3D
      XORIG_F = REAL( IDNINT( ONEK * XORIG_F ), 8 ) / ONEK
      YORIG_F = YORIG3D / YCELL3D
      YORIG_F = REAL( IDNINT( ONEK * YORIG_F ), 8 ) / ONEK
 
C Check if file (cross or dot) is "lined up"

      RELOFFX = THOU * REAL( IDNINT( ONEK * ( XORIG_C - XORIG_F ) ), 8 )
      RELOFFX = RELOFFX - ONE * REAL( IDNINT( RELOFFX ), 8 )

      IF ( ABS( RELOFFX ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFX ) .LT. HALF - MIN_DOUBLE .OR.
     &        ABS( RELOFFX ) .GT. HALF + MIN_DOUBLE ) THEN
            WRITE( LOGDEV,* ) '    RELOFFX: ', RELOFFX
            WRITE( LOGDEV,* ) '    XORIG_C, XORIG_F: ', XORIG_C, XORIG_F
            XMSG = 'Inconsistent Base/File Xorig'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            END IF
      END IF

      RELOFFY = THOU * REAL( IDNINT( ONEK * ( YORIG_C - YORIG_F ) ), 8 )
      RELOFFY = RELOFFY - ONE * REAL( IDNINT( RELOFFY ), 8 )

      IF ( ABS( RELOFFY ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFY ) .LT. HALF - MIN_DOUBLE .OR.
     &        ABS( RELOFFY ) .GT. HALF + MIN_DOUBLE ) THEN
            WRITE( LOGDEV,* ) '    RELOFFY: ', RELOFFY
            WRITE( LOGDEV,* ) '    YORIG_C, YORIG_F: ', YORIG_C, YORIG_F
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
      RELOFFX = THOU * REAL( IDNINT( ONEK * ( XORIG_C - XORIG_F ) ), 8 )
      RELOFFX = RELOFFX - ONE * REAL( IDNINT( RELOFFX ), 8 )

      IF ( ABS( RELOFFX ) .GT. MIN_DOUBLE ) THEN ! it better be a dot file
         IF ( ABS( RELOFFX ) .LT. HALF - MIN_DOUBLE .OR.
     &        ABS( RELOFFX ) .GT. HALF + MIN_DOUBLE ) THEN
            WRITE( LOGDEV,* ) '    RELOFFX: ', RELOFFX
            WRITE( LOGDEV,* ) '    XORIG_GD, XORIG_F: ', XORIG_C, XORIG_F
            XMSG = 'File Xorig inconsistent with GRIDDESC'
            CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
            END IF
      END IF

      RELOFFY = THOU * REAL( IDNINT( ONEK * ( YORIG_C - YORIG_F ) ), 8 )
      RELOFFY = RELOFFY - ONE * REAL( IDNINT( RELOFFY ), 8 )

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
      GXOFF = IDINT( XORIG_C - XORIG_F )
      LOC_STRTCOL = GXOFF + COLSX_PE( 1,INDX )
      LOC_ENDCOL  = LOC_STRTCOL + NCOLS - 1 + DOTFILE
      GYOFF = IDINT( YORIG_C - YORIG_F )
      LOC_STRTROW = GYOFF + ROWSX_PE( 1,INDX )
      LOC_ENDROW  = LOC_STRTROW + NROWS - 1 + DOTFILE
#ifndef twoway
      IF ( GXOFF .LT. 0 .OR. GYOFF .LT. 0 ) THEN
         XMSG = 'Model domain is outside file domain'
         CALL M3EXIT( PNAME, 0, 0, XMSG, XSTAT2 )
      END IF
#endif

#ifdef twoway
      IF ( ( FNAME .EQ. GRID_CRO_2D ) .OR.
     &     ( FNAME .EQ. GRID_DOT_2D ) ) THEN
         LOC_ENDCOL  = LOC_ENDCOL - LOC_STRTCOL + 1
         LOC_STRTCOL = 1
         LOC_ENDROW  = LOC_ENDROW - LOC_STRTROW + 1
         LOC_STRTROW = 1
      ELSE IF ( ( FNAME .EQ. MET_CRO_2D ) .OR.
     &          ( FNAME .EQ. MET_CRO_3D ) .OR.
     &          ( FNAME .EQ. MET_DOT_3D ) ) THEN
         LOC_ENDCOL  = LOC_ENDCOL - LOC_STRTCOL + 2
         LOC_STRTCOL = 2
         LOC_ENDROW  = LOC_ENDROW - LOC_STRTROW + 2
         LOC_STRTROW = 2
      END IF
#endif

      STRTCOL = LOC_STRTCOL
      ENDCOL  = LOC_ENDCOL
      STRTROW = LOC_STRTROW
      ENDROW  = LOC_ENDROW
#endif

      RETURN
      END SUBROUTINE SUBHFILE
