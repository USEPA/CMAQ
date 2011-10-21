
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
C $Header: /project/yoj/arc/CCTM/src/plrise/smoke/ungridb2.f,v 1.4 2011/10/21 16:11:31 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C.......................................................................
C Version "@(#)$Header: /project/yoj/arc/CCTM/src/plrise/smoke/ungridb2.f,v 1.4 2011/10/21 16:11:31 yoj Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.......................................................................

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        SUBROUTINE UNGRIDB2( GTYPE, NCOLS, NROWS, XORIG, YORIG, XCELL, YCELL,
     &                       NPTS, XLOC, YLOC, NU, CU, in )

C-----------------------------------------------------------------------
 
C  FUNCTION:
C       computes "ungridding" matrices to be used by BMATVEC() and BILIN(),
C      for program LAYPOINT, etc., to perform bilinear interpolation
C      from a grid to a set of locations { <XLOC(S),YLOC(S)>, S=1:NPTS }
 
C  SEE ALSO:
C       BILIN()   which performs combined interpolate-only,
C                 preserving the subscript-order.
C       BMATVEC() which performs combined interpolate-and-transpose,
C                 e.g., for SMOKE program LAYPOINT, changing LAYER
C                 from an outermost subscript to an innermost
 
C  PRECONDITIONS:  none
 
C  SUBROUTINES AND FUNCTIONS CALLED:  none
 
C  REVISION  HISTORY:
C      prototype 12/95 by CJC
C      6 aug 07: yoj - fix indexing for left or below grid
C      13 Apr 10: David Wong - parallel bug fix:
C                              1. translate C, R in the local domain and adjust
C                                 it to the expanded local domain
C                              2. use the translated values to compute index into
C                                 singly-indexed grid
C                              3. revert back to the original calculations for
C                                 C, R, X and Y
C      27 Jul 11: David Wong - used lower left corner instead of the cell center
C                              to determine cell location
C                            - removed extension to west and south direction
C                            - properly put double precision value to a single
C                              precision variable
 
C-----------------------------------------------------------------------

      USE HGRD_DEFN, ONLY: MYPE, GL_NCOLS, GL_NROWS,
     &                     COLSX_PE, ROWSX_PE, COLSD_PE, ROWSD_PE
      USE PTBILIN, ONLY: XBND, YBND

      IMPLICIT NONE

C Arguments:

      CHARACTER, INTENT( IN )  :: GTYPE         ! grid type, cross or dot
      INTEGER,   INTENT( IN )  :: NCOLS, NROWS  ! number of grid columns, rows
      REAL( 8 ), INTENT( IN )  :: XORIG, YORIG  ! X,Y coords of LL grid corner [m]
      REAL( 8 ), INTENT( IN )  :: XCELL, YCELL  ! X,Y direction cell size [m]
      INTEGER,   INTENT( IN )  :: NPTS          ! number of (point-source) locations
      REAL,      INTENT( IN )  :: XLOC( NPTS )  ! X point coordinates from xorig [m]
      REAL,      INTENT( IN )  :: YLOC( NPTS )  ! Y point coordinates from yorig [m]
      INTEGER,   INTENT( OUT ) :: NU( 4,NPTS )  ! single-indexed subscripts into grid
      REAL,      INTENT( OUT ) :: CU( 4,NPTS )  ! coefficients
!     INTEGER,   INTENT( OUT ) :: K             ! single-indexed subscript into grid
      integer,   intent( out ) :: in            ! count in grid

C Local Variables:

      INTEGER      S          ! source counter
      INTEGER      C, R       ! indices into doubly-indexed grid
      INTEGER      K          ! index   into singly-indexed grid
      REAL( 8 ) :: DDX, DDY   ! inverse cell size
      REAL( 8 ) :: XD0, YD0   ! center of LL cell
      REAL         X, Y       ! grid-normal coords of point
      REAL         P, Q       ! linear-interpolation coeffs
      INTEGER ::   LOC_C, LOC_R

C-----------------------------------------------------------------------

      DDX = 1.0D0 / XCELL           ! [1/m] DDX truncated to REAL( 4 )
      DDY = 1.0D0 / YCELL           ! [1/m] DDY truncated to REAL( 4 )
!     XD0 = XORIG + 0.5D0 * XCELL   ! [m]   XD0 truncated to REAL( 4 )
!     YD0 = YORIG + 0.5D0 * YCELL   ! [m]   YD0 truncated to REAL( 4 )
      XD0 = XORIG
      YD0 = YORIG

      in = 0

      DO 11 S = 1, NPTS
            
         !!  Hacks to fix this up to deal with the fact
         !!  that computer languages do the WRONG THING
         !!  for negative-number integer conversions and remainders:

         X = SNGL( DDX * ( XLOC( S ) - XD0 ) ) ! normalized grid coords
         IF ( X .GE. 0.0 ) THEN
            C = 1 + INT( X )                  ! truncated to integer
            X = MOD( X, 1.0 )                 ! trapped between 0 and 1
         ELSE
            C = -1 - INT( -X )                ! truncated to integer
            X = 1.0 - MOD( -X, 1.0 )          ! trapped between 0 and 1
         END IF

         Y = SNGL( DDY * ( YLOC( S ) - YD0 ) )  !  normalized grid coords
         IF ( Y .GE. 0.0 ) THEN
            R = 1 + INT( Y )                  ! truncated to integer
            Y = MOD( Y, 1.0 )                 ! trapped between 0 and 1
         ELSE
            R = -1 - INT( -Y )                ! truncated to integer
            Y = 1.0 - MOD( -Y, 1.0 )          ! trapped between 0 and 1
         END IF

         IF ( GTYPE .EQ. 'X' ) THEN
           LOC_C = C - COLSX_PE( 1,MYPE+1 ) + 1
           LOC_R = R - ROWSX_PE( 1,MYPE+1 ) + 1
         ELSE
           LOC_C = C - COLSD_PE( 1,MYPE+1 ) + 1
           LOC_R = R - ROWSD_PE( 1,MYPE+1 ) + 1
         END IF

!        IF ( .NOT. XBND( 2 ) ) THEN    ! west side
!          LOC_C = LOC_C + 1
!        END IF
!        IF ( .NOT. YBND( 1 ) ) THEN    ! south side
!          LOC_R = LOC_R + 1
!        END IF

         IF ( R .LT. 1 ) THEN                 ! r below grid

            IF ( C .LT. 1 ) THEN              ! c left of grid

               K = 1
               NU( 1,S ) = K
               NU( 2,S ) = K
               NU( 3,S ) = K
               NU( 4,S ) = K
               CU( 1,S ) = 1.0
               CU( 2,S ) = 0.0
               CU( 3,S ) = 0.0
               CU( 4,S ) = 0.0

            ELSE IF ( C .GT. GL_NCOLS - 1 ) THEN ! c right of grid

               K = NCOLS
               NU( 1,S ) = K
               NU( 2,S ) = K
               NU( 3,S ) = K
               NU( 4,S ) = K
               CU( 1,S ) = 1.0
               CU( 2,S ) = 0.0
               CU( 3,S ) = 0.0
               CU( 4,S ) = 0.0

            ELSE                                 ! c in the grid

               K = C
               NU( 1,S ) = K
               NU( 2,S ) = K + 1
               NU( 3,S ) = K
               NU( 4,S ) = K
               CU( 1,S ) = 1.0 - X
               CU( 2,S ) = X
               CU( 3,S ) = 0.0 
               CU( 4,S ) = 0.0

            END IF

         ELSE IF ( R .GT. GL_NROWS - 1 ) THEN    ! r above grid

            IF ( C .LT. 1 ) THEN                 ! c left of grid

               K = ( NROWS - 1 ) * NCOLS + 1
               NU( 1,S ) = K
               NU( 2,S ) = K
               NU( 3,S ) = K
               NU( 4,S ) = K
               CU( 1,S ) = 1.0
               CU( 2,S ) = 0.0
               CU( 3,S ) = 0.0
               CU( 4,S ) = 0.0

            ELSE IF ( C .GT. GL_NCOLS - 1 ) THEN ! c right of grid

               K = NROWS * NCOLS
               NU( 1,S ) = K
               NU( 2,S ) = K
               NU( 3,S ) = K
               NU( 4,S ) = K
               CU( 1,S ) = 1.0
               CU( 2,S ) = 0.0
               CU( 3,S ) = 0.0
               CU( 4,S ) = 0.0

            ELSE                                 ! c in the grid

               K = ( NROWS - 1 ) * NCOLS + LOC_C
               NU( 1,S ) = K
               NU( 2,S ) = K + 1
               NU( 3,S ) = K
               NU( 4,S ) = K
               CU( 1,S ) = 1.0 - X
               CU( 2,S ) = X
               CU( 3,S ) = 0.0
               CU( 4,S ) = 0.0

            END IF

         ELSE                                    ! r in the grid

            IF ( C .LT. 1 ) THEN                 ! c left of grid

               K = ( LOC_R - 1 ) * NCOLS + 1
               NU( 1,S ) = K
               NU( 2,S ) = K
               NU( 3,S ) = K + NCOLS
               NU( 4,S ) = K + NCOLS
               CU( 1,S ) = 1.0 - Y
               CU( 2,S ) = 0.0
               CU( 3,S ) = Y
               CU( 4,S ) = 0.0

            ELSE IF ( C .GT. GL_NCOLS - 1 ) THEN ! c right of grid

               K = LOC_R * NCOLS
               NU( 1,S ) = K
               NU( 2,S ) = K
               NU( 3,S ) = K + NCOLS
               NU( 4,S ) = K + NCOLS
               CU( 1,S ) = 1.0 - Y
               CU( 2,S ) = 0.0
               CU( 3,S ) = Y
               CU( 4,S ) = 0.0

            ELSE                                 ! c in the grid

               K = ( LOC_R - 1 ) * NCOLS + LOC_C
               NU( 1,S ) = K
               NU( 2,S ) = K + 1
               NU( 3,S ) = K + NCOLS
               NU( 4,S ) = K + NCOLS + 1
               P = 1.0 - X
               Q = 1.0 - Y
               CU( 1,S ) = P * Q
               CU( 2,S ) = X * Q
               CU( 3,S ) = P * Y
               CU( 4,S ) = X * Y

               in = in + 1

            END IF

         END IF     !  end computing bilinear interpolation matrix

11    CONTINUE   !  end matrix computation loop on point sources

      RETURN
      END

