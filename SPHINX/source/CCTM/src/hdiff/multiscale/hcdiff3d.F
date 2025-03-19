
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
      SUBROUTINE HCDIFF3D ( JDATE, JTIME, K11BAR, K22BAR, DT )
      
C-----------------------------------------------------------------------
C Function:
C   Computes the contravariant diffusivities in x1 or x2 directions
C   using a constant physical horizontal diffusivity.
      
C Preconditions:
C   This routine can only be used for conformal map coordinates 
C   in the horizontal.
C   Dates and times should be represented YYYYDDD:HHMMSS.
 
C Subroutines and functions called:
C   INTERP3, M3EXIT, DEFORM 
 
C Revision history:
C   October 17, 1995 by M. Talat Odman and Clint L. Ingram at NCSC:
C   created for SAQM-type coordinates
      
C    5 Nov 97 Jeff targetted

C    Sep. 1998 David Wong
C      -- parallelize the code
C      -- use GLOBAL_MAX to compute the global max

C    1/19/99 David Wong
C      -- add a loop_index call
C      -- change loop index ending point to avoid accessing invalid region.
C         (reason to do this is to prevent using boundary data from PINTERP,
C          which sets pseudo-boundary data to 0)

C    Jul. 8 1999 David Wong
C      -- replace GLOBAL_MAX with GLOBAL_RMAX for naming consistency
C
C    10/10/2000 Daewon Byun
C      -- generalized 3d horizontal diffusivity

C    23 Dec 00 J.Young: GLOBAL_RMAX -> Dave Wong's f90 stenex GLOBAL_MAX
C                       PE_COMM3 -> Dave Wong's f90 stenex COMM

C    6 Aug 01 J.Young: Use HGRD_DEFN; replace INTERP3 with INTERPX;
C                      allocatable arrays
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S. Roselle: replaced I/O-API include files w/UTILIO_DEFN
C   03 Aug 11 David Wong: moved DT calculation outside the loop for efficency
C                         purposes
C   01 Feb 19 David Wong: Implemented centralized I/O approach, removed all MY_N
C                         clauses
C-----------------------------------------------------------------------
      
      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE UTILIO_DEFN
      USE CENTRALIZED_IO_MODULE
#ifdef parallel
      USE SE_MODULES            ! stenex (using SE_GLOBAL_MAX_MODULE, SE_COMM_MODULE,
                                !               SE_UTIL_MODULE)
#else
      USE NOOP_MODULES          ! stenex (using NOOP_GLOBAL_MAX_MODULE, NOOP_COMM_MODULE,
                                !               NOOP_UTIL_MODULE)
#endif

      IMPLICIT NONE
      
C Includes:
      
      INCLUDE SUBST_CONST       ! constants
      INCLUDE SUBST_FILES_ID    ! file name parameters
      INCLUDE SUBST_PE_COMM     ! PE communication displacement and direction

C Arguments:
      
      INTEGER, INTENT( IN )  :: JDATE  ! current model date, coded YYYYDDD
      INTEGER, INTENT( IN )  :: JTIME  ! current model time, coded HHMMSS
                                       ! Contravariant diffusivity
!     REAL         K11BAR3D( NCOLS+1,NROWS+1,NLAYS ) ! x1-flux points
!     REAL         K22BAR3D( NCOLS+1,NROWS+1,NLAYS ) ! x2-flux points
      REAL,    INTENT( OUT ) :: K11BAR( :,:,: ) ! x1-flux points
      REAL,    INTENT( OUT ) :: K22BAR( :,:,: ) ! x2-flux points
      REAL,    INTENT( OUT ) :: DT              ! diffusivity time step
 
C Parameters:
 
C Horizontal eddy diffusivity (m^2/s) 
!     REAL, PARAMETER :: KH = 3.3E+04 ! From Brost et al., J.Geophys.Res., 1988
!     REAL, PARAMETER :: KH = 50.0    ! For 12 km SARMAP simulation as per SAQM
      REAL, PARAMETER :: KH = 2000.0  ! For  4 km SARMAP simulation as per SAQM

      REAL, PARAMETER :: KHMIN = 200.0 ! For min KH assigned for deformation
      REAL, PARAMETER :: DXB = 4000.0
      REAL, PARAMETER :: ALP = 0.28

C "Courant" factor = 99%(1/sqrt(2))
!     REAL, PARAMETER :: CFC = 0.700
      REAL, PARAMETER :: CFC = 0.300
      
C local variables:
      
      CHARACTER( 16 ) :: PNAME = 'HCDIFF3D'
      CHARACTER( 96 ) :: XMSG = ' '

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER, SAVE :: MLAYS
 
      REAL, SAVE :: DX1, DX2            ! CX x1- and x2-cell widths
      REAL, SAVE :: KHA                 ! resolution-adjusted base diffusivity
      REAL, SAVE :: ACOEF               ! ALP**2 * DX1 * DX2
      REAL         KHD                 ! Deformation induced KH
      REAL         DEFORM3D( NCOLS+1,NROWS+1,NLAYS ) ! wind deformation

      REAL         EDDYH3D ( NCOLS+1,NROWS+1,NLAYS ) ! Contra. diffusivity

      REAL         EFFKB               ! Effective Kbar
!     REAL         EKHMAX              ! max Contra. diffusivity (diagnos)
!     REAL         MS2MAX              ! max squared map scale factor (diagnos)
 
      INTEGER      ALLOCSTAT
      INTEGER      COL, ROW, LVL       ! column,row,level indices

      INTEGER MY_TEMP
      INTEGER, SAVE :: STARTROW, ENDROW
      INTEGER, SAVE :: STARTCOL, ENDCOL
 
      INTERFACE
         SUBROUTINE DEFORM( JDATE, JTIME, DEFORM3D )
            INTEGER, INTENT( IN )  :: JDATE, JTIME
            REAL,    INTENT( OUT ) :: DEFORM3D( :,:,: )
         END SUBROUTINE DEFORM
      END INTERFACE

C-----------------------------------------------------------------------
      
      IF ( FIRSTIME ) THEN
         FIRSTIME = .FALSE.

         MLAYS = SIZE ( K11BAR,3 )
 
         CALL SUBST_LOOP_INDEX ( 'R', 1, NROWS, 1, MY_TEMP, STARTROW, ENDROW )

         CALL SUBST_LOOP_INDEX ( 'C', 1, NCOLS, 1, MY_TEMP, STARTCOL, ENDCOL )

         IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
            DX1 = DG2M * XCELL_GD ! in m.
            DX2 = DG2M * YCELL_GD
     &        * COS( PI180*( YORIG_GD + YCELL_GD * FLOAT( GL_NROWS/2 ))) ! in m.
            ELSE
            DX1 = XCELL_GD        ! in m.
            DX2 = YCELL_GD        ! in m.
            END IF

C Get map scale factor

         KHA = ( DXB * DXB ) / ( DX1 * DX2 ) * KH 

         ACOEF = ALP * ALP * ( DX1 * DX2 )

         END IF                    ! if firstime
 
C get wind deformation 
 
      CALL DEFORM ( JDATE, JTIME, DEFORM3D )

      EDDYH3D = 0.0

      DO LVL = 1, MLAYS
!        EKHMAX = 0.0
         DO ROW = STARTROW, ENDROW      !   DO ROW = 1, NROWS+1
            DO COL = STARTCOL, ENDCOL   !       DO COL = 1, NCOLS+1
!              EDDYH3D( COL,ROW,LVL ) = MSFD2( COL,ROW ) *  
!    &               ( ACOEF * KHA * DEFORM3D( COL,ROW,LVL ) 
!    &              / ( KHA + ACOEF * DEFORM3D( COL,ROW,LVL ) ) 
!    &              + KHMIN )
! Daewon prefers the following
               KHD = MAX( KHMIN, ACOEF * DEFORM3D( COL,ROW,LVL ) )
               EDDYH3D( COL,ROW,LVL ) = MSFD2( COL,ROW )
     &                                * KHA * KHD / ( KHA + KHD )
!              EKHMAX = MAX( EKHMAX, EDDYH3D( COL,ROW,LVL ) )
            END DO
         END DO
      END DO

      CALL SUBST_COMM ( EDDYH3D, DSPL_N1_E1_S0_W0, DRCN_N_E )

C Obtain flux average values of contravariant diffusivities

      EFFKB = 0.0
      DO LVL = 1, MLAYS
         DO ROW = 1, NROWS + 1
            DO COL = 1, NCOLS + 1
               K11BAR( COL,ROW,LVL ) = 0.0
               K22BAR( COL,ROW,LVL ) = 0.0
               END DO
             END DO
          END DO

1003  FORMAT( / '@2@Layer', 5X, 'Time Step', 9X, 'EffKB' )

      DO LVL = 1, MLAYS
         DO ROW = 1, NROWS
            DO COL = STARTCOL, ENDCOL
               K11BAR( COL,ROW,LVL ) = 0.5 * ( EDDYH3D( COL,ROW+1,LVL )
     &                               +         EDDYH3D( COL,ROW,LVL ) )
               END DO
            END DO
         DO COL = STARTCOL, ENDCOL
            K11BAR( COL,NROWS+1,LVL ) = 0.0
            END DO

         DO ROW = STARTROW, ENDROW
            DO COL = 1, NCOLS
               K22BAR( COL,ROW,LVL ) = 0.5 * ( EDDYH3D( COL,ROW,LVL )
     &                               +         EDDYH3D( COL+1,ROW,LVL ) )
              END DO
            END DO
         DO ROW = STARTROW, ENDROW
            K22BAR( NCOLS+1,ROW,LVL ) = 0.0
            END DO

         DO ROW = 1, NROWS
            DO COL = 1, NCOLS
               EFFKB =  MAX ( EFFKB, 
     &                        K11BAR( COL,ROW,LVL ),
     &                        K22BAR( COL,ROW,LVL ) )
               END DO
            END DO

!        DT = CFC * DX1 * DX2 / SUBST_GLOBAL_MAX ( EFFKB )

1005     FORMAT( '@2@ ', I3, 1X, F18.7, 1X, F12.7 )

         END DO ! for LVL

      DT = CFC * DX1 * DX2 / SUBST_GLOBAL_MAX ( EFFKB )

      RETURN
      END
