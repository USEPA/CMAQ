!------------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in           !
!  continuous development by various groups and is based on information        !
!  from these groups: Federal Government employees, contractors working        !
!  within a United States Government contract, and non-Federal sources         !
!  including research institutions.  These groups give the Government          !
!  permission to use, prepare derivative works of, and distribute copies       !
!  of their work in the CMAQ system to the public and to permit others         !
!  to do so.  The United States Environmental Protection Agency                !
!  therefore grants similar permission to use the CMAQ system software,        !
!  but users are requested to provide copies of derivative works or            !
!  products designed to operate in the CMAQ system to the United States        !
!  Government without restrictions as to use by others.  Software              !
!  that is used with the CMAQ system but distributed under the GNU             !
!  General Public License or the GNU Lesser General Public License is          !
!  subject to their copyright restrictions.                                    !
!------------------------------------------------------------------------------!

SUBROUTINE vertnhy

!-------------------------------------------------------------------------------
! Name:     Vertical Velocities -- Non-Hydrostatic
! Purpose:  Generates a complete set of meteorological data for transport
!           processors of Models-3 CTM.
!           But, for X3 coordinate
!
!             WHAT = 0 at X3 = 0 (i.e., at the surface)
!             WHAT = 0 at X3 = 1 (i.e., at the top of the atmosphere)
!
!           With X3 coordinate, WHAT (although we are using XWHAT0 array here)
!           is positive upward.
!
! Revised:  07 Nov 1997  Original version.  (D. Byun)
!           27 Dec 1997  Updated following Talat and Ingram's vconvel.F.  
!                        (D. Byun)
!           04 Feb 1998  Changed include method nonglobal includes.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  Removed ISTAT since
!                        it's not used.  Changed arrays to allocatable.
!                        Changed array bounds from MAXK to METLAY.  (T. Otte)
!           18 Mar 2003  Corrected calculation of weights used for vertical
!                        interpolation to full levels.  Corrected algorithm
!                        to calculate XWHAT from XUHAT and XVHAT.  (T. Otte)
!           04 Feb 2005  Adjusted calculation of XWHAT to reflect that XUU_S
!                        and XVV_T contain wind components on flux points,
!                        rather than dot points, and are not coupled with
!                        map-scale factors.  Since this routine is used with
!                        MM5 which has wind components on dot points, use
!                        XUU_D and XVV_D now instead of XUU_S and XVV_T.
!                        (T. Otte)
!           20 Jun 2006  Updated calculations with double precision scalars
!                        XCELL_GD and YCELL_GD.  Removed unused variables
!                        ITER, MAP1, and MAP2.  (T. Otte)
!           30 Jul 2007  Corrected error in calculation of contravariant
!                        vertical velocity which used the total surface
!                        pressure from the first time period processed by MCIP
!                        rather than the beginning of the MM5 simulation (i.e.,
!                        the reference surface pressure).  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE const
  USE coord

  IMPLICIT NONE

  INTEGER                      :: cm1
  INTEGER                      :: col
  INTEGER                      :: cp1
  REAL,          SAVE          :: ddx2
  REAL,          SAVE          :: ddy2
  REAL,          SAVE          :: dx
  REAL,          SAVE          :: dy
  LOGICAL,       SAVE          :: firstime   = .TRUE.
  INTEGER                      :: lbnd
  INTEGER                      :: lp1
  INTEGER                      :: lvl
  REAL,  SAVE,   ALLOCATABLE   :: pstari     ( : , : )
  INTEGER                      :: rm1
  INTEGER                      :: row
  INTEGER                      :: rp1
  REAL,  SAVE,   ALLOCATABLE   :: wght_bot   ( : )
  REAL,  SAVE,   ALLOCATABLE   :: wght_top   ( : )
  REAL,  SAVE,   ALLOCATABLE   :: xfac       ( : , : )
  REAL,  SAVE,   ALLOCATABLE   :: yfac       ( : , : )
  REAL,  SAVE,   ALLOCATABLE   :: zfac       ( : , : , : )

!-------------------------------------------------------------------------------
! On first call to subroutine, calculate reference factors and arrays.
! Note that P-star is constant in time in the non-hydrostatic application.
!-------------------------------------------------------------------------------

  IF ( firstime ) THEN

    ALLOCATE ( pstari   (ncols_x, nrows_x)         )
    ALLOCATE ( wght_bot                   (metlay) )
    ALLOCATE ( wght_top                   (metlay) )
    ALLOCATE ( xfac     (ncols_x, nrows_x)         )
    ALLOCATE ( yfac     (ncols_x, nrows_x)         )
    ALLOCATE ( zfac     (ncols_x, nrows_x, metlay) )

    dx   = REAL(xcell_gd)
    dy   = REAL(ycell_gd)

    ddx2 = 0.5 / dx
    ddy2 = 0.5 / dy
                           
    DO lvl = 1, metlay-1
      wght_top(lvl) = ( xx3face(lvl)   - xx3midl(lvl) ) /  &
                      ( xx3midl(lvl+1) - xx3midl(lvl) )
      wght_bot(lvl) = 1.0 - wght_top(lvl)
    ENDDO

    wght_bot(metlay) = 1.0
    wght_top(metlay) = 0.0

    DO row = 1, nrows_x 
      DO col = 1, ncols_x
        pstari(col,row) = 1.0 / xpstar0(col,row)
      ENDDO
    ENDDO

    DO row = 1, nrows_x
      rp1 = MIN(row+1,nrows_x)
      rm1 = MAX(row-1,1)        
      DO col = 1, ncols_x
        cp1 = MIN(col+1,ncols_x)
        cm1 = MAX(col-1,1)

        ! First time XPRSFC must include reference surface pressure at given
        ! topography for MM5 nonhydrostatic application, i.e., sigma-po coord.            
        xfac(col,row) = ddx2 * ( xpstar0(cp1,row) - xpstar0(cm1,row) ) *  &
                        pstari(col,row) 

        yfac(col,row) = ddy2 * ( xpstar0(col,rp1) - xpstar0(col,rm1) ) *  &
                        pstari(col,row) 

        DO lvl = 1, metlay
          zfac(col,row,lvl) = grav * pstari(col,row) * xdensaf_ref(col,row,lvl)
        ENDDO

      ENDDO
    ENDDO

    firstime = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Compute vertical velocity on full levels and scalar points.  Here, the
! horizontal wind components are on dot points and mid-layers, so interpolation
! is performed to obtain approximations of horizontal wind components on full
! levels and at scalar points.  The calculation of XWHAT follows from equation
! 12-124 in "Science Algorithms of the EPA Models-3/CMAQ Modeling System" 
! (EPA/600/R-99/030).
!-------------------------------------------------------------------------------

  lbnd = LBOUND(xwhat,3)
  xwhat(:,:,lbnd+metlay) = 0.0   ! velocity zero, at top
  xwhat(:,:,lbnd)        = 0.0   ! velocity zero, at bottom

  DO lvl = 1, metlay-1
    lp1 = lvl + 1
    DO ROW = 1, nrows_x
      rp1 = row + 1
      DO col = 1, ncols_x
        cp1 = col + 1

        xwhat(col,row,lvl) = 0.25 * ( 1.0 - xx3face(lvl) ) * xmapc(col,row) *  &
                             ( xfac(col,row) *                                 &
                             ( wght_bot(lvl)                                   &
                             * ( xuu_d(col,row,lvl) + xuu_d(cp1,row,lvl) +     &
                                 xuu_d(col,rp1,lvl) + xuu_d(cp1,rp1,lvl) )     &
                             + wght_top(lvl)                                   &
                             * ( xuu_d(col,row,lp1) + xuu_d(cp1,row,lp1) +     &
                                 xuu_d(col,rp1,lp1) + xuu_d(cp1,rp1,lp1) ) )   &
                             + yfac(col,row) *                                 &
                             ( wght_bot(lvl)                                   &
                             * ( xvv_d(col,row,lvl) + xvv_d(cp1,row,lvl) +     &
                                 xvv_d(col,rp1,lvl) + xvv_d(cp1,rp1,lvl) )     &
                             + wght_top(lvl)                                   &
                             * ( xvv_d(col,row,lp1) + xvv_d(cp1,row,lp1) +     &
                                 xvv_d(col,rp1,lp1) + xvv_d(cp1,rp1,lp1) ) ) ) &
                             + zfac(col,row,lvl) * xwwind(col,row,lvl)

      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE vertnhy
