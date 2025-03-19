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

SUBROUTINE vertnhy_wrf

!-------------------------------------------------------------------------------
! Name:     Vertical Velocities -- Non-Hydrostatic for WRF EM
! Purpose:  Computes contravariant vertical velocity for WRF EM data where
!           WRF is run with non-hydrostatic formulations.
! Notes:    Based on equation 12-123 in "Science Algorithms of the EPA Models-3/
!           CMAQ Modeling System" (EPA/600/R-99/030).
! Revised:  22 Mar 2005  Original version.  (T. Otte)
!           20 Jun 2006  Updated calculations with double precision scalars
!                        XCELL_GD and YCELL_GD.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE coord

  IMPLICIT NONE

  INTEGER                      :: cm1
  INTEGER                      :: col
  INTEGER                      :: cp1
  REAL,          SAVE          :: ddx2
  REAL,          SAVE          :: ddy2
  REAL                         :: dphidx
  REAL                         :: dphidy
  REAL                         :: dx
  REAL                         :: dy
  LOGICAL,       SAVE          :: firsttime  = .TRUE.
  REAL,          PARAMETER     :: giwrf      = 1.0 / 9.81
  REAL                         :: ji
  INTEGER                      :: lp1
  INTEGER                      :: lvl
  REAL                         :: mogn
  INTEGER                      :: rm1
  INTEGER                      :: row
  INTEGER                      :: rp1
  REAL                         :: ufcrs
  REAL                         :: vfcrs
  REAL,  SAVE,   ALLOCATABLE   :: wght_bot   ( : )
  REAL,  SAVE,   ALLOCATABLE   :: wght_top   ( : )

!-------------------------------------------------------------------------------
! On first call to subroutine, calculate vertical weights for interpolation.
!-------------------------------------------------------------------------------

  IF ( firsttime ) THEN

    ALLOCATE ( wght_bot (metlay) )
    ALLOCATE ( wght_top (metlay) )

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

    firsttime = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Compute vertical velocity on full levels and scalar points.  Here, the
! horizontal wind components are on face points and mid-layers, so interpolation
! is performed to obtain approximations of horizontal wind components on full
! levels and at scalar points.  The calculation of XWHAT follows from equation
! 12-123 in "Science Algorithms of the EPA Models-3/CMAQ Modeling System" 
! (EPA/600/R-99/030).  (XWHAT = XIdot)
!    XIdot = d(XI)/dt + (-m V dot del(H)  + w) d(XI)/dz
!          =     0    + (-m V dot del(H)  + w) * (1/Jacobian)
!          =          - m u/J dH/dx  - m v/J dH/dy  + w/J
!          =          - m u/Jg d(phi)/dx  - m v/Jg d(phi)/dy + w/J
!-------------------------------------------------------------------------------

  xwhat(:,:,metlay) = 0.0   ! velocity zero, at top
  xwhat(:,:,0)      = 0.0   ! velocity zero, at bottom

  DO row = 1, nrows_x
    rp1 = MIN(row+1,nrows_x)
    rm1 = MAX(row-1,1)

    DO col = 1, ncols_x
      cp1 = MIN(col+1,ncols_x)
      cm1 = MAX(col-1,1)

      mogn = - xmapc(col,row) * giwrf

      DO lvl = 1, metlay-1
        lp1 = lvl + 1

        ji = 1.0 / x3jacobf(col,row,lvl)

        ufcrs = 0.5 *  &
                ((xuu_s(col,row,lvl) + xuu_s(cp1,row,lvl)) * wght_bot(lvl) +  &
                 (xuu_s(col,row,lp1) + xuu_s(cp1,row,lp1)) * wght_top(lvl))

        vfcrs = 0.5 *  &
                ((xvv_t(col,row,lvl) + xvv_t(col,rp1,lvl)) * wght_bot(lvl) +  &
                 (xvv_t(col,row,lp1) + xvv_t(col,rp1,lp1)) * wght_top(lvl))

        dphidx = (xgeof(cp1,row,lvl) - xgeof(cm1,row,lvl)) * ddx2

        dphidy = (xgeof(col,rp1,lvl) - xgeof(col,rm1,lvl)) * ddy2

        xwhat(col,row,lvl) = ji * ( (mogn * ufcrs * dphidx) +  &
                                    (mogn * vfcrs * dphidy) +  &
                                    xwwind(col,row,lvl) )

      ENDDO
    ENDDO
  ENDDO

END SUBROUTINE vertnhy_wrf
