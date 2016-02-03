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

SUBROUTINE pvs

!-------------------------------------------------------------------------------
! Name:     Potential Vorticity on Sigma
! Purpose:  Compute potential vorticity on sigma surfaces from Ertel's form.
! Notes:    Formalism based on Ebel et al., "Simulation of ozone intrusion
!           caused by tropopause fold and cut-off low, Atmos. Environ.,
!           Part A, 25, 2131-2144.
! Revised:  ?? ??? 1999  Original version.  (S. McKeen)
!           ?? ??? 2007  Adapted for use in air quality forecasting model.
!                        (H.-M. Lin and R. Mathur)
!           17 Sep 2009  Adapted for MCIP by changing array indexing and using
!                        arrays available in MCIP.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  Changed SCALE to SCALEF to avoid
!                        conflict with F90 intrinsic.  (T. Otte)
!-------------------------------------------------------------------------------

  USE coord
  USE mcipparm
  USE xvars

  IMPLICIT NONE

  INTEGER                     :: c
  INTEGER                     :: cp1
  REAL                        :: dsx
  REAL                        :: dsy
  REAL,    SAVE, ALLOCATABLE  :: dtds       ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: dtdx       ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: dtdy       ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: duds       ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: dvds       ( : , : )
  REAL                        :: dx
  REAL                        :: dy
  REAL                        :: f0
  REAL                        :: f1
  REAL                        :: f2
  INTEGER                     :: k
  INTEGER                     :: k0
  INTEGER                     :: k1
  INTEGER                     :: k2
  INTEGER                     :: r
  INTEGER                     :: rp1
  REAL,          PARAMETER    :: scalef     = -1.0e6
  REAL,    SAVE, ALLOCATABLE  :: sigma      ( : )
  REAL                        :: t00
  REAL                        :: t1
  REAL                        :: t2
  REAL                        :: t3
  REAL                        :: vor

!-------------------------------------------------------------------------------
! Define variables for interaction.
!-------------------------------------------------------------------------------

  dx  = REAL(xcell_gd)
  dy  = REAL(ycell_gd)

  dsx = 2.0 * dx
  dsy = 2.0 * dy

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( duds  ) ) ALLOCATE ( duds ( ncols_x, nrows_x ) )
  IF ( .NOT. ALLOCATED ( dvds  ) ) ALLOCATE ( dvds ( ncols_x, nrows_x ) )
  IF ( .NOT. ALLOCATED ( dtdx  ) ) ALLOCATE ( dtdx ( ncols_x, nrows_x ) )
  IF ( .NOT. ALLOCATED ( dtdy  ) ) ALLOCATE ( dtdy ( ncols_x, nrows_x ) )
  IF ( .NOT. ALLOCATED ( dtds  ) ) ALLOCATE ( dtds ( ncols_x, nrows_x ) )

  IF ( .NOT. ALLOCATED ( sigma ) ) ALLOCATE ( sigma ( metlay ) )

!-------------------------------------------------------------------------------
! Transfer monotonically increased X3 to monotonically decreased SIGMA.
! This form allows subroutine to remain general for hybrid vertical coordinates.
!-------------------------------------------------------------------------------

  DO k = 1, metlay
    sigma(k) = 1.0 - xx3midl(k)
  ENDDO

!-------------------------------------------------------------------------------
! Compute vertical gradients using 2nd order polynomials at all levels.
! Gradients obtained at model sigma levels, 
!   not at sigma=.5*(sigma(K+1)+sigma(K-1))
!-------------------------------------------------------------------------------

  kloop: DO k = 1, metlay

    IF ( k == 1 ) THEN

      k0 = k
      k1 = k + 1
      k2 = k + 2

      f0 = -1.0 / (sigma(k1) - sigma(k0)) - 1.0 / (sigma(k2) - sigma(k0))
      f1 =  1.0 / (sigma(k1) - sigma(k0)) + 1.0 / (sigma(k2) - sigma(k1))
      f2 = -1.0 * ( (sigma(k1) - sigma(k0)) /   &
                  ( (sigma(k2) - sigma(k0)) * (sigma(k2) - sigma(k1)) ) )

    ELSE IF ( k == metlay ) THEN

      k0 = k - 2
      k1 = k - 1
      k2 = k

      f0 =        (sigma(k2) - sigma(k1)) /  &
                ( (sigma(k2) - sigma(k0)) * (sigma(k1) - sigma(k0)) )
      f1 = -1.0 / (sigma(k1) - sigma(k0)) - 1.0 / (sigma(k2) - sigma(k1))
      f2 =  1.0 / (sigma(k2) - sigma(k0)) + 1.0 / (sigma(k2) - sigma(k1))

    ELSE

      k0 = k - 1
      k1 = k
      k2 = k + 1

      f0 = -1.0 * (sigma(k2) - sigma(k1)) /  &
                ( (sigma(k1) - sigma(k0)) * (sigma(k2) - sigma(k0)) )
      f1 =  1.0 / (sigma(k1) - sigma(k0)) - 1.0 / (sigma(k2) - sigma(k1))
      f2 =        (sigma(k1) - sigma(k0)) /  &
                ( (sigma(k2) - sigma(k1)) * (sigma(k2) - sigma(k0)) )

    ENDIF

!-------------------------------------------------------------------------------
! Compute vertical derivatives: dU/ds, dV/ds, dTHETA/ds.
!-------------------------------------------------------------------------------

    DO r = 1, nrows_x
      rp1 = r + 1

      DO c = 1, ncols_x
        cp1 = c + 1

        duds(c,r) = 0.5 * ( f0 * ( xuu_s(cp1,r  ,k0) + xuu_s(c,r,k0) ) +  &
                            f1 * ( xuu_s(cp1,r  ,k1) + xuu_s(c,r,k1) ) +  &
                            f2 * ( xuu_s(cp1,r  ,k2) + xuu_s(c,r,k2) ) )

        dvds(c,r) = 0.5 * ( f0 * ( xvv_t(c  ,rp1,k0) + xvv_t(c,r,k0) ) +  &
                            f1 * ( xvv_t(c  ,rp1,k1) + xvv_t(c,r,k1) ) +  &
                            f2 * ( xvv_t(c  ,rp1,k2) + xvv_t(c,r,k2) ) )

      ENDDO
    ENDDO

    DO r = 1, nrows_x
      DO c = 1, ncols_x

        t00 = xtheta(c,r,k0)
        t1  = xtheta(c,r,k1)
        t2  = xtheta(c,r,k2)

        dtds(c,r) = f0*t00 + f1*t1 + f2*t2

      ENDDO
    ENDDO

!-------------------------------------------------------------------------------
! Compute horizontal derivatives: dTHETA/dx, dTHETA/dy.
!
! 6/8/99, Horizontal gradients also calculated by 2nd order polynomials
! at all levels. In particular, this modifies orig code so PV at side
! boundaries are not biased low.
!-------------------------------------------------------------------------------

    DO r = 1, nrows_x

      DO c = 2, ncols_x-1
        t1        = xtheta(c-1,r,k) / xmapc(c-1,r)
        t2        = xtheta(c+1,r,k) / xmapc(c+1,r)
        dtdx(c,r) = xmapc2(c,r) * (t2-t1) / dsx
      ENDDO

      t1        = xtheta(1,r,k) / xmapc(1,r)
      t2        = xtheta(2,r,k) / xmapc(2,r)
      t3        = xtheta(3,r,k) / xmapc(3,r)
      dtdx(1,r) = xmapc2(1,r) * (-1.5*t1 + 2.0*t2 - 0.5*t3) / dx

      t00             = xtheta(ncols_x-2,r,k) / xmapc(ncols_x-2,r)
      t1              = xtheta(ncols_x-1,r,k) / xmapc(ncols_x-1,r)
      t2              = xtheta(ncols_x,  r,k) / xmapc(ncols_x,  r)
      dtdx(ncols_x,r) = xmapc2(ncols_x,r) * (0.5*t00 - 2.0*t1 + 1.5*t2) / dx

    ENDDO

    DO c = 1, ncols_x

      DO r = 2, nrows_x-1
        t1        = xtheta(c,r-1,k) / xmapc(c,r-1)
        t2        = xtheta(c,r+1,k) / xmapc(c,r+1)
        dtdy(c,r) = xmapc2(c,r) * (t2-t1) / dsy
      ENDDO

      t1        = xtheta(c,1,k) / xmapc(c,1)
      t2        = xtheta(c,2,k) / xmapc(c,2)
      t3        = xtheta(c,3,k) / xmapc(c,3)
      dtdy(c,1) = xmapc2(c,1) * (-1.5*t1 + 2.0*t2 - 0.5*t3) / dy

      t00             = xtheta(c,nrows_x-2,k) / xmapc(c,nrows_x-2)
      t1              = xtheta(c,nrows_x-1,k) / xmapc(c,nrows_x-1)
      t2              = xtheta(c,nrows_x,  k) / xmapc(c,nrows_x)
      dtdy(c,nrows_x) = xmapc2(c,nrows_x) * (0.5*t00 - 2.0*t1 + 1.5*t2) / dy

    ENDDO

!-------------------------------------------------------------------------------
! Compute slab absolute vorticity, and store potential vorticity in XPVC.
!
!     1. Because we use X3 instead of SIGMA in equation,
!        GRAV/PSB is replaced by 1.0/XRHOJM (density * Jacobian).
!
!     2. As a shortcut, 1.0/XRHOJM is not included in XPVC here;
!        it will be included in subroutine METCRO before PV is output.
!-------------------------------------------------------------------------------

    DO r = 1, nrows_x
      rp1 = r + 1

      DO c = 1, ncols_x
        cp1 = c + 1

        vor = xmapc2(c,r) * ((xvv_d(cp1,r,  k) + xvv_d(cp1,rp1,k) -          &
                              xvv_d(c,  r,  k) - xvv_d(c,  rp1,k)) / dsx  -  &
                             (xuu_d(c,  rp1,k) + xuu_d(cp1,rp1,k) -          &
                              xuu_d(c,  r,  k) - xuu_d(cp1,r,  k)) / dsy) +  &
              xcorl(c,r)

        xpvc(c,r,k) = scalef * ( vor * dtds(c,r)               &
                                     - dvds(c,r) * dtdx(c,r)   &
                                     + duds(c,r) * dtdy(c,r) )

      ENDDO
    ENDDO

  ENDDO kloop

END SUBROUTINE pvs
