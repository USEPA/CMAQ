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

SUBROUTINE metgrid2ctm

!-------------------------------------------------------------------------------
! Name:     Meteorology Grid to CTM Grid
! Purpose:  Puts time-invariant meteorology arrays on CTM grid.
! Notes:    Algorithms taken from MCIP v1 getmet_mm5.F.
! Revised:  17 Sep 2001  Original version.  (T. Otte)
!           16 Oct 2001  Corrected error in translation between input
!                        domain and MCIP "X" domain.  (T. Otte)
!           21 Dec 2001  Changed order of variable declarations in interface
!                        to improve portability.  (S. Howard and T. Otte)
!           22 Jan 2002  Added definition of XPRSFC for non-hydrostatic
!                        runs.  (T. Otte)
!           26 Mar 2003  Simplified algorithm to map input meteorology to
!                        MCIP_X domain.  Enabled dot-point MCIP_X arrays to
!                        be filled to the correct dimensions.  (T. Otte)
!           07 Jul 2004  Removed XFLAGS.  (T. Otte)
!           21 Jan 2005  Removed NDX and option to interpolate to finer scale
!                        meteorology.  Changed I and J to X and Y to make
!                        code more general.  Added optimization of loops
!                        using F90 implicit loop structures.  (T. Otte and
!                        D. Wong)
!           19 Jun 2006  Removed unused variables COL, II, IIL, JJ, JJL, LU,
!                        PNAME, and ROW.  (T. Otte)
!           31 Jul 2007  Removed use of MET_INHYD for MM5 processing.  Removed
!                        filling of XPRSFC with reference pressure here since
!                        it is not used and is now saved from REFSTATE.
!                        Removed dependency on module METINFO.  (T. Otte)
!           17 Sep 2009  Added map-scale factors squared (on cross points,
!                        XMAPC2) to minimize repeated squaring in other places
!                        in MCIP.  Added latitude, longitude, and map-scale
!                        factors on U and V faces.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE metvars

  IMPLICIT NONE

  INTEGER                      :: ec
  INTEGER                      :: er
  INTEGER                      :: jw
  INTEGER                      :: lbndface
  INTEGER                      :: lbndsigf
  INTEGER                      :: maxface
  INTEGER                      :: sc
  INTEGER                      :: sr
  INTEGER                      :: ubndsigf
  REAL,          ALLOCATABLE   :: x3workf   ( : )

!-------------------------------------------------------------------------------
! Set up vertical coordinate, where XX3FACE and XX3MIDL are layer face and
! middle definitions.  XX3FACE and XX3MIDL are dimensioned in MCIPCOM
! and increase monotonically with height.
!-------------------------------------------------------------------------------

  lbndsigf = LBOUND(sigmaf,1)
  lbndface = LBOUND(xx3face,1)
  ubndsigf = UBOUND(sigmaf,1)
  maxface  = lbndface + SIZE(sigmaf) - 1

  ALLOCATE ( x3workf ( lbndsigf:ubndsigf ) )

  IF ( sigmaf(lbndsigf) > sigmaf(ubndsigf) ) THEN  ! NOT monotonic increase
    DO jw = lbndsigf, ubndsigf
      x3workf(jw) = sigmaf(lbndsigf) - sigmaf(jw)
    ENDDO
  ELSE
    DO jw = lbndsigf, ubndsigf
      x3workf(jw) = sigmaf(jw) - sigmaf(lbndsigf)
    ENDDO
  ENDIF

  xx3face(lbndface:maxface) = x3workf(lbndsigf:ubndsigf)

  DO jw = 1, SIZE(sigmah)
    xx3midl(jw) = 0.5 * ( xx3face(jw-1) + xx3face(jw) )
  ENDDO

  DEALLOCATE ( x3workf )

!-------------------------------------------------------------------------------
! Put time-invariant cross-point arrays on MCIP_X grid.
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x - 1
  sr = y0
  er = y0 + nrows_x - 1

  xtopo(:,:) = terrain(sc:ec,sr:er)
  xmapc(:,:) = mapcrs (sc:ec,sr:er)
  xlatc(:,:) = latcrs (sc:ec,sr:er)
  xlonc(:,:) = loncrs (sc:ec,sr:er)

  xmapc2(:,:) = xmapc(:,:) * xmapc(:,:)

!-------------------------------------------------------------------------------
! Put time-invariant dot-point and face arrays on MCIP_X grid.  Note that
! the face arrays are one column or one row smaller than the dot-point arrays
! on the physical grid, but the face arrays are oversized to the dot-point
! array space in MCIP out of convenience.
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x
  sr = y0
  er = y0 + nrows_x

  xmapd(:,:) = mapdot(sc:ec,sr:er)
  xlatd(:,:) = latdot(sc:ec,sr:er)
  xlond(:,:) = londot(sc:ec,sr:er)

  xmapu(:,:) = mapu(sc:ec,sr:er)
  xlatu(:,:) = latu(sc:ec,sr:er)
  xlonu(:,:) = lonu(sc:ec,sr:er)

  xmapv(:,:) = mapv(sc:ec,sr:er)
  xlatv(:,:) = latv(sc:ec,sr:er)
  xlonv(:,:) = lonv(sc:ec,sr:er)

END SUBROUTINE metgrid2ctm
