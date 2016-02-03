!*******************************************************************************
!$RCSfile: srfdos_inc.f90,v $
!$Revision: 1.1.1.1 $
!$Date: 2006/10/06 19:35:18 $
!*******************************************************************************
module srfdos_inc
  use struct_inc
  save

!------ include file for surface_dose (in surface.f)

  real ARGMAX
  parameter ( ARGMAX   = 20.0 )

  type ( puff_str ) p

  integer nxs, nys, ityp, ioff, i1, i2
!n      integer*4 i, ig, igt, j, j1, j2, nvx, iv, nlev
  integer i, j, j1, j2, nvx, iv, nlev, ng, ig(MAXSTYP) !V0.3
  integer igf(MAXSTYP)
  integer mlev, m1, n1

!n      real ccell(MAXSF+2)                                          !hascal
!n      real ccellt(MAXSF+2)                                         !hascal
  real ccell(MAXSTYP) !V0.3
  real xr(3), dxs, dys, zp, h, hx, hy, vfac
!n      real voli, det, cfac, ccfac, slfac, cctfac, sltfac, arg, fac
  real voli, det, cfac(MAXSTYP), arg, fac, argfac !V0.3
  real x, xp, xs, xplus, xminus, cxy, y, yp, ys, ym
  real xmap, ymap, xbar, ybar
  real axx, axy, axz, ayy, ayz, azz
  real bxx, bxy, byy
  real rrr, xlam, del, xfac, delx, dely, dfac
  real conc_min, cmax

end module srfdos_inc
