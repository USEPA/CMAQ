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

module mio_map_projection_module

!-------------------------------------------------------------------------------
! Name:     Constants
! Purpose:  Contains fundamental constants for air quality modeling.
!           Sets universal constants for all CMAQ programs.
! Revised:  ?? Jun 1992  Adapted from ROM's PI.EXT.  (C. Coats)
!           01 Mar 1993  Included constants needed by LCM aqueous chemistry.
!                        (J. McHenry)
!           ?? Sep 1993  Included additional constants needed for FMEM clouds
!                        and aqueous chemistry.  (J. McHenry)
!           04 Mar 1996  Reflect current Models3 view that MKS units should be
!                        used wherever possible and that sources be documented.
!                        Some variables have been added, names changed, and
!                        values revised.  (F. Binkowski)
!           07 Mar 1996  Add universal gas constant and compute gas constant
!                        in chemical form.  TWOPI is now calculated rather
!                        than input.  (???)
!           13 Mar 1996  Group declarations and parameter statements.  (???)
!           13 Sep 1996  Include more physical constants.  (???)
!           24 Dec 1996  Eliminate silly EPSILON, AMISS.  (???)
!           06 Jan 1997  Eliminate most derived constants.  (J. Young)
!           17 Jan 1997  Comments only to provide numerical values as
!                        reference.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90 and changed name from
!                        CONST.EXT to module_const.f90.  (T. Otte)
!           16 Aug 2005  Updated declaration of PI to F90 standard.  (T. Otte)
!           21 Jun 2006  Updated calculations that use PI.  (T. Otte)
!           23 Apr 2008  Changed REARTH and DG2M (derived from REARTH) from
!                        F90 parameters to values set from user input.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! References:
!
!      CRC76,        "CRC Handbook of Chemistry and Physics (76th Ed)",
!                     CRC Press, 1995
!      Hobbs, P.V.   "Basic Physical Chemistry for the Atmospheric Sciences",
!                     Cambridge Univ. Press, 206 pp, 1995.
!      Snyder, J.P., "Map Projections-A Working Manual, U.S. Geological Survey
!                     Paper 1395 U.S.GPO, Washington, DC, 1987.
!      Stull, R. B., "An Introduction to Bounday Layer Meteorology", Kluwer,
!                     Dordrecht, 1988
!-------------------------------------------------------------------------------

  implicit none


! Geometric Constants:

  ! pi  (single precision: 3.141593)
  real(8),       parameter     :: pi = 3.14159265358979324d0

  ! pi/180 [ rad/deg ]
  real,          parameter     :: pi180 = real (pi / 1.8d2)


! Geodetic Constants:

  ! radius of earth [ m ]
  ! -- radius of sphere having same surface area as Clarke ellipsoid of 1866
  !    (Source: Snyder, 1987)
!!!  REAL,          PARAMETER     :: rearth = 6370997.0
  real                         :: rearth

  ! length of a sidereal day [ sec ]  (Source:  CRC76, pp. 14-6)
  real,          parameter     :: siday = 86164.09

  ! mean gravitational acceleration [ m/sec**2 ]
  ! --  mean of polar and equatorial values  (Source:  CRC76, pp. 14-6)
  real,          parameter     :: grav = 9.80622

  ! latitude degrees to meters
!!!  REAL,          PARAMETER     :: dg2m = rearth * pi180
  real                         :: dg2m

  ! Solar Constant  [ W/m**2 ]  (Source:  CRC76, pp. 14-2)
  real,          parameter     :: solcnst = 1373.0


! Fundamental Constants: (Source: CRC76, pp. 1-1 to 1-6)

  ! Avogadro's Constant [ number/mol ]
  real,          parameter     :: avo = 6.0221367e23

  ! universal gas constant [ J/mol-K ]
  real,          parameter     :: rgasuniv = 8.314510

  ! standard atmosphere [ Pa ]
  real,          parameter     :: stdatmpa = 101325.0

  ! standard temperature [ K ]
  real,          parameter     :: stdtemp = 273.15

  ! Stefan-Boltzmann [ W/(m**2 K**4) ]
  real,          parameter     :: stfblz = 5.67051e-8


! Non-MKS:

  ! Molar volume at STP [ L/mol ] Non MKS units
  real,          parameter     :: molvol = 22.41410


! Atmospheric Constants:

  ! mean molecular weight for dry air [ g/mol ]
  ! -- 78.06% N2, 21% O2, and 0.943% A on a mole fraction basis
  !    (Source: Hobbs, 1995, pp. 69-70)
  real,          parameter     :: mwair = 28.9628

  ! dry-air gas constant [ 287.07548994 J/kg-K ]
  real,          parameter     :: rdgas = 1.0e3 * rgasuniv / mwair

  ! mean molecular weight for water vapor [ g/mol ]
  real,          parameter     :: mwwat = 18.0153

  ! gas constant for water vapor [ 461.52492604 J/kg-K ]
  real,          parameter     :: rwvap = 1.0e3 * rgasuniv / mwwat

  ! FSB NOTE: CPD, CVD, CPWVAP and CVWVAP are calculated assuming dry air and
  ! water vapor are classical ideal gases, i.e. vibration does not contribute
  ! to internal energy.

  ! specific heat of dry air at constant pressure [ 1004.7642148 J/kg-K ]
  real,          parameter     :: cpd = 7.0 * rdgas / 2.0

  ! specific heat of dry air at constant volume [ 717.68872485 J/kg-K ]
  real,          parameter     :: cvd = 5.0 * rdgas / 2.0

  ! specific heat for water vapor at constant pressure [ 1846.0997042 J/kg-K ]
  real,          parameter     :: cpwvap = 4.0 * rwvap

  ! specific heat for water vapor at constant volume [ 1384.5747781 J/kg-K ]
  real,          parameter     :: cvwvap = 3.0 * rwvap

  ! vapor press of water at 0 C [ Pa ]  (Source: CRC76 pp. 6-15)
  real,          parameter     :: vp0 = 611.29

  ! The following values are taken from p. 641 of Stull (1988):

  ! latent heat of vaporization of water at 0 C [ J/kg ]
  real,          parameter     :: lv0 = 2.501e6

  ! Rate of change of latent heat of vaporization w.r.t. temperature [ J/kg-K ]
  real,          parameter     :: dlvdt = 2370.0

  ! latent heat of fusion of water at 0 C [ J/kg ]
  real,          parameter     :: lf0 = 3.34e5

contains

subroutine ll2xy_lam (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Lambert Conformal Projection
! Purpose:  Determines secant or tangent Lambert conformal case, and calls
!           appropriate routine.
! Revised:  03 Jun 2008  Original version.  (T. Otte)
!           26 Nov 2008  Added argument for reference latitude, PHI0.
!                        Prevent users from having tangent Lambert conformal
!                        case until it can be tested with the Spatial
!                        Allocator.  (Known problem is that the Spatial
!                        Allocator does not work properly when the
!                        reference latitude is equal to the first true
!                        latitude.  Work-around is to set reference latitude
!                        to average of true latitudes for Lambert conformal.
!                        But average of true latiudes for tangent Lambert
!                        conformal case is the first true latitude, which
!                        will result in the same problem as solution used
!                        in MCIPv3.4.)  (T. Otte)
!           01 Sep 2011  Improved error handling.  Changed XX and YY from
!                        double-precision to single-precision reals.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  real,               intent(in)    :: lambda  ! longitude [deg]
  real,               intent(in)    :: lambda0 ! standard longitude [deg]
  real,               intent(in)    :: phi     ! latitude [deg]
  real,               intent(in)    :: phi0    ! reference latitude [deg]
  real,               intent(in)    :: phi1    ! true latitude 1 [deg]
  real,               intent(in)    :: phi2    ! true latitude 2 [deg]
  real,               parameter     :: phitol  = 0.001  ! tolerance [deg]
  character(len=16),  parameter     :: pname   = 'LL2XY_LAM'
  real,               intent(out)   :: xx      ! X-coordinate from origin
  real,               intent(out)   :: yy      ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  character(len=256), parameter :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   TANGENT LAMBERT CONFORMAL PROJECTION DETECTED', &
    & /, 1x, '***   TRUE LATITUDES = ', f8.3, 2x, f8.3, &
    & /, 1x, '***   MAY NOT WORK PROPERLY IN SPATIAL ALLOCATOR', &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Determine whether Lambert conformal is tangent or secant.
!-------------------------------------------------------------------------------

  if ( abs( phi1 - phi2 ) < phitol ) then  ! tangent case
    write (*,f9000) trim(pname), phi1, phi2
    call graceful_stop (pname)
!   CALL ll2xy_lam_tan (phi, lambda, phi1, lambda0, xx, yy)
  else  ! secant case
    call ll2xy_lam_sec (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)
  endif

end subroutine ll2xy_lam

!------------------------------------------------------------------------------!
subroutine ll2xy_lam_sec (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Lambert Conformal Projection
! Purpose:  Calculates (X,Y) from origin for a given latitude-longitude pair
!           and Lambert conformal projection information for secant case.
! Notes:    Equations taken from "Map Projections: Theory and Applications"
!           by Frederick Pearson, II (1990), pp. 181-182.
! Revised:  03 Jun 2008  Original version.  (T. Otte)
!           04 Dec 2008  Added argument for reference latitude, PHI0.
!                        Changed routine so it is no longer hard-wired to
!                        have a reference latitude at the first true
!                        latitude.  (T. Otte and J. Pleim)
!           17 Sep 2009  Corrected inline comments associated with definitions
!                        of RHO and RHO0.  Corrected calculation of PSI (with
!                        no impact on results).  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  real(8)                      :: deg2rad ! convert degrees to radians
  real(8)                      :: dlambda ! delta lambda
  real(8)                      :: drearth ! double-precision radius of earth [m]
  real,          intent(in)    :: lambda  ! longitude [deg]
  real,          intent(in)    :: lambda0 ! standard longitude [deg]
  real,          intent(in)    :: phi     ! latitude [deg]
  real(8)                      :: phirad  ! latitude [rad]
  real,          intent(in)    :: phi0    ! reference latitude [deg]
  real(8)                      :: phi0rad ! reference latitude [rad]
  real,          intent(in)    :: phi1    ! true latitude 1 [deg]
  real(8)                      :: phi1rad ! true latitude 1 [rad]
  real,          intent(in)    :: phi2    ! true latitude 2 [deg]
  real(8)                      :: phi2rad ! true latitude 2 [rad]
  real(8)                      :: pi
  real(8)                      :: piover4 ! pi/4
  real(8)                      :: psi     ! auxiliary function
  real(8)                      :: rho     ! polar radius to latitude phi
  real(8)                      :: rho0    ! polar radius to origin
  real(8)                      :: term
  real(8)                      :: term0
  real(8)                      :: term1
  real(8)                      :: term2
  real(8)                      :: theta   ! polar angle
  real(8)                      :: sinphi0 ! cone constant
  real,          intent(out)   :: xx      ! X-coordinate from origin
  real,          intent(out)   :: yy      ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = datan(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

  drearth = dble(rearth)

!-------------------------------------------------------------------------------
! Compute cone constant, SINPHI0.
! Note:  PHI0 is the reference latitude, which is user-defined.  It is NOT
!        used in the calculation of SINPHI0, which is the cone constant.
!-------------------------------------------------------------------------------

  phi0rad = dble(phi0) * deg2rad  ! convert PHI0 from degrees to radians
  phi1rad = dble(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phi2rad = dble(phi2) * deg2rad  ! convert PHI2 from degrees to radians

  term0 = dtan (piover4 - phi0rad/2.0d0)
  term1 = dtan (piover4 - phi1rad/2.0d0)
  term2 = dtan (piover4 - phi2rad/2.0d0)

  sinphi0 = dlog ( dcos(phi1rad) / dcos(phi2rad) )
  sinphi0 = sinphi0 / dlog (term1 / term2)

!-------------------------------------------------------------------------------
! Compute polar angle, THETA.
!-------------------------------------------------------------------------------

  dlambda = dble(lambda - lambda0) * deg2rad
  theta   = dlambda * sinphi0

!-------------------------------------------------------------------------------
! Compute polar radius to origin, RHO0, where origin is at PHI0.
!-------------------------------------------------------------------------------

  psi  = drearth * dcos(phi1rad) / sinphi0 / (term1**sinphi0)
  rho0 = psi * (term0**sinphi0)

!-------------------------------------------------------------------------------
! Compute polar radius to latitude PHI, RHO.
!-------------------------------------------------------------------------------

  phirad = dble(phi) * deg2rad  ! convert PHI from degrees to radians
  term   = dtan (piover4 - phirad/2.0d0)
  rho    = psi * (term**sinphi0)

!-------------------------------------------------------------------------------
! Compute Cartesian coordinates, XX and YY.
!-------------------------------------------------------------------------------

  xx = real(        rho * dsin(theta) )
  yy = real( rho0 - rho * dcos(theta) )

end subroutine ll2xy_lam_sec

!------------------------------------------------------------------------------!
subroutine ll2xy_lam_tan (phi, lambda, phi0, lambda0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Lambert Conformal Projection
! Purpose:  Calcluates (X,Y) from origin for a given latitude-longitude pair
!           and Lambert conformal projection information for tangent case.
! Notes:    Equations taken from "Map Projections: Theory and Applications"
!           by Frederick Pearson, II (1990), pp. 168-175.
! Revised:  03 Jun 2008  Original version.  (T. Otte)
!           01 Sep 2011  Changed XX and YY from double-precision to single-
!                        precision reals.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  real(8)                      :: deg2rad ! convert degrees to radians
  real(8)                      :: dlambda ! delta lambda
  real(8)                      :: drearth ! double-precision radius of earth [m]
  real,          intent(in)    :: lambda  ! longitude [deg]
  real,          intent(in)    :: lambda0 ! standard longitude [deg]
  real,          intent(in)    :: phi     ! latitude [deg]
  real(8)                      :: phirad  ! latitude [rad]
  real,          intent(in)    :: phi0    ! true latitude [deg]
  real(8)                      :: phi0rad ! true latitude [rad]
  real(8)                      :: pi
  real(8)                      :: piover4 ! pi/4
  real(8)                      :: rho     ! polar radius to origin
  real(8)                      :: rho0    ! polar radius to latitude phi
  real(8)                      :: term
  real(8)                      :: term0
  real(8)                      :: theta   ! polar angle
  real(8)                      :: sinphi0 ! cone constant
  real,          intent(out)   :: xx      ! X-coordinate from origin
  real,          intent(out)   :: yy      ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = datan(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

  drearth = dble(rearth)

!-------------------------------------------------------------------------------
! Compute cone constant, SINPHI0.
!-------------------------------------------------------------------------------

  phi0rad = phi0 * deg2rad  ! convert PHI0 from degrees to radians
  sinphi0 = dsin (phi0rad)

!-------------------------------------------------------------------------------
! Compute polar angle, THETA.
!-------------------------------------------------------------------------------

  dlambda = (lambda - lambda0) * deg2rad
  theta   = dlambda * sinphi0

!-------------------------------------------------------------------------------
! Compute polar radius to origin, RHO0, where origin is at PHI0.
!-------------------------------------------------------------------------------

  rho0 = drearth * dcos(phi0rad) / sinphi0

!-------------------------------------------------------------------------------
! Compute polar radius to latitude PHI, RHO.
!-------------------------------------------------------------------------------

  phirad = phi  * deg2rad  ! convert PHI from degrees to radians
  term   = dtan (piover4 - phirad /2.0d0)
  term0  = dtan (piover4 - phi0rad/2.0d0)
  rho    = rho0 * (( term / term0 )**sinphi0)

!-------------------------------------------------------------------------------
! Compute Cartesian coordinates, XX and YY.
!-------------------------------------------------------------------------------

  xx = real(       rho * dsin(theta))
  yy = real(rho0 - rho * dcos(theta))

end subroutine ll2xy_lam_tan

!------------------------------------------------------------------------------!
subroutine ll2xy_merc (phi, lambda, lambda0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Mercator Projection
! Purpose:  Calcluates (X,Y) from origin for a given latitude-longitude pair
!           and Mercator projection information.
! Notes:    Equations taken from "Map Projections: Theory and Applications"
!           by Frederick Pearson, II (1990), pp. 190-192.
! Revised:  23 Sep 2009  Original version.  (T. Otte)
!           12 Feb 2010  Removed unused variable FAC.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  real(8)                      :: deg2rad ! convert degrees to radians
  real(8)                      :: drearth ! earth radius [m]
  real,          intent(in)    :: lambda0 ! center longitude [deg]
  real(8)                      :: lambda0rad ! center longitude [rad]
  real,          intent(in)    :: lambda  ! longitude [deg]
  real(8)                      :: lambdarad ! longitude [rad]
  real,          intent(in)    :: phi     ! latitude [deg]
  real(8)                      :: phirad  ! latitude [rad]
  real(8)                      :: pi
  real(8)                      :: piover4 ! pi/4
  real,          intent(out)   :: xx      ! X-coordinate from origin
  real,          intent(out)   :: yy      ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = datan(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

  drearth = dble(rearth)

!-------------------------------------------------------------------------------
! Compute Cartesian coordinates, XX and YY.
!-------------------------------------------------------------------------------

  phirad     = dble(phi)     * deg2rad  ! convert degrees to radians
  lambdarad  = dble(lambda)  * deg2rad  ! convert degrees to radians
  lambda0rad = dble(lambda0) * deg2rad  ! convert degrees to radians

  xx  = real( drearth * (lambdarad - lambda0rad) )
  yy  = real( drearth * dlog( dtan( piover4 + (phirad/2.0d0) ) ) )

end subroutine ll2xy_merc

!------------------------------------------------------------------------------!
subroutine ll2xy_ps (phi, lambda, phi1, lambda0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Polar Stereographic Projection
! Purpose:  Calcluates (X,Y) from origin for a given latitude-longitude pair
!           and polar stereographic projection information.
! Notes:    Adapted from equations found at http://starbase.jpl.nasa.gov/
!           mgn-v-rdrs-5-dvdr-v1.0/gvdr0001/catalog/dsmp.lbl.
! Revised:  28 Sep 2009  Original version.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  real(8)                      :: deg2rad ! convert degrees to radians
  real(8)                      :: drearth ! earth radius [m]
  real(8)                      :: hemi    ! +/-1 for Northern/Southern Hemis
  real,          intent(in)    :: lambda  ! longitude [deg]
  real,          intent(in)    :: lambda0 ! standard longitude [deg]
  real,          intent(in)    :: phi     ! latitude [deg]
  real(8)                      :: phirad  ! latitude [rad]
  real,          intent(in)    :: phi1    ! true latitude 1 [deg]
  real(8)                      :: phi1rad ! true latitude 1 [rad]
  real(8)                      :: pi
  real(8)                      :: piover4 ! pi/4
  real(8)                      :: scalefac
  real(8)                      :: sigma   ! image scale
  real(8)                      :: theta   ! polar angle
  real(8)                      :: tt
  real,          intent(out)   :: xx      ! X-coordinate from origin
  real,          intent(out)   :: yy      ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = datan(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

  drearth = dble(rearth)

!-------------------------------------------------------------------------------
! Compute image scale, SIGMA.
!-------------------------------------------------------------------------------

  hemi = dsign (1.0d0, dble(phi1))

  phi1rad = dble(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phirad  = dble(phi)  * deg2rad  ! convert PHI  from degrees to radians

  sigma   = (1.0d0 + dsin(phi1rad)) / 2.0d0 * hemi

  scalefac = drearth / sigma

  tt = dtan ( piover4 - phirad/2.0d0)

!-------------------------------------------------------------------------------
! Compute polar angle, THETA.
!-------------------------------------------------------------------------------

  theta = dble(lambda - lambda0) * deg2rad

!-------------------------------------------------------------------------------
! Compute Cartesian coordinates, XX and YY.
!-------------------------------------------------------------------------------

  xx = real(         2.0d0 * scalefac * tt * dsin(theta) )
  yy = real( -hemi * 2.0d0 * scalefac * tt * dcos(theta) )

end subroutine ll2xy_ps

!------------------------------------------------------------------------------!
real function mapfac_lam (phi, phi1, phi2)

!-------------------------------------------------------------------------------
! Name:     Map-Scale Factor for Lambert conformal projection.
! Purpose:  Calculates map-scale factors for secant Lambert conformal projection
!           from latitude and true latitudes.
! Notes:    Equations taken from "Numerical Prediction and Dynamic Meteorology",
!           Second Edition, by George J. Haltiner and Roger Terry Williams
!           (1980), pp. 13-14.  (Equations modified; see comments in code.)
! Revised:  03 Sep 2009  Original version.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  real(8)                      :: deg2rad ! convert degrees to radians
  real,          intent(in)    :: phi     ! latitude [deg]
  real(8)                      :: phirad  ! latitude [rad]
  real,          intent(in)    :: phi1    ! true latitude 1 [deg]
  real(8)                      :: phi1rad ! true latitude 1 [rad]
  real,          intent(in)    :: phi2    ! true latitude 2 [deg]
  real(8)                      :: phi2rad ! true latitude 2 [rad]
  real(8)                      :: pi
  real(8)                      :: piover4 ! pi/4
  real(8)                      :: sinphi0 ! cone constant
  real(8)                      :: term1
  real(8)                      :: term2
  real(8)                      :: term3
  real(8)                      :: term4
  real(8)                      :: term5

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = datan(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

!-------------------------------------------------------------------------------
! Compute cone constant, SINPHI0.  ("K" in Haltiner and Williams, Eqn. 1-40.)
!-------------------------------------------------------------------------------

  phi1rad = dble(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phi2rad = dble(phi2) * deg2rad  ! convert PHI2 from degrees to radians

  term1 = dtan (piover4 - phi1rad/2.0d0)
  term2 = dtan (piover4 - phi2rad/2.0d0)

  sinphi0 = dlog ( dcos(phi1rad) / dcos(phi2rad) )
  sinphi0 = sinphi0 / dlog (term1 / term2)

!-------------------------------------------------------------------------------
! Compute map-scale factor, MAPFAC.
!
! M(phi) = (COS phi / COS phi1)**(K - 1) * ((1 + SIN phi1) / (1 + SIN phi))**K

! Note:  Original equation in Haltiner and Williams (1-40) is incorrect because
!        K-1 and K are exponents rather than multiplicative terms.
!-------------------------------------------------------------------------------

  phirad = dble(phi) * deg2rad  ! convert PHI from degrees to radians

  term3  = dcos(phirad) / dcos(phi1rad)
  term4  = sinphi0 - 1.0
  term5  = (1.0d0 + dsin(phi1rad)) / (1.0d0 + dsin(phirad))

  mapfac_lam = real(term3**term4 * term5**sinphi0)

end function mapfac_lam

!------------------------------------------------------------------------------!
real function mapfac_merc (phi)

!-------------------------------------------------------------------------------
! Name:     Map-Scale Factor for Mercator projection
! Purpose:  Calculates map-scale factors for Mercator projection from latitude.
! Notes:    Equation taken from "Numerical Prediction and Dynamic Meteorology",
!           Second Edition, by George J. Haltiner and Roger Terry Williams
!           (1980), pp. 13.
! Revised:  15 Sep 2009  Original version.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  real(8)                      :: deg2rad ! convert degrees to radians
  real,          intent(in)    :: phi     ! latitude [deg]
  real(8)                      :: phirad  ! latitude [rad]
  real(8)                      :: pi
  real(8)                      :: piover4 ! pi/4

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = datan(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

!-------------------------------------------------------------------------------
! Compute map-scale factor, MAPFAC.
!
! M(phi) = COS phi0 / COS phi
!
!   where phi0 is the latitude of the "true" projection (assumed to be equator)
!-------------------------------------------------------------------------------

  phirad = dble(phi) * deg2rad  ! convert PHI from degrees to radians

  mapfac_merc = real( 1.0d0 / dcos(phirad) )

end function mapfac_merc

!------------------------------------------------------------------------------!
real function mapfac_ps (phi, phi1)

!-------------------------------------------------------------------------------
! Name:     Map-Scale Factor for polar stereographic projection.
! Purpose:  Calculates map-scale factors for polar stereographic projection
!           from latitude.
! Notes:    Equation taken from "Numerical Prediction and Dynamic Meteorology",
!           Second Edition, by George J. Haltiner and Roger Terry Williams
!           (1980), pp. 11-13.
! Revised:  24 Sep 2009  Original version.  (T. Otte)
!           21 Sep 2010  Corrected error in calculation that assumed that
!                        true latitude was at the pole.  Now calculation
!                        correctly accounts for true latitude.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  real(8)                      :: deg2rad ! convert degrees to radians
  real(8)                      :: hemi    ! +/-1 for Northern/Southern Hemis
  real,          intent(in)    :: phi     ! latitude [deg]
  real(8)                      :: phirad  ! latitude [rad]
  real(8)                      :: phi1rad ! true latitude 1 [rad]
  real,          intent(in)    :: phi1    ! true latitude 1 [deg]
  real(8)                      :: pi
  real(8)                      :: piover4 ! pi/4

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = datan(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

!-------------------------------------------------------------------------------
! Compute map-scale factor, MAPFAC.
!
! M(phi) = (1 + SIN(hemi*phi1)) / (1 + SIN phi)
!-------------------------------------------------------------------------------

  hemi      = dsign( 1.0d0, dble(phi1) )
  phirad    = dble(phi)  * deg2rad  ! convert PHI  from degrees to radians
  phi1rad   = dble(phi1) * deg2rad  ! convert PHI1 from degrees to radians

  mapfac_ps = real( (1.0d0 + hemi * dsin(phi1rad)) / (1.0d0 + dsin(phirad)) )

end function mapfac_ps

!------------------------------------------------------------------------------!
subroutine xy2ll_lam (xx, yy, phi1, phi2, lambda0, phi0, phi, lambda)

!-------------------------------------------------------------------------------
! Name:     (X,Y) to Latitude-Longitude for Lambert Conformal Projection
! Purpose:  Calcluates latitude-longitude for a given (X,Y) pair from origin
!           and Lambert conformal projection information.
! Notes:    Equations adapted from http://mathworld.wolfram.com.
! Revised:  12 Dec 2007  Original version.  (T. Otte)
!           18 Sep 2009  Added reference latitude (PHI0) as an argument.
!                        Converted to double-precision.  Corrected comments
!                        associated with RHO0.  Corrected calculation of PSI
!                        (with no impact on results).  (T. Otte)
!           02 Oct 2009  Changed algorithm to follow Wolfram to eliminate the
!                        divide-by-zero condition for computing latitude along
!                        the standard longitude.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  real(8)                      :: deg2rad    ! convert degrees to radians
  real(8)                      :: drearth    ! earth radius [m]
  real,          intent(out)   :: lambda     ! longitude [deg]
  real(8)                      :: lambdarad  ! longitude [rad]
  real,          intent(in)    :: lambda0    ! standard longitude [deg]
  real(8)                      :: lambda0rad ! standard longitude [rad]
  real,          intent(out)   :: phi        ! latitude [deg]
  real(8)                      :: phirad     ! latitude [rad]
  real,          intent(in)    :: phi0       ! reference latitude [deg]
  real(8)                      :: phi0rad    ! reference latitude [rad]
  real,          intent(in)    :: phi1       ! true latitude 1 [deg]
  real(8)                      :: phi1rad    ! true latitude 1 [rad]
  real,          intent(in)    :: phi2       ! true latitude 2 [deg]
  real(8)                      :: phi2rad    ! true latitude 2 [rad]
  real(8)                      :: pi
  real(8)                      :: piover2    ! pi/2
  real(8)                      :: piover4    ! pi/4
  real(8)                      :: psi        ! auxiliary function
  real(8)                      :: rad2deg
  real(8)                      :: rho
  real(8)                      :: rho0       ! polar radius to origin
  real(8)                      :: term0
  real(8)                      :: term1
  real(8)                      :: term2
  real(8)                      :: theta      ! polar angle
  real(8)                      :: sinphi0    ! cone constant
  real(8)                      :: sinphi0inv ! 1/sinphi0
  real,          intent(in)    :: xx         ! X-coordinate from origin
  real,          intent(in)    :: yy         ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = datan(1.0d0)
  pi      = 4.0d0 * piover4
  piover2 = 2.0d0 * piover4
  deg2rad = pi / 1.8d2
  rad2deg = 1.8d2 / pi

  drearth = dble(rearth)

!-------------------------------------------------------------------------------
! Compute cone constant, SINPHI0.
!-------------------------------------------------------------------------------

  phi0rad = dble(phi0) * deg2rad  ! convert PHI0 from degrees to radians
  phi1rad = dble(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phi2rad = dble(phi2) * deg2rad  ! convert PHI2 from degrees to radians

  term0 = dtan ( piover4 + phi0rad/2.0d0 )
  term1 = dtan ( piover4 + phi1rad/2.0d0 )
  term2 = dtan ( piover4 + phi2rad/2.0d0 )

  sinphi0 = dlog ( dcos(phi1rad) / dcos(phi2rad) )
  sinphi0 = sinphi0 / dlog (term2 / term1)

  sinphi0inv = 1.0d0 / sinphi0

!-------------------------------------------------------------------------------
! Compute polar radius to origin, RHO0, where origin is at PHI0.
!-------------------------------------------------------------------------------

  psi  = drearth * dcos(phi1rad) * sinphi0inv * (term1**sinphi0)
  rho0 = psi / (term0**sinphi0)

!-------------------------------------------------------------------------------
! Compute longitude, LAMBDA.
!-------------------------------------------------------------------------------

  lambda0rad = lambda0 * deg2rad

  theta     = datan( dble(xx) / (rho0 - dble(yy)) )
  lambdarad = lambda0rad + theta * sinphi0inv
  lambda    = real(lambdarad * rad2deg)

!-------------------------------------------------------------------------------
! Compute latitude, PHI.
!-------------------------------------------------------------------------------

  rho = dsqrt( dble(xx)*dble(xx) + (rho0-dble(yy))*(rho0-dble(yy)) )
  rho = dsign(1.0d0, sinphi0) * rho

  phirad = (psi / rho)**sinphi0inv
  phirad = 2.0d0 * datan(phirad) - piover2
  phi    = real(phirad * rad2deg)

end subroutine xy2ll_lam

!------------------------------------------------------------------------------!
subroutine xy2ll_merc (xx, yy, lambda0, phi, lambda)

!-------------------------------------------------------------------------------
! Name:     (X,Y) to Latitude-Longitude for Polar Stereographic Projection
! Purpose:  Calcluates latitude-longitude for a given (X,Y) pair from origin
!           and polar stereographic projection information.
! Notes:    Equations taken from "Map Projections: Theory and Applications"
!           by Frederick Pearson, II (1990), pp. 190-192.
! Revised:  18 Sep 2009  Original version.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!-------------------------------------------------------------------------------

  real(8)                      :: deg2rad    ! convert degrees to radians
  real(8)                      :: drearth    ! earth radius [m]
  real,          intent(out)   :: lambda     ! longitude [deg]
  real(8)                      :: lambdarad  ! longitude [rad]
  real,          intent(in)    :: lambda0    ! center longitude [deg]
  real(8)                      :: lambda0rad ! center longitude [rad]
  real,          intent(out)   :: phi        ! latitude [deg]
  real(8)                      :: phirad     ! latitude [rad]
  real(8)                      :: pi
  real(8)                      :: piover2    ! pi/2
  real(8)                      :: piover4    ! pi/4
  real(8)                      :: rad2deg
  real,          intent(in)    :: xx         ! X-coordinate from origin
  real(8)                      :: xxd
  real,          intent(in)    :: yy         ! Y-coordinate from origin
  real(8)                      :: yyd

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = datan(1.0d0)
  piover2 = 2.0d0 * piover4
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2
  rad2deg = 1.8d2 / pi

  drearth = dble(rearth)

!-------------------------------------------------------------------------------
! Set up geometric constants.
!-------------------------------------------------------------------------------

  xxd  = dble(xx)
  yyd  = dble(yy)

!-------------------------------------------------------------------------------
! Compute latitude (PHI).
!-------------------------------------------------------------------------------

  phirad  = ( 2.0d0 * datan ( dexp(yyd/drearth) ) ) - piover2
  phi     = real( phirad * rad2deg )

!-------------------------------------------------------------------------------
! Compute longitude (LAMBDA).
!-------------------------------------------------------------------------------

  lambda0rad = dble(lambda0) * deg2rad
  lambdarad  = lambda0rad + xxd/drearth
  lambda     = real( lambdarad * rad2deg )

end subroutine xy2ll_merc

end module mio_map_projection_module
