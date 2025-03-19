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

SUBROUTINE metvars2ctm

!-------------------------------------------------------------------------------
! Name:     Meteorology Variables to CTM (X) Grid
! Purpose:  Puts time-variant meteorology arrays on CTM (X) grid.
! Notes:    MM5 algorithms taken from original getmet_mm5.F.  Vegetation
!           information does not include seasonal changes.
! Revised:  20 Sep 2001  Original version.  (T. Otte)
!           03 Oct 2001  Updated calculation of XRNET.  (T. Otte and J. Pleim)
!           16 Oct 2001  Corrected error in translation between input
!                        domain and MCIP "X" domain.  (T. Otte)
!           21 Dec 2001  Changed order of variable declarations in interface
!                        to improve portability.  (S. Howard and T. Otte)
!           23 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  Added updates
!                        to XFLAGS for variables filled here.  Changed missing
!                        value on XRSTOM to BADVAL3.  (T. Otte)
!           26 Mar 2003  Simplified algorithm to map input meteorology to
!                        MCIP_X domain.  Enabled dot-point MCIP_X arrays to
!                        be filled to the correct dimensions.  (T. Otte)
!           29 May 2003  Added minimum leaf area index and vegetation values
!                        to represent winter conditions for USGS land use.
!                        LAI and VEG are based on values used by Vegeland (for
!                        P-X) in MM5.  Also added F2 as a function of USGS land
!                        use.  Added translation of snow cover to MCIP_X domain.
!                        (T. Otte, J. Pleim, and D. Schwede)
!           09 Aug 2004  Added graupel processing.  Added 2-m temperature, if
!                        it is available from incoming meteorology.  Removed
!                        XFLAGS.  (T. Otte and D. Schwede)
!           26 May 2005  Removed NDX and option to interpolate to finer scale
!                        meteorology.  Added optimization of loops using F90
!                        implicit loop structures.  Removed unused variables
!                        REGIME and MAVAIL.  Changed XUU and XVV to XUU_D and
!                        XVV_D, and changed definitions to be input model-
!                        specific.  Added definitions of XUU_S and XVV_T.
!                        Added contents of MET3DSUP.  Added WRF algorithms.
!                        Added processing of 10-m wind components.  Added
!                        floor value for QV.  (T. Otte)
!           19 Aug 2005  Removed unused variables FSOIL and XFSOIL.  Changed
!                        internal variable EPSILON to EPSILONQ to avoid
!                        confusion with F90 intrinsic function.  (T. Otte)
!           13 Sep 2005  Changed logic for defining surface density from 2-m
!                        temperature to prevent "null pointer" error with
!                        PGF90 compiler.  (T. Otte)
!           19 Jun 2006  Removed dependency on module CONST_PBL.  Removed
!                        unused variables DETA, IIL, JJL, LVL, and LVLF1.
!                        (T. Otte)
!           30 Jul 2007  Removed algorithms that supported hydrostatic MM5
!                        output (e.g., from MM5v2).  Changed XUSTAR and XRADYN
!                        to 2D arrays without a dimension for fractional land
!                        use that was required for RADMdry.  Changed 2-m
!                        temperature from XT2 to XTEMP2.  Removed internal
!                        variables for emissivity and net radiation.  Moved
!                        land-use-based filling of F2 and RSTMIN to subroutine
!                        RESISTCALC.  Removed references to logical variable
!                        "PX" to make code more general.  (T. Otte)
!           28 Apr 2008  Changed setting for LAI to a fixed value if NOAH LSM
!                        was used with WRF and if LAI is not found in the input
!                        meteorology file(s).  Added 2-m mixing ratio and
!                        turbulent kinetic energy.  Changed to use gravitational
!                        constant and dry gas constant values from MM5 system
!                        rather than from CMAQ system for coordinate
!                        calculations.  Removed unused variable GRAVI, and
!                        removed dependency on module CONST.  Expanded lookup
!                        tables for LAI and VEG for 33-category USGS, and
!                        deleted unused lookup tables for RST.  (T. Otte)
!           29 Oct 2009  Added potential temperature and Coriolis for when
!                        potential vorticity is needed.  Added default values
!                        of LAI and VEG for MODIS-NOAH and NLCD-MODIS land-use
!                        classification systems.  Changed real number
!                        comparisons from "equivalences" to "less than
!                        tolerances".  (T. Otte)
!           19 Mar 2010  Corrected comment that provided Jacobian equations for
!                        WRF.  Corrected logic in LAI algorithm for when IFLAI
!                        is FALSE.  (T. Otte)
!           23 Dec 2010  Corrected scaling of XWR for WRF runs so that water
!                        density is used rather than air density.  (J. Pleim)
!                        Added sea ice.  Set ice threshold temperature over
!                        water to 271.36 K to be consistent with CMAQ. (T. Otte)
!           23 Feb 2010  Replaced statement functions with external routines.
!                        (T. Otte)
!           01 Sep 2011  Replaced module PARMS3 with I/O API module M3UTILIO.
!                        Improved error handling.  Replaced DATA statements
!                        with parameters.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           09 Sep 2011  Fill convective rain array with negative values if
!                        no cumulus parameterization was used in the
!                        meteorological model.  Works with change to sub-grid
!                        cloud scheme in CMAQv5.0.  (T. Otte)
!           10 Sep 2012  Added handling for 40-category 2006 NLCD-MODIS land
!                        use classification as "NLCD40".  Added alternate name
!                        for 50-category 2001 NLCD-MODIS land use classification
!                        as "NLCD50".  Appended "D50" to the ends of LAINLC,
!                        LAIMNNLC, VEGNLC, and VEGMNNLC to distinguish them
!                        for NLCD50.  Added analogous variables for NLCD40.
!                        Some of the values for NCLD50 were updated with the
!                        current values from module_sf_pxlsm_data.F from
!                        WRFv3.4.  The values for NCLD40 are also consistent
!                        with the same routine.  (T. Otte)
!           24 Apr 2015  Added 3D resolved cloud fraction to output if it is
!                        available in incoming meteorological model data.
!                        (T. Spero)
!           21 Aug 2015  Changed latent heat flux from QFX to LH.  Fill THETA
!                        and add moisture flux (QFX) for IFMOLACM.  (T. Spero)
!           17 Sep 2015  Changed IFMOLACM to IFMOLPX.  (T. Spero)
!           16 Mar 2018  Added SNOWH to output.  Changed calculation of Jacobian
!                        for hybrid vertical coordinate in WRF, but disabled it
!                        for this release until addtional modifications and
!                        testing with CMAQ can be conducted.  Calculate soil
!                        layer depths in XZSOIL, and map 3D soil data into 3D
!                        soil arrays.  Process data for NOAH Mosaic.  Added
!                        mapping of Noah wind speed and dynamic LAI.  (T. Spero)
!           28 Aug 2018  Enabled Jacobian calculation for hybrid vertical
!                        coordinate in WRF.  (T. Spero)
!           19 Sep 2018  Removed support for MM5v3 input.  Now use netCDF tokens
!                        for missing data.  Changed fill value for "no cumulus
!                        parameterization" from -1 to a large negative number
!                        to prevent inadverent use in CMAQ.  (T. Spero)
!           18 Jun 2019  Added new surface variables with PX LSM that can
!                        improve dust simulation in CCTM.  Added optional
!                        variables from KF convective scheme with radiative
!                        feedbacks.  (T. Spero)
!           13 May 2021  Corrected minor error in assigning bounds for XCLAY_PX
!                        as identified through CMAS Forum. (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE metinfo
  USE xvars
  USE metvars

  IMPLICIT NONE

  INTEGER                           :: c
  INTEGER                           :: ec
  REAL,               PARAMETER     :: epsilonq   = 1.0e-30
  REAL,               PARAMETER     :: epsilonqv  = 1.0e-14
  INTEGER                           :: er
  REAL,               PARAMETER     :: giwrf      = 1.0 / 9.81  ! [s2/m]
  REAL,               PARAMETER     :: icethresh  = 271.36  ! [K]
  INTEGER                           :: ii
  INTEGER                           :: ilu
  INTEGER                           :: jj
  INTEGER                           :: k
  INTEGER                           :: kp1
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'METVARS2CTM'
  REAL                              :: qf
  INTEGER                           :: r
  REAL,               PARAMETER     :: rdovcp     = 2.0 / 7.0  ! Rd / cP
  REAL,               PARAMETER     :: rdwrf      = 287.0  ! [J/kg/K]
  REAL,               PARAMETER     :: rvwrf      = 461.6  ! [J/kg/K]
  INTEGER                           :: sc
  REAL,               PARAMETER     :: smallnum   = 1.0e-7
  INTEGER                           :: sr
  REAL                              :: tf
  REAL,               EXTERNAL      :: vtemp
  REAL                              :: wgt1
  REAL                              :: wgt2
  REAL                              :: z0
  REAL                              :: z1
  REAL                              :: z2

  INTERFACE

    SUBROUTINE layht (xx3face, xx3midl, x3jacobf, x3jacobm, x3htf, x3htm)
      IMPLICIT NONE
      REAL,               INTENT(OUT)   :: x3htf      ( : , : , : )
      REAL,               INTENT(OUT)   :: x3htm      ( : , : , : )
      REAL,               INTENT(IN)    :: x3jacobf   ( : , : , : )
      REAL,               INTENT(IN)    :: x3jacobm   ( : , : , : )
      REAL,               INTENT(IN)    :: xx3face    ( : )
      REAL,               INTENT(IN)    :: xx3midl    ( : )
    END SUBROUTINE layht

  END INTERFACE

!-------------------------------------------------------------------------------
! Land-use based arrays.
!-------------------------------------------------------------------------------

  REAL, PARAMETER :: laimod ( 33 ) = &
    (/  5.0,    5.0,    5.0,    5.0,    5.0,    3.0,    2.0,    2.5,    2.0,   &
        2.5,    3.0,    3.0,    3.0,    3.0,    0.1,    1.0,    0.0,    3.4,   &
        2.4,    1.4,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,   &
        0.0,    0.0,    0.0,    2.2,    2.1,    2.0 /)

  REAL, PARAMETER :: laimnmod ( 33 ) = &
    (/  3.0,    4.0,    1.0,    1.0,    2.0,    1.0,    1.0,    1.0,    1.0,   &
        1.0,    1.0,    0.5,    1.0,    1.0,    0.1,    0.5,    0.0,    2.0,   &
        1.0,    0.1,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,    0.0,   &
        0.0,    0.0,    0.0,    0.7,    0.6,    0.5 /)

  REAL, PARAMETER :: lainlcd50 ( 50 ) = &
    (/  0.0,    0.1,    3.0,    3.0,    3.0,    3.0,    1.0,    0.5,    5.0,   &
        4.0,    5.0,    2.0,    2.5,    2.5,    2.0,    1.0,    1.0,    1.0,   &
        3.0,    3.0,    5.0,    5.0,    3.0,    5.0,    3.0,    2.0,    2.0,   &
        2.0,    1.0,    1.0,    0.0,    4.0,    5.0,    5.0,    5.0,    5.0,   &
        3.0,    2.5,    2.5,    2.0,    2.5,    3.0,    3.0,    3.0,    3.0,   &
        0.1,    1.0,    0.0,    0.0,    0.0 /)

  REAL, PARAMETER :: laimnnlcd50 ( 50 ) = &
    (/  0.0,    0.1,    1.0,    1.0,    1.0,    1.0,    0.5,    0.2,    1.0,   &
        3.0,    2.0,    1.0,    1.0,    1.0,    1.0,    1.0,    1.0,    1.0,   &
        1.0,    0.5,    2.0,    2.0,    1.0,    2.0,    1.0,    1.0,    1.0,   &
        1.0,    0.5,    0.5,    0.0,    3.0,    4.0,    1.0,    1.0,    2.0,   &
        1.0,    1.0,    1.0,    1.0,    1.0,    1.0,    0.5,    1.0,    1.0,   &
        0.1,    0.5,    0.0,    0.0,    0.0 /)

  REAL, PARAMETER :: lainlcd40 ( 40 ) = &
    (/  4.0,    5.0,    5.0,    5.0,    5.0,    3.0,    2.5,    2.5,    2.0,   &
        2.5,    3.0,    3.0,    3.0,    3.0,    0.1,    1.0,    0.0,    0.0,   &
        0.0,    0.0,    0.0,    0.1,    3.0,    3.0,    3.0,    3.0,    1.0,   &
        5.0,    4.0,    5.0,    2.0,    2.5,    2.5,    2.0,    1.0,    1.0,   &
        3.0,    3.0,    5.0,    2.0 /)

  REAL, PARAMETER :: laimnnlcd40 ( 40 ) = &
    (/  3.0,    4.0,    1.0,    1.0,    2.0,    1.0,    1.0,    1.0,    1.0,   &
        1.0,    1.0,    0.5,    1.0,    1.0,    0.1,    0.5,    0.0,    0.0,   &
        0.0,    0.0,    0.0,    0.1,    1.0,    1.0,    1.0,    1.0,    0.5,   &
        1.0,    3.0,    2.0,    1.0,    1.0,    1.0,    1.0,    1.0,    1.0,   &
        1.0,    0.5,    2.0,    1.0 /)

  REAL, PARAMETER :: laimm5 ( 13 ) = &
    (/  2.0,    3.0,    3.0,    5.0,    4.5,    5.0,    0.0,    2.0,    0.5,   &
        1.0,    0.0,    5.0,    2.0 /)

  REAL, PARAMETER :: laiusgs ( 33 ) = &
    (/  2.0,    3.0,    3.0,    3.0,    2.5,    4.0,    2.5,    3.0,    3.0,   &
        2.0,    5.0,    5.0,    5.0,    4.0,    5.0,    0.0,    2.0,    5.0,   &
        0.5,    1.0,    1.0,    1.0,    0.1,    0.1,    0.1,    0.1,    0.1,   &
        0.0,    0.0,    0.0,    2.2,    2.1,    2.0 /)

  REAL, PARAMETER :: laimnusgs ( 33 ) = &
    (/  0.5,    0.5,    0.5,    0.5,    1.0,    1.5,    1.0,    1.0,    1.0,   &
        1.0,    1.0,    1.0,    4.0,    3.0,    2.0,    0.0,    1.0,    3.0,   &
        0.2,    0.5,    0.5,    0.5,    0.1,    0.1,    0.1,    0.1,    0.1,   &
        0.0,    0.0,    0.0,    0.7,    0.6,    0.5 /)

  REAL, PARAMETER :: vegmod ( 33 ) = &
    (/  0.90,   0.95,   0.95,   0.95,   0.95,   0.90,   0.75,   0.80,   0.70,  &
        0.85,   0.75,   0.95,   0.40,   0.95,   0.05,   0.20,   0.00,   0.70,  &
        0.40,   0.20,   0.00,   0.00,   0.00,   0.00,   0.00,   0.00,   0.00,  &
        0.00,   0.00,   0.00,   0.50,   0.45,   0.40 /)

  REAL, PARAMETER :: vegmnmod ( 33 ) = &
    (/  0.80,   0.85,   0.50,   0.50,   0.60,   0.50,   0.50,   0.60,   0.50,  &
        0.60,   0.45,   0.10,   0.20,   0.40,   0.02,   0.05,   0.00,   0.50,  &
        0.20,   0.05,   0.00,   0.00,   0.00,   0.00,   0.00,   0.00,   0.00,  &
        0.00,   0.00,   0.00,   0.30,   0.25,   0.20 /)

  REAL, PARAMETER :: vegnlcd50 ( 50 ) = &
    (/  0.00,   0.05,   0.90,   0.70,   0.40,   0.15,   0.20,   0.15,   0.95,  &
        0.90,   0.95,   0.50,   0.75,   0.85,   0.80,   0.80,   0.80,   0.50,  &
        0.95,   0.95,   0.90,   0.90,   0.90,   0.90,   0.90,   0.60,   0.80,  &
        0.80,   0.60,   0.60,   0.00,   0.90,   0.95,   0.95,   0.95,   0.95,  &
        0.90,   0.75,   0.80,   0.70,   0.85,   0.75,   0.95,   0.40,   0.95,  &
        0.05,   0.20,   0.00,   0.00,   0.00 /)

  REAL, PARAMETER :: vegmnnlcd50 ( 50 ) = &
    (/  0.00,   0.02,   0.80,   0.60,   0.30,   0.05,   0.05,   0.05,   0.50,  &
        0.80,   0.60,   0.20,   0.50,   0.60,   0.20,   0.20,   0.20,   0.20,  &
        0.80,   0.10,   0.80,   0.80,   0.80,   0.80,   0.80,   0.40,   0.40,  &
        0.40,   0.20,   0.20,   0.00,   0.80,   0.85,   0.50,   0.50,   0.60,  &
        0.50,   0.50,   0.60,   0.50,   0.60,   0.45,   0.10,   0.20,   0.40,  &
        0.02,   0.05,   0.00,   0.00,   0.00 /)

  REAL, PARAMETER :: vegnlcd40 ( 40 ) = &
    (/  0.90,   0.95,   0.95,   0.95,   0.95,   0.90,   0.75,   0.80,   0.70,  &
        0.85,   0.75,   0.95,   0.40,   0.95,   0.05,   0.20,   0.00,   0.00,  &
        0.00,   0.00,   0.00,   0.05,   0.90,   0.70,   0.40,   0.15,   0.20,  &
        0.95,   0.90,   0.95,   0.50,   0.75,   0.85,   0.80,   0.80,   0.80,  &
        0.95,   0.95,   0.90,   0.60 /)

  REAL, PARAMETER :: vegmnnlcd40 ( 40 ) = &
    (/  0.80,   0.85,   0.50,   0.50,   0.60,   0.50,   0.50,   0.60,   0.50,  &
        0.60,   0.45,   0.10,   0.20,   0.40,   0.02,   0.05,   0.00,   0.00,  &
        0.00,   0.00,   0.00,   0.02,   0.80,   0.60,   0.30,   0.05,   0.05,  &
        0.50,   0.80,   0.60,   0.20,   0.50,   0.60,   0.20,   0.20,   0.20,  &
        0.80,   0.10,   0.80,   0.40 /)

  REAL, PARAMETER :: vegmm5 ( 13 ) = &
    (/  0.40,   0.95,   0.95,   0.95,   0.95,   0.95,   0.00,   0.75,   0.10,  &
        0.20,   0.00,   0.90,   0.80 /)

  REAL, PARAMETER :: vegusgs ( 33 ) = &
    (/  0.40,   0.95,   0.95,   0.95,   0.95,   0.95,   0.95,   0.70,   0.85,  &
        0.80,   0.95,   0.95,   0.95,   0.90,   0.95,   0.00,   0.60,   0.90,  &
        0.10,   0.20,   0.30,   0.20,   0.05,   0.05,   0.02,   0.02,   0.02,  &
        0.00,   0.00,   0.00,   0.50,   0.45,   0.40 /)

  REAL, PARAMETER :: vegmnusgs ( 33 ) = &
    (/  0.20,   0.15,   0.10,   0.15,   0.35,   0.40,   0.70,   0.50,   0.60,  &
        0.60,   0.50,   0.50,   0.85,   0.80,   0.60,   0.00,   0.40,   0.80,  &
        0.05,   0.10,   0.10,   0.05,   0.02,   0.02,   0.01,   0.01,   0.01,  &
        0.00,   0.00,   0.00,   0.30,   0.25,   0.20 /)

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNKNOWN LAND USE INPUT DATA SOURCE', &
    & /, 1x, '***   DATA SOURCE IS ', a, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Put time-variant cross-point arrays on MCIP_X grid.
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x - 1
  sr = y0
  er = y0 + nrows_x - 1

  xtempg (:,:) = groundt (sc:ec,sr:er)
  IF ( met_model == 2 .AND. met_cumulus == 0 ) THEN  ! no cumulus scheme
    xrainc (:,:) = fillreal  ! fill with negative values as flag for CMAQv5.0
  ELSE
    xrainc (:,:) = raincon (sc:ec,sr:er)  ! rain is already incremental here
  ENDIF
  xrainn (:,:) = rainnon (sc:ec,sr:er)    ! rain is already incremental here
  xpbl   (:,:) = zpbl    (sc:ec,sr:er)
  xhfx   (:,:) = hfx     (sc:ec,sr:er)
  xlh    (:,:) = lh      (sc:ec,sr:er)
  xustar (:,:) = ust     (sc:ec,sr:er)
  xrgrnd (:,:) = rgrnd   (sc:ec,sr:er)
  xglw   (:,:) = glw     (sc:ec,sr:er)
  xzruf  (:,:) = znt     (sc:ec,sr:er)
  xalbedo(:,:) = albedo  (sc:ec,sr:er)
  xsnocov(:,:) = snowcovr(sc:ec,sr:er)
  xsnowh (:,:) = snowh   (sc:ec,sr:er)

  xgsw   (:,:) = xrgrnd(:,:) * ( 1.0 - xalbedo(:,:) )

  IF ( ift2m ) THEN
    xtemp2(:,:) = t2     (sc:ec,sr:er)
  ELSE
!  ~ Calculate in PBLSUP.
  ENDIF

  IF ( ifq2m ) THEN
    xq2   (:,:) = q2     (sc:ec,sr:er)
  ELSE
    xq2   (:,:) = qva    (sc:ec,sr:er,1)  ! fill Q2 with QV from layer 1
  ENDIF

  IF ( ifw10m ) THEN
    xu10 (:,:) = u10     (sc:ec,sr:er)
    xv10 (:,:) = v10     (sc:ec,sr:er)
  ELSE
!  ~ Calculate in PBLSUP.
  ENDIF

  IF ( ifmolpx ) THEN
    xqfx(:,:) = qfx(sc:ec,sr:er)
  ENDIF

  IF ( iflai .AND. ifpxwrf41 ) THEN
      xlai(:,:) = lai_px(sc:ec,sr:er)
  ELSE IF ( iflai  .AND. ( MAXVAL(lai) > smallnum ) ) THEN
      xlai(:,:) = lai(sc:ec,sr:er)
  ELSE

    IF ( ( met_model == 2 ) .AND. ( met_soil_lsm == 2 ) ) THEN
      xlai(:,:) = 4.0
    ELSE

      DO c = 1, ncols_x
        ii = x0 + c - 1
        DO r = 1, nrows_x
          jj = y0 + r - 1

          ilu = landuse(ii,jj)

          IF ( met_lu_src(1:3) == "USG" ) THEN       ! USGS 24- or 33-cat. l.u.
            IF ( met_season == 1 ) THEN         ! summer
              xlai (c,r) = laiusgs(ilu)
            ELSE IF ( met_season == 2 ) THEN    ! winter
              xlai (c,r) = laimnusgs(ilu)
            ENDIF
          ELSE IF ( met_lu_src(1:3) == "OLD" ) THEN  ! MM5 13-category old l.u.
            xlai   (c,r) = laimm5(ilu)
          ELSE IF ( met_lu_src(4:6) == "D50" ) THEN  ! NLCD-MODIS 50-cat. l.u.
            IF ( met_season == 1 ) THEN         ! summer
              xlai (c,r) = lainlcd50(ilu)
            ELSE IF ( met_season == 2 ) THEN    ! winter
              xlai (c,r) = laimnnlcd50(ilu)
            ENDIF
          ELSE IF ( met_lu_src(4:6) == "D40" ) THEN  ! NLCD-MODIS 40-cat. l.u.
            IF ( met_season == 1 ) THEN         ! summer
              xlai (c,r) = lainlcd40(ilu)
            ELSE IF ( met_season == 2 ) THEN    ! winter
              xlai (c,r) = laimnnlcd40(ilu)
            ENDIF
          ELSE IF ( met_lu_src(1:3) == "MOD" ) THEN  ! MODIS-NOAH 20/33-cat.
            IF ( met_season == 1 ) THEN         ! summer
              xlai (c,r) = laimod(ilu)
            ELSE IF ( met_season == 2 ) THEN    ! winter
              xlai (c,r) = laimnmod(ilu)
            ENDIF
          ELSE
            WRITE (*,f9000) TRIM(pname), TRIM(met_lu_src)
            CALL graceful_stop (pname)
          ENDIF

        ENDDO
      ENDDO
    ENDIF

  ENDIF  ! iflai

  IF ( ifveg ) THEN
    xveg   (:,:) = veg   (sc:ec,sr:er)
  ELSE
    DO c = 1, ncols_x
      ii = x0 + c - 1
      DO r = 1, nrows_x
        jj = y0 + r - 1

        ilu = landuse(ii,jj)

        IF ( met_lu_src(1:3) == "USG" ) THEN       ! USGS 24- or 33-cat. l.u.
          IF ( met_season == 1 ) THEN         ! summer
            xveg (c,r) = vegusgs(ilu)
          ELSE IF ( met_season == 2 ) THEN    ! winter
            xveg (c,r) = vegmnusgs(ilu)
          ENDIF
        ELSE IF ( met_lu_src(1:3) == "OLD" ) THEN  ! MM5 13-category old l.u.
          xveg   (c,r) = vegmm5(ilu)
        ELSE IF ( met_lu_src(4:6) == "D50" ) THEN  ! NLCD-MODIS 50-cat. l.u.
          xveg   (c,r) = vegnlcd50(ilu)
        ELSE IF ( met_lu_src(4:6) == "D40" ) THEN  ! NLCD-MODIS 40-cat. l.u.
          xveg   (c,r) = vegnlcd40(ilu)
        ELSE IF ( met_lu_src(1:3) == "MOD" ) THEN  ! MODIS-NOAH 20/33-cat.
          xveg   (c,r) = vegmod(ilu)
        ELSE
          WRITE (*,f9000) TRIM(pname), TRIM(met_lu_src)
          CALL graceful_stop (pname)
        ENDIF

      ENDDO
    ENDDO
  ENDIF

  IF ( ifresist ) THEN

    xradyn (:,:) = ra    (sc:ec,sr:er)
    xrstom (:,:) = rstom (sc:ec,sr:er)

    ! At water points, stomatal resistance is 0.0.  Since
    ! inverse of XRSTOM is output in ctmproc.f90, need to avoid
    ! division by 0.0.  Use netCDF token as place-holder.

    WHERE ( xrstom == 0.0 ) xrstom = fillreal

  ELSE
!  ~ Calculate in RESISTCALC.
  ENDIF

  IF ( ifmol ) THEN
    xmol (:,:)    = mol (sc:ec,sr:er)
  ELSE
!  ~ Calculate in PBLSUP.
  ENDIF

  IF ( ifwr ) THEN
    xwr  (:,:)    = wr (sc:ec,sr:er)
    IF ( met_model == 2 ) THEN  ! WRF: divide by water density
      xwr(:,:) = xwr(:,:) * 0.001  ! kg/m2 -> m
    ENDIF
  ELSE
!  ~ Approximate in M3DRY and do not output.
  ENDIF

  IF ( .NOT. needseaice ) THEN
    xseaice(:,:) = seaice(sc:ec,sr:er)
  ELSE
    DO c = 1, ncols_x
      DO r = 1, nrows_x
        IF ( ( NINT(xlwmask(c,r)) == 0 ) .AND.  &
             ( xtempg(c,r) <= icethresh ) ) THEN  ! very cold water
          xseaice(c,r) = 1.0  ! ice
        ELSE
          xseaice(c,r) = 0.0  ! not ice
        ENDIF
      ENDDO
    ENDDO
  ENDIF

  IF ( ifsoil ) THEN
    xw2a   (:,:)   =        w2    (sc:ec,sr:er)
    xwga   (:,:)   =        wg    (sc:ec,sr:er)
    xsltyp (:,:)   = FLOAT( isltyp(sc:ec,sr:er) )
    xtga   (:,:)   =        soilt1(sc:ec,sr:er)
    xt2a   (:,:)   =        soilt2(sc:ec,sr:er)
    xsoim3d(:,:,:) =        soim3d(sc:ec,sr:er,:)
    xsoit3d(:,:,:) =        soit3d(sc:ec,sr:er,:)
  ELSE
!  ~ Downstream options that request these fields cannot be invoked
!  ~ as they will not be in the output.
  ENDIF

  IF ( ifmosaic ) THEN
    xlai_mos(:,:,:) = lai_mos(sc:ec,sr:er,:)
    xrs_mos (:,:,:) =  rs_mos(sc:ec,sr:er,:)
    xtsk_mos(:,:,:) = tsk_mos(sc:ec,sr:er,:)
    xznt_mos(:,:,:) = znt_mos(sc:ec,sr:er,:)
    xwspdsfc(:,:)   = wspdsfc(sc:ec,sr:er)
    xxlaidyn(:,:)   = xlaidyn(sc:ec,sr:er)
  ENDIF

  IF ( ifpxwrf41 ) THEN
    xwwlt_px(:,:)   = wwlt_px  (sc:ec,sr:er)
    xwfc_px(:,:)    = wfc_px   (sc:ec,sr:er)
    xwsat_px(:,:)   = wsat_px  (sc:ec,sr:er)
    xcsand_px(:,:)  = csand_px (sc:ec,sr:er)
    xfmsand_px(:,:) = fmsand_px(sc:ec,sr:er)
    xclay_px(:,:)   = clay_px  (sc:ec,sr:er)
  ENDIF

  IF ( lpv > 0 ) THEN
    xcorl  (:,:)  = coriolis(sc:ec,sr:er)
  ENDIF

  xtempm (:,:,:)  = ta (sc:ec,sr:er,:)
  xwvapor(:,:,:)  = qva(sc:ec,sr:er,:)
  xcldwtr(:,:,:)  = qca(sc:ec,sr:er,:)
  xranwtr(:,:,:)  = qra(sc:ec,sr:er,:)
  xqice  (:,:,:)  = qia(sc:ec,sr:er,:)
  xqsnow (:,:,:)  = qsa(sc:ec,sr:er,:)
  xqgraup(:,:,:)  = qga(sc:ec,sr:er,:)

  IF ( ifkfradextras ) THEN
    xqc_cu  (:,:,:)  = qc_cu    (sc:ec,sr:er,:)
    xqi_cu  (:,:,:)  = qi_cu    (sc:ec,sr:er,:)
    xcldfrad(:,:,:)  = cldfra_dp(sc:ec,sr:er,:)
    xcldfras(:,:,:)  = cldfra_sh(sc:ec,sr:er,:)
  ENDIF

  xwwind (:,:,0:) = wa(sc:ec,sr:er,1:)

  IF ( ( iftke ) .AND. ( .NOT. iftkef ) ) THEN  ! TKE on half-layers
    xtke   (:,:, :) = tke(sc:ec,sr:er, :)
  ELSE IF ( ( iftke ) .AND. ( iftkef ) ) THEN   ! TKE on full-levels
    xtke   (:,:,0:) = tke(sc:ec,sr:er,1:)
  ENDIF

  ! Ensure that very small (and sometimes negative!) values from WRF moisture
  ! fields are not used.  Here, EPSILONQ is the same minimum value as is set
  ! in ctmproc.f90.  Floor value for XWVAPOR (EPSILONQV) is based on MM5 value.

  WHERE ( xwvapor < epsilonqv ) xwvapor = epsilonqv
  WHERE ( xcldwtr < epsilonq  ) xcldwtr = 0.0
  WHERE ( xranwtr < epsilonq  ) xranwtr = 0.0
  WHERE ( xqice   < epsilonq  ) xqice   = 0.0
  WHERE ( xqsnow  < epsilonq  ) xqsnow  = 0.0
  WHERE ( xqgraup < epsilonq  ) xqgraup = 0.0

  IF ( ifkfradextras ) THEN
    WHERE ( xqc_cu  < epsilonq  ) xqc_cu  = 0.0
    WHERE ( xqi_cu  < epsilonq  ) xqi_cu  = 0.0
  ENDIF

!-------------------------------------------------------------------------------
! Put time-variant dot-point arrays on MCIP_X grid.  XUU_D and XVV_D are on
! B-grid (dot points).  XUU_S and XVV_T are on C-grid (face points).
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x
  sr = y0
  er = y0 + nrows_x

  IF ( met_model == 2 ) THEN  ! WRF: UA and VA on C-grid (face points)

    xuu_d(:,1,        :) = ua(sc:ec,sr,:)
    xuu_d(:,2:nrows_x,:) = 0.5 * (ua(sc:ec,sr:er-2,:) + ua(sc:ec,sr+1:er-1,:))
    xuu_d(:,nrows_x+1,:) = ua(sc:ec,er-1,:)
    xvv_d(1,        :,:) = va(sc,sr:er,:)
    xvv_d(2:ncols_x,:,:) = 0.5 * (va(sc:ec-2,sr:er,:) + va(sc+1:ec-1,sr:er,:))
    xvv_d(ncols_x+1,:,:) = va(ec-1,sr:er,:)

    xuu_s(:,:,:)         = ua(sc:ec,sr:er,:)
    xvv_t(:,:,:)         = va(sc:ec,sr:er,:)

  ENDIF

!------------------------------------------------------------------------------
! Compute pressure and potential temperature (if necessary).  Put cloud
! fraction on transfer grid, if available.
!------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x - 1
  sr = y0
  er = y0 + nrows_x - 1

  IF ( met_model == 2 ) THEN  ! WRF

    xpresm(:,:,:) = pb (sc:ec,sr:er,:) + pp(sc:ec,sr:er,:)
    xmu   (:,:)   = mub(sc:ec,sr:er)   + mu(sc:ec,sr:er)
    xgeof (:,:,:) = phb(sc:ec,sr:er,:) + ph(sc:ec,sr:er,:)

    ! As of WRFv2.0.3.1, PSFC = 0.0 at WRF initial time.

    IF ( MINVAL(psa) > 0.0 ) THEN
      xprsfc(:,:) = psa(sc:ec,sr:er)  ! PSA contains PSFC for WRF
    ELSE
      DO c = 1, ncols_x
        DO r = 1, nrows_x
          z0   = xgeof(c,r,0)   ! Z0, Z1, Z2 really defined as Zx/grav
          z1   = 0.5 * ( xgeof(c,r,0) + xgeof(c,r,1) )
          z2   = 0.5 * ( xgeof(c,r,1) + xgeof(c,r,2) )
          wgt1 = (z2 - z0) / (z2 - z1)
          wgt2 = 1.0 - wgt1
          xprsfc(c,r) = wgt1 * xpresm(c,r,1) + wgt2 * xpresm(c,r,2)
        ENDDO
      ENDDO
    ENDIF

    xpresf(:,:,0) = xprsfc(:,:)

    ! Calculate full-level pressure from geopotential and hydrostatic equation.
    ! Assume temperature at mid-layers is sufficient "average" between full
    ! levels.

    DO k = 1, metlay
      xpresf(:,:,k) = xpresf(:,:,k-1) *  &
                      EXP( (xgeof(:,:,k-1) - xgeof(:,:,k)) /  &
                           (rdwrf * xtempm(:,:,k)) )
    ENDDO

    IF ( lpv > 0 .OR. ifmolpx ) THEN  ! need theta
      xtheta(:,:,:) = theta(sc:ec,sr:er,:)
    ENDIF

    IF ( ifcld3d ) THEN  ! passing through 3D cloud fraction
      xcfrac3d(:,:,:) = cldfra(sc:ec,sr:er,:)
    ENDIF

  ENDIF

!------------------------------------------------------------------------------
! Compute density.
! IF using WRF output and if canopy wetness is in the output, convert from
! kg m^-2 to m by dividing by density.
!------------------------------------------------------------------------------

  IF ( met_model == 2 ) THEN  ! WRF

    DO k = 1, metlay
      kp1 = MIN(k+1,metlay)

      DO c = 1, ncols_x
        DO r = 1, nrows_x

          ! Use formula for "alt" (total inverse density, alpha) from WRF's
          ! module_initialize_real.F, and replace potential temperature with
          ! temperature using Poisson's equation.  Density is 1./alt.  Note
          ! that this computed density matches well (to 6 or so decimal places)
          ! with WRF's prognostic density, expressed as 1./(alb+al); alb and al
          ! are not in the default WRF Registry as output to the history file
          ! (as of v2.0.3.1).  It only matches to 3 decimal places with 1./alt
          ! when alt is output in the WRF history file.

          xdensam(c,r,k) = ( xpresm(c,r,k) / ( rdwrf * xtempm(c,r,k) *  &
                             (1.0 + rvwrf*xwvapor(c,r,k)/rdwrf) ) )

          tf = 0.5 * (xtempm (c,r,k) + xtempm (c,r,kp1))
          qf = 0.5 * (xwvapor(c,r,k) + xwvapor(c,r,kp1))

          xdensaf(c,r,k) = ( xpresf(c,r,k) / ( rdwrf * tf *  &
                             (1.0 + rvwrf*qf/rdwrf) ) )

        ENDDO
      ENDDO
    ENDDO

    IF ( ( ift2m ) .AND. ( MAXVAL(xtemp2) > smallnum ) ) THEN  ! T2 = 0 at init
      xdensaf(:,:,0) = ( xpresf(:,:,0) / ( rdwrf * xtemp2(:,:) *      &
                         (1.0 + rvwrf*xwvapor(:,:,1)/rdwrf) ) )
    ELSE
      xdensaf(:,:,0) = ( xpresf(:,:,0) / ( rdwrf * xtempm(:,:,1) *  &
                         (1.0 + rvwrf*xwvapor(:,:,1)/rdwrf) ) )
    ENDIF
    xdenss(:,:) = xdensaf(:,:,0)

  ENDIF

  xdenswm(:,:,:) = xdensam(:,:,:) * xwvapor(:,:,:) / ( 1.0 + xwvapor(:,:,:) )

!-------------------------------------------------------------------------------
! If input meteorology has a time-varying vertical coordinate, compute Jacobian
! and layer heights.
!-------------------------------------------------------------------------------

  IF ( met_model == 2 ) THEN

    IF ( met_hybrid >= 0 ) THEN
      DO k = 0, metlay
        ! Adjust mu (a.k.a., ps - ptop) for hybrid coordinate.
        ! Calculate Jacobian from WRF relation:
        !   J*g = - d(phi)/d(eta) = - d(g z)/d(eta) = mu alpha = mu/rho
        xmuhyb(:,:)     = c1f(k+1) * xmu(:,:) + c2f(k+1)
        x3jacobf(:,:,k) = giwrf  * xmuhyb(:,:) / xdensaf(:,:,k)
        IF ( k == 0 ) CYCLE
        xmuhyb(:,:)     = c1h(k) * xmu(:,:) + c2h(k)
        x3jacobm(:,:,k) = giwrf  * xmuhyb(:,:) / xdensam(:,:,k)
      ENDDO
    ELSE
      DO k = 0, metlay
        ! Calculate Jacobian from WRF relation:
        !   J*g = - d(phi)/d(eta) = - d(g z)/d(eta) = mu alpha = mu/rho
        x3jacobf(:,:,k) = giwrf * xmu(:,:) / xdensaf(:,:,k)
        IF ( k == 0 ) CYCLE
        x3jacobm(:,:,k) = giwrf * xmu(:,:) / xdensam(:,:,k)
      ENDDO
    ENDIF

    CALL layht  (xx3face, xx3midl, x3jacobf, x3jacobm, x3htf, x3htm)

  ENDIF

!-------------------------------------------------------------------------------
! Calculate height differences.
!-------------------------------------------------------------------------------

  DO k = 1, metlay
    xdx3htf(:,:,k) = x3htf(:,:,k) - x3htf(:,:,k-1)  ! X3HTF starts at 0 in vert
  ENDDO

!-------------------------------------------------------------------------------
! Calculate contravariant velocity (w-component).
!-------------------------------------------------------------------------------

  IF ( met_model == 2 ) THEN  ! WRF-ARW
    CALL vertnhy_wrf
  ENDIF

!-------------------------------------------------------------------------------
! Calculate depths of soil layers.
!-------------------------------------------------------------------------------

  IF ( met_model == 2 ) THEN  ! WRF-ARW
    IF ( met_ns > 0 ) THEN
      DO k = 1, met_ns
        xzsoil(k) = 0.0 - (SUM(dzs(1:k)))  ! m
      ENDDO
    ENDIF
  ENDIF

END SUBROUTINE metvars2ctm
