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

SUBROUTINE setgriddefs

!-------------------------------------------------------------------------------
! Name:     Set Grid Definitions
! Purpose:  Gets information from user input and input meteorology, and
!           sets up grid definitions.
! Notes:    Some algorithms taken from MCIP v1 getmet_mm5.F.
! Revised:  20 Sep 2001  Original version.  (T. Otte)
!           16 Oct 2001  Added variable COORDNAM.  Corrected definition
!                        of P_GAM_GD.  Added provisions for windowing a
!                        subset of the meteorology domain.  (T. Otte)
!           20 Nov 2001  Corrected setting of XORIG_GD and YORIG_GD.  (T. Otte)
!           10 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           27 Feb 2002  Removed minimum grid size on windows.  (T. Otte)
!           06 Mar 2003  Modified definitions of XORIG_GD and Y0RIG_GD to
!                        minimize real number round-off issues which can
!                        cause header mismatches in CMAQ.  Added double
!                        precision attributes to variables assigned to
!                        I/O API header.  (T. Otte)
!           10 Aug 2004  Added new grid projections.  Moved definition of
!                        PX to setupv2 and setupv3.  Added T2OUT.  Removed
!                        checks from obsolete land-use input sources. (T. Otte)
!           24 Feb 2005  Removed NDX and option to interpolate to finer scale
!                        meteorology.  Changed I and J naming conventions to
!                        Y and X to make code more general.  Added vertical
!                        coordinate definition for WRF.  Modified prints of
!                        user options to reflect new LDDEP = 3 or 4 for dry
!                        deposition velocity calculations of chlorine and
!                        mercury species with M3DRY.  Added calculation of
!                        XORIG and YORIG for WRF grid definitions.  Added logic
!                        to make center latitude more precise for WRF. (T. Otte)
!           15 Jul 2005  Added debugging for variable retrievals from WRF
!                        files.  Added error-exit for problems defining YORIG
!                        for WRF.  Added provisions for defining WRF XORIG and 
!                        YORIG when dot-point lower-left corner coordinate is
!                        not available in WRF output.  (T. Otte)
!           11 Aug 2005  Removed unused variable X_RESOL.  (T. Otte)
!           27 Feb 2006  Changed tolerances on XORIG_M and YORIG_M for WRF to
!                        allow center of projection be at a face point or scalar
!                        point, rather than only on dot points.  Updated
!                        vertical coordinate definitions to correspond with
!                        updates to I/O API PARMS3.EXT for WRF.  Corrected
!                        I/O API grid definition for Mercator projection.
!                        (T. Otte)
!           07 Apr 2006  Corrected settings of P_ALP_GD, P_BET_GD, and
!                        P_GAM_GD for Mercator projection...again.  (T. Otte)
!           12 May 2006  Corrected setting of GDTYP_GD for polar stereographic
!                        projection.  Revised definitions of I/O API header
!                        variables for WRF-based domains.  Corrected 
!                        calculation of print column and row for center of
!                        grid.  (T. Otte)
!           20 Jun 2006  Corrected operations involving double-precision
!                        variables XCELL_GD and YCELL_GD.  (T. Otte)
!           31 Jul 2007  Added IMPLICIT NONE.  Eliminated prints for former
!                        user-definable run options to recalculate PBL, cloud,
!                        and radiation.  Removed I/O API header settings for
!                        MM5v2-based vertical coordinate.  Removed T2OUT.
!                        Added prints to show whether or not some variables
!                        were part of input meteorology.  Added print statements
!                        to show whether or not some meteorological fields were
!                        found in the input file; those that are not found may
!                        be calculated.  Updated computation of LPRT_COL and
!                        LPRT_ROW for a one-cell domain.  Changed code so that
!                        M3Dry with chlorine and mercury is the only option to
!                        compute dry deposition velocities in MCIP.  Removed
!                        dependencies on modules FILE and WRF_NETCDF.  (T. Otte)
!           06 May 2008  Changed settings for XCENT_GD, YCENT_GD, XORIG_GD, and
!                        YORIG_GD for WRF for Lambert conformal projection so
!                        that headers will be compatible with M3IO utility
!                        routines LL2LAM and LAM2LL.  (Still need to test
!                        settings for polar stereographic and Mercator for
!                        WRF.)  Removed NTHIKD and NBNDYD.  Added error-checking
!                        to prevent NTHIK=0.  Added print statements to log file
!                        to indicate whether or not each of Q2 and TKE were
!                        found in the MM5 or WRF file.  Added print statements
!                        to log file to indicate whether or not UAH cloud field
!                        adjustment for photolysis has been invoked, and to
!                        indicate whether or not the urban canopy model has
!                        been invoked.  (T. Otte)
!           26 Nov 2008  Changed setting for YCENT_GD (and, thus, XORIG_GD and
!                        YORIG_GD) for WRF for Lambert conformal projection so
!                        that the headers will not fail in the Spatial
!                        Allocator.  Reference latitude now is set to the
!                        average of the true latitudes for the secant Lambert
!                        conformal case.  (Still need to test settings for polar
!                        stereographic and Mercator for WRF.)  (T. Otte)
!           23 Dec 2008  Added user-definable reference latitude for WRF
!                        Lambert conformal data sets.  Best used for
!                        consistency with existing MM5 data sets.  (T. Otte)
!           28 Apr 2009  Changed setting of XCENT and YCENT for polar
!                        stereographic WRF domains.  (T. Otte)
!           28 Oct 2009  Changed MET_UCMCALL to MET_URBAN_PHYS, and allowed
!                        for variable to be set to be greater than 1.  Removed
!                        setting of reference latitude for Lambert conformal
!                        because it is now done in SETUP_MM5V3 and SETUP_WRFEM.
!                        Remove subroutine GRIDGEOMETRY, and use dot-point
!                        latitude array directly to fill Mercator reference
!                        point information for I/O API headers.  Changed
!                        XORIG and YORIG truncation logic to allow for grid
!                        cells to be increments of half of a grid cell removed
!                        (rather than limiting to a whole grid cell).  Changed
!                        logic (again) to define I/O API projection parameters.
!                        Added user option to output vertical velocity predicted
!                        by the meteorological model rather than output it by
!                        default.  Changed format on print statements in MCIP
!                        log file so that larger numbers in XORIG3D and YORIG3D
!                        can be accommodated and more precision is given after
!                        the decimal point.  (T. Otte)
!           12 Feb 2010  Removed unused variables CNTRX, CNTRY, YLAT, and YLON,
!                        and removed unused format 9800.  (T. Otte)
!           09 Sep 2010  Removed option to compute dry deposition velocities
!                        in MCIP.  (T. Otte)
!           11 Aug 2011  Replaced module PARMS3 with I/O API module M3UTILIO.
!                        (T. Otte)
!           31 Aug 2011  Removed dependency on netcdf.inc, and improved error
!                        handling.  Changed F77 character definitions to F90
!                        standard.  (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           10 Apr 2015  Updated log file to indicate if 3D resolved cloud
!                        fraction will be part of output.  (T. Spero)
!           21 Aug 2015  Updated log file to indicate if WRF/ACM2 was used and
!                        Monin-Obukhov length will be recalculated by MCIP to
!                        be consistent with other ACM2 physics in WRF.
!                        (T. Spero)
!           17 Sep 2015  Changed IFMOLACM to IFMOLPX.  (T. Spero)
!           23 Jun 2017  Added MET_HYBRID setting to log output.  (T. Spero)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE metinfo
  USE coord
  USE m3utilio
  USE sat2mcip

  IMPLICIT NONE

  CHARACTER(LEN=60)                 :: option
  CHARACTER(LEN=16),  PARAMETER     :: pname     = 'SETGRIDDEFS'
  REAL,               PARAMETER     :: pole      = 90.0  ! degrees
  REAL                              :: rnthik
  REAL                              :: xorig_ctm
  REAL                              :: xorig_m
  REAL                              :: xorig_x
  REAL                              :: xtemp
  CHARACTER(LEN=4)                  :: yesno
  REAL                              :: yorig_ctm
  REAL                              :: yorig_m
  REAL                              :: yorig_x
  REAL                              :: ytemp

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f6000 = "(/, 1x, 'Output start date = ', a, &
    & /, 1x, 'Output end date   = ', a, /, 1x, 'Output interval   = ', i3, &
    & ' minutes', // )"

  CHARACTER(LEN=256), PARAMETER :: f6100 = "(1x, a, ' = ', i3, ':  ', a)"

  CHARACTER(LEN=256), PARAMETER :: f6150 = &
      "(/, 1x, a, ' was ', a, ' found in the meteorology input file')"

  CHARACTER(LEN=256), PARAMETER :: f6155 = &
      "(/, 1x, a, ' will ', a, ' be recomputed by MCIP (Only for WRF-ACM2)')"

  CHARACTER(LEN=256), PARAMETER :: f6160 = &
      "(1x, a, ' will be read from the ', a, ' file')"

  CHARACTER(LEN=256), PARAMETER :: f6175 = &
      "(1x, a, ' will ', a, ' be in the output file')"

  CHARACTER(LEN=256), PARAMETER :: f6180 = &
      "(/, 1x, a, ' was ', a, ' used in the meteorology model')"

  CHARACTER(LEN=256), PARAMETER :: f6200 = &
      "(1x, a, ' domain dimensions (col, row, lay):', 3(2x, i3))"

  CHARACTER(LEN=256), PARAMETER :: f6300 = "(4x, a, 1x, a, 3x, i4, 2x, i4)"
  CHARACTER(LEN=256), PARAMETER :: f6400 = "(4x, a, 2x, i14)"
  CHARACTER(LEN=256), PARAMETER :: f6500 = "(4x, a, 2x, a)"
  CHARACTER(LEN=256), PARAMETER :: f6600 = "(4x, a, 2x, f14.3)"

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   NTHIK cannot be set to zero', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9025 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   MCIP lower-left corner is not in met domain', &
    & /, 1x, '***   X0, Y0 = ', 2(2x, i4), &
    & /, 1x, '***   NX, NY = ', 2(2x, i4), &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9050 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   MCIP domain must have 1 or more cells per side', &
    & /, 1x, '***   NCOLS, NROWS = ', 2(2x, i4), &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Requested MCIP X domain exceeds met domain', &
    & /, 1x, '***   METCOL, METROW = ', i4, 2x, i4, &
    & /, 1x, '***   MCIP domain (col, row) = ', i4, 2x, i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Requested MCIP dim.: ', i4, ' x ', i4, &
    & /, 1x, '***   Met. dim.: ', i4, ' x ', i4, &
    & /, 1x, '***   Max. allowable dim.: ', i4, ' x ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9250 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Requested MCIP dim. exceed the actual Met. dim.', &
    & /, 1x, '***   Input meteorology dimensions: ', 2(2x, i4), &
    & /, 1x, '***   MCIP output domain in terms of met: ', 2(2x, i4), &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9275 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   INVALID INPUT METEOROLOGY MAP PROJECTION ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '      MCIP output interval cannot be shorter than input', &
    & /, 1x, '      User-defined MCIP output interval = ', i3, &
    & /, 1x, '      Meteorology output interval       = ', f5.1, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   MCIP start date is earlier than input start time', &
    & /, 1x, '***   User-defined MCIP start date = ', a, &
    & /, 1x, '***   Input meteorology start date = ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9600 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   Diagnostic print cell is outside domain', &
    & /, 1x, '***   Input LPRT_COL and LPRT_ROW are ', i4, 2x, i4, &
    & /, 1x, '***   Output domain NCOLS and NROWS are ', i4, 2x, i4, &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Define MCIP grid coordinate information from meteorology grid input.
!-------------------------------------------------------------------------------

  metcol = met_nx
  metrow = met_ny
  metlay = met_nz

  IF ( nthik == 0 ) THEN
    WRITE (*,f9000) TRIM(pname)
    CALL graceful_stop (pname)
  ENDIF

  IF ( nbdrytrim >= 0 ) THEN  ! not windowing...need to define NCOLS, NROWS
    ncols = met_nx - (2 * nbdrytrim) - (2 * nthik) - 1
    nrows = met_ny - (2 * nbdrytrim) - (2 * nthik) - 1
  ENDIF

  nrows_x = nrows + 2 * nthik
  ncols_x = ncols + 2 * nthik

  nbndy   = 2 * nthik * (ncols + nrows + 2*nthik)

!-------------------------------------------------------------------------------
! Check dimensions of domain.
!-------------------------------------------------------------------------------

  IF ( ( x0 < 1          ) .OR. ( y0 < 1          ) .OR.  &
       ( x0 > met_nx - 1 ) .OR. ( y0 > met_ny - 1 ) ) THEN
    WRITE (*,f9025) TRIM(pname), x0, y0, met_nx, met_ny
    CALL graceful_stop (pname)
  ENDIF

  IF ( ( ncols < 1 ) .OR. ( nrows < 1 ) ) THEN
    WRITE (*,f9050) TRIM(pname), ncols, nrows
    CALL graceful_stop (pname)
  ENDIF

  IF ( ( met_nx < (ncols_x + 1) ) .OR.  &
       ( met_ny < (nrows_x + 1) ) ) THEN
    WRITE (*,f9100) TRIM(pname), met_nx, met_ny, ncols_x + 1, nrows_x + 1
    CALL graceful_stop (pname)
  ENDIF

  IF ( ( ncols > met_nx-2*nthik-1 ) .OR.  &
       ( nrows > met_ny-2*nthik-1 ) ) THEN
    WRITE (*,f9200) TRIM(pname), ncols, nrows, met_nx, met_ny,  &
                   met_nx-2*nthik-1, met_ny-2*nthik-1
    CALL graceful_stop (pname)
  ENDIF

  IF ( ( x0+2*nthik+ncols-1 > met_nx-1 ) .OR.  &
       ( y0+2*nthik+nrows-1 > met_ny-1 ) ) THEN
    WRITE (*,f9250) TRIM(pname), met_nx, met_ny,  &
                   x0+2*nthik+ncols-1, y0+2*nthik+nrows-1
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Calculate window domain size in terms of MET grid.
!-------------------------------------------------------------------------------

  ncg_x = 1 + INT( ncols + 2 * nthik - 1 )
  ncg_y = 1 + INT( nrows + 2 * nthik - 1 )

!-------------------------------------------------------------------------------
! GDTYP_GD:
! The map projection types in I/O API are:
!   1: LATGRD for lat-lon coordinates
!   2: LAMGRD for Lambert coordinates
!   3: MERGRD for Mercator coordinates
!   4: STEGRD for Stereographic coordinates
!   5: UTMGRD for UTM coordinates
!   6: POLGRD for polar stereographic coordinates
!   7: EQMGRD for equatorial Mercator coordinates
!   8: TRMGRD for transverse Mercator coordinates
!   9: ALBGRD for Albers equal-area conic
!  10: LEQGRD for Lambert azimuthal equal-area
!-------------------------------------------------------------------------------

  IF ( met_mapproj == 1 ) THEN       ! Lambert conformal
    gdtyp_gd = lamgrd3
  ELSE IF ( met_mapproj == 2 ) THEN  ! polar stereographic
    gdtyp_gd = polgrd3
  ELSE IF ( met_mapproj == 3 ) THEN  ! equatorial Mercator
    gdtyp_gd = eqmgrd3
  ELSE
    WRITE (*,f9275) TRIM(pname), met_mapproj
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! The definitions of the map projection specification parameters, 
! P_ALP_GD (alpha),  P_BET_GD (beta), and P_GAM_GD (gamma), depend upon the
! projection type.  (Note: if P_ALP_GD < AMISS, then the grid description is
! missing or invalid.)
!
! The following descriptions were liberally borrowed from the I/O API
! grid definition page:  http://www.baronams.com/products/ioapi/GRIDS.html
!
! Lambert:       P_ALP_GD <= P_BET_GD are the two latitudes that
!                determine the projection cone; P_GAM_GD is the
!                central meridian.
!
! Polar:         P_ALP_GD is 1.0 for North Polar and -1.0 for South Polar.
!                P_BET_GD is the secant latitude (latitude of true scale).
!                P_GAM_GD is the central meridian.

! Eq. Mercator:  P_ALP_GD is the latitude of the true scale, P_BET_GD is unused,
!                and P_GAM_GD is the longitude of the central meridian.
!-------------------------------------------------------------------------------

  p_alp_gd = DBLE(met_p_alp_d)
  p_bet_gd = DBLE(met_p_bet_d)
  p_gam_gd = DBLE(met_p_gam_d)

!-------------------------------------------------------------------------------
! (XCENT_GD, YCENT_GD):
! For most projections, these are the longitude, -180 < X <= 180, and the
!   latitude, -90 <= Y <= 90, for the center of the grid's respective Cartesian
!   coordinate system.  Units are meters.
!-------------------------------------------------------------------------------

  IF ( ( met_model == 2 ) .AND. ( gdtyp_gd == lamgrd3 ) ) THEN
    xcent_gd = DBLE(met_proj_clon)  ! [degrees longitude]
    ycent_gd = DBLE(met_ref_lat)    ! [degrees latitude]
  ELSE IF ( gdtyp_gd == eqmgrd3 ) THEN
    xcent_gd = DBLE(met_proj_clon)  ! [degrees longitude]
    ycent_gd = 0.0d0                ! [degrees latitude]
  ELSE
    xcent_gd = DBLE(met_proj_clon)  ! [degrees longitude]
    ycent_gd = DBLE(met_proj_clat)  ! [degrees latitude]
  ENDIF

!-------------------------------------------------------------------------------
! (XCELL_GD, YCELL_GD):
! The X-direction and Y-direction cell dimensions (m) for a regular grid
! If zero, the grid is assumed irregular and described by other means (e.g.
! a grid-geometry file).
!-------------------------------------------------------------------------------

  xcell_gd   =  DBLE(met_resoln)  ! [m]
  ycell_gd   =  DBLE(met_resoln)  ! [m]

!-------------------------------------------------------------------------------
! VGTYP_GD:
! The vertical grid type:
!   1: VGSGPH3 for hydrostatic sigma-P coordinates
!   2: VGSGPN3 for non-hydrostatic sigma-P0 coordinates
!   3: VGSIGZ3 for sigma-Z coordinates
!   4: VGPRES3 for pressure (mb) coordinates
!   5: VGZVAL3 for Z (meters above mean sea level)
!   6: VHZVAL3 for H (meters above ground)
!   7: VGWRFEM for WRF mass-core sigma
!   8: VGWRFNM for WRF NMM
!   -: IMISS   for vertical coordinates not stored in VGLVSD
!              (e.g., temporally or spatially changing vertical coordinates)
!-------------------------------------------------------------------------------

  IF ( met_model == 1 ) THEN       ! MM5
    IF ( met_iversion == 3 ) THEN  !    v3: terrain-following non-hydrostatic
      vgtyp_gd = vgsgpn3
    ENDIF
  ELSE IF ( met_model == 2 ) THEN  ! WRF EM
    vgtyp_gd = vgwrfem             ! terrain-following dry hydrostatic pressure
  ENDIF

!-------------------------------------------------------------------------------
! VGTPUN_GD:
! The units of the vertical coordinate top.
!-------------------------------------------------------------------------------

  vgtpun_gd  = 'Pa'

!-------------------------------------------------------------------------------
! VGTOP_GD:
! The value for the model top used in the definition of the sigma
! coordinate systems in the VGTPUN_GD units
! For sigma-P, the relationship between pressure levels P and sigma-P is
! given by the following formula:
!    sigma-P = ( P - VGTOP_GD ) / (P_srf - VGTOP_GD ),
! where P_srf is the surface pressure.
!-------------------------------------------------------------------------------

  vgtop_gd   = met_ptop
  x3top      = met_ptop

!-------------------------------------------------------------------------------
! VGLVUN_GD:
! The units of the vertical coordinate surface values
!-------------------------------------------------------------------------------

  vglvun_gd  = 'none'

!-------------------------------------------------------------------------------
! COORDNAM_GD:
! The coordinate system name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  coordnam_gd  = coordnam

!-------------------------------------------------------------------------------
! GDNAME_GD:
! The grid name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  gdname_gd  = grdnam

!-------------------------------------------------------------------------------
! Check origins of output MCIP domain and met from offsets.  Take into account
! resolution of MET, MCIP, and NTHIK.
!   (X0, Y0) = (COL_OFFSET, ROW_OFFSET)
! *** Note:  The XORIG and YORIG values for WRF Lambert conformal are forced
!            to increments of half-delta-X if a user-defined reference
!            latitude was specified.
!-------------------------------------------------------------------------------

  IF ( ( met_model == 2 ) .OR. ( gdtyp_gd == eqmgrd3 ) ) THEN  ! WRF or Mercator

    xorig_ctm = met_xxctr - ( met_rictr_dot - FLOAT(x0+nthik) ) * met_resoln
    yorig_ctm = met_yyctr - ( met_rjctr_dot - FLOAT(y0+nthik) ) * met_resoln

    IF ( ( gdtyp_gd == lamgrd3 ) .AND. ( wrf_lc_ref_lat > -900.0 ) ) THEN
      ! Force XORIG and YORIG to be even increments of 0.5*delta-X.
      xtemp = xorig_ctm / 500.0
      ytemp = yorig_ctm / 500.0
      xtemp = FLOAT(NINT(xtemp))
      ytemp = FLOAT(NINT(ytemp))
      xorig_ctm = xtemp * 500.0
      yorig_ctm = ytemp * 500.0
    ENDIF
      
  ELSE  ! MM5

    xorig_m = ( ( met_x_11 - 0.5 * FLOAT(met_nxcoarse + 1) ) *  &
                  met_gratio ) * met_resoln

    yorig_m = ( ( met_y_11 - 0.5 * FLOAT(met_nycoarse + 1) ) *  &
                  met_gratio ) * met_resoln

    xorig_x = xorig_m + FLOAT(x0-nthik) * REAL(xcell_gd)
    yorig_x = yorig_m + FLOAT(y0-nthik) * REAL(ycell_gd)

    rnthik = FLOAT(nthik)

    xorig_ctm = xorig_x + rnthik * REAL(xcell_gd)
    yorig_ctm = yorig_x + rnthik * REAL(ycell_gd)

  ENDIF

!-------------------------------------------------------------------------------
! (XORIG_GD, YORIG_GD):
! For Lambert, Mercator, Stereographic, and UTM these are the
!     location in map units (Km) of the origin cell (1,1) (lower left corner)
!     of the of the horizontal grid measured from (XCENT_GD, YCENT_GD).
! For Lat-Lon: units are degrees - unused
!-------------------------------------------------------------------------------

  IF ( met_model == 2 ) THEN  ! WRF -- Allow trailing digits.
    xorig_gd   = DBLE(xorig_ctm)        ! X-origin [m]
    yorig_gd   = DBLE(yorig_ctm)        ! Y-origin [m]
  ELSE  ! MM5 -- By restriction in setup of grids, must be "round" number.
    xorig_gd   = DBLE(NINT(xorig_ctm))  ! X-origin [m]
    yorig_gd   = DBLE(NINT(yorig_ctm))  ! Y-origin [m]
  ENDIF

!-------------------------------------------------------------------------------
! Check user-defined MCIP output time info against input meteorology.
!-------------------------------------------------------------------------------

  IF ( intvl < NINT(met_tapfrq) ) THEN
    WRITE (*,f9300) TRIM(pname), intvl, met_tapfrq
    CALL graceful_stop (pname)
  ENDIF

  IF ( mcip_start < met_startdate ) THEN
    WRITE (*,f9400) TRIM(pname), mcip_start, met_startdate
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Set up coordinates for diagnostic print on all domains.
!-------------------------------------------------------------------------------

  IF ( ( lprt_col > ncols ) .OR. ( lprt_row > nrows ) .OR.  &
       ( lprt_col < 0     ) .OR. ( lprt_row < 0     ) ) THEN
    WRITE (*,f9600) TRIM(pname), lprt_col, lprt_row, ncols, nrows
    CALL graceful_stop (pname)
  ENDIF

  IF ( lprt_col == 0 ) THEN
    IF ( ncols > 1 ) THEN
      lprt_col = (ncols + 1) / 2
    ELSE
      lprt_col = 1
    ENDIF
  ENDIF

  IF ( lprt_row == 0 ) THEN
    IF ( nrows > 1 ) THEN
      lprt_row = (nrows + 1) / 2
    ELSE
      lprt_row = 1
    ENDIF
  ENDIF

  lprt_xcol = lprt_col + nthik
  lprt_xrow = lprt_row + nthik

  lprt_metx = lprt_col + x0
  lprt_mety = lprt_row + y0

!-------------------------------------------------------------------------------
! Echo user options and grid definitions to log file.
!-------------------------------------------------------------------------------

  WRITE (*, "(/, 1x, 78('-'), /)")
  WRITE (*, "(24x, a, /)") 'USER OPTIONS AND GRID DEFINITIONS'

  WRITE (*,f6000) mcip_start, mcip_end, intvl

  SELECT CASE ( lpv )
    CASE ( 0 )
      option = 'Will not calculate and output 3D potential vorticity'
    CASE ( 1 )
      option = 'Will calculate and output 3D potential vorticity'
    CASE DEFAULT
      option = '*** invalid ***'
  END SELECT
  WRITE (*,f6100) 'LPV    ', lpv, TRIM(option)

  SELECT CASE ( lwout )
    CASE ( 0 )
      option = 'Will not output vertical velocity from the met model'
    CASE ( 1 )
      option = 'Will output vertical velocity from the met model'
    CASE DEFAULT
      option = '*** invalid ***'
  END SELECT
  WRITE (*,f6100) 'LWOUT  ', lwout, TRIM(option)

  SELECT CASE ( luvcout )
    CASE ( 0 )
      option = 'Will not output u- and v-component winds on C grid'
    CASE ( 1 )
      option = 'Will output u- and v-component winds on C grid'
    CASE DEFAULT
      option = '*** invalid ***'
  END SELECT
  WRITE (*,f6100) 'LUVCOUT', luvcout, TRIM(option)

  SELECT CASE ( lsat )
    CASE ( 0 )
      option = 'Will not use satellite adjustment of clouds for photolysis'
    CASE ( 1 )
      option = 'Using GOES observed cloud fields to replace model fields'
    CASE DEFAULT
      option = '*** invalid ***'
  END SELECT
  WRITE (*,f6100) 'LSAT   ', lsat, TRIM(option)

  IF ( iflai ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) 'LAI', TRIM(yesno)

  IF ( iflufrc ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) 'FRACTIONAL LAND USE', TRIM(yesno)

  IF ( ( iflufrc ) .AND. ( met_model == 2 ) ) THEN
    IF ( ifluwrfout ) THEN
      yesno = 'WRF'
    ELSE
      yesno = 'GEO'
    ENDIF
    WRITE (*,f6160) 'FRACTIONAL LAND USE', TRIM(yesno)
  ENDIF

  IF ( ifmol ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) 'MONIN-OBUKHOV LENGTH', TRIM(yesno)

  IF ( ifmolpx ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6155) 'MONIN-OBUKHOV LENGTH', TRIM(yesno)

  IF ( ifresist ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) 'AERODYNAMIC AND STOMATAL RESISTANCE', TRIM(yesno)

  IF ( ift2m ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) '2-m TEMPERATURE', TRIM(yesno)

  IF ( ifq2m ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) '2-m MIXING RATIO', TRIM(yesno)

  IF ( ifveg ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) 'VEGETATION FRACTION', TRIM(yesno)

  IF ( ifw10m ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) '10-m WIND', TRIM(yesno)

  IF ( ifwr ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) 'CANOPY WETNESS', TRIM(yesno)

  IF ( ifznt ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) 'ROUGHNESS LENGTH', TRIM(yesno)

  IF ( ifsoil ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) 'SOIL MOISTURE, TEMPERATURE, AND TYPE', TRIM(yesno)
  WRITE (*,f6175) 'SOIL MOISTURE, TEMPERATURE, AND TYPE', TRIM(yesno)

  IF ( iftke ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) 'TURBULENT KINETIC ENERGY', TRIM(yesno)
  IF ( ( iftke ) .AND. ( .NOT. iftkef ) ) THEN
    WRITE (*,*) '  TKE is on half-layers'
  ELSE IF ( ( iftke ) .AND. ( iftkef ) ) THEN
    WRITE (*,*) '  TKE is on full-levels'
  ENDIF

  IF ( met_urban_phys >= 1 ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  IF ( met_model == 2 ) THEN
    WRITE (*,f6180) 'URBAN CANOPY MODEL (WRF ONLY)', TRIM(yesno)
  ENDIF

  IF ( ifcld3d ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6150) '3D RESOLVED CLOUD FRACTION', TRIM(yesno)

  IF ( met_hybrid > 0 ) THEN
    yesno = ' '
  ELSE
    yesno = 'NOT'
  ENDIF
  WRITE (*,f6180) 'HYBRID VERTICAL COORDINATE (WRF ONLY)', TRIM(yesno)

  WRITE (*,'(/)')
  WRITE (*,f6200) 'Met   ', metcol,  metrow,  metlay
  WRITE (*,f6200) 'MCIP X', ncols_x, nrows_x, metlay
  WRITE (*,f6200) 'Output', ncols,   nrows,   nlays
  WRITE (*,'(/)')

  WRITE (*,*) 'Output grid resolution: ', xcell_gd / 1000.0,  ' km'
  WRITE (*,*) 'Window domain origin on met domain (col,row):     ',  &
              x0, ', ', y0
  WRITE (*,*) 'Window domain far corner on met domain (col,row): ',  &
              x0 + ncg_x, ', ', y0 + ncg_y

  WRITE (*,'(/)')
  WRITE (*,"(' Cells and points for diagnostic prints')")
  WRITE (*,f6300) 'LPRT_COL ', 'LPRT_ROW ', lprt_col,  lprt_row
  WRITE (*,f6300) 'LPRT_XCOL', 'LPRT_XROW', lprt_xcol, lprt_xrow
  WRITE (*,f6300) 'LPRT_METX', 'LPRT_METY', lprt_metx, lprt_mety

  WRITE (*,'(/)')
  WRITE (*,"(' IOAPI header variables:')")

  WRITE (*,f6400) 'GDTYP3D', gdtyp_gd
  WRITE (*,f6500) 'GDNAM3D', gdname_gd
  WRITE (*,f6600) 'P_ALP3D', p_alp_gd
  WRITE (*,f6600) 'P_BET3D', p_bet_gd
  WRITE (*,f6600) 'P_GAM3D', p_gam_gd
  WRITE (*,f6600) 'XCENT3D', xcent_gd
  WRITE (*,f6600) 'YCENT3D', ycent_gd
  WRITE (*,f6600) 'XORIG3D', xorig_gd
  WRITE (*,f6600) 'YORIG3D', yorig_gd
  WRITE (*,f6600) 'XCELL3D', xcell_gd
  WRITE (*,f6600) 'YCELL3D', ycell_gd
  WRITE (*,f6400) 'VGTYP3D', vgtyp_gd
  WRITE (*,f6600) 'VGTOP3D', vgtop_gd

END SUBROUTINE setgriddefs
