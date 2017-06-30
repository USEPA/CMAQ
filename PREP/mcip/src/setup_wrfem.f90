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

SUBROUTINE setup_wrfem (cdfid, ctmlays)

!-------------------------------------------------------------------------------
! Name:     Set Up the WRF Domain Attributes
! Purpose:  Establishes bounds for WRF post-processing.
! Revised:  ?? Jun 2004  Modified from MCIP2.2 for WRF. (S.-B. Kim)
!           26 May 2005  Changed vertical dimension to reflect full-layer
!                        dimension in WRFv2 header.  Added dynamic calculation
!                        of MET_TAPFRQ.  Converted dimensions to X,Y as opposed
!                        to the (former) convention that aligned with MM5.
!                        Included updates from MCIPv2.3.  Added calculation of
!                        cone factor.  Added logic for moist species, 2-m
!                        temperature, and 10-m winds.  Added definitions for
!                        WRF base state variables.  Added capability to use all
!                        WRF layers for MCIP without defining a priori.
!                        Cleaned up code.  (T. Otte)
!           15 Jul 2005  Added debugging on variable retrievals.  Changed check
!                        on 3D mixing ratios from rain to ice.  Corrected RADM
!                        seasons for Southern Hemisphere.  Corrected variable
!                        name for retrieval of surface physics option. (T. Otte)
!           18 Aug 2005  Changed internal variable SIGN to FAC to avoid
!                        confusion with F90 intrinsic function.  (T. Otte)
!           10 Apr 2006  Corrected checking of I/O API variables for Mercator
!                        projection.  (T. Otte)
!           12 May 2006  Corrected setting of I/O API variables for polar
!                        stereographic projection.  Revised defining and
!                        setting projection variables for module METINFO.
!                        Added restriction on using Eta/Ferrier microphysics
!                        scheme where QCLOUD represents total condensate.
!                        (T. Otte)
!           20 Jun 2006  Changed setting of IDTSEC from REAL to INTEGER
!                        value.  (T. Otte)
!           27 Jul 2007  Removed settings for RADMdry variable ISESN and for
!                        MET_INHYD.  Updated read of P_TOP to account for new
!                        method of storing "real" scalars in WRF I/O API with
!                        WRFv2.2.  Added checks for fractional land use, leaf
!                        area index, Monin-Obukhov length, aerodynamic and
!                        stomatal resistances, vegetation fraction, canopy
!                        wetness, and soil moisture, temperature, and type in
!                        WRF file.  Added read for number of land use
!                        categories...new with WRFV2.2.  Added read for number
!                        of soil layers, MET_RELEASE, MET_FDDA_3DAN and
!                        MET_FDDA_OBS.  Set MET_FDDA_SFAN to 0 for now because
!                        that option is not in WRF ARW as of V2.2.  Changed
!                        MET_RADIATION into MET_LW_RAD and MET_SW_RAD.
!                        (T. Otte)
!           06 May 2008  Changed criteria for setting NUMMETLU when netCDF
!                        dimension "land_cat_stag" does not exist.  Added
!                        checks to determine if 2-m mixing ratio (Q2) and
!                        turbulent kinetic energy (TKE_MYJ) arrays exist, and
!                        set flags appropriately.  Extract nudging coefficients
!                        from header to use in metadata.  Extract whether or
!                        not the urban canopy model was used.  (T. Otte)
!           27 Oct 2009  Cleaned up file opening and logging in WRF I/O API to
!                        prevent condition with too many files open for long
!                        simulations.  Added MODIFIED IGBP MODIS NOAH and 
!                        NLCD/MODIS as land-use classification options.
!                        Changed MET_UCMCALL to MET_URBAN_PHYS, and allowed
!                        for variable to be set to be greater than 1.  Chnaged
!                        code to allow for surface analysis nudging option
!                        and coefficients to be defined per WRFv3.1.  Define
!                        MET_CEN_LAT, MET_CEN_LON, MET_RICTR_DOT, MET_RJCTR_DOT,
!                        and MET_REF_LAT.  Increased MAX_TIMES to 1000.  Compute
!                        MET_XXCTR and MET_YYCTR.  Corrected setting for
!                        DATE_INIT, and fill variable MET_RESTART.  Read number
!                        of land use categories from WRF global attributes for
!                        WRFV3.1 and beyond.  Allow output from WRF
!                        Preprocessing System (WPS) routine, GEOGRID, to provide
!                        fractional land use output if it is unavailable in WRF
!                        output.  Fill MET_P_ALP_D and MET_P_BET_D here
!                        rather than in setgriddefs.F for Mercator.  Added
!                        new logical variables IFLUWRFOUT and IFZNT.  (T. Otte)
!           12 Feb 2010  Removed unused variables COMM and SYSDEP_INFO.
!                        (T. Otte)
!           18 Mar 2010  Added CDFID as an input argument, and no longer open
!                        and close WRF history file here.  Added CDFIDG as an
!                        input argument for subroutine CHKWPSHDR.  (T. Otte)
!           15 Dec 2010  Improved support for long MCIP runs from long WRF
!                        runs by increasing MAX_TIMES to 9999.  Added
!                        MET_RAIN_BUCKET.  (T. Otte)
!           23 Feb 2011  Refined error checking for MET_RAIN_BUCKET.  (T. Otte)
!           11 Aug 2011  Added MET_SHAL_CU to input.  Replaced module PARMS3
!                        with I/O API module M3UTILIO.  (T. Otte)
!           24 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Updated netCDF commands
!                        to F90, and improved error handling.  Replaced calls
!                        to GET_TIMES_CDF with explicit netCDF functions.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           21 Nov 2011  Force 2-m water vapor mixing ratio from WRF with
!                        YSU PBL to be filled with layer 1 QVAPOR to avoid
!                        occasional Q2 < 0 in wintertime.  (T. Otte)
!           07 Dec 2011  Removed requirement to fill nudging coefficient for
!                        moisture when spectral nudging is used in WRF; as of
!                        WRFv3.3.1, spectral nudging toward moisture is not
!                        released in WRF.  Also added provision to collect
!                        nudging coefficient for geopotential when spectral
!                        nudging is used; was added to WRF header with WRFv3.2.
!                        (T. Otte)
!           21 Aug 2012  Added MET_PCP_INCR for WRFV3.2+.  (T. Otte)
!           10 Sep 2012  Added handling for 40-category 2006 NLCD-MODIS land
!                        use classification as "NLCD40".  Added alternate name
!                        for 50-category 2001 NLCD-MODIS land use classification
!                        as "NLCD50".  (T. Otte)
!           26 Nov 2014  Added reads of ice, lake, and urban land use indices,
!                        and moved those definitions from getluse.f90 to this
!                        routine.  (T. Spero)
!           10 Apr 2015  Determine if 3D cloud fraction is part of WRF output
!                        and if it represents resolved clouds.  Fill new logical
!                        variable IFCLD3D appropriately so that if resolved
!                        cloud fraction is available, it will be passed through
!                        in output.  (T. Spero)
!           21 Aug 2015  Added flag to capture whether ACM2 was run so that
!                        Monin-Obukhov length can be recalculated following
!                        the "corrector" part of the predictor-corrector in
!                        WRF/ACM2.  (T. Spero)
!           17 Sep 2015  Changed IFMOLACM to IFMOLPX.  (T. Spero)
!           21 Apr 2017  Added MODIS category 21 as "Lake".  (T. Spero)
!           23 Jun 2017  Added a check for WRF's hybrid vertical coordinate
!                        in WRFv3.9 and beyond.  Currently disabled MCIP when
!                        that coordinate is detected.  To be implemented in
!                        a later release of MCIP.  (T. Spero)
!-------------------------------------------------------------------------------

  USE metinfo
  USE date_pack
  USE mcipparm
  USE files
  USE m3utilio, ONLY: badval3
  USE wrf_netcdf
  USE const, ONLY: pi180
  USE netcdf

  IMPLICIT NONE

  INTEGER     ,       INTENT(IN)    :: cdfid
  INTEGER                           :: cdfid2
  INTEGER                           :: cdfidg
  REAL,               INTENT(INOUT) :: ctmlays    ( maxlays )
  CHARACTER(LEN=19)                 :: date_init
  CHARACTER(LEN=19)                 :: date_start
  INTEGER                           :: dimid
  INTEGER                           :: dimids     ( nf90_max_var_dims )
  REAL,               ALLOCATABLE   :: dum1d      ( : )
  REAL,               ALLOCATABLE   :: dum2d      ( : , : )
  REAL                              :: dx
  REAL                              :: dy
  REAL                              :: fac
  CHARACTER(LEN=256)                :: fl
  CHARACTER(LEN=256)                :: fl2
  CHARACTER(LEN=256)                :: flg
  CHARACTER(LEN=256)                :: geofile
  INTEGER                           :: icloud_cu
  INTEGER                           :: id_data
  INTEGER                           :: idtsec
  LOGICAL                           :: ifgeo
  LOGICAL                           :: ifisltyp
  LOGICAL                           :: ifra
  LOGICAL                           :: ifrs
  LOGICAL                           :: ifsmois
  LOGICAL                           :: iftslb
  LOGICAL                           :: ifu10m
  LOGICAL                           :: ifv10m
  INTEGER                           :: it
  INTEGER                           :: ival
  INTEGER                           :: lent
  INTEGER                           :: n_times
  INTEGER                           :: nxm
  INTEGER                           :: nym
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'SETUP_WRFEM'
  INTEGER                           :: rcode
  REAL                              :: rval
  CHARACTER(LEN=19),  ALLOCATABLE   :: times      ( : )
  CHARACTER(LEN=19),  ALLOCATABLE   :: times2     ( : )
  INTEGER                           :: varid
  CHARACTER(LEN=80)                 :: wrfversion

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f6000 = "(/, 1x, &
    & '- SUBROUTINE SETUP_WRFEM - READING WRF HEADER')"
  CHARACTER(LEN=256), PARAMETER :: f6100 = "(3x, &
    & 'WRF GRID DIMENSIONS (X,Y,Z) ', i4, 1x, i4, 1x, i3, //)"

  CHARACTER(LEN=256), PARAMETER :: f9000 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   MISMATCH IN DX AND DY', &
    & /, 1x, '***   DX, DY = ', 2(f7.2), &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNKNOWN LAND USE CLASSIFICATION', &
    & /, 1x, '***   FIRST THREE LETTERS = ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9225 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   QCLOUD NOT FOUND IN WRF OUTPUT...STOPPING', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9250 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ETA/FERRIER SCHEME IS NOT SUPPORTED IN CMAQ', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9275 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   FOUND QCLOUD BUT NOT QRAIN...STOPPING', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   NQSPECIES SET AT 3',&
    & /, 1x, '***   MCIP NEEDS TO BE MODIFIED FOR THIS CASE', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE', &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   NCF: ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9410 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR RETRIEVING NCF ID FROM WRF FILE', &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   NCF: ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9420 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR INQUIRING ABOUT VAR IN WRF FILE', &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   NCF: ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9430 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR RETRIEVING DIMS FROM WRF FILE', &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   NCF: ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9500 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ONLY FOUND ONE FILE WITH ONE TIME PERIOD', &
    & /, 1x, '***   SETTING OUTPUT FREQUENCY TO 1 MINUTE', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9550 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   NEED PRECIPITATION ACCUMULATION IN WRF TO MATCH', &
    & /, 1x, '***   MCIP OUTPUT INTERVAL', &
    & /, 1x, '***   PREC_ACC_DT from WRF: ', i4, &
    & /, 1x, '***   INTVL from MCIP: ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9600 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR OPENING WRF NETCDF FILE', &
    & /, 1x, '***   FILE = ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9700 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CLOSING WRF NETCDF FILE', &
    & /, 1x, '***   FILE = ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9800 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   DID NOT FIND FRACTIONAL LAND USE IN wrfout', &
    & /, 1x, '***   AND DID NOT FIND GEOGRID FILE' &
    & /, 1x, '***   -- WILL NOT USE FRACTIONAL LAND USE DATA' &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9900 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   HYBRID VERTICAL COORDINATE DETECTED IN WRF!!!', &
    & /, 1x, '***   MCIP AND CMAQ HAVE NOT BEEN TESTED WITH THIS OPTION YET!', &
    & /, 1x, '***   -- TO BE IMPLEMENTED LATER', &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Extract NX, NY, and NZ.
!-------------------------------------------------------------------------------

  WRITE (*,f6000)

  fl = file_mm(1)

  rcode = nf90_get_att (cdfid, nf90_global, 'WEST-EAST_GRID_DIMENSION', met_nx)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'WEST-EAST_GRID_DIMENSION',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'SOUTH-NORTH_GRID_DIMENSION',  &
                        met_ny)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'SOUTH-NORTH_GRID_DIMENSION',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'BOTTOM-TOP_GRID_DIMENSION', ival)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'BOTTOM-TOP_GRID_DIMENSION',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ELSE
    met_nz = ival - 1
  ENDIF

  WRITE (*,f6100) met_nx, met_ny, met_nz

  met_rictr_dot = FLOAT(met_nx - 1) / 2.0 + 1.0
  met_rjctr_dot = FLOAT(met_ny - 1) / 2.0 + 1.0

!-------------------------------------------------------------------------------
! If layer structure was not defined in user namelist, use WRF layers.
!-------------------------------------------------------------------------------

  IF ( needlayers ) THEN
    nlays = met_nz
    CALL get_var_1d_real_cdf (cdfid, 'ZNW', ctmlays(1:nlays+1), 1, rcode)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'ZNW', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Extract domain attributes.
!-------------------------------------------------------------------------------

  rcode = nf90_get_att (cdfid, nf90_global, 'TITLE', wrfversion)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'TITLE', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'DX', dx)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'DX', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'DY', dy)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'DY', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF (dx == dy) THEN
    met_resoln = dx
  ELSE
    WRITE (*,f9000) TRIM(pname), dx, dy
    CALL graceful_stop (pname)
  ENDIF

  met_nxcoarse = met_nx 
  met_nycoarse = met_ny
  met_gratio   = 1
  met_x_11     = 1
  met_y_11     = 1

  rcode = nf90_get_att (cdfid, nf90_global, 'MAP_PROJ', met_mapproj)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'MAP_PROJ', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'STAND_LON', met_proj_clon)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'STAND_LON', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'MOAD_CEN_LAT', met_proj_clat)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'MOAD_CEN_LAT', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'CEN_LON', met_cen_lon)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'CEN_LON', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF
  met_x_centd = met_cen_lon

  rcode = nf90_get_att (cdfid, nf90_global, 'CEN_LAT', met_cen_lat)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'CEN_LAT', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF
  met_y_centd = met_cen_lat

  rcode = nf90_get_att (cdfid, nf90_global, 'TRUELAT1', met_tru1)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'TRUELAT1', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'TRUELAT2', met_tru2)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'TRUELAT2', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  SELECT CASE ( met_mapproj )
    
    CASE (1)  ! Lambert conformal 
      met_p_alp_d  = MIN(met_tru1, met_tru2)  ! true latitude 1  [degrees]
      met_p_bet_d  = MAX(met_tru1, met_tru2)  ! true latitude 2  [degrees]
      met_p_gam_d  = met_proj_clon            ! central meridian [degrees]
      IF ( met_proj_clat < 0.0 ) THEN
        fac = -1.0  ! Southern Hemisphere
      ELSE
        fac =  1.0  ! Northern Hemisphere
      ENDIF
      IF ( ABS(met_tru1 - met_tru2) > 1.0e-1 ) THEN
        met_cone_fac = ALOG10(COS(met_tru1 * pi180)) -  &
                       ALOG10(COS(met_tru2 * pi180))
        met_cone_fac = met_cone_fac /                                      &
                       ( ALOG10(TAN((45.0 - fac*met_tru1/2.0) * pi180)) -  &
                         ALOG10(TAN((45.0 - fac*met_tru2/2.0) * pi180)) )
      ELSE
        met_cone_fac = fac * SIN(met_tru1*pi180)
      ENDIF

      IF ( wrf_lc_ref_lat > -999.0 ) THEN
        met_ref_lat = wrf_lc_ref_lat
      ELSE
        met_ref_lat = ( met_tru1 + met_tru2 ) * 0.5
      ENDIF

      CALL ll2xy_lam (met_cen_lat, met_cen_lon, met_tru1, met_tru2,  &
                      met_proj_clon, met_ref_lat, met_xxctr, met_yyctr)
    
    CASE (2)  ! polar stereographic
      met_p_alp_d  = SIGN(1.0, met_y_centd)   ! +/-1.0 for North/South Pole
      met_p_bet_d  = met_tru1                 ! true latitude    [degrees]
      met_p_gam_d  = met_proj_clon            ! central meridian [degrees]
      met_cone_fac = 1.0                      ! cone factor
      met_ref_lat  = -999.0                   ! not used

      CALL ll2xy_ps (met_cen_lat, met_cen_lon, met_tru1, met_proj_clon,  &
                     met_xxctr, met_yyctr)
    
    CASE (3)  ! Mercator
      met_p_alp_d  = 0.0                      ! lat of coord origin [deg]
      met_p_bet_d  = 0.0                      ! (not used)
      met_p_gam_d  = met_proj_clon            ! lon of coord origin [deg]
      met_cone_fac = 0.0                      ! cone factor
      met_ref_lat  = -999.0                   ! not used

      CALL ll2xy_merc (met_cen_lat, met_cen_lon, met_proj_clon,  &
                       met_xxctr, met_yyctr)
    
    CASE DEFAULT
      met_p_bet_d  = badval3                  ! missing
      met_p_alp_d  = badval3                  ! missing
      met_p_gam_d  = badval3                  ! missing
      met_cone_fac = badval3                  ! missing
      met_ref_lat  = badval3                  ! missing
  
  END SELECT

!-------------------------------------------------------------------------------
! Extract model run options.
!-------------------------------------------------------------------------------

  rcode = nf90_get_att (cdfid, nf90_global, 'MMINLU', met_lu_src)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'MMINLU', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'ISWATER', met_lu_water)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'ISWATER', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_inq_dimid (cdfid, 'soil_layers_stag', dimid)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'ID for soil_layers_stag',  &
                   TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF
  rcode = nf90_inquire_dimension (cdfid, dimid, len=met_ns)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'value for soil_layers_stag',  &
                   TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  ! NUM_LAND_CAT was added in WRFv3.1 to define number of land use categories.
  ! "land_cat_stag" was added in WRFv2.2 to define fractional land use.
  ! Older WRF runs do not include this dimension and they are restricted
  ! to 24-category USGS land cover.

  IF ( wrfversion(18:22) >= "V3.1" ) THEN  ! WRFv3.1 or later

    rcode = nf90_get_att (cdfid, nf90_global, 'NUM_LAND_CAT', nummetlu)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'NUM_LAND_CAT', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    rcode = nf90_get_att (cdfid, nf90_global, 'ISICE', met_lu_ice)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'ISICE', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    rcode = nf90_get_att (cdfid, nf90_global, 'ISLAKE', met_lu_lake)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'ISLAKE', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    rcode = nf90_get_att (cdfid, nf90_global, 'ISURBAN', met_lu_urban)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'ISURBAN', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ELSE
    rcode = nf90_inq_dimid (cdfid, 'land_cat_stag', dimid)
    IF ( rcode /= nf90_noerr ) THEN  ! only exists with fractional land use
      SELECT CASE ( met_lu_src(1:3) )
        CASE ( "USG" )  ! USGS -- typically 24, but can be up to 33 in V2.2+
          IF ( ( wrfversion(18:21) == "V2.2" ) .OR.  &
               ( wrfversion(18:19) == "V3"   ) ) THEN
            nummetlu = 33
          ELSE
            nummetlu = 24
          ENDIF
          met_lu_water = 16
          met_lu_ice   = 24
          met_lu_urban =  1
          met_lu_lake  = -1
        CASE ( "OLD" )  ! old MM5 13-category system
          nummetlu     = 13
          met_lu_water =  7
          met_lu_ice   = 11
          met_lu_urban =  1
          met_lu_lake  = -1
        CASE ( "SiB" )  ! SiB 16-category system
          nummetlu     = 16
          met_lu_water = 15
          met_lu_ice   = 16
          met_lu_urban = -1
          met_lu_lake  = -1
        CASE ( "MOD" )  ! Modified IGBP MODIS NOAH 33-category system
          nummetlu     = 33
          met_lu_water = 17
          met_lu_ice   = 15
          met_lu_urban = 13
          IF ( wrfversion(18:22) >= "V3.8" ) THEN  ! WRFv3.8 or later
            met_lu_lake = 21
          ELSE
            met_lu_lake = -1
          ENDIF
        CASE ( "NLC" )  ! NLCD/MODIS combined system
          IF ( met_lu_src(4:6) == "D40") THEN
            nummetlu     = 40
            met_lu_water = 17
            met_lu_ice   = 15
            met_lu_urban = 13
            met_lu_lake  = -1
          ELSE
            nummetlu     = 50
            met_lu_water =  1
            met_lu_ice   =  2
            met_lu_urban =  3
            met_lu_lake  = -1
          ENDIF
        CASE DEFAULT
          WRITE (*,f9100) TRIM(pname), met_lu_src(1:3)
          CALL graceful_stop (pname)
      END SELECT
    ELSE
      rcode = nf90_inquire_dimension (cdfid, dimid, len=nummetlu)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9400) TRIM(pname), 'value for land_cat_stag',  &
                        TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'RA_LW_PHYSICS', met_lw_rad)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'RA_LW_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'RA_SW_PHYSICS', met_sw_rad)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'RA_SW_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'CU_PHYSICS', met_cumulus)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'CU_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'MP_PHYSICS', met_expl_moist)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'MP_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'BL_PBL_PHYSICS', met_pbl)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'BL_PBL_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'SF_SFCLAY_PHYSICS', met_sfc_lay)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'SF_SFCLAY_PHYSICS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'SF_SURFACE_PHYSICS', met_soil_lsm)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'SF_SURFACE_PHYSICS',  &
                   TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  ! Determine if an urban model was used.

  IF ( wrfversion(18:21) >= "V3.1" ) THEN

    rcode = nf90_get_att (cdfid, nf90_global, 'SF_URBAN_PHYSICS',  &
                          met_urban_phys)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'SF_URBAN_PHYSICS',  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ELSE IF ( wrfversion(18:21) == "V3.0" ) THEN

    rcode = nf90_get_att (cdfid, nf90_global, 'UCMCALL', met_urban_phys)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'SF_URBAN_PHYSICS',  &
                      TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ELSE 

    ! In v2.2, header variable UCMCALL seems to always be 0 for nested runs,
    ! even when UCM is invoked.  For now, use field TC_URB (canopy temperature)
    ! as a proxy to determine if the UCM was used.  If the field does not exist,
    ! then the UCM was not used.  If the field exists, determine if the data are
    ! "reasonable" (i.e., positive and non-zero); assume that UCM was used if
    ! the field contains "physical" data.

    nxm = met_nx - 1
    nym = met_ny - 1
    it  = 1  ! use first time in file since some files just have one time
    ALLOCATE ( dum2d ( nxm, nym ) )
      CALL get_var_2d_real_cdf (cdfid, 'TC_URB', dum2d, it, rcode)
      IF ( ( rcode == nf90_noerr ) .AND. ( MAXVAL(dum2d) > 100.0 ) ) THEN  ! UCM
        met_urban_phys = 1
      ELSE
        met_urban_phys = 0
      ENDIF
    DEALLOCATE ( dum2d )

  ENDIF

  ! Determine if shallow convection was used.

  IF ( wrfversion(18:21) >= "V3.3" ) THEN

    rcode = nf90_get_att (cdfid, nf90_global, 'SHCU_PHYSICS', met_shal_cu)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'SHCU_PHYSICS', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    IF ( met_shal_cu == 0 .AND. met_cumulus == 5 ) THEN  ! Grell shallow on?
      rcode = nf90_get_att (cdfid, nf90_global, 'ISHALLOW', met_shal_cu)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9400) TRIM(pname), 'ISHALLOW', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF

  ELSE  ! no way to easily tell if Grell 3D used shallow convection

    IF ( met_cumulus == 5 ) THEN  ! Grell 3D
      met_shal_cu = -1
    ELSE
      met_shal_cu = 0
    ENDIF

  ENDIF

  met_snow_opt = 1  ! not used for WRF yet

  rcode = nf90_get_att (cdfid, nf90_global, 'BUCKET_MM', met_rain_bucket)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( wrfversion(18:22) >= "V3.2" ) then  ! BUCKET_MM implemented in WRFv3.2
      WRITE (*,f9400) TRIM(pname), 'BUCKET_MM', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ELSE
      met_rain_bucket = -1.0
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'PREC_ACC_DT', rval)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( wrfversion(18:22) >= "V3.2" ) then  ! PREC_ACC_DT added in WRFv3.2
      WRITE (*,f9400) TRIM(pname), 'PREC_ACC_DT', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ELSE
      met_pcp_incr = 0
    ENDIF
  ELSE
    met_pcp_incr = NINT(rval)
  ENDIF

  IF ( met_pcp_incr > 0 ) THEN
    IF ( met_pcp_incr /= intvl ) THEN  ! can't compute precip for CMAQ
      WRITE (*,f9550) TRIM(pname), met_pcp_incr, intvl
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Extract WRF start date and time information.
!-------------------------------------------------------------------------------

  rcode = nf90_get_att (cdfid, nf90_global, 'SIMULATION_START_DATE', date_init)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'SIMULATION_START_DATE',  &
                    TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF
  met_startdate =  date_init(1:19) // '.0000'
  met_startdate(11:11) = "-"  ! change from "_" to "-" for consistency

  rcode = nf90_get_att (cdfid, nf90_global, 'START_DATE', date_start)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'START_DATE', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( date_init == date_start ) THEN
    met_restart = 0
  ELSE
    met_restart = 1
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'Times', id_data)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9410) TRIM(pname), 'Times', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF
  rcode = nf90_inquire_variable (cdfid, id_data, dimids=dimids)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9420) TRIM(pname), 'Times', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF
  rcode = nf90_inquire_dimension (cdfid, dimids(1), len=lent)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9430) TRIM(pname), 'Times', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF
  rcode = nf90_inquire_dimension (cdfid, dimids(2), len=n_times)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9430) TRIM(pname), 'Times', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF
  IF ( ALLOCATED ( times ) ) DEALLOCATE ( times )
  ALLOCATE ( times ( n_times ) )
  rcode = nf90_get_var (cdfid, id_data, times,   &
                        start=(/1,1/), count=(/lent,n_times/))
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'Times', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( n_times > 1 ) THEN
    CALL geth_idts (times(2)(1:19), times(1)(1:19), idtsec)
  ELSE
    fl2 = file_mm(2)
    IF ( fl2(1:10) == '          ' ) THEN
      WRITE (*,f9500) TRIM(pname)
      idtsec = 60
    ELSE
      rcode = nf90_open (fl2, nf90_nowrite, cdfid2)
      IF ( rcode == nf90_noerr ) THEN
        rcode = nf90_inq_varid (cdfid2, 'Times', id_data)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9410) TRIM(pname), 'Times', TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
        rcode = nf90_inquire_variable (cdfid2, id_data, dimids=dimids)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9420) TRIM(pname), 'Times', TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
        rcode = nf90_inquire_dimension (cdfid2, dimids(1), len=lent)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9430) TRIM(pname), 'Times', TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
        rcode = nf90_inquire_dimension (cdfid2, dimids(2), len=n_times)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9430) TRIM(pname), 'Times', TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
        IF ( ALLOCATED ( times2 ) ) DEALLOCATE ( times2 )
        ALLOCATE ( times2 ( n_times ) )
        rcode = nf90_get_var (cdfid2, id_data, times2,   &
                              start=(/1,1/), count=(/lent,n_times/))
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9400) TRIM(pname), 'Times', TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
        CALL geth_idts (times2(1)(1:19), times(1)(1:19), idtsec)
      ELSE
        WRITE (*,f9600) TRIM(pname), TRIM(fl2)
        CALL graceful_stop (pname)
      ENDIF
      rcode = nf90_close (cdfid2)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9700) TRIM(pname), TRIM(fl2)
        CALL graceful_stop (pname)
      ENDIF
    ENDIF
  ENDIF
  met_tapfrq = REAL(idtsec / 60)  ! convert sec --> min

!-------------------------------------------------------------------------------
! Set variables for non-hydrostatic base state.  There is no option for
! hydrostatic run in WRF.  The base state variables are not currently output
! (as of WRFv2.2), so fill in "default" values from WRF namelist.
!
! Note:  In WRFv2.2 NCAR changed the way "real" scalars (e.g., P_TOP) are
!        stored in the WRF I/O API.
!-------------------------------------------------------------------------------

  IF ( (wrfversion(18:21) == "V2.2") .OR. (wrfversion(18:19) >= "V3") ) THEN
    CALL get_var_real_cdf (cdfid, 'P_TOP', met_ptop, rcode)
  ELSE
    ALLOCATE ( dum1d ( 1 ) )
    CALL get_var_1d_real_cdf (cdfid, 'P_TOP', dum1d, 1, rcode)
    met_ptop = dum1d(1)
  ENDIF

  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'P_TOP', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  met_p00   = 100000.0 ! base state sea-level pressure [Pa]
  met_ts0   =    290.0 ! base state sea-level temperature [K]
  met_tlp   =     50.0 ! base state lapse rate d(T)/d(ln P) from 1000 to 300 mb
  met_tiso  = badval3  ! base state stratospheric isothermal T [K]  ! not used

!-------------------------------------------------------------------------------
! Determine WRF release.
!-------------------------------------------------------------------------------

  met_release = '        '

  IF ( wrfversion(18:18) == "V" ) THEN
    met_release(1:2) = wrfversion(18:19)
  ENDIF

  IF ( wrfversion(20:20) == '.' ) THEN
    met_release(3:4) = wrfversion(20:21)
  ENDIF

  IF ( wrfversion(22:22) == '.' ) THEN
    met_release(5:6) = wrfversion(22:23)
  ENDIF

  IF ( wrfversion(24:24) == '.' ) THEN
    met_release(7:8) = wrfversion(24:25)
  ENDIF

!-------------------------------------------------------------------------------
! Determine FDDA options.
!-------------------------------------------------------------------------------

  rcode = nf90_get_att (cdfid, nf90_global, 'GRID_FDDA', met_fdda_3dan)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( TRIM(met_release) < 'V2.2' ) THEN
      met_fdda_3dan = 0  ! not implemented until V2.2
    ELSE
      WRITE (*,f9400) TRIM(pname), 'GRID_FDDA', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GUV', met_fdda_gv3d)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( TRIM(met_release) < 'V2.2' ) THEN
      met_fdda_gv3d = -1.0  ! not in header until V2.2
    ELSE IF ( met_fdda_3dan == 0 ) THEN
      met_fdda_gv3d = -1.0  ! not in header if analysis nudging is off
    ELSE
      WRITE (*,f9400) TRIM(pname), 'GUV', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GT', met_fdda_gt3d)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( TRIM(met_release) < 'V2.2' ) THEN
      met_fdda_gt3d = -1.0  ! not in header until V2.2
    ELSE IF ( met_fdda_3dan == 0 ) THEN
      met_fdda_gt3d = -1.0  ! not in header if analysis nudging is off
    ELSE
      WRITE (*,f9400) TRIM(pname), 'GT', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GQ', met_fdda_gq3d)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( TRIM(met_release) < 'V2.2' ) THEN
      met_fdda_gq3d = -1.0  ! not in header until V2.2
    ELSE IF ( met_fdda_3dan /= 1 ) THEN
      met_fdda_gq3d = -1.0  ! not in header if analysis nudging is off
    ELSE
      WRITE (*,f9400) TRIM(pname), 'GQ', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'GPH', met_fdda_gph3d)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( TRIM(met_release) < 'V3.2' ) THEN
      met_fdda_gph3d = -1.0  ! not in header until V3.2
    ELSE IF ( met_fdda_3dan /= 2 ) THEN
      met_fdda_gph3d = -1.0  ! not in header if spectral nudging is off
    ELSE
      WRITE (*,f9400) TRIM(pname), 'GPH', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( TRIM(met_release) >= 'V3.1' ) THEN  ! find sfc analysis nudging info

    rcode = nf90_get_att (cdfid, nf90_global, 'GRID_SFDDA', met_fdda_sfan)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'GRID_SFDDA', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    IF ( met_fdda_sfan == 1 ) THEN

      rcode = nf90_get_att (cdfid, nf90_global, 'GUV_SFC', met_fdda_gvsfc)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9400) TRIM(pname), 'GUV_SFC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      rcode = nf90_get_att (cdfid, nf90_global, 'GT_SFC', met_fdda_gtsfc)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9400) TRIM(pname), 'GT_SFC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      rcode = nf90_get_att (cdfid, nf90_global, 'GQ_SFC', met_fdda_gqsfc)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9400) TRIM(pname), 'GQ_SFC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

    ELSE

      met_fdda_gvsfc = -1.0
      met_fdda_gtsfc = -1.0
      met_fdda_gqsfc = -1.0

    ENDIF

  ELSE
    met_fdda_sfan  =  0  ! sfc analysis nudging not in WRF until V3.1
    met_fdda_gvsfc = -1.0
    met_fdda_gtsfc = -1.0
    met_fdda_gqsfc = -1.0
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'OBS_NUDGE_OPT', met_fdda_obs)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( TRIM(met_release) < 'V2.2' ) THEN
      met_fdda_obs = 0  ! not implemented until V2.2
    ELSE
      WRITE (*,f9400) TRIM(pname), 'OBS_NUDGE_OPT', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'OBS_COEF_WIND', met_fdda_giv)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( TRIM(met_release) < 'V2.2' ) THEN
      met_fdda_giv = -1.0  ! not in header until V2.2
    ELSE IF ( met_fdda_obs == 0 ) THEN
      met_fdda_giv = -1.0  ! not in header if obs nudging is off
    ELSE
      WRITE (*,f9400) TRIM(pname), 'OBS_COEF_WIND', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'OBS_COEF_TEMP', met_fdda_git)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( TRIM(met_release) < 'V2.2' ) THEN
      met_fdda_git = -1.0  ! not in header until V2.2
    ELSE IF ( met_fdda_obs == 0 ) THEN
      met_fdda_git = -1.0  ! not in header if obs nudging is off
    ELSE
      WRITE (*,f9400) TRIM(pname), 'OBS_COEF_TEMP', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_get_att (cdfid, nf90_global, 'OBS_COEF_MOIS', met_fdda_giq)
  IF ( rcode /= nf90_noerr ) THEN
    IF ( TRIM(met_release) < 'V2.2' ) THEN
      met_fdda_giq = -1.0  ! not in header until V2.2
    ELSE IF ( met_fdda_obs == 0 ) THEN
      met_fdda_giq = -1.0  ! not in header if obs nudging is off
    ELSE
      WRITE (*,f9400) TRIM(pname), 'OBS_COEF_MOIS', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Determine whether or not fractional land use is available in the output.
! Set the flag appropriately.
!-------------------------------------------------------------------------------

  rcode = nf90_inq_varid (cdfid, 'LANDUSEF', varid)
  IF ( rcode == nf90_noerr ) THEN
    iflufrc    = .TRUE.  ! fractional land use is available
    ifluwrfout = .TRUE.  ! fractional land use is located in WRF history file
  ELSE
    ifluwrfout = .FALSE.  ! fractional land use is not available in WRF history
    geofile = TRIM( file_ter )
    INQUIRE ( FILE=geofile, EXIST=ifgeo )
    IF ( .NOT. ifgeo ) THEN
      WRITE (*,f9800) TRIM(pname)
      iflufrc = .FALSE.
    ELSE
      flg = file_ter
      rcode = nf90_open (flg, nf90_nowrite, cdfidg)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9600) TRIM(pname), TRIM(flg)
        CALL graceful_stop (pname)
      ENDIF
      CALL chkwpshdr (flg, cdfidg)
      rcode = nf90_inq_varid (cdfidg, 'LANDUSEF', varid)
      IF ( rcode == nf90_noerr ) THEN
        iflufrc = .TRUE.  ! fractional land use is in the file
      ELSE
        iflufrc = .FALSE. ! fractional land use is not in the file
      ENDIF
      rcode = nf90_close (cdfidg)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9700)  TRIM(pname),TRIM(flg)
        CALL graceful_stop (pname)
      ENDIF
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Determine whether or not the 2-m temperature, the 2-m mixing ratio, the
! 10-m wind components, and the turbulent kinetic energy are in the output,
! and set the flags appropriately.
!-------------------------------------------------------------------------------

  rcode = nf90_inq_varid (cdfid, 'T2', varid)
  IF ( rcode == nf90_noerr ) THEN
    ift2m = .TRUE.  ! 2-m temperature is in the file
  ELSE
    ift2m = .FALSE. ! 2-m temperature is not in the file
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'Q2', varid)
  IF ( rcode == nf90_noerr ) THEN
    IF ( met_pbl == 1 ) THEN  ! YSU PBL scheme
      ifq2m = .FALSE. ! do not use Q2 from YSU PBL; occasional winter negatives
    ELSE
      ifq2m = .TRUE.  ! 2-m mixing ratio is in the file
    ENDIF
  ELSE
    ifq2m = .FALSE. ! 2-m mixing ratio is not in the file
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'U10', varid)
  IF ( rcode == nf90_noerr ) THEN
    ifu10m = .TRUE.  ! 10-m u-component wind is in the file
  ELSE
    ifu10m = .FALSE. ! 10-m u-component wind is not in the file
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'V10', varid)
  IF ( rcode == nf90_noerr ) THEN
    ifv10m = .TRUE.  ! 10-m v-component wind is in the file
  ELSE
    ifv10m = .FALSE. ! 10-m v-component wind is not in the file
  ENDIF

  IF ( ( ifu10m ) .AND. ( ifv10m ) ) THEN
    ifw10m = .TRUE.
  ELSE
    ifw10m = .FALSE.
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'TKE_MYJ', varid)
  IF ( rcode == nf90_noerr ) THEN
    IF ( met_pbl == 2 ) THEN  ! Mellor-Yamada-Janjic (Eta)
      iftke  = .TRUE.  ! turbulent kinetic energy is in the file
      iftkef = .FALSE. ! TKE is not on full-levels; it is on half-layers
    ELSE
      iftke  = .FALSE. ! turbulent kinetic energy is not in the file
      iftkef = .FALSE.
    ENDIF
  ELSE
    iftke  = .FALSE. ! turbulent kinetic energy is not in the file
    iftkef = .FALSE.
  ENDIF

!-------------------------------------------------------------------------------
! Determine whether or not some surface variables are in the output, and set
! the flags appropriately.
!-------------------------------------------------------------------------------

  rcode = nf90_inq_varid (cdfid, 'LAI', varid)
  IF ( rcode == nf90_noerr ) THEN
    iflai = .TRUE.  ! leaf area index is in the file
  ELSE
    iflai = .FALSE. ! leaf area index is not in the file
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'RMOL', varid)
  IF ( rcode == nf90_noerr ) THEN
    ifmol = .TRUE.  ! (inverse) Monin-Obukhov length is in the file
  ELSE
    ifmol = .FALSE. ! (inverse) Monin-Obukhov length is not in the file
  ENDIF

  IF ( met_soil_lsm == 7 ) THEN  ! PX was used in WRF
    ifmolpx = .TRUE.
  ELSE
    ifmolpx = .FALSE.
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'RA', varid)
  IF ( rcode == nf90_noerr ) THEN
    ifra = .TRUE.  ! aerodynamic resistance is in the file
  ELSE
    ifra = .FALSE. ! aerodynamic resistance is not in the file
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'RS', varid)
  IF ( rcode == nf90_noerr ) THEN
    ifrs = .TRUE.  ! stomatal resistance is in the file
  ELSE
    ifrs = .FALSE. ! stomatal resistance is not in the file
  ENDIF

  IF ( ( ifra ) .AND. ( ifrs ) ) THEN
    ifresist = .TRUE.
  ELSE
    ifresist = .FALSE.
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'VEGFRA', varid)
  IF ( rcode == nf90_noerr ) THEN
    ifveg = .TRUE.  ! vegetation fraction is in the file
  ELSE
    ifveg = .FALSE. ! vegetation fraction is not in the file
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'CANWAT', varid)
  IF ( rcode == nf90_noerr ) THEN
    ifwr = .TRUE.  ! canopy wetness is in the file
  ELSE
    ifwr = .FALSE. ! canopy wetness is not in the file
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'SMOIS', varid)
  IF ( rcode == nf90_noerr ) THEN
    ifsmois = .TRUE.  ! soil moisture is in the file
  ELSE
    ifsmois = .FALSE. ! soil moisture is not in the file
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'TSLB', varid)
  IF ( rcode == nf90_noerr ) THEN
    iftslb = .TRUE.  ! soil temperature is in the file
  ELSE
    iftslb = .FALSE. ! soil temperature is not in the file
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'ISLTYP', varid)
  IF ( rcode == nf90_noerr ) THEN
    ifisltyp = .TRUE.  ! soil type is in the file
  ELSE
    ifisltyp = .FALSE. ! soil type is not in the file
  ENDIF

  If ( ( ifsmois ) .AND. ( iftslb ) .AND. ( ifisltyp ) ) THEN
    ifsoil = .TRUE.
  ELSE
    ifsoil = .FALSE.
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'ZNT', varid)
  IF ( rcode == nf90_noerr ) THEN
    ifznt = .TRUE.  ! roughness length is in the file
  ELSE
    ifznt = .FALSE. ! roughness length is not in the file
  ENDIF

!-------------------------------------------------------------------------------
! Determine the number of 3D cloud moisture species.  Assume that cloud water
! mixing ratio and rain water mixing ratio will occur together.  Also assume
! that cloud ice mixing ratio and cloud snow mixing ratio will occur together,
! but check for availability.  Check for graupel, as well.
! Note:  In WRFv2.1.2 and prior, the Eta/Ferrier microphysics scheme only
! outputs QCLOUD which represents total condensate, not cloud water mixing
! ratio.  CMAQv4.6 and prior cannot handle this field, so MCIP will stop in
! this case.
!-------------------------------------------------------------------------------

  rcode = nf90_inq_varid (cdfid, 'QCLOUD', varid)
  IF ( rcode == nf90_noerr ) THEN
    nqspecies = 1  ! QCLOUD is in the file
  ELSE  ! need hydrometeor fields for CMAQ
    WRITE (*,f9225) TRIM(pname)
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'QRAIN', varid)
  IF ( rcode == nf90_noerr ) THEN
    nqspecies = nqspecies + 1  ! QRAIN is in the file
  ELSE
    IF ( met_expl_moist == 5 ) THEN  ! Eta/Ferrier scheme
      WRITE (*,f9250) TRIM(pname)
      CALL graceful_stop (pname)
    ELSE
      WRITE (*,f9275) TRIM(pname)
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'QICE', varid)
  IF ( rcode == nf90_noerr ) THEN
    nqspecies = nqspecies + 1  ! QICE is in the file
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'QSNOW', varid)
  IF ( rcode == nf90_noerr ) THEN
    nqspecies = nqspecies + 1  ! QSNOW is in the file
  ENDIF

  IF ( nqspecies == 3 ) THEN  ! not set up for QI w/o QS or vice versa
    WRITE (*,f9300) TRIM(pname)
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'QGRAUP', varid)
  IF ( rcode == nf90_noerr ) THEN
    nqspecies = nqspecies + 1  ! QGRAUP is in the file
  ENDIF

  IF ( nqspecies == 3 ) THEN  ! not set up for QG without QI and QS
    WRITE (*,f9300) TRIM(pname)
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! Determine whether 3D resolved cloud fraction is part of WRF output.  If
! Kain-Fritsch scheme with radiative feedbacks to subgrid clouds is used (new
! in WRFv3.6) or if MSKF is used (new in WRFv3.7) in WRF, then the 3D cloud
! fraction includes both resolved and subgrid clouds.
!-------------------------------------------------------------------------------

  rcode = nf90_inq_varid (cdfid, 'CLDFRA', varid)
  IF ( rcode == nf90_noerr ) THEN
    IF ( TRIM(met_release) >= 'V3.6' ) THEN
      rcode = nf90_get_att (cdfid, nf90_global, 'ICLOUD_CU', icloud_cu)
      IF ( rcode == nf90_noerr ) THEN
        IF ( ( ( met_cumulus ==  1 ) .AND. ( icloud_cu == 2 ) ) .OR. &
               ( met_cumulus == 11 ) ) THEN
          ifcld3d = .FALSE.  ! 3D resolved cloud fraction is not in the file
        ELSE
          ifcld3d = .TRUE.  ! 3D resolved cloud fraction is in the file
        ENDIF
      ELSE
        ifcld3d = .TRUE.  ! 3D resolved cloud fraction is in the file
      ENDIF
    ELSE
      ifcld3d = .TRUE.  ! 3D resolved cloud fraction is in the file
    ENDIF
  ELSE
    ifcld3d = .FALSE. ! 3D cloud fraction is not if the file
  ENDIF

!-------------------------------------------------------------------------------
! Determine if the hybrid vertical coordinate has been used in WRF.  It is
! available as of WRFv3.9.  Currently it has not been tested with CMAQ and
! additional modifications would be required to handle some of the variable
! transformations in metvars2ctm.f90, as well as layer collapsing.
! +++ For now, flag this as a problem.
!-------------------------------------------------------------------------------

  IF ( wrfversion(18:19) >= "V9") THEN
    rcode = nf90_get_att (cdfid, nf90_global, 'HYBRID_OPT', met_hybrid)
    IF ( rcode == nf90_noerr ) THEN
      IF ( met_hybrid > 0 ) THEN
        WRITE (*,f9900) TRIM(pname)
        CALL graceful_stop (pname)
      ELSE
        WRITE (*,f9400) TRIM(pname), 'HYBRID_OPT', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF
  ELSE
    met_hybrid = -1
  ENDIF

END SUBROUTINE setup_wrfem
