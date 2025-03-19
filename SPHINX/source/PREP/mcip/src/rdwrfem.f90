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

SUBROUTINE rdwrfem (mcip_now)

!-------------------------------------------------------------------------------
! Name:     Read WRFv2 and WRFv3 (Eulerian Mass Core) Output
! Purpose:  Reads incoming WRFv2 and WRFv3 output files for use in MCIP.
! Notes:    Adapted from S.-B. Kim's get_wrf.F in WCIP.
! Revised:  31 Mar 2005  Original version.  (T. Otte)
!           15 Jul 2005  Modified variable retrievals so that the code will
!                        stop if a variable is not found.  Corrected print
!                        statement for sample output.  (T. Otte)
!           30 Jul 2007  Corrected error in processing incremental precipitation
!                        for first MCIP output time period when the first WRF
!                        WRF output time is not used by MCIP.  Added reads for
!                        fractional land use, leaf area index, aerodynamic and
!                        stomatal resistances, inverse Monin-Obukhov length, and
!                        soil moisture, temperature, and type, if those fields
!                        are available.  Removed read for emissivity.  Allowed
!                        for roughness length to be filled from a lookup table
!                        if it is not available in WRF output.  (T. Otte)
!           14 May 2008  Read TSLB (layer 1) if TSK is unavailable.  Change
!                        static data tables for roughness length to allow for
!                        up to 33 categories of USGS, and added error-checking
!                        when ZNT is set from the lookup table.  Corrected
!                        season assignment for lookup table for the Southern
!                        Hemisphere.  Check LAI, RA, and RSTOM to ensure that
!                        there are non-zero values in the fields, if they
!                        exist.  If the values of RA and/or RSTOM are all 0.0,
!                        reset IFRESIST flag so that they will be calculated
!                        later.  If LAI is in output but is 0.0, set LAI to
!                        realistic values for NOAH LSM.  Added 2-m mixing ratio
!                        (Q2) and turbulent kinetic energy (TKE), if available.
!                        Changed read on vegetation fraction to preferentially
!                        use VEGF_PX rather than VEGFRA for Pleim-Xiu land-
!                        surface model.  Changed algorithm to find "valid"
!                        data to require time difference to be < TTOL rather
!                        than <= TTOL.  Added urban fraction (FRC_URB),
!                        urban roughness length (Z0C_URB2D), and urban Monin-
!                        Obukhov length (XXXC_URB) for MET_UCMCALL=1.  Added
!                        error checking to ensure that WRF files used in this
!                        MCIP run are from the same simulation so that
!                        incremental precipitation totals in RN and RC are
!                        processed correctly.  (T. Otte)
!           29 Oct 2009  Cleaned up file opening and logging for WRF I/O API,
!                        particularly when the WRF headers of new files are
!                        checked, to prevent condition with too many files open
!                        for long simulations.  Changed MET_UCMCALL to
!                        MET_URBAN_PHYS, and allowed for variable to be set to
!                        be greater than 1.  Capture potential temperature
!                        (THETA) and Coriolis (CORIOLIS) when potential
!                        vorticity is needed.  Changed method of computing
!                        latitude, longitude, and map-scale factor arrays to
!                        be more general; removed subroutine GRIDGEOMETRY.
!                        Added default roughness length values for NCLD-MODIS,
!                        SiB, and MODIS-NOAH.  Increased MAX_TIMES to 1000 to
!                        enable processing of longer data sets.  Removed
!                        DUM2D_D.  Added latitude, longitude, and map-scale
!                        factors on U and V faces.  Allow output from WRF
!                        Preprocessing System (WPS) routine, GEOGRID, to
!                        provide fractional land use output if it is unavailable
!                        in WRF output.  Removed Z0C_URB2D.  Corrected units
!                        for U10 and V10 in log file.  Changed error condition
!                        to warning condition if LAI is set to zero on input
!                        and LSM other than NOAH was used.  Changed reads of
!                        fractional land use and roughness length so that they
!                        are only performed if those fields are known to exist.
!                        Changed real-number comparisons of maximum values from
!                        "equivalences" to "less than tolerances".  (T. Otte)
!           12 Feb 2010  Removed unused variables COMM and SYSDEP_INFO, and
!                        removed unused format 9600.  (T. Otte)
!           18 Mar 2010  Added CDFID as an input argument for subroutine
!                        CHKWRFHDR.  Changed all calls to netCDF routines to use
!                        the Fortran interface rather than the C interface.
!                        Changed input arguments for routines in WRF_NETCDF
!                        from FILENAME to CDFID to minimize I/O.  (T. Otte)
!           23 Dec 2010  Improved support for long MCIP runs from long WRF
!                        runs by increasing MAX_TIMES to 9999.  Also added
!                        missing "close" command for incoming WRF files.
!                        Added sea ice.  Added support for precipitation
!                        tipping bucket option in WRF.  Changed latitude and
!                        longitude calculations for polar stereograhic
!                        projection to interpolations.  (T. Otte)
!           31 Aug 2011  Changed name of module FILE to FILES to avoid conflict
!                        with F90 protected intrinsic.  Updated netCDF commands
!                        to F90, and improved error handling.  Replaced calls
!                        to GET_TIMES_CDF with explicit netCDF functions.
!                        Changed F77 character declarations to F90 standard.
!                        Changed DATA statements to parameters.  Changed
!                        arguments to 19-character elements for GETH_IDTS.
!                        (T. Otte)
!           07 Sep 2011  Updated disclaimer.  (T. Otte)
!           21 Nov 2011  Corrected error in tipping bucket precipitation
!                        calculation.  (T. Otte)
!           21 Aug 2012  Added MET_PCP_INCR to accommodate WRFv3.3 option to
!                        output incremental precipitation.  (T. Otte)
!           11 Sep 2012  Added handling for 40-category 2006 NLCD-MODIS land
!                        use classification as "NLCD40".  Added alternate name
!                        for 50-category 2001 NLCD-MODIS land use classification
!                        as "NLCD50".  Changed SFZ0NLCSUM and SFZ0NLCWIN to
!                        SFZ0NLCD50SUM and SFZ0NLCD50WIN.  Added analogous
!                        arrays for NLCD40.  Updated values for roughness length
!                        for both NLCD-MODIS classifications using tables from
!                        WRFv3.4 module_sf_pxlsm_data.F.  Added read of array
!                        LANDMASK to be used with runs that used Pleim-Xiu LSM.
!                        (T. Otte)
!           26 Nov 2014  Corrected formatting error in error-handling F9400,
!                        and corrected four locations of this routine that now
!                        reference F9400 incorrectly.  Removed requirement to
!                        have FRC_URB available when urban canopy model is used
!                        in WRF.  (T. Spero)
!           10 Apr 2015  If 3D resolved cloud fraction is in the WRF output,
!                        collect that field to pass through to output.
!                        (T. Spero)
!           25 Aug 2015  Changed latent heat flux from QFX to LH.  Fill THETA
!                        and moisture flux (QFX) for IFMOLACM.  If Pleim-Xiu
!                        land-surface model is used, realign soil categories
!                        to be consistent with WRF documentation.  (T. Spero)
!           08 Sep 2015  Commented out realignment of soil categories for
!                        Pleim-Xiu land-surface model because CMAQ cannot
!                        handle this yet.  (T. Spero)
!           17 Sep 2015  Changed IFMOLACM to IFMOLPX.  (T. Spero)
!           30 Oct 2015  Changed WRITE statements for printing sampled data to
!                        log file to eliminate warning messages.  (T. Spero)
!           22 Nov 2016  Changed urban model variable FRC_URB to FRC_URB2D to
!                        be consistent with its use in WRF.  (T. Spero)
!           21 Apr 2017  Updated SFZ0 for MODIS so that category 21 is "Lake".
!                        (T. Spero)
!           16 Mar 2018  Corrected the settings for II and JJ in the loop for
!                        calculating dot-point latitude and longitude for polar
!                        stereographic WRF projections.  Moved TTOL to
!                        MCIPPARM_MOD, and changed its local name to TTOL_SEC.
!                        Corrected error in print statement for WRF variable
!                        CLDFRA.  Added SNOWH to output.  Created a minimum
!                        value for rainfall in order to avoid underflow
!                        condition.  Corrected minor error in array mapping in
!                        rain buckets in unused column and row.  Added
!                        LUFRAC2, MOSCATIDX, LAI_MOS, RA_MOS, RS_MOS, TSK_MOS,
!                        ZNT_MOS, and DUM3D_M to support NOAH Mosaic land-
!                        surface model.  Added DZS to capture soil layers, and
!                        added 3D soil arrays, SOIT3D and SOIM3D.  Added
!                        WSPDSFC and XLAIDYN for Noah.  (T. Spero)
!           27 Jun 2018  Changed name of module with netCDF IO to broaden its
!                        usage.  Removed local aliases for dimensions of input
!                        meteorological fields.  (T. Spero)
!           14 Sep 2018  Changed condition to enable hybrid vertical coordinate
!                        from WRF.  Removed support for MM5v3 input.  (T. Spero)
!           16 Oct 2018  Corrected error in computing precipitation amounts when
!                        the tipping bucket is used and less than 0.5 mm of
!                        precipitation accumulated during the same hour that the
!                        bucket tips; corrects erroneous precipitation spikes.
!                        Corrected error in array mapping for precipitation on
!                        initial time step in the outermost row and column
!                        (dummy cells not used by CMAQ).
!                        (C. Nolte and T. Spero)
!           23 Nov 2018  Changed local usages of NX, NY, and NZ to MET_NX,
!                        MET_NY, and MET_NZ to avoid confusion with generic
!                        usages of those variables for global dimensions in
!                        netCDF output.  (T. Spero)
!           18 Jun 2019  Added new surface variables with PX LSM that can
!                        improve dust simulation in CCTM.  Added optional
!                        variables from KF convective scheme with radiative
!                        feedbacks.  (T. Spero)
!           06 Mar 2020  Removed need to read "F" (Coriolis parameter) from WRF
!                        output for potential vorticity scaling.  Instead,
!                        calculate F inside this routine (called "CORIOLIS"
!                        here) from latitude.  Value of angular momentum of
!                        earth (omega in new variable TWOOMEGA) is from WRF
!                        variable "EOMEG" in WRF routine:
!                        share/module_model_constants.f90.  (T. Spero)
!           17 Jun 2021  Modified most recent change that calculates Coriolis
!                        parameter so that it does not rely on a non-standard
!                        Fortran intrinsic (SIND), which is only available
!                        for select compilers. (T. Spero)
!           13 Dec 2023  Removed redundant NF90_OPEN/NF90_CLOSE couplet to
!                        improve efficiency and memory management. (T. Spero)
!-------------------------------------------------------------------------------

  USE date_pack
  USE files
  USE metinfo
  USE metvars
  USE mcipparm
  USE netcdf_io
  USE netcdf

  IMPLICIT NONE

  INTEGER, SAVE                     :: cdfid
  INTEGER                           :: cdfidg
  REAL                              :: deg2rad
  INTEGER                           :: dimids     ( nf90_max_var_dims )
  REAL,    SAVE,      ALLOCATABLE   :: dum2d      ( : , : )
  INTEGER, SAVE,      ALLOCATABLE   :: dum2d_i    ( : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum2d_u    ( : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum2d_v    ( : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum3d_l    ( : , : , : )
  INTEGER, SAVE,      ALLOCATABLE   :: dum3d_li   ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum3d_m    ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum3d_p    ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum3d_s    ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum3d_t    ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum3d_u    ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum3d_v    ( : , : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum3d_w    ( : , : , : )
  CHARACTER(LEN=19)                 :: endseas
  LOGICAL, SAVE                     :: first      = .TRUE.
  CHARACTER(LEN=256)                :: fl
  CHARACTER(LEN=256)                :: flg
  LOGICAL                           :: gotfaces   = .TRUE.
  LOGICAL                           :: gotseaice
  LOGICAL                           :: gotznt
  INTEGER                           :: i
  INTEGER                           :: id_data
  INTEGER                           :: idts_end
  INTEGER                           :: idts_start
  INTEGER                           :: idtsec
  LOGICAL                           :: iffl
  CHARACTER(LEN=64)                 :: ifmt1
  CHARACTER(LEN=64)                 :: ifmt1a
  CHARACTER(LEN=64)                 :: ifmt2
  CHARACTER(LEN=64)                 :: ifmt3
  CHARACTER(LEN=64)                 :: ifmt4
  CHARACTER(LEN=64)                 :: ifmt5
  INTEGER                           :: ii
  INTEGER                           :: im1
  INTEGER                           :: it
  INTEGER, SAVE                     :: it_start
  INTEGER                           :: itm1
  INTEGER                           :: j
  INTEGER                           :: jj
  INTEGER                           :: jm1
  INTEGER                           :: k
  INTEGER                           :: k1
  INTEGER                           :: k2
  REAL                              :: latrad
  INTEGER                           :: lent
  REAL,               EXTERNAL      :: mapfac_lam
  REAL,               EXTERNAL      :: mapfac_merc
  REAL,               EXTERNAL      :: mapfac_ps
  CHARACTER(LEN=24),  INTENT(IN)    :: mcip_now
  CHARACTER(LEN=24)                 :: mcip_previous
  INTEGER                           :: m1count    = 1
  INTEGER, SAVE                     :: mmcount    = 1
  INTEGER, SAVE                     :: n_times
  LOGICAL, SAVE                     :: newfile    = .TRUE.
  LOGICAL                           :: newfilem1  = .TRUE.
  INTEGER                           :: nxm
  INTEGER                           :: nym
  INTEGER                           :: nzp
  REAL                              :: pi
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'RDWRFEM'
  INTEGER                           :: rcode
  REAL,               PARAMETER     :: rdovcp     = 2.0 / 7.0
  REAL,               PARAMETER     :: smallnum   = 1.0e-7
  CHARACTER(LEN=19)                 :: startseas
  CHARACTER(LEN=2)                  :: str1
  CHARACTER(LEN=2)                  :: str2
  CHARACTER(LEN=19),SAVE,ALLOCATABLE:: times      ( : )
  REAL,               PARAMETER     :: twoomega   = 2.0 * 7.2921e-5 ! [s-1]
  REAL                              :: xoff
  REAL                              :: xxin
  REAL                              :: yoff
  REAL                              :: yyin

  ! Define roughness length as functions of land use and season in case
  ! it is not available in WRF output.

  REAL, PARAMETER :: sfz0oldsum ( 13 ) = &  ! summer [cm]
    (/ 50.0,  15.0,  12.0,  50.0,  50.0,  40.0,  0.01, 20.0,   &
       10.0,  10.0,   5.0,  50.0,  15.0 /)

  REAL, PARAMETER :: sfz0oldwin ( 13 ) = &  ! winter [cm]
    (/ 50.0,   5.0,  10.0,  50.0,  50.0,  40.0,  0.01, 20.0,   &
       10.0,  10.0,   5.0,  50.0,  15.0 /)

  REAL, PARAMETER :: sfz0modsum ( 33 ) = &  ! summer [cm]
    (/ 50.0,  50.0,  50.0,  50.0,  50.0,   5.0,  6.0,   5.0,   &
       15.0,  12.0,  30.0,  15.0,  80.0,  14.0,  0.1,   1.0,   &
        0.01, 30.0,  15.0,  10.0,   0.01, 80.0,  80.0,  80.0,  &
       80.0,  80.0,  80.0,  80.0,  80.0,  80.0,  80.0,  80.0,  80.0 /)

  REAL, PARAMETER :: sfz0modwin ( 33 ) = &  ! winter [cm]
    (/ 50.0,  50.0,  50.0,  50.0,  20.0,   1.0,   1.0,   1.0,  &
       15.0,  50.0,  30.0,   5.0,  80.0,   5.0,   0.1,   1.0,  &
        0.01, 10.0,  30.0,  15.0,   0.01, 80.0,  80.0,  80.0,  &
       80.0,  80.0,  80.0,  80.0,  80.0,  80.0,  80.0,  80.0,  80.0 /)

  REAL, PARAMETER :: sfz0nlcd50sum ( 50 ) = &  ! summer [cm]
    (/  0.1,   1.2,  30.0,  40.0,  60.0, 100.0,   5.0,   5.0,  &
      100.0, 100.0, 100.0,  10.0,  15.0,   7.0,   7.0,   5.0,  &
        5.0,   5.0,   7.0,  10.0,  55.0,  80.0,  30.0,  60.0,  &
       30.0,  11.0,  11.0,  11.0,   5.0,   5.0,   0.1, 100.0,  &
       90.0, 100.0, 100.0, 100.0,  15.0,  15.0,  25.0,  15.0,  &
        7.0,  20.0,  10.0,  80.0,  30.0,   1.2,   5.0,   0.1,  &
        0.1,   0.1 /)

  REAL, PARAMETER :: sfz0nlcd50win ( 50 ) = &  ! winter [cm]
    (/  0.1,   1.2,  30.0,  40.0,  60.0, 100.0,   5.0,   5.0,  &
      100.0, 100.0, 100.0,  10.0,  15.0,   7.0,   7.0,   5.0,  &
        5.0,   5.0,   7.0,  10.0,  55.0,  80.0,  30.0,  60.0,  &
       30.0,  11.0,  11.0,  11.0,   5.0,   5.0,   0.1, 100.0,  &
       90.0, 100.0, 100.0, 100.0,  15.0,  15.0,  25.0,  15.0,  &
        7.0,  20.0,  10.0,  80.0,  30.0,   1.2,   5.0,   0.1,  &
        0.1,   0.1 /)

  REAL, PARAMETER :: sfz0nlcd40sum ( 40 ) = &  ! summer [cm]
    (/100.0,  90.0, 100.0, 100.0, 100.0,  30.0,  15.0,  25.0,  &
       15.0,   7.0,  20.0,  10.0,  80.0,  30.0,   1.2,   5.0,  &
        0.1,   0.1,   0.1,   0.1,   0.1,   1.2,  30.0,  40.0,  &
       60.0, 100.0,   5.0, 100.0, 100.0, 100.0,  10.0,  15.0,  &
        7.0,   7.0,   5.0,   5.0,   7.0,  10.0,  55.0,  11.0 /)

  REAL, PARAMETER :: sfz0nlcd40win ( 40 ) = &  ! winter [cm]
    (/100.0,  90.0, 100.0, 100.0, 100.0,  30.0,  15.0,  25.0,  &
       15.0,   7.0,  20.0,  10.0,  80.0,  30.0,   1.2,   5.0,  &
        0.1,   0.1,   0.1,   0.1,   0.1,   1.2,  30.0,  40.0,  &
       60.0, 100.0,   5.0, 100.0, 100.0, 100.0,  10.0,  15.0,  &
        7.0,   7.0,   5.0,   5.0,   7.0,  10.0,  55.0,  11.0 /)

  REAL, PARAMETER :: sfz0sibsum ( 16 ) = &  ! summer [cm]
    (/ 50.0,  50.0,  40.0,  50.0,  50.0,  15.0,  12.0,  12.0,  &
       12.0,  10.0,  10.0,  15.0,  20.0,  12.0,   0.01,  5.0 /)

  REAL, PARAMETER :: sfz0sibwin ( 16 ) = &  ! winter [cm]
    (/ 50.0,  50.0,  40.0,  50.0,  50.0,  15.0,  10.0,  10.0,  &
       10.0,  10.0,  10.0,   5.0,  20.0,  10.0,   0.01,  5.0 /)

  REAL, PARAMETER :: sfz0usgssum ( 33 ) = &  ! summer [cm]
    (/ 80.0,  15.0,  10.0,  15.0,  14.0,  20.0,  12.0,   5.0,  &
        6.0,  15.0,  50.0,  50.0,  50.0,  50.0,  50.0,   0.01, &
       20.0,  40.0,   1.0,  10.0,  30.0,  15.0,  10.0,   5.0,  &
        1.0,  15.0,   1.0,  80.0,  80.0,  80.0,  80.0,  80.0,  80.0 /)

  REAL, PARAMETER :: sfz0usgswin ( 33 ) = &  ! winter [cm]
    (/ 80.0,   5.0,   2.0,   5.0,   5.0,  20.0,  10.0,   1.0,  &
        1.0,  15.0,  50.0,  50.0,  50.0,  50.0,  20.0,   0.01, &
       20.0,  40.0,   1.0,  10.0,  30.0,  15.0,   5.0,   5.0,  &
        1.0,  15.0,   1.0,  80.0,  80.0,  80.0,  80.0,  80.0,  80.0 /)

!-------------------------------------------------------------------------------
! Error, warning, and informational messages.
!-------------------------------------------------------------------------------

  CHARACTER(LEN=256), PARAMETER :: f6000 = "(1x, a, 1x, f12.4, 2x, a)"
  CHARACTER(LEN=256), PARAMETER :: f6100 = "(1x, a, 1x, i12,   2x, a)"

  CHARACTER(LEN=256), PARAMETER :: f9100 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a, &
    & /, 1x, '***   NO MORE INPUT WRF FILES', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9200 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a, &
    & /, 1x, '***   INPUT FILE NUMBER ', i3, ' IS BLANK', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9300 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a, &
    & /, 1x, '***   COULD NOT FIND FILE ', a, &
    & /, 1x, '***   FILE MAY NOT EXIST', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9400 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE', &
    & /, 1x, '***   VARIABLE = ', a, &
    & /, 1x, '***   RCODE = ', a, &
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
    & /, 1x, '***   UNKNOWN LAND USE CLASSIFICATION SYSTEM', &
    & /, 1x, '***   LAND USE SOURCE = ', a, &
    & /, 1x, '***   HIGHEST INDEX FOUND = ', i4, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9700 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNABLE TO SET ZNT FROM LOOKUP TABLE', &
    & /, 1x, '***   LAND USE SOURCE = ', a, &
    & /, 1x, '***   NUMBER OF LAND USE CATEGORIES = ', i3, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9800 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   UNABLE TO BLEND ', a, ' FOR UCM', &
    & /, 1x, '***   UNKNOWN LAND USE SOURCE', &
    & /, 1x, '***   LAND USE SOURCE = ', a, &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9900 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR OPENING WRF NETCDF FILE', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9950 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   ERROR CLOSING WRF NETCDF FILE', &
    & /, 1x, 70('*'))"

  CHARACTER(LEN=256), PARAMETER :: f9975 = "(/, 1x, 70('*'), &
    & /, 1x, '*** SUBROUTINE: ', a, &
    & /, 1x, '***   DID NOT FIND ARRAY ', a, &
    & /, 1x, '***   WILL DEFINE FROM OTHER FIELDS LATER', &
    & /, 1x, 70('*'))"

!-------------------------------------------------------------------------------
! Define additional staggered grid dimensions.
!-------------------------------------------------------------------------------

  nxm = met_nx - 1
  nym = met_ny - 1
  nzp = met_nz + 1

!-------------------------------------------------------------------------------
! Set up print statements.
!-------------------------------------------------------------------------------

  k1 = met_nz / 5
  k2 = MOD(met_nz, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)),/,10x," &
        & // str2 // "(1x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a,5(1x,f12.4),/,11x," // str2 // "(1x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a,5(1x,f12.4))"
    ENDIF
  ENDIF

  k1 = (nzp) / 5
  k2 = MOD(nzp, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1a = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)),/,10x," &
        & // str2 // "(1x,f12.4))"
    ELSE
      ifmt1a = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1a = "(/,1x,a,5(1x,f12.4),/,10x," // str2 // "(1x,f12.4))"
    ELSE
      ifmt1a = "(/,1x,a,5(1x,f12.4))"
    ENDIF
  ENDIF

  k1 = nummetlu / 5
  k2 = MOD(nummetlu, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt2 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)),/,10x," &
        & // str2 // "(1x,f12.4))"
      ifmt3 = "(/,1x,a,5(i12,1x)," // str1 // "(/,10x,5(1x,i12)),/,10x," &
        & // str2 // "(1x,i12))"
    ELSE
      ifmt2 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)))"
      ifmt3 = "(/,1x,a,5(i12,1x)," // str1 // "(/,10x,5(1x,i12)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt2 = "(/,1x,a,5(1x,f12.4),/,10x," // str2 // "(1x,f12.4))"
      ifmt3 = "(/,1x,a,5(i12,1x),/,10x," // str2 // "(1x,i12))"
    ELSE
      ifmt2 = "(/,1x,a,5(1x,f12.4))"
      ifmt3 = "(/,1x,a,5(i12,1x))"
    ENDIF
  ENDIF

  k1 = nummosaic / 5
  k2 = MOD(nummosaic, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt4 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)),/,10x," &
        & // str2 // "(1x,f12.4))"
    ELSE
      ifmt4 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt4 = "(/,1x,a,5(1x,f12.4),/,10x," // str2 // "(1x,f12.4))"
    ELSE
      ifmt4 = "(/,1x,a,5(1x,f12.4))"
    ENDIF
  ENDIF

  k1 = met_ns / 5
  k2 = MOD(met_ns, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt5 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)),/,10x," &
        & // str2 // "(1x,f12.4))"
    ELSE
      ifmt5 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt5 = "(/,1x,a,5(1x,f12.4),/,11x," // str2 // "(1x,f12.4))"
    ELSE
      ifmt5 = "(/,1x,a,5(1x,f12.4))"
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( dum2d   ) )  & 
    ALLOCATE ( dum2d   (nxm, nym)      )        ! 2D, cross points
  IF ( .NOT. ALLOCATED ( dum2d_i ) )  &
    ALLOCATE ( dum2d_i (nxm, nym)      )        ! 2D integer, cross points
  IF ( .NOT. ALLOCATED ( dum2d_u ) )  &
    ALLOCATE ( dum2d_u (met_nx, nym ) )         ! 2D, E-W flux pts
  IF ( .NOT. ALLOCATED ( dum2d_v ) )  &
    ALLOCATE ( dum2d_v (nxm, met_ny  ) )        ! 2D, N-S flux pts
  IF ( .NOT. ALLOCATED ( dum3d_l ) )  &
    ALLOCATE ( dum3d_l (nxm, nym, nummetlu ) )  ! 3D, cross points, lu
  IF ( .NOT. ALLOCATED ( dum3d_li ) ) &
    ALLOCATE ( dum3d_li (nxm, nym, nummetlu ) ) ! 3D, cross points, lu int
  IF ( .NOT. ALLOCATED ( dum3d_m ) )  &
    ALLOCATE ( dum3d_m (nxm, nym, nummosaic) )  ! 3D, cross pts in mosaic cat
  IF ( .NOT. ALLOCATED ( dum3d_p ) )  &
    ALLOCATE ( dum3d_p (nxm, nym, met_nz ) )    ! 3D, cross points, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_s ) )  &
    ALLOCATE ( dum3d_s (nxm, nym, met_ns ) )    ! 3D, cross points, soil lvls
  IF ( .NOT. ALLOCATED ( dum3d_t ) )  &
    ALLOCATE ( dum3d_t (nxm, nym, met_nz ) )    ! 3D, cross points, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_u ) )  &
    ALLOCATE ( dum3d_u (met_nx, nym, met_nz ) ) ! 3D, E-W flux pts, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_v ) )  &
    ALLOCATE ( dum3d_v (nxm, met_ny, met_nz ) ) ! 3D, N-S flux pts, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_w ) )  &
    ALLOCATE ( dum3d_w (nxm, nym, nzp) )        ! 3D, cross points, full lvls

!-------------------------------------------------------------------------------
! If not processing the first output time of the WRF run (and if not using the
! incremental precipitation option available in WRFv3.2+), retrieve accumulated
! precipitation totals from time increment before first MCIP step so that
! first incremental precipitation "rates" can be computed.  This step ensures
! that the "hold" values for convective and non-convective precipitation are
! correctly set with last accumulated total.
!-------------------------------------------------------------------------------

  gotseaice = .FALSE.

  IF ( ( first ) .AND. ( mcip_now > met_startdate )  &
       .AND. ( met_pcp_incr == 0 ) ) THEN

    CALL geth_newdate (mcip_previous, mcip_now, intvl*(-60))

    fl = file_mm(m1count)

    findprev: DO
      IF ( newfilem1 ) THEN
        rcode = nf90_open (fl, nf90_nowrite, cdfid)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9900) TRIM(pname)
          CALL graceful_stop (pname)
        ENDIF
        CALL chkwrfhdr (fl, cdfid)
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
        newfilem1  = .FALSE.
      ENDIF
      DO i = 1, n_times
        CALL geth_idts (times(i), mcip_previous(1:19), idtsec)
        IF ( ABS(idtsec) < ttol_sec ) THEN  ! found MCIP_PREVIOUS in WRF output
          itm1 = i
          EXIT findprev
        ENDIF
      ENDDO
      IF ( i > n_times ) THEN
        rcode = nf90_close (cdfid)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9950) TRIM(pname)
          CALL graceful_stop (pname)
        ENDIF
        newfilem1 = .TRUE.
        m1count   = m1count + 1
        IF ( m1count > max_mm ) THEN
          WRITE (*,f9100) TRIM(pname), mcip_previous
          CALL graceful_stop (pname)
        ENDIF
        fl = file_mm(m1count)
        IF ( fl(1:10) == '          ' ) THEN
          WRITE (*,f9200) TRIM(pname), mcip_previous, m1count
          CALL graceful_stop (pname)
        ENDIF
        INQUIRE (FILE=fl, EXIST=iffl)
        IF ( .NOT. iffl ) THEN
          WRITE (*,f9300) TRIM(pname), mcip_previous, TRIM(fl)
          CALL graceful_stop (pname)
        ENDIF
      ENDIF
    ENDDO findprev

    CALL get_var_2d_real_cdf (cdfid, 'RAINC', dum2d, itm1, rcode)
    IF ( rcode == nf90_noerr ) THEN
      rcold(1:nxm,1:nym) = dum2d(:,:)
      rcold(met_nx,:) = rcold(nxm,:)
      rcold(:,met_ny) = rcold(:,nym)
      WHERE ( rcold < smallnum )
        rcold = 0.0
      ENDWHERE
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RAINC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'RAINNC', dum2d, itm1, rcode)
    IF ( rcode == nf90_noerr ) THEN
      rnold(1:nxm,1:nym) = dum2d(:,:)
      rnold(met_nx,:) = rnold(nxm,:)
      rnold(:,met_ny) = rnold(:,nym)
      WHERE ( rnold < smallnum )
        rnold = 0.0
      ENDWHERE
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RAINNC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    IF ( met_rain_bucket > 0.0 ) THEN  ! tipping bucket is on
    
      CALL get_var_2d_int_cdf (cdfid, 'I_RAINC', dum2d_i, itm1, rcode)
      IF ( rcode == nf90_noerr ) THEN
        ircold(1:nxm,1:nym) = dum2d_i(:,:)
        ircold(met_nx,:) = ircold(nxm,:)
        ircold(:,met_ny) = ircold(:,nym)
      ELSE
        WRITE (*,f9400) TRIM(pname), 'I_RAINC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      CALL get_var_2d_int_cdf (cdfid, 'I_RAINNC', dum2d_i, itm1, rcode)
      IF ( rcode == nf90_noerr ) THEN
        irnold(1:nxm,1:nym) = dum2d_i(:,:)
        irnold(met_nx,:) = irnold(nxm,:)
        irnold(:,met_ny) = irnold(:,nym)
      ELSE
        WRITE (*,f9400) TRIM(pname), 'I_RAINNC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

    ENDIF  ! tipping bucket

    rcode = nf90_close (cdfid)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9950) TRIM(pname)
      CALL graceful_stop (pname)
    ENDIF

  ENDIF

!-------------------------------------------------------------------------------
! Find time index (IT) for MCIP_NOW in WRF output file.
!-------------------------------------------------------------------------------

  fl = file_mm(mmcount)

  findit: DO
    IF ( newfile ) THEN
      rcode = nf90_open (fl, nf90_nowrite, cdfid)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9900) TRIM(pname)
        CALL graceful_stop (pname)
      ENDIF
      CALL chkwrfhdr (fl, cdfid)
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
      newfile  = .FALSE.
      it_start = 1
    ENDIF
    DO i = it_start, n_times
      CALL geth_idts (times(i), mcip_now(1:19), idtsec)
      IF ( ABS(idtsec) < ttol_sec ) THEN  ! found MCIP_NOW in WRF output
        it = i
        IF ( i < n_times ) it_start = i + 1
        EXIT findit
      ENDIF
    ENDDO
    IF ( i > n_times ) THEN
      rcode = nf90_close (cdfid)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9950) TRIM(pname)
        CALL graceful_stop (pname)
      ENDIF
      newfile = .TRUE.
      mmcount = mmcount + 1
      IF ( mmcount > max_mm ) THEN
        WRITE (*,f9100) TRIM(pname), mcip_now
        CALL graceful_stop (pname)
      ENDIF
      fl = file_mm(mmcount)
      IF ( fl(1:10) == '          ' ) THEN
        WRITE (*,f9200) TRIM(pname), mcip_now, mmcount
        CALL graceful_stop (pname)
      ENDIF
      INQUIRE (FILE=fl, EXIST=iffl)
      IF ( .NOT. iffl ) THEN
        WRITE (*,f9300) TRIM(pname), mcip_now, TRIM(fl)
        CALL graceful_stop (pname)
      ENDIF
    ENDIF
  ENDDO findit

!-------------------------------------------------------------------------------
! Read WRF data for this domain.
!-------------------------------------------------------------------------------

  CALL get_var_3d_real_cdf (cdfid, 'U', dum3d_u, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    ua(:,1:nym,   :) = dum3d_u(:,:,:)
    ua(:,  met_ny,:) = ua(:,nym,:)
    WRITE (*,ifmt1) 'U        ', (ua(lprt_metx,lprt_mety,k),k=1,met_nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'U', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'V', dum3d_v, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    va(1:nxm   ,:,:) = dum3d_v(:,:,:)
    va(  met_nx,:,:) = va(nxm,:,:)
    WRITE (*,ifmt1) 'V        ', (va(lprt_metx,lprt_mety,k),k=1,met_nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'V', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'W', dum3d_w, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    wa(1:nxm,   1:nym,   :) = dum3d_w(:,:,:)
    wa(  met_nx, :,      :) = wa(nxm,:,:)
    wa( :,        met_ny,:) = wa(:,nym,:)
    WRITE (*,ifmt1a) 'W        ', (wa(lprt_metx,lprt_mety,k),k=1,nzp)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'W', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'PH', dum3d_w, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    ph(1:nxm,   1:nym,   :) = dum3d_w(:,:,:)
    ph(  met_nx, :,      :) = ph(nxm,:,:)
    ph( :,        met_ny,:) = ph(:,nym,:)
    WRITE (*,ifmt1a) 'PH       ', (ph(lprt_metx,lprt_mety,k),k=1,nzp)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'PH', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'PHB', dum3d_w, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    phb(1:nxm,   1:nym,   :) = dum3d_w(:,:,:)
    phb(  met_nx, :,      :) = phb(nxm,:,:)
    phb( :,        met_ny,:) = phb(:,nym,:)
    WRITE (*,ifmt1a) 'PHB      ', (phb(lprt_metx,lprt_mety,k),k=1,nzp)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'PHB', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'P', dum3d_p, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    pp(1:nxm,   1:nym,   :) = dum3d_p(:,:,:)
    pp(  met_nx, :,      :) = pp(nxm,:,:)
    pp( :,        met_ny,:) = pp(:,nym,:)
    WRITE (*,ifmt1) 'P        ', (pp(lprt_metx,lprt_mety,k),k=1,met_nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'P', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'PB', dum3d_p, it, rcode)
  IF ( rcode == 0 ) THEN
    pb(1:nxm,   1:nym,   :) = dum3d_p(:,:,:)
    pb(  met_nx, :,      :) = pb(nxm,:,:)
    pb( :,        met_ny,:) = pb(:,nym,:)
    WRITE (*,ifmt1) 'PB       ', (pb(lprt_metx,lprt_mety,k),k=1,met_nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'PB', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'T', dum3d_t, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    dum3d_p(:,:,:) = dum3d_p(:,:,:) + pp(1:nxm,1:nym,:)   ! pressure [Pa]
    dum3d_t(:,:,:) = dum3d_t(:,:,:) + 300.0               ! theta [K]
    IF ( lpv > 0 .OR. ifmolpx ) THEN  ! need theta
      theta(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
      theta(  met_nx, :,      :) = theta(nxm,:,:)
      theta( :,        met_ny,:) = theta(:,nym,:)
      WRITE (*,ifmt1) 'THETA    ', (theta(lprt_metx,lprt_mety,k),k=1,met_nz)
    ENDIF
    ta(1:nxm,   1:nym,   :) = dum3d_t(:,:,:) * (dum3d_p(:,:,:)/100000.0)**rdovcp
    ta(  met_nx, :,      :) = ta(nxm,:,:)
    ta( :,        met_ny,:) = ta(:,nym,:)
    WRITE (*,ifmt1) 'T        ', (ta(lprt_metx,lprt_mety,k),k=1,met_nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'T', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'QVAPOR', dum3d_t, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    qva(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
    qva(  met_nx, :,      :) = qva(nxm,:,:)
    qva( :,        met_ny,:) = qva(:,nym,:)
    WRITE (*,ifmt1) 'QVAPOR   ', (qva(lprt_metx,lprt_mety,k),k=1,met_nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'QVAPOR', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'QCLOUD', dum3d_t, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    qca(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
    qca(  met_nx, :,      :) = qca(nxm,:,:)
    qca( :,        met_ny,:) = qca(:,nym,:)
    WRITE (*,ifmt1) 'QCLOUD   ', (qca(lprt_metx,lprt_mety,k),k=1,met_nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'QCLOUD', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'QRAIN', dum3d_t, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    qra(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
    qra(  met_nx, :,      :) = qra(nxm,:,:)
    qra( :,        met_ny,:) = qra(:,nym,:)
    WRITE (*,ifmt1) 'QRAIN    ', (qra(lprt_metx,lprt_mety,k),k=1,met_nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'QRAIN', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'QICE', rcode)
  IF ( rcode == nf90_noerr ) THEN
    CALL get_var_3d_real_cdf (cdfid, 'QICE', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      qia(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
      qia(  met_nx, :,      :) = qia(nxm,:,:)
      qia( :,        met_ny,:) = qia(:,nym,:)
      WRITE (*,ifmt1) 'QICE     ', (qia(lprt_metx,lprt_mety,k),k=1,met_nz)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'QICE', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    qia(:,:,:) = 0.0
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'QSNOW', rcode)
  IF ( rcode == nf90_noerr ) THEN
    CALL get_var_3d_real_cdf (cdfid, 'QSNOW', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      qsa(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
      qsa(  met_nx, :,      :) = qsa(nxm,:,:)
      qsa( :,        met_ny,:) = qsa(:,nym,:)
      WRITE (*,ifmt1) 'QSNOW    ', (qsa(lprt_metx,lprt_mety,k),k=1,met_nz)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'QSNOW', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    qsa(:,:,:) = 0.0
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'QGRAUP', rcode)
  IF ( rcode == nf90_noerr ) THEN
    CALL get_var_3d_real_cdf (cdfid, 'QGRAUP', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      qga(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
      qga(  met_nx, :,      :) = qga(nxm,:,:)
      qga( :,        met_ny,:) = qga(:,nym,:)
      WRITE (*,ifmt1) 'QGRAUP   ', (qga(lprt_metx,lprt_mety,k),k=1,met_nz)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'QGRAUP', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    qga(:,:,:) = 0.0
  ENDIF

  IF ( ( iftke ) .AND. ( iftkef ) ) THEN  ! TKE on full-levels
    CALL get_var_3d_real_cdf (cdfid, 'TKE', dum3d_w, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      tke(1:nxm,   1:nym,   :) = dum3d_w(:,:,:)
      tke(  met_nx, :,      :) = tke(nxm,:,:)
      tke( :,        met_ny,:) = tke(:,nym,:)
      WRITE (*,ifmt1a) 'TKE      ', (tke(lprt_metx,lprt_mety,k),k=1,nzp)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'TKE', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ELSE IF ( ( iftke ) .AND. ( .NOT. iftkef ) ) THEN  ! TKE on half-layers
    CALL get_var_3d_real_cdf (cdfid, 'TKE_MYJ', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      tke(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
      tke(  met_nx, :,      :) = tke(nxm,:,:)
      tke( :,        met_ny,:) = tke(:,nym,:)
      WRITE (*,ifmt1) 'TKE_MYJ  ', (tke(lprt_metx,lprt_mety,k),k=1,met_nz)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'TKE_MYJ', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( ifcld3d ) THEN  ! 3D resolved cloud fraction
    CALL get_var_3d_real_cdf (cdfid, 'CLDFRA', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      cldfra(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
      cldfra(  met_nx, :,      :) = cldfra(nxm,:,:)
      cldfra( :,        met_ny,:) = cldfra(:,nym,:)
      WRITE (*,ifmt1a) 'CLDFRA   ', (cldfra(lprt_metx,lprt_mety,k),k=1,met_nz)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'CLDFRA', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MU', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    mu(1:nxm,1:nym) = dum2d(:,:)
    mu(met_nx,:) = mu(nxm,:)
    mu(:,met_ny) = mu(:,nym)
    WRITE (*,f6000) 'MU       ', mu(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MU', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MUB', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    mub(1:nxm,1:nym) = dum2d(:,:)
    mub(met_nx,:) = mub(nxm,:)
    mub(:,met_ny) = mub(:,nym)
    WRITE (*,f6000) 'MUB      ', mub(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MUB', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( ift2m ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'T2', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      t2(1:nxm,1:nym) = dum2d(:,:)
      t2(met_nx,:) = t2(nxm,:)
      t2(:,met_ny) = t2(:,nym)
      WRITE (*,f6000) 'T2       ', t2(lprt_metx, lprt_mety), 'K'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'T2', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( ifq2m ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'Q2', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      q2(1:nxm,1:nym) = dum2d(:,:)
      q2(met_nx,:) = t2(nxm,:)
      q2(:,met_ny) = t2(:,nym)
      WRITE (*,f6000) 'Q2       ', q2(lprt_metx, lprt_mety), 'kg kg-1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'Q2', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( ifw10m ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'U10', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      u10(1:nxm,1:nym) = dum2d(:,:)
      u10(met_nx,:) = u10(nxm,:)
      u10(:,met_ny) = u10(:,nym)
      WRITE (*,f6000) 'U10      ', u10(lprt_metx, lprt_mety), 'm s-1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'U10', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
    CALL get_var_2d_real_cdf (cdfid, 'V10', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      v10(1:nxm,1:nym) = dum2d(:,:)
      v10(met_nx,:) = v10(nxm,:)
      v10(:,met_ny) = v10(:,nym)
      WRITE (*,f6000) 'V10      ', v10(lprt_metx, lprt_mety), 'm s-1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'V10', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'PSFC', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    psa(1:nxm,1:nym) = dum2d(:,:)
    psa(met_nx,:) = psa(nxm,:)
    psa(:,met_ny) = psa(:,nym)
    WRITE (*,f6000) 'PSFC     ', psa(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'PSFC', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MAPFAC_M', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    mapcrs(1:nxm,1:nym) = dum2d(:,:)
    mapcrs(met_nx,:) = mapcrs(nxm,:)
    mapcrs(:,met_ny) = mapcrs(:,nym)
    WRITE (*,f6000) 'MAPFAC_M ', mapcrs(lprt_metx, lprt_mety), 'dimensionless'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MAPFAC_M', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MAPFAC_U', dum2d_u, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    mapu(:,1:nym)  = dum2d_u(:,:)
    mapu(:,met_ny) = mapu(:,nym)
    WRITE (*,f6000) 'MAPFAC_U ', mapu(lprt_metx, lprt_mety), 'dimensionless'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MAPFAC_V', dum2d_v, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    mapv(1:nxm,:)  = dum2d_v(:,:)
    mapv(met_nx,:) = mapv(nxm,:)
    WRITE (*,f6000) 'MAPFAC_V ', mapv(lprt_metx, lprt_mety), 'dimensionless'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'HGT', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    terrain(1:nxm,1:nym) = dum2d(:,:)
    terrain(met_nx,:) = terrain(nxm,:)
    terrain(:,met_ny) = terrain(:,nym)
    WRITE (*,f6000) 'HGT      ', terrain(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'HGT', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( met_pcp_incr == 0 ) THEN  ! compute incremental precip in MCIP

    CALL get_var_2d_real_cdf (cdfid, 'RAINC', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      WHERE ( dum2d < smallnum )
        dum2d = 0.0
      ENDWHERE
      raincon(1:nxm,1:nym) = (dum2d(:,:) - rcold(1:nxm,1:nym))/10.0
      raincon(met_nx,:) = raincon(nxm,:)
      raincon(:,met_ny) = raincon(:,nym)
      rcold(1:nxm,1:nym) = dum2d(:,:)
      WRITE (*,f6000) 'RAINC    ', raincon(lprt_metx, lprt_mety), 'cm'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RAINC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'RAINNC', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      WHERE ( dum2d < smallnum )
        dum2d = 0.0
      ENDWHERE
      rainnon(1:nxm,1:nym) = (dum2d(:,:) - rnold(1:nxm,1:nym))/10.0
      rainnon(met_nx,:) = rainnon(nxm,:)
      rainnon(:,met_ny) = rainnon(:,nym)
      rnold(1:nxm,1:nym) = dum2d(:,:)
      WRITE (*,f6000) 'RAINNC   ', rainnon(lprt_metx, lprt_mety), 'cm'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RAINNC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    IF ( met_rain_bucket > 0.0 ) THEN  ! adjust RAINC and RAINNC for bucket
    
      CALL get_var_2d_int_cdf (cdfid, 'I_RAINC', dum2d_i, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        i_rainc(1:nxm,1:nym) = dum2d_i(:,:) - ircold(1:nxm,1:nym)
        i_rainc(met_nx,:) = i_rainc(nxm,:)
        i_rainc(:,met_ny) = i_rainc(:,nym)
        raincon(:,:) = raincon(:,:) + 0.1 * met_rain_bucket * FLOAT(i_rainc(:,:))
        ircold (1:nxm,1:nym) = dum2d_i(:,:)
        ircold (met_nx,:) = ircold(nxm,:)
        ircold (:,met_ny) = ircold(:,nym)
        WRITE (*,f6100) 'I_RAINC  ', i_rainc(lprt_metx, lprt_mety), 'times'
        WRITE (*,f6000) 'CONV RAIN', raincon(lprt_metx, lprt_mety), 'cm'
      ELSE
        WRITE (*,f9400) TRIM(pname), 'I_RAINC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
  
      CALL get_var_2d_int_cdf (cdfid, 'I_RAINNC', dum2d_i, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        i_rainnc(1:nxm,1:nym) = dum2d_i(:,:) - irnold(1:nxm,1:nym)
        i_rainnc(met_nx,:) = i_rainnc(nxm,:)
        i_rainnc(:,met_ny) = i_rainnc(:,nym)
        rainnon (:,:) = rainnon(:,:) + 0.1 * met_rain_bucket * FLOAT(i_rainnc(:,:))
        irnold  (1:nxm,1:nym) = dum2d_i(:,:)
        irnold  (met_nx,:) = irnold(nxm,:)
        irnold  (:,met_ny) = irnold(:,nym)
        WRITE (*,f6100) 'I_RAINNC ', i_rainnc(lprt_metx, lprt_mety), 'times'
        WRITE (*,f6000) 'NONC RAIN', rainnon(lprt_metx, lprt_mety), 'cm'
      ELSE
        WRITE (*,f9400) TRIM(pname), 'I_RAINNC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
  
    ENDIF  ! tipping bucket

    ! Ensure precipitation values for this time increment are not negative.

    raincon(:,:) = MAX(0.0, raincon(:,:))
    rainnon(:,:) = MAX(0.0, rainnon(:,:))
  
  ELSE  ! incremental precip taken directly from WRF

    CALL get_var_2d_real_cdf (cdfid, 'PREC_ACC_C', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      WHERE ( dum2d < smallnum )
        dum2d = 0.0
      ENDWHERE
      raincon(1:nxm,1:nym) = dum2d(:,:) / 10.0
      raincon(met_nx,:) = raincon(nxm,:)
      raincon(:,met_ny) = raincon(:,nym)
      WRITE (*,f6000) 'RAINC    ', raincon(lprt_metx, lprt_mety), 'cm'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'PREC_ACC_C', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'PREC_ACC_NC', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      WHERE ( dum2d < smallnum )
        dum2d = 0.0
      ENDWHERE
      rainnon(1:nxm,1:nym) = dum2d(:,:) / 10.0
      rainnon(met_nx,:) = rainnon(nxm,:)
      rainnon(:,met_ny) = rainnon(:,nym)
      WRITE (*,f6000) 'RAINNC   ', rainnon(lprt_metx, lprt_mety), 'cm'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'PREC_ACC_NC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF  ! incremental precip

  CALL get_var_2d_real_cdf (cdfid, 'SWDOWN', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    rgrnd(1:nxm,1:nym) = dum2d(:,:)
    rgrnd(met_nx,:) = rgrnd(nxm,:)
    rgrnd(:,met_ny) = rgrnd(:,nym)
    WRITE (*,f6000) 'SWDOWN   ', rgrnd(lprt_metx, lprt_mety), 'W m-2'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SWDOWN', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'GLW', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    glw(1:nxm,1:nym) = dum2d(:,:)
    glw(met_nx,:) = glw(nxm,:)
    glw(:,met_ny) = glw(:,nym)
    WRITE (*,f6000) 'GLW      ', glw(lprt_metx, lprt_mety), 'W m-2'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'GLW', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLAT', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    latcrs(1:nxm,1:nym) = dum2d(:,:)
    latcrs(met_nx,:) = latcrs(nxm,:)
    latcrs(:,met_ny) = latcrs(:,nym)
    WRITE (*,f6000) 'XLAT     ', latcrs(lprt_metx, lprt_mety), 'degrees_north'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'XLAT', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLAT_U', dum2d_u, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    latu(:,1:nym)  = dum2d_u(:,:)
    latu(:,met_ny) = latu(:,nym)
    WRITE (*,f6000) 'XLAT_U   ', latu(lprt_metx, lprt_mety), 'degrees_north'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLAT_V', dum2d_v, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    latv(1:nxm,:)  = dum2d_v(:,:)
    latv(met_nx,:) = latv(nxm,:)
    WRITE (*,f6000) 'XLAT_V   ', latv(lprt_metx, lprt_mety), 'degrees_north'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLONG', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    loncrs(1:nxm,1:nym) = dum2d(:,:)
    loncrs(met_nx,:) = loncrs(nxm,:)
    loncrs(:,met_ny) = loncrs(:,nym)
    WRITE (*,f6000) 'XLONG    ', loncrs(lprt_metx, lprt_mety), 'degrees_east'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'XLONG', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLONG_U', dum2d_u, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    lonu(:,1:nym)  = dum2d_u(:,:)
    lonu(:,met_ny) = lonu(:,nym)
    WRITE (*,f6000) 'XLONG_U  ', lonu(lprt_metx, lprt_mety), 'degrees_east'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLONG_V', dum2d_v, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    lonv(1:nxm,:)  = dum2d_v(:,:)
    lonv(met_nx,:) = lonv(nxm,:)
    WRITE (*,f6000) 'XLONG_V  ', lonv(lprt_metx, lprt_mety), 'degrees_east'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'LU_INDEX', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    IF ( MAXVAL(dum2d) > nummetlu ) THEN
      WRITE (*,f9500) TRIM(pname), met_lu_src, MAXVAL(dum2d)
      CALL graceful_stop (pname)
    ENDIF
    landuse(1:nxm,1:nym) = NINT(dum2d(:,:))
    landuse(met_nx,:) = landuse(nxm,:)
    landuse(:,met_ny) = landuse(:,nym)
    WRITE (*,f6100) 'LU_INDEX ', landuse(lprt_metx, lprt_mety), 'category'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'LU_INDEX', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'LANDMASK', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    landmask(1:nxm,1:nym) = dum2d(:,:)
    landmask(met_nx,:) = landmask(nxm,:)
    landmask(:,met_ny) = landmask(:,nym)
    WRITE (*,f6000) 'LANDMASK ', landmask(lprt_metx, lprt_mety), 'category'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'LANDMASK', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'HFX', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    hfx(1:nxm,1:nym) = dum2d(:,:)
    hfx(met_nx,:) = hfx(nxm,:)
    hfx(:,met_ny) = hfx(:,nym)
    WRITE (*,f6000) 'HFX      ', hfx(lprt_metx, lprt_mety), 'W m-2'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'HFX', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'LH', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    lh(1:nxm,1:nym) = dum2d(:,:)
    lh(met_nx,:) = lh(nxm,:)
    lh(:,met_ny) = lh(:,nym)
    WRITE (*,f6000) 'LH       ', lh(lprt_metx, lprt_mety), 'W m-2'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'LH', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'UST', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    ust(1:nxm,1:nym) = dum2d(:,:)
    ust(met_nx,:) = ust(nxm,:)
    ust(:,met_ny) = ust(:,nym)
    WRITE (*,f6000) 'UST      ', ust(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'UST', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( ifmol ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'RMOL', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      mol(1:nxm,1:nym) = 1.0 / dum2d(:,:)
      mol(met_nx,:) = mol(nxm,:)
      mol(:,met_ny) = mol(:,nym)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RMOL', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
    IF ( met_urban_phys >= 1 ) THEN  ! UCM used; get MOL above urban canopy
      CALL get_var_2d_real_cdf (cdfid, 'XXXC_URB', dum2d, it, rcode)
      IF ( rcode == nf90_noerr ) THEN  ! blend urban M-O length with RMOL
        IF ( ( met_lu_src(1:4) == 'USGS' ) .AND.  &
             ( MAXVAL(landuse)  >  24    ) ) THEN  ! 33-category USGS/NLCD
          DO j = 1, nym
            DO i = 1, nxm
              IF ( ( landuse(i,j) ==  1 ) .OR. ( landuse(i,j) == 31 ) .OR.  &
                   ( landuse(i,j) == 32 ) .OR. ( landuse(i,j) == 33 ) ) THEN
                mol(i,j) = dum2d(i,j)  ! XXXC_URB is not inverted
              ENDIF
            ENDDO
          ENDDO
        ELSE IF ( met_lu_src(1:4) == 'USGS' ) THEN  ! 24-category USGS
          DO j = 1, nym
            DO i = 1, nxm
              IF ( landuse(i,j) == 1 ) THEN  ! urban
                mol(i,j) = dum2d(i,j)  ! XXXC_URB is not inverted
              ENDIF
            ENDDO
          ENDDO
        ELSE
          WRITE (*,f9800) TRIM(pname), 'XXXC_URB (URBAN MOL)', met_lu_src(1:4)
          CALL graceful_stop (pname)
        ENDIF
      ELSE
!~~~    Just use RMOL to fill Monin-Obukhov length without extra urban field
      ENDIF
    ENDIF
    WRITE (*,f6000) 'MOL      ', mol(lprt_metx, lprt_mety), 'm'
  ENDIF

  IF ( ifmolpx ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'QFX', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      qfx(1:nxm,1:nym) = dum2d(:,:)
      qfx(met_nx,:) = qfx(nxm,:)
      qfx(:,met_ny) = qfx(:,nym)
      WRITE (*,f6000) 'QFX      ', qfx(lprt_metx, lprt_mety), 'kg m-2 s-1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'QFX ', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'PBLH', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    zpbl(1:nxm,1:nym) = dum2d(:,:)
    zpbl(met_nx,:) = zpbl(nxm,:)
    zpbl(:,met_ny) = zpbl(:,nym)
    WRITE (*,f6000) 'PBLH     ', zpbl(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'PBLH', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( ifresist ) THEN

    CALL get_var_2d_real_cdf (cdfid, 'RA', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      ra(1:nxm,1:nym) = dum2d(:,:)
      ra(met_nx,:) = ra(nxm,:)
      ra(:,met_ny) = ra(:,nym)
      IF ( ABS(MAXVAL(ra)) < smallnum ) THEN
        ifresist = .FALSE.
      ENDIF
      WRITE (*,f6000) 'RA       ', ra(lprt_metx, lprt_mety), 's m-1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RA', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'RS', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      rstom(1:nxm,1:nym) = dum2d(:,:)
      rstom(met_nx,:) = rstom(nxm,:)
      rstom(:,met_ny) = rstom(:,nym)
      IF ( ABS(MAXVAL(rstom)) < smallnum ) THEN
        ifresist = .FALSE.
      ENDIF
      WRITE (*,f6000) 'RS       ', rstom(lprt_metx, lprt_mety), 's m-1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RS', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF

  IF ( iflai ) THEN
    IF ( ifpxwrf41 ) THEN
      CALL get_var_2d_real_cdf (cdfid, 'LAI_PX', dum2d, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        lai_px(1:nxm,1:nym) = dum2d(:,:)
        lai_px(met_nx,:) = lai_px(nxm,:)
        lai_px(:,met_ny) = lai_px(:,nym)
        WRITE (*,f6000) 'LAI_PX   ', lai_px(lprt_metx, lprt_mety), 'm2 m-2'
      ELSE
        WRITE (*,f9400) TRIM(pname), 'LAI_PX', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ELSE
      CALL get_var_2d_real_cdf (cdfid, 'LAI', dum2d, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        lai(1:nxm,1:nym) = dum2d(:,:)
        lai(met_nx,:) = lai(nxm,:)
        lai(:,met_ny) = lai(:,nym)
        IF ( ABS(MAXVAL(lai)) < smallnum ) THEN
          IF ( met_soil_lsm == 2 ) THEN  ! NOAH LSM
            lai(:,:) = 4.0
          ENDIF
        ENDIF
        WRITE (*,f6000) 'LAI      ', lai(lprt_metx, lprt_mety), 'm2 m-2'
      ELSE
        WRITE (*,f9400) TRIM(pname), 'LAI', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF
  ENDIF

  IF ( ifwr ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'CANWAT', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      wr(1:nxm,1:nym) = dum2d(:,:)
      wr(met_nx,:) = wr(nxm,:)
      wr(:,met_ny) = wr(:,nym)
      WRITE (*,f6000) 'CANWAT   ', wr(lprt_metx, lprt_mety), 'kg m-2'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'CANWAT', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( ifveg ) THEN
    IF ( met_soil_lsm == 7 ) THEN  ! Pleim-Xiu land-surface model
      CALL get_var_2d_real_cdf (cdfid, 'VEGF_PX', dum2d, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        veg(1:nxm,1:nym) = dum2d(:,:)
        veg(met_nx,:) = veg(nxm,:)
        veg(:,met_ny) = veg(:,nym)
        WRITE (*,f6000) 'VEGF_PX  ', veg(lprt_metx, lprt_mety), 'm2 m-2'
      ELSE
        CALL get_var_2d_real_cdf (cdfid, 'VEGFRA', dum2d, it, rcode)
        IF ( rcode == nf90_noerr ) THEN
          veg(1:nxm,1:nym) = dum2d(:,:) * 0.01
          veg(met_nx,:) = veg(nxm,:)
          veg(:,met_ny) = veg(:,nym)
          WRITE (*,f6000) 'VEGFRA   ', veg(lprt_metx, lprt_mety), 'fraction'
        ELSE
          WRITE (*,f9400) TRIM(pname), 'VEGFRA', TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
      ENDIF
    ELSE
      CALL get_var_2d_real_cdf (cdfid, 'VEGFRA', dum2d, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        veg(1:nxm,1:nym) = dum2d(:,:) * 0.01
        veg(met_nx,:) = veg(nxm,:)
        veg(:,met_ny) = veg(:,nym)
        WRITE (*,f6000) 'VEGFRA   ', veg(lprt_metx, lprt_mety), 'fraction'
      ELSE
        WRITE (*,f9400) TRIM(pname), 'VEGFRA', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF
  ENDIF

  IF ( ifsoil ) THEN

    CALL get_var_2d_int_cdf (cdfid, 'ISLTYP', dum2d_i, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      isltyp(1:nxm,1:nym) = dum2d_i(:,:)
      isltyp(met_nx,:) = isltyp(nxm,:)
      isltyp(:,met_ny) = isltyp(:,nym)
!!!   IF ( met_soil_lsm == 7 ) THEN  ! Pleim-Xiu used; detangle soil categories
!!!     CALL detangle_soil_px
!!!   ENDIF
      WRITE (*,f6100) 'ISLTYP   ', isltyp(lprt_metx, lprt_mety), 'category'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'ISLTYP', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'SMOIS', dum3d_s, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      wg(1:nxm,1:nym) = dum3d_s(:,:,1)
      wg(met_nx,:) = wg(nxm,:)
      wg(:,met_ny) = wg(:,nym)
      WRITE (*,f6000) 'SMOIS 1  ', wg(lprt_metx, lprt_mety), 'm3 m-3'
      w2(1:nxm,1:nym) = dum3d_s(:,:,2)
      w2(met_nx,:) = w2(nxm,:)
      w2(:,met_ny) = w2(:,nym)
      WRITE (*,f6000) 'SMOIS 2  ', w2(lprt_metx, lprt_mety), 'm3 m-3'
      soim3d(1:nxm,1:nym,:) = dum3d_s(:,:,:)
      soim3d(met_nx,:,:) = soim3d(nxm,:,:)
      soim3d(:,met_ny,:) = soim3d(:,nym,:)
      WRITE (*,ifmt5) 'SMOIS    ', (soim3d(lprt_metx,lprt_mety,k),k=1,met_ns)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'SMOIS', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'TSLB', dum3d_s, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      soilt1(1:nxm,1:nym) = dum3d_s(:,:,1)
      soilt1(met_nx,:) = soilt1(nxm,:)
      soilt1(:,met_ny) = soilt1(:,nym)
      WRITE (*,f6000) 'TSLB 1   ', soilt1(lprt_metx, lprt_mety), 'K'
      soilt2(1:nxm,1:nym) = dum3d_s(:,:,2)
      soilt2(met_nx,:) = soilt2(nxm,:)
      soilt2(:,met_ny) = soilt2(:,nym)
      WRITE (*,f6000) 'TSLB 2   ', soilt2(lprt_metx, lprt_mety), 'K'
      soit3d(1:nxm,1:nym,:) = dum3d_s(:,:,:)
      soit3d(met_nx,:,:) = soit3d(nxm,:,:)
      soit3d(:,met_ny,:) = soit3d(:,nym,:)
      WRITE (*,ifmt5) 'TSLB     ', (soit3d(lprt_metx,lprt_mety,k),k=1,met_ns)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'TSLB', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'TSK', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    groundt(1:nxm,1:nym) = dum2d(:,:)
    groundt(met_nx,:) = groundt(nxm,:)
    groundt(:,met_ny) = groundt(:,nym)
    WRITE (*,f6000) 'TSK      ', groundt(lprt_metx, lprt_mety), 'K'
  ELSE
    IF ( ifsoil ) THEN
      groundt(:,:) = soilt1(:,:)
      WRITE (*,f6000) 'TSK      ', groundt(lprt_metx, lprt_mety), 'K'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'TSK', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'ALBEDO', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    albedo(1:nxm,1:nym) = dum2d(:,:)
    albedo(met_nx,:) = albedo(nxm,:)
    albedo(:,met_ny) = albedo(:,nym)
    WRITE (*,f6000) 'ALBEDO   ', albedo(lprt_metx, lprt_mety), 'fraction'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'ALBEDO', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( first ) THEN
    IF ( iflufrc ) THEN
      IF ( ifluwrfout ) THEN  ! land use fractions in WRF history file
        CALL get_var_3d_real_cdf (cdfid, 'LANDUSEF', dum3d_l, it, rcode)
        IF ( rcode == nf90_noerr ) THEN
          lufrac(1:nxm,   1:nym,   :) = dum3d_l(:,:,:)
          lufrac(  met_nx, :,      :) = lufrac(nxm,:,:)
          lufrac( :,        met_ny,:) = lufrac(:,nym,:)
          WRITE (*,ifmt2) 'LANDUSEF ', (lufrac(lprt_metx,lprt_mety,k),k=1,nummetlu)
        ELSE
          WRITE (*,f9400) TRIM(pname), 'LANDUSEF', TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
        IF ( iflu2wrfout ) THEN  ! land use fractions (ranked) in WRF file
          CALL get_var_3d_real_cdf (cdfid, 'LANDUSEF2', dum3d_l, it, rcode)
          IF ( rcode == nf90_noerr ) THEN
            lufrac2(1:nxm,   1:nym,   :) = dum3d_l(:,:,:)
            lufrac2(  met_nx, :,      :) = lufrac2(nxm,:,:)
            lufrac2( :,        met_ny,:) = lufrac2(:,nym,:)
            WRITE (*,ifmt2) 'LANDUSEF2', (lufrac2(lprt_metx,lprt_mety,k),k=1,nummetlu)
          ELSE
            WRITE (*,f9400) TRIM(pname), 'LANDUSEF2', TRIM(nf90_strerror(rcode))
            CALL graceful_stop (pname)
          ENDIF
          CALL get_var_3d_int_cdf (cdfid, 'MOSAIC_CAT_INDEX', dum3d_li, it, rcode)
          IF ( rcode == nf90_noerr ) THEN
            moscatidx(1:nxm,   1:nym,   :) = dum3d_li(:,:,:)
            moscatidx(  met_nx, :,      :) = moscatidx(nxm,:,:)
            moscatidx( :,        met_ny,:) = moscatidx(:,nym,:)
            WRITE (*,ifmt3) 'MOSAIC_CAT', (moscatidx(lprt_metx,lprt_mety,k),k=1,nummetlu)
          ELSE
            ! Will be filled in getluse.f90, if NOAH Mosaic LSM was used
          ENDIF
        ENDIF
      ELSE  ! land use fractions in GEOGRID file from WPS
        flg = file_geo
        rcode = nf90_open (flg, nf90_nowrite, cdfidg)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9900) TRIM(pname)
          CALL graceful_stop (pname)
        ENDIF
        CALL get_var_3d_real_cdf (cdfidg, 'LANDUSEF', dum3d_l, 1, rcode)
        IF ( rcode == nf90_noerr ) THEN
          lufrac(1:nxm,   1:nym,   :) = dum3d_l(:,:,:)
          lufrac(  met_nx, :,      :) = lufrac(nxm,:,:)
          lufrac( :,        met_ny,:) = lufrac(:,nym,:)
          WRITE (*,ifmt2) 'LANDUSEF ', lufrac(lprt_metx,lprt_mety,:)
        ELSE
          WRITE (*,f9400) TRIM(pname), 'LANDUSEF', TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
        rcode = nf90_close (cdfidg)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9950) TRIM(pname)
          CALL graceful_stop (pname)
        ENDIF
      ENDIF
    ENDIF
    IF ( met_urban_phys >= 1 ) THEN  ! urban canopy model used
      CALL get_var_2d_real_cdf (cdfid, 'FRC_URB2D', dum2d, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        frc_urb(1:nxm,1:nym) = dum2d(:,:)
        frc_urb(met_nx,:) = frc_urb(nxm,:)
        frc_urb(:,met_ny) = frc_urb(:,nym)
        WRITE (*,f6000) 'FRC_URB2D', frc_urb(lprt_metx, lprt_mety), 'fraction'
      ELSE
        WRITE (*,f9400) TRIM(pname), 'FRC_URB2D', TRIM(nf90_strerror(rcode))
      ENDIF
    ENDIF
  ENDIF

  IF ( ifznt ) THEN  ! expecting roughness length in file
    CALL get_var_2d_real_cdf (cdfid, 'ZNT', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      znt(1:nxm,1:nym) = dum2d(:,:)
      znt(met_nx,:) = znt(nxm,:)
      znt(:,met_ny) = znt(:,nym)
      WRITE (*,f6000) 'ZNT      ', znt(lprt_metx, lprt_mety),    'm'
      gotznt = .TRUE.
    ELSE
      WRITE (*,f9400) TRIM(pname), 'ZNT    ', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ELSE
    gotznt = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'SNOWC', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    snowcovr(1:nxm,1:nym) = dum2d(:,:)
    snowcovr(met_nx,:) = snowcovr(nxm,:)
    snowcovr(:,met_ny) = snowcovr(:,nym)
    WRITE (*,f6000) 'SNOWC    ', snowcovr(lprt_metx, lprt_mety), 'category'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SNOWC', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'SEAICE', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    seaice(1:nxm,1:nym) = dum2d(:,:)
    seaice(met_nx,:) = seaice(nxm,:)
    seaice(:,met_ny) = seaice(:,nym)
    gotseaice = .TRUE.
    WRITE (*,f6000) 'SEAICE   ', seaice(lprt_metx, lprt_mety), 'fraction'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SEAICE', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'SNOWH', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    snowh(1:nxm,1:nym) = dum2d(:,:)
    snowh(met_nx,:) = snowh(nxm,:)
    snowh(:,met_ny) = snowh(:,nym)
    WRITE (*,f6000) 'SNOWH    ', snowh(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SNOWH', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( ifpxwrf41 ) THEN

    CALL get_var_2d_real_cdf (cdfid, 'WSAT_PX', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      wsat_px(1:nxm,1:nym) = dum2d(:,:)
      wsat_px(met_nx,:) = wsat_px(nxm,:)
      wsat_px(:,met_ny) = wsat_px(:,nym)
      WRITE (*,f6000) 'WSAT_PX  ', wsat_px(lprt_metx, lprt_mety), 'm3 m-3'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'WSAT_PX', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'WFC_PX', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      wfc_px(1:nxm,1:nym) = dum2d(:,:)
      wfc_px(met_nx,:) = wfc_px(nxm,:)
      wfc_px(:,met_ny) = wfc_px(:,nym)
      WRITE (*,f6000) 'WFC_PX  ', wfc_px(lprt_metx, lprt_mety), 'm3 m-3'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'WFC_PX', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'WWLT_PX', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      wwlt_px(1:nxm,1:nym) = dum2d(:,:)
      wwlt_px(met_nx,:) = wwlt_px(nxm,:)
      wwlt_px(:,met_ny) = wwlt_px(:,nym)
      WRITE (*,f6000) 'WWLT_PX  ', wwlt_px(lprt_metx, lprt_mety), 'm3 m-3'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'WWLT_PX', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'CSAND_PX', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      csand_px(1:nxm,1:nym) = dum2d(:,:)
      csand_px(met_nx,:) = csand_px(nxm,:)
      csand_px(:,met_ny) = csand_px(:,nym)
      WRITE (*,f6000) 'CSAND_PX ', csand_px(lprt_metx, lprt_mety), '1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'CSAND_PX', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'FMSAND_PX', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      fmsand_px(1:nxm,1:nym) = dum2d(:,:)
      fmsand_px(met_nx,:) = fmsand_px(nxm,:)
      fmsand_px(:,met_ny) = fmsand_px(:,nym)
      WRITE (*,f6000) 'FMSAND_PX', fmsand_px(lprt_metx, lprt_mety), '1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'FMSAND_PX', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'CLAY_PX', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      clay_px(1:nxm,1:nym) = dum2d(:,:)
      clay_px(met_nx,:) = clay_px(nxm,:)
      clay_px(:,met_ny) = clay_px(:,nym)
      WRITE (*,f6000) 'CLAY_PX  ', clay_px(lprt_metx, lprt_mety), '1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'CLAY_PX', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF

  IF ( ifmosaic ) THEN

    CALL get_var_3d_real_cdf (cdfid, 'LAI_MOSAIC', dum3d_m, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      lai_mos(1:nxm,   1:nym,   :) = dum3d_m(:,:,:)
      lai_mos(  met_nx, :,      :) = lai_mos(nxm,:,:)
      lai_mos( :,        met_ny,:) = lai_mos(:,nym,:)
      WRITE (*,ifmt4) 'LAI_MOS  ', (lai_mos(lprt_metx,lprt_mety,k),k=1,nummosaic)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'LAI_MOSAIC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'RS_MOSAIC', dum3d_m, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      rs_mos(1:nxm,   1:nym,   :) = dum3d_m(:,:,:)
      rs_mos(  met_nx, :,      :) = rs_mos(nxm,:,:)
      rs_mos( :,        met_ny,:) = rs_mos(:,nym,:)
      WRITE (*,ifmt4) 'RS_MOS   ', (rs_mos(lprt_metx,lprt_mety,k),k=1,nummosaic)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RS_MOSAIC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'TSK_MOSAIC', dum3d_m, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      tsk_mos(1:nxm,   1:nym,   :) = dum3d_m(:,:,:)
      tsk_mos(  met_nx, :,      :) = tsk_mos(nxm,:,:)
      tsk_mos( :,        met_ny,:) = tsk_mos(:,nym,:)
      WRITE (*,ifmt4) 'TSK_MOS  ', (tsk_mos(lprt_metx,lprt_mety,k),k=1,nummosaic)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'TSK_MOSAIC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'ZNT_MOSAIC', dum3d_m, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      znt_mos(1:nxm,   1:nym,   :) = dum3d_m(:,:,:)
      znt_mos(  met_nx, :,      :) = znt_mos(nxm,:,:)
      znt_mos( :,        met_ny,:) = znt_mos(:,nym,:)
      WRITE (*,ifmt4) 'ZNT_MOS  ', (znt_mos(lprt_metx,lprt_mety,k),k=1,nummosaic)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'ZNT_MOSAIC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'WSPD', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      wspdsfc(1:nxm,1:nym) = dum2d(:,:)
      wspdsfc(met_nx,:) = wspdsfc(nxm,:)
      wspdsfc(:,met_ny) = wspdsfc(:,nym)
      WRITE (*,f6000) 'WSPDSFC  ', wspdsfc(lprt_metx, lprt_mety), 'm s-1'
    ELSE
      ! Original version was stored in "WSPDSFC"; released WRF code uses "WSPD"
      CALL get_var_2d_real_cdf (cdfid, 'WSPDSFC', dum2d, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        wspdsfc(1:nxm,1:nym) = dum2d(:,:)
        wspdsfc(met_nx,:) = wspdsfc(nxm,:)
        wspdsfc(:,met_ny) = wspdsfc(:,nym)
        WRITE (*,f6000) 'WSPDSFC  ', wspdsfc(lprt_metx, lprt_mety), 'm s-1'
      ELSE
        WRITE (*,f9400) TRIM(pname), 'WSPDSFC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'XLAIDYN', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      xlaidyn(1:nxm,1:nym) = dum2d(:,:)
      xlaidyn(met_nx,:) = xlaidyn(nxm,:)
      xlaidyn(:,met_ny) = xlaidyn(:,nym)
      WRITE (*,f6000) 'XLAIDYN  ', xlaidyn(lprt_metx, lprt_mety), 'm2 m-2'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'XLAIDYN', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF  ! ifmosaic

  IF ( ifkfradextras ) THEN  ! Extra vars from KF scheme w radiative feedbacks

    CALL get_var_3d_real_cdf (cdfid, 'QC_CU', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      qc_cu(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
      qc_cu(  met_nx, :,      :) = qc_cu(nxm,:,:)
      qc_cu( :,        met_ny,:) = qc_cu(:,nym,:)
      WRITE (*,ifmt1a) 'QC_CU    ', (qc_cu(lprt_metx,lprt_mety,k),k=1,met_nz)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'QC_CU', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'QI_CU', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      qi_cu(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
      qi_cu(  met_nx, :,      :) = qi_cu(nxm,:,:)
      qi_cu( :,        met_ny,:) = qi_cu(:,nym,:)
      WRITE (*,ifmt1a) 'QI_CU    ', (qi_cu(lprt_metx,lprt_mety,k),k=1,met_nz)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'QI_CU', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'CLDFRA_DP', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      cldfra_dp(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
      cldfra_dp(  met_nx, :,      :) = cldfra_dp(nxm,:,:)
      cldfra_dp( :,        met_ny,:) = cldfra_dp(:,nym,:)
      WRITE (*,ifmt1a) 'CLDFRA_DP', (cldfra_dp(lprt_metx,lprt_mety,k),k=1,met_nz)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'CLDFRA_DP', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'CLDFRA_SH', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      cldfra_sh(1:nxm,   1:nym,   :) = dum3d_t(:,:,:)
      cldfra_sh(  met_nx, :,      :) = cldfra_sh(nxm,:,:)
      cldfra_sh( :,        met_ny,:) = cldfra_sh(:,nym,:)
      WRITE (*,ifmt1a) 'CLDFRA_SH', (cldfra_sh(lprt_metx,lprt_mety,k),k=1,met_nz)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'CLDFRA_SH', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF  ! ifkfradextras

  CALL get_var_1d_real_cdf (cdfid, 'ZNU', sigmah, it, rcode)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'ZNU', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_1d_real_cdf (cdfid, 'ZNW', sigmaf, it, rcode)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'ZNW', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( met_hybrid >= 0 ) THEN

    CALL get_var_1d_real_cdf (cdfid, 'C1F', c1f, it, rcode)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'C1F', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_1d_real_cdf (cdfid, 'C1H', c1h, it, rcode)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'C1H', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_1d_real_cdf (cdfid, 'C2F', c2f, it, rcode)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'C2F', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_1d_real_cdf (cdfid, 'C2H', c2h, it, rcode)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9400) TRIM(pname), 'C2H', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF


  CALL get_var_1d_real_cdf (cdfid, 'DZS', dzs, it, rcode)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9400) TRIM(pname), 'DZS', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

!-------------------------------------------------------------------------------
! If this is the first time in this routine, then get latitude, longitude, and
! map-scale factors on dot points.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    ! Compute distance from origin (at reflat, standlon) to domain center, and
    ! store in MET_XXCTR and MET_YYCTR.  Then calculate latitude, longitude,
    ! and map-scale factors using offset distance of given grid point from
    ! center of domain.

    SELECT CASE ( met_mapproj )

      CASE (1)  ! Lambert conformal

        xoff = 0.0  ! dot-point grid: no offset from dot-point center value
        yoff = 0.0  ! dot-point grid: no offset from dot-point center value

        DO j = 1, met_ny
          DO i = 1, met_nx

            xxin = met_xxctr -  &
                   ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

            yyin = met_yyctr -  &
                   ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

            CALL xy2ll_lam (xxin, yyin, met_tru1, met_tru2, met_proj_clon,  &
                            met_ref_lat, latdot(i,j), londot(i,j))

            mapdot(i,j) = mapfac_lam (latdot(i,j), met_tru1, met_tru2)

          ENDDO
        ENDDO

        IF ( .NOT. gotfaces ) THEN  ! get lat, lon, map-scale factor on faces

          xoff = 0.0  ! U-face: no offset in X from dot-point center value
          yoff = 0.5  ! U-face: 0.5-cell offset in Y from dot-point center value

          DO j = 1, met_ny  ! use all Y to fill array; last row outside domain
            DO i = 1, met_nx

              xxin = met_xxctr -  &
                     ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

              yyin = met_yyctr -  &
                     ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

              CALL xy2ll_lam (xxin, yyin, met_tru1, met_tru2, met_proj_clon,  &
                              met_ref_lat, latu(i,j), lonu(i,j))

              mapu(i,j) = mapfac_lam (latu(i,j), met_tru1, met_tru2)

            ENDDO
          ENDDO

          xoff = 0.5  ! V-face: 0.5-cell offset in X from dot-point center value
          yoff = 0.0  ! V-face: no offset in Y from dot-point center value

          DO j = 1, met_ny
            DO i = 1, met_nx  ! use all X to fill array; last col outside domain

              xxin = met_xxctr -  &
                     ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

              yyin = met_yyctr -  &
                     ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

              CALL xy2ll_lam (xxin, yyin, met_tru1, met_tru2, met_proj_clon,  &
                              met_ref_lat, latv(i,j), lonv(i,j))

              mapv(i,j) = mapfac_lam (latv(i,j), met_tru1, met_tru2)

            ENDDO
          ENDDO

        ENDIF


      CASE (2)  ! polar stereographic

        DO j = 1, met_ny
          DO i = 1, met_nx

            ! Use four-point interpolation here for latitude and longitude.
            ! Because CMAQ will never use outermost row and column from WRF
            ! due to location of CMAQ boundaries, inexact values in the
            ! outermost row and column will not matter.

            ii = MIN(i,nxm)
            jj = MIN(j,nym)

            im1 = MIN(i-1,1)
            jm1 = MIN(j-1,1)

            latdot(i,j) = ( latcrs(im1,jj)  + latcrs(ii,jj) +   &
                            latcrs(im1,jm1) + latcrs(ii,jm1) ) * 0.25

            londot(i,j) = ( loncrs(im1,jj)  + loncrs(ii,jj) +   &
                            loncrs(im1,jm1) + loncrs(ii,jm1) ) * 0.25

            mapdot(i,j) = mapfac_ps (latdot(i,j), met_tru1)

          ENDDO
        ENDDO

        IF ( .NOT. gotfaces ) THEN  ! get lat, lon, map-scale factor on faces

          DO j = 1, met_ny
            DO i = 1, met_nx

              ! Use linear interpolation here for latitude and longitude.
              ! Because CMAQ will never use outermost row and column from WRF
              ! due to location of CMAQ boundaries, inexact values in the
              ! outermost row and column will not matter.

              ii = MIN(i,nxm)
              jj = MIN(j,nym)

              im1 = MIN(i-1,1)
              jm1 = MIN(j-1,1)

              latu(i,j) = ( latcrs(im1,jj) + latcrs(ii,jj) ) * 0.5
              lonu(i,j) = ( loncrs(im1,jj) + loncrs(ii,jj) ) * 0.5
              mapu(i,j) = mapfac_ps (latu(i,j), met_tru1)

              latv(i,j) = ( latcrs(ii,jm1) + latcrs(ii,jj) ) * 0.5
              lonv(i,j) = ( loncrs(ii,jm1) + loncrs(ii,jj) ) * 0.5
              mapv(i,j) = mapfac_ps (latv(i,j), met_tru1)

            ENDDO
          ENDDO

        ENDIF


      CASE (3)  ! Mercator

        xoff = 0.0  ! dot-point grid: no offset from dot-point center value
        yoff = 0.0  ! dot-point grid: no offset from dot-point center value

        DO j = 1, met_ny
          DO i = 1, met_nx

            xxin = met_xxctr -  &
                   ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

            yyin = met_yyctr -  &
                   ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

            CALL xy2ll_merc (xxin, yyin, met_proj_clon,  &
                             latdot(i,j), londot(i,j))

            mapdot(i,j) = mapfac_merc (latdot(i,j))

          ENDDO
        ENDDO

        IF ( .NOT. gotfaces ) THEN  ! get lat, lon, map-scale factor on faces

          xoff = 0.0  ! U-face: no offset in X from dot-point center value
          yoff = 0.5  ! U-face: 0.5-cell offset in Y from dot-point center value

          DO j = 1, met_ny  ! use all Y to fill array; last row outside domain
            DO i = 1, met_nx

              xxin = met_xxctr -  &
                     ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

              yyin = met_yyctr -  &
                     ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

              CALL xy2ll_merc (xxin, yyin, met_proj_clon,  &
                               latu(i,j), lonu(i,j))

              mapu(i,j) = mapfac_merc (latu(i,j))

            ENDDO
          ENDDO

          xoff = 0.5  ! V-face: 0.5-cell offset in X from dot-point center value
          yoff = 0.0  ! V-face: no offset in Y from dot-point center value

          DO j = 1, met_ny
            DO i = 1, met_nx  ! use all X to fill array; last col outside domain

              xxin = met_xxctr -  &
                     ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

              yyin = met_yyctr -  &
                     ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

              CALL xy2ll_merc (xxin, yyin, met_proj_clon,  &
                               latv(i,j), lonv(i,j))

              mapv(i,j) = mapfac_merc (latv(i,j))

            ENDDO
          ENDDO

        ENDIF


    END SELECT

  ENDIF

!-------------------------------------------------------------------------------
! If this is the first time in this routine and potential vorticity scaling
! will be used in CMAQ, then calculate the Coriolis parameter.
!-------------------------------------------------------------------------------

  IF ( first .AND. lpv > 0 ) THEN

    pi = 4.0 * ATAN(1.0)
    deg2rad = pi / 180.0

    DO j = 1, nym
      DO i = 1, nxm
!!!     coriolis(i,j) = twoomega * SIND(latcrs(i,j))
        latrad = latcrs(i,j) * deg2rad
        coriolis(i,j) = twoomega * SIN(latrad)
      ENDDO
    ENDDO

    coriolis(met_nx,:) = coriolis(nxm,:)
    coriolis(:,met_ny) = coriolis(:,nym)

  ENDIF

!-------------------------------------------------------------------------------
! If this is the first time in this routine, then determine season.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    ! These seasons are used in MM5 and WRF for land-use lookup tables.

    startseas = met_startdate(1:4) // "-04-15-00:00:00"
    endseas   = met_startdate(1:4) // "-10-15-00:00:00"

    CALL geth_idts (met_startdate(1:19), startseas,           idts_start)
    CALL geth_idts (endseas,             met_startdate(1:19), idts_end)

    IF ( ( idts_start < 0 ) .OR. ( idts_end < 0 ) ) THEN
      IF ( met_cen_lat >= 0.0 ) THEN  ! Northern Hemisphere
        met_season = 2   ! winter
      ELSE  ! Southern Hemisphere
        met_season = 1   ! summer
      ENDIF
    ELSE
      IF ( met_cen_lat >= 0.0 ) THEN  ! Northern Hemisphere
        met_season = 1   ! summer
      ELSE  ! Southern Hemisphere
        met_season = 2   ! winter
      ENDIF
    ENDIF

!-------------------------------------------------------------------------------
! If roughness length was not available in output, fill it from lookup tables.
! If the urban model was used in WRF, replace roughness length with urban-
! specific arrays.
!-------------------------------------------------------------------------------

    IF ( .NOT. gotznt ) THEN

      IF ( met_season == 1 ) THEN  ! summer

        DO j = 1, nym
          DO i = 1, nxm
            IF ( ( met_lu_src(1:4) == "USGS" ) .AND.  &
                 ( met_lu_water == 16 ) ) THEN
              znt(i,j) = sfz0usgssum(landuse(i,j)) * 0.01  ! cm --> m
            ELSE IF ( ( met_lu_src(1:3) == "OLD" ) .AND.  &
                      ( met_lu_water == 7 ) ) THEN
              znt(i,j) = sfz0oldsum(landuse(i,j))  * 0.01  ! cm --> m
            ELSE IF ( met_lu_src(1:6) == "NLCD50" ) THEN
              znt(i,j) = sfz0nlcd50sum(landuse(i,j))  * 0.01  ! cm --> m
            ELSE IF ( met_lu_src(1:6) == "NLCD40" ) THEN
              znt(i,j) = sfz0nlcd40sum(landuse(i,j))  * 0.01  ! cm --> m
            ELSE IF ( met_lu_src(1:3) == "SIB" ) THEN
              znt(i,j) = sfz0sibsum(landuse(i,j))  * 0.01  ! cm --> m
            ELSE IF ( met_lu_src(1:3) == "MOD" ) THEN
              znt(i,j) = sfz0modsum(landuse(i,j))  * 0.01  ! cm --> m
            ELSE
              WRITE (*,f9700) TRIM(pname), met_lu_src, met_lu_water
              CALL graceful_stop (pname)
            ENDIF
          ENDDO
        ENDDO

      ELSE IF ( met_season == 2 ) THEN  ! winter

        DO j = 1, nym
          DO i = 1, nxm
            IF ( ( met_lu_src(1:4) == "USGS" ) .AND.  &
                 ( met_lu_water == 16 ) ) THEN
              znt(i,j) = sfz0usgswin(landuse(i,j)) * 0.01  ! cm --> m
            ELSE IF ( ( met_lu_src(1:3) == "OLD" ) .AND.  &
                      ( met_lu_water == 7 ) ) THEN
              znt(i,j) = sfz0oldwin(landuse(i,j))  * 0.01  ! cm --> m
            ELSE IF ( met_lu_src(1:6) == "NLCD50" ) THEN
              znt(i,j) = sfz0nlcd50win(landuse(i,j))  * 0.01  ! cm --> m
            ELSE IF ( met_lu_src(1:6) == "NLCD40" ) THEN
              znt(i,j) = sfz0nlcd40win(landuse(i,j))  * 0.01  ! cm --> m
            ELSE IF ( met_lu_src(1:3) == "SIB" ) THEN
              znt(i,j) = sfz0sibwin(landuse(i,j))  * 0.01  ! cm --> m
            ELSE IF ( met_lu_src(1:3) == "MOD" ) THEN
              znt(i,j) = sfz0modwin(landuse(i,j))  * 0.01  ! cm --> m
            ELSE
              WRITE (*,f9700) TRIM(pname), met_lu_src, met_lu_water
              CALL graceful_stop (pname)
            ENDIF
          ENDDO
        ENDDO

      ENDIF

      znt(:,met_ny) = znt(:,nym)
      znt(met_nx,:) = znt(nxm,:)

      IF ( met_urban_phys < 1 ) THEN  ! if UCM, write after urban update
        WRITE (*,f6000) 'ZNT      ', znt   (lprt_metx, lprt_mety), 'm'
      ENDIF

    ENDIF

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! If sea ice was not part of the output, set flag to compute it later
! in METVARS2CTM.
!-------------------------------------------------------------------------------

  IF ( .NOT. gotseaice ) THEN
    WRITE (*,f9975) TRIM(pname), 'SEAICE'
    needseaice = .TRUE.
  ELSE
    needseaice = .FALSE.
  ENDIF

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

! DEALLOCATE ( dum2d )    ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum2d_i )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum2d_u )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum2d_v )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_l )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_li ) ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_m )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_p )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_s )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_t )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_u )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_v )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_w )  ! commented out to avoid memory fragmentation
 
END SUBROUTINE rdwrfem
