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
!-------------------------------------------------------------------------------

  USE date_pack
  USE files
  USE metinfo, nx => met_nx, ny => met_ny, nz => met_nz, ns => met_ns
  USE metvars
  USE mcipparm
  USE wrf_netcdf
  USE netcdf

  IMPLICIT NONE

  INTEGER, SAVE                     :: cdfid
  INTEGER                           :: cdfidg
  INTEGER                           :: dimids     ( nf90_max_var_dims )
  REAL,    SAVE,      ALLOCATABLE   :: dum2d      ( : , : )
  INTEGER, SAVE,      ALLOCATABLE   :: dum2d_i    ( : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum2d_u    ( : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum2d_v    ( : , : )
  REAL,    SAVE,      ALLOCATABLE   :: dum3d_l    ( : , : , : )
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
  CHARACTER(LEN=16),  PARAMETER     :: pname      = 'RDWRFEM'
  INTEGER                           :: rcode
  REAL,               PARAMETER     :: rdovcp     = 2.0 / 7.0
  REAL,               PARAMETER     :: smallnum   = 1.0e-7
  CHARACTER(LEN=19)                 :: startseas
  CHARACTER(LEN=2)                  :: str1
  CHARACTER(LEN=2)                  :: str2
  CHARACTER(LEN=19),SAVE,ALLOCATABLE:: times      ( : )
  INTEGER,            PARAMETER     :: ttol       = 300  ! [sec]
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

  nxm = nx - 1
  nym = ny - 1
  nzp = nz + 1

!-------------------------------------------------------------------------------
! Set up print statements.
!-------------------------------------------------------------------------------

  k1 = nz / 5
  k2 = MOD(nz, 5)

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
    ELSE
      ifmt2 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt2 = "(/,1x,a,5(1x,f12.4),/,11x," // str2 // "(1x,f12.4))"
    ELSE
      ifmt2 = "(/,1x,a,5(1x,f12.4))"
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( dum2d   ) )  & 
    ALLOCATE ( dum2d   (nxm, nym)      )  ! 2D array on cross points
  IF ( .NOT. ALLOCATED ( dum2d_i ) )  &
    ALLOCATE ( dum2d_i (nxm, nym)      )  ! 2D integer array on cross points
  IF ( .NOT. ALLOCATED ( dum2d_u ) )  &
    ALLOCATE ( dum2d_u (nx,  nym ) )      ! 2D array on E-W flux pts
  IF ( .NOT. ALLOCATED ( dum2d_v ) )  &
    ALLOCATE ( dum2d_v (nxm, ny  ) )      ! 2D array on N-S flux pts
  IF ( .NOT. ALLOCATED ( dum3d_l ) )  &
    ALLOCATE ( dum3d_l (nxm, nym, nummetlu ) )  ! 3D array on cross points, lu
  IF ( .NOT. ALLOCATED ( dum3d_p ) )  &
    ALLOCATE ( dum3d_p (nxm, nym, nz ) )  ! 3D array on cross points, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_s ) )  &
    ALLOCATE ( dum3d_s (nxm, nym, ns ) )  ! 3D array on cross points, soil lvls
  IF ( .NOT. ALLOCATED ( dum3d_t ) )  &
    ALLOCATE ( dum3d_t (nxm, nym, nz ) )  ! 3D array on cross points, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_u ) )  &
    ALLOCATE ( dum3d_u (nx,  nym, nz ) )  ! 3D array on E-W flux pts, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_v ) )  &
    ALLOCATE ( dum3d_v (nxm, ny,  nz ) )  ! 3D array on N-S flux pts, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_w ) )  &
    ALLOCATE ( dum3d_w (nxm, nym, nzp) )  ! 3D array on cross points, full lvls

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

    rcode = nf90_open (fl, nf90_nowrite, cdfid)
    IF ( rcode /= nf90_noerr ) THEN
      WRITE (*,f9900) TRIM(pname)
      CALL graceful_stop (pname)
    ENDIF

    findprev: DO
      IF ( newfilem1 ) THEN
        rcode = nf90_close (cdfid)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9950) TRIM(pname)
          CALL graceful_stop (pname)
        ENDIF
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
        IF ( ABS(idtsec) < ttol ) THEN  ! found MCIP_PREVIOUS in WRF output
          itm1 = i
          EXIT findprev
        ENDIF
      ENDDO
      IF ( i > n_times ) THEN
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
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RAINC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'RAINNC', dum2d, itm1, rcode)
    IF ( rcode == nf90_noerr ) THEN
      rnold(1:nxm,1:nym) = dum2d(:,:)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RAINNC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    IF ( met_rain_bucket > 0.0 ) THEN  ! tipping bucket is on
    
      CALL get_var_2d_int_cdf (cdfid, 'I_RAINC', dum2d_i, itm1, rcode)
      IF ( rcode == nf90_noerr ) THEN
        ircold(1:nxm,1:nym) = dum2d_i(:,:)
      ELSE
        WRITE (*,f9400) TRIM(pname), 'I_RAINC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

      CALL get_var_2d_int_cdf (cdfid, 'I_RAINNC', dum2d_i, itm1, rcode)
      IF ( rcode == nf90_noerr ) THEN
        irnold(1:nxm,1:nym) = dum2d_i(:,:)
      ELSE
        WRITE (*,f9400) TRIM(pname), 'I_RAINNC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF

    ENDIF  ! tipping bucket

  ENDIF

!-------------------------------------------------------------------------------
! Find time index (IT) for MCIP_NOW in WRF output file.
!-------------------------------------------------------------------------------

  fl = file_mm(mmcount)

  rcode = nf90_open (fl, nf90_nowrite, cdfid)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9900) TRIM(pname)
    CALL graceful_stop (pname)
  ENDIF

  findit: DO
    IF ( newfile ) THEN
      rcode = nf90_close (cdfid)
      IF ( rcode /= nf90_noerr ) THEN
        WRITE (*,f9950) TRIM(pname)
        CALL graceful_stop (pname)
      ENDIF
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
      IF ( ABS(idtsec) < ttol ) THEN  ! found MCIP_NOW in WRF output
        it = i
        IF ( i < n_times ) it_start = i + 1
        EXIT findit
      ENDIF
    ENDDO
    IF ( i > n_times ) THEN
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
    ua(:,1:nym,:) = dum3d_u(:,:,:)
    ua(:,  ny, :) = ua(:,nym,:)
    WRITE (*,ifmt1) 'U        ', (ua(lprt_metx,lprt_mety,k),k=1,nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'U', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'V', dum3d_v, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    va(1:nxm,:,:) = dum3d_v(:,:,:)
    va(  nx, :,:) = va(nxm,:,:)
    WRITE (*,ifmt1) 'V        ', (va(lprt_metx,lprt_mety,k),k=1,nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'V', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'W', dum3d_w, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    wa(1:nxm,1:nym,:) = dum3d_w(:,:,:)
    wa(  nx,  :,   :) = wa(nxm,:,:)
    wa( :,     ny, :) = wa(:,nym,:)
    WRITE (*,ifmt1a) 'W        ', (wa(lprt_metx,lprt_mety,k),k=1,nzp)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'W', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'PH', dum3d_w, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    ph(1:nxm,1:nym,:) = dum3d_w(:,:,:)
    ph(  nx,  :,   :) = ph(nxm,:,:)
    ph( :,     ny, :) = ph(:,nym,:)
    WRITE (*,ifmt1a) 'PH       ', (ph(lprt_metx,lprt_mety,k),k=1,nzp)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'PH', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'PHB', dum3d_w, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    phb(1:nxm,1:nym,:) = dum3d_w(:,:,:)
    phb(  nx,  :,   :) = phb(nxm,:,:)
    phb( :,     ny, :) = phb(:,nym,:)
    WRITE (*,ifmt1a) 'PHB      ', (phb(lprt_metx,lprt_mety,k),k=1,nzp)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'PHB', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'P', dum3d_p, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    pp(1:nxm,1:nym,:) = dum3d_p(:,:,:)
    pp(  nx,  :,   :) = pp(nxm,:,:)
    pp( :,     ny, :) = pp(:,nym,:)
    WRITE (*,ifmt1) 'P        ', (pp(lprt_metx,lprt_mety,k),k=1,nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'P', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'PB', dum3d_p, it, rcode)
  IF ( rcode == 0 ) THEN
    pb(1:nxm,1:nym,:) = dum3d_p(:,:,:)
    pb(  nx,  :,   :) = pb(nxm,:,:)
    pb( :,     ny, :) = pb(:,nym,:)
    WRITE (*,ifmt1) 'PB       ', (pb(lprt_metx,lprt_mety,k),k=1,nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'PB', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'T', dum3d_t, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    dum3d_p(:,:,:)    = dum3d_p(:,:,:) + pp(1:nxm,1:nym,:)   ! pressure [Pa]
    dum3d_t(:,:,:)    = dum3d_t(:,:,:) + 300.0               ! theta [K]
    IF ( lpv > 0 .OR. ifmolpx ) THEN  ! need theta
      theta(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      theta(  nx,  :,   :) = theta(nxm,:,:)
      theta( :,     ny, :) = theta(:,nym,:)
      WRITE (*,ifmt1) 'THETA    ', (theta(lprt_metx,lprt_mety,k),k=1,nz)
    ENDIF
    ta(1:nxm,1:nym,:) = dum3d_t(:,:,:) * (dum3d_p(:,:,:)/100000.0)**rdovcp
    ta(  nx,  :,   :) = ta(nxm,:,:)
    ta( :,     ny, :) = ta(:,nym,:)
    WRITE (*,ifmt1) 'T        ', (ta(lprt_metx,lprt_mety,k),k=1,nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'T', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'QVAPOR', dum3d_t, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    qva(1:nxm,1:nym,:) = dum3d_t(:,:,:)
    qva(  nx,  :,   :) = qva(nxm,:,:)
    qva( :,     ny, :) = qva(:,nym,:)
    WRITE (*,ifmt1) 'QVAPOR   ', (qva(lprt_metx,lprt_mety,k),k=1,nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'QVAPOR', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'QCLOUD', dum3d_t, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    qca(1:nxm,1:nym,:) = dum3d_t(:,:,:)
    qca(  nx,  :,   :) = qca(nxm,:,:)
    qca( :,     ny, :) = qca(:,nym,:)
    WRITE (*,ifmt1) 'QCLOUD   ', (qca(lprt_metx,lprt_mety,k),k=1,nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'QCLOUD', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'QRAIN', dum3d_t, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    qra(1:nxm,1:nym,:) = dum3d_t(:,:,:)
    qra(  nx,  :,   :) = qra(nxm,:,:)
    qra( :,     ny, :) = qra(:,nym,:)
    WRITE (*,ifmt1) 'QRAIN    ', (qra(lprt_metx,lprt_mety,k),k=1,nz)
  ELSE
    WRITE (*,f9400) TRIM(pname), 'QRAIN', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  rcode = nf90_inq_varid (cdfid, 'QICE', rcode)
  IF ( rcode == nf90_noerr ) THEN
    CALL get_var_3d_real_cdf (cdfid, 'QICE', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      qia(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      qia(  nx,  :,   :) = qia(nxm,:,:)
      qia( :,     ny, :) = qia(:,nym,:)
      WRITE (*,ifmt1) 'QICE     ', (qia(lprt_metx,lprt_mety,k),k=1,nz)
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
      qsa(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      qsa(  nx,  :,   :) = qsa(nxm,:,:)
      qsa( :,     ny, :) = qsa(:,nym,:)
      WRITE (*,ifmt1) 'QSNOW    ', (qsa(lprt_metx,lprt_mety,k),k=1,nz)
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
      qga(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      qga(  nx,  :,   :) = qga(nxm,:,:)
      qga( :,     ny, :) = qga(:,nym,:)
      WRITE (*,ifmt1) 'QGRAUP   ', (qga(lprt_metx,lprt_mety,k),k=1,nz)
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
      tke(1:nxm,1:nym,:) = dum3d_w(:,:,:)
      tke(  nx,  :,   :) = tke(nxm,:,:)
      tke( :,     ny, :) = tke(:,nym,:)
      WRITE (*,ifmt1a) 'TKE      ', (tke(lprt_metx,lprt_mety,k),k=1,nzp)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'TKE', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ELSE IF ( ( iftke ) .AND. ( .NOT. iftkef ) ) THEN  ! TKE on half-layers
    CALL get_var_3d_real_cdf (cdfid, 'TKE_MYJ', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      tke(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      tke(  nx,  :,   :) = tke(nxm,:,:)
      tke( :,     ny, :) = tke(:,nym,:)
      WRITE (*,ifmt1) 'TKE_MYJ  ', (tke(lprt_metx,lprt_mety,k),k=1,nz)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'TKE_MYJ', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( ifcld3d ) THEN  ! 3D resolved cloud fraction
    CALL get_var_3d_real_cdf (cdfid, 'CLDFRA', dum3d_t, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      cldfra(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      cldfra(  nx,  :,   :) = cldfra(nxm,:,:)
      cldfra( :,     ny, :) = cldfra(:,nym,:)
      WRITE (*,ifmt1a) 'CLDFRA   ', (cldfra(lprt_metx,lprt_mety,k),k=1,nzp)
    ELSE
      WRITE (*,f9400) TRIM(pname), 'CLDFRA', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MU', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    mu(1:nxm,1:nym) = dum2d(:,:)
    mu(nx,:) = mu(nxm,:)
    mu(:,ny) = mu(:,nym)
    WRITE (*,f6000) 'MU       ', mu(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MU', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MUB', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    mub(1:nxm,1:nym) = dum2d(:,:)
    mub(nx,:) = mub(nxm,:)
    mub(:,ny) = mub(:,nym)
    WRITE (*,f6000) 'MUB      ', mub(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MUB', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( ift2m ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'T2', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      t2(1:nxm,1:nym) = dum2d(:,:)
      t2(nx,:) = t2(nxm,:)
      t2(:,ny) = t2(:,nym)
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
      q2(nx,:) = t2(nxm,:)
      q2(:,ny) = t2(:,nym)
      WRITE (*,f6000) 'Q2       ', q2(lprt_metx, lprt_mety), 'kg/kg'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'Q2', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( ifw10m ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'U10', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      u10(1:nxm,1:nym) = dum2d(:,:)
      u10(nx,:) = u10(nxm,:)
      u10(:,ny) = u10(:,nym)
      WRITE (*,f6000) 'U10      ', u10(lprt_metx, lprt_mety), 'm/s'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'U10', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
    CALL get_var_2d_real_cdf (cdfid, 'V10', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      v10(1:nxm,1:nym) = dum2d(:,:)
      v10(nx,:) = v10(nxm,:)
      v10(:,ny) = v10(:,nym)
      WRITE (*,f6000) 'V10      ', v10(lprt_metx, lprt_mety), 'm/s'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'V10', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'PSFC', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    psa(1:nxm,1:nym) = dum2d(:,:)
    psa(nx,:) = psa(nxm,:)
    psa(:,ny) = psa(:,nym)
    WRITE (*,f6000) 'PSFC     ', psa(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'PSFC', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MAPFAC_M', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    mapcrs(1:nxm,1:nym) = dum2d(:,:)
    mapcrs(nx,:) = mapcrs(nxm,:)
    mapcrs(:,ny) = mapcrs(:,nym)
    WRITE (*,f6000) 'MAPFAC_M ', mapcrs(lprt_metx, lprt_mety), 'dimensionless'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'MAPFAC_M', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MAPFAC_U', dum2d_u, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    mapu(:,1:nym) = dum2d_u(:,:)
    mapu(:,  ny ) = mapu(:,nym)
    WRITE (*,f6000) 'MAPFAC_U ', mapu(lprt_metx, lprt_mety), 'dimensionless'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MAPFAC_V', dum2d_v, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    mapv(1:nxm,:) = dum2d_v(:,:)
    mapv(  nx, :) = mapv(nxm,:)
    WRITE (*,f6000) 'MAPFAC_V ', mapv(lprt_metx, lprt_mety), 'dimensionless'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'HGT', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    terrain(1:nxm,1:nym) = dum2d(:,:)
    terrain(nx,:) = terrain(nxm,:)
    terrain(:,ny) = terrain(:,nym)
    WRITE (*,f6000) 'HGT      ', terrain(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'HGT', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( met_pcp_incr == 0 ) THEN  ! compute incremental precip in MCIP

    CALL get_var_2d_real_cdf (cdfid, 'RAINC', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      raincon(1:nxm,1:nym) = (dum2d(:,:) - rcold(1:nxm,1:nym))/10.0
      raincon(nx,:) = raincon(nxm,:)
      raincon(:,ny) = raincon(:,nym)
      rcold(1:nxm,1:nym) = dum2d(:,:)
      WRITE (*,f6000) 'RAINC    ', raincon(lprt_metx, lprt_mety), 'cm'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RAINC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'RAINNC', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      rainnon(1:nxm,1:nym) = (dum2d(:,:) - rnold(1:nxm,1:nym))/10.0
      rainnon(nx,:) = rainnon(nxm,:)
      rainnon(:,ny) = rainnon(:,nym)
      rnold(1:nxm,1:nym) = dum2d(:,:)
      WRITE (*,f6000) 'RAINNC   ', rainnon(lprt_metx, lprt_mety), 'cm'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RAINNC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    IF ( met_rain_bucket > 0.0 ) THEN  ! adjust RAINC and RAINNC for bucket
    
      CALL get_var_2d_int_cdf (cdfid, 'I_RAINC', dum2d_i, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        i_rainc(:,:) = dum2d_i(:,:) - ircold(:,:)
        raincon(:,:) = raincon(:,:) + 0.1 * met_rain_bucket * FLOAT(i_rainc(:,:))
        ircold (:,:) = dum2d_i(:,:)
        WRITE (*,f6100) 'I_RAINC  ', i_rainc(lprt_metx, lprt_mety), 'times'
        WRITE (*,f6000) 'CONV RAIN', raincon(lprt_metx, lprt_mety), 'cm'
      ELSE
        WRITE (*,f9400) TRIM(pname), 'I_RAINC', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
  
      CALL get_var_2d_int_cdf (cdfid, 'I_RAINNC', dum2d_i, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        i_rainnc(:,:) = dum2d_i(:,:) - irnold(:,:)
        rainnon (:,:) = rainnon(:,:) + 0.1 * met_rain_bucket * FLOAT(i_rainnc(:,:))
        irnold  (:,:) = dum2d_i(:,:)
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
      raincon(1:nxm,1:nym) = dum2d(:,:) / 10.0
      raincon(nx,:) = raincon(nxm,:)
      raincon(:,ny) = raincon(:,nym)
      WRITE (*,f6000) 'RAINC    ', raincon(lprt_metx, lprt_mety), 'cm'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'PREC_ACC_C', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'PREC_ACC_NC', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      rainnon(1:nxm,1:nym) = dum2d(:,:) / 10.0
      rainnon(nx,:) = rainnon(nxm,:)
      rainnon(:,ny) = rainnon(:,nym)
      WRITE (*,f6000) 'RAINNC   ', rainnon(lprt_metx, lprt_mety), 'cm'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'PREC_ACC_NC', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF  ! incremental precip

  CALL get_var_2d_real_cdf (cdfid, 'SWDOWN', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    rgrnd(1:nxm,1:nym) = dum2d(:,:)
    rgrnd(nx,:) = rgrnd(nxm,:)
    rgrnd(:,ny) = rgrnd(:,nym)
    WRITE (*,f6000) 'SWDOWN   ', rgrnd(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SWDOWN', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'GLW', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    glw(1:nxm,1:nym) = dum2d(:,:)
    glw(nx,:) = glw(nxm,:)
    glw(:,ny) = glw(:,nym)
    WRITE (*,f6000) 'GLW      ', glw(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'GLW', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLAT', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    latcrs(1:nxm,1:nym) = dum2d(:,:)
    latcrs(nx,:) = latcrs(nxm,:)
    latcrs(:,ny) = latcrs(:,nym)
    WRITE (*,f6000) 'XLAT     ', latcrs(lprt_metx, lprt_mety), 'degrees'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'XLAT', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLAT_U', dum2d_u, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    latu(:,1:nym) = dum2d_u(:,:)
    latu(:,  ny ) = latu(:,nym)
    WRITE (*,f6000) 'XLAT_U   ', latu(lprt_metx, lprt_mety), 'degrees'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLAT_V', dum2d_v, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    latv(1:nxm,:) = dum2d_v(:,:)
    latv(  nx, :) = latv(nxm,:)
    WRITE (*,f6000) 'XLAT_V   ', latv(lprt_metx, lprt_mety), 'degrees'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLONG', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    loncrs(1:nxm,1:nym) = dum2d(:,:)
    loncrs(nx,:) = loncrs(nxm,:)
    loncrs(:,ny) = loncrs(:,nym)
    WRITE (*,f6000) 'XLONG    ', loncrs(lprt_metx, lprt_mety), 'degrees'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'XLONG', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLONG_U', dum2d_u, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    lonu(:,1:nym) = dum2d_u(:,:)
    lonu(:,  ny ) = lonu(:,nym)
    WRITE (*,f6000) 'XLONG_U  ', lonu(lprt_metx, lprt_mety), 'degrees'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLONG_V', dum2d_v, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    lonv(1:nxm,:) = dum2d_v(:,:)
    lonv(  nx, :) = lonv(nxm,:)
    WRITE (*,f6000) 'XLONG_V  ', lonv(lprt_metx, lprt_mety), 'degrees'
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
    landuse(nx,:) = landuse(nxm,:)
    landuse(:,ny) = landuse(:,nym)
    WRITE (*,f6100) 'LU_INDEX ', landuse(lprt_metx, lprt_mety), 'category'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'LU_INDEX', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'LANDMASK', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    landmask(1:nxm,1:nym) = dum2d(:,:)
    landmask(nx,:) = landmask(nxm,:)
    landmask(:,ny) = landmask(:,nym)
    WRITE (*,f6000) 'LANDMASK ', landmask(lprt_metx, lprt_mety), 'category'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'LANDMASK', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'HFX', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    hfx(1:nxm,1:nym) = dum2d(:,:)
    hfx(nx,:) = hfx(nxm,:)
    hfx(:,ny) = hfx(:,nym)
    WRITE (*,f6000) 'HFX      ', hfx(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'HFX', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'LH', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    lh(1:nxm,1:nym) = dum2d(:,:)
    lh(nx,:) = lh(nxm,:)
    lh(:,ny) = lh(:,nym)
    WRITE (*,f6000) 'LH       ', lh(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'LH', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'UST', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    ust(1:nxm,1:nym) = dum2d(:,:)
    ust(nx,:) = ust(nxm,:)
    ust(:,ny) = ust(:,nym)
    WRITE (*,f6000) 'UST      ', ust(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'UST', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( ifmol ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'RMOL', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      mol(1:nxm,1:nym) = 1.0 / dum2d(:,:)
      mol(nx,:) = mol(nxm,:)
      mol(:,ny) = mol(:,nym)
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
      qfx(nx,:) = qfx(nxm,:)
      qfx(:,ny) = qfx(:,nym)
      WRITE (*,f6000) 'QFX      ', qfx(lprt_metx, lprt_mety), 'kg m-2 s-1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'QFX ', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'PBLH', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    zpbl(1:nxm,1:nym) = dum2d(:,:)
    zpbl(nx,:) = zpbl(nxm,:)
    zpbl(:,ny) = zpbl(:,nym)
    WRITE (*,f6000) 'PBLH     ', zpbl(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'PBLH', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  IF ( ifresist ) THEN

    CALL get_var_2d_real_cdf (cdfid, 'RA', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      ra(1:nxm,1:nym) = dum2d(:,:)
      ra(nx,:) = ra(nxm,:)
      ra(:,ny) = ra(:,nym)
      IF ( ABS(MAXVAL(ra)) < smallnum ) THEN
        ifresist = .FALSE.
      ENDIF
      WRITE (*,f6000) 'RA       ', ra(lprt_metx, lprt_mety), 's m^-1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RA', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'RS', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      rstom(1:nxm,1:nym) = dum2d(:,:)
      rstom(nx,:) = rstom(nxm,:)
      rstom(:,ny) = rstom(:,nym)
      IF ( ABS(MAXVAL(rstom)) < smallnum ) THEN
        ifresist = .FALSE.
      ENDIF
      WRITE (*,f6000) 'RS       ', rstom(lprt_metx, lprt_mety), 's m^-1'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'RS', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF

  IF ( iflai ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'LAI', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      lai(1:nxm,1:nym) = dum2d(:,:)
      lai(nx,:) = lai(nxm,:)
      lai(:,ny) = lai(:,nym)
      IF ( ABS(MAXVAL(lai)) < smallnum ) THEN
        IF ( met_soil_lsm == 2 ) THEN  ! NOAH LSM
          lai(:,:) = 4.0
        ENDIF
      ENDIF
      WRITE (*,f6000) 'LAI      ', lai(lprt_metx, lprt_mety), 'area/area'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'LAI', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF
  ENDIF

  IF ( ifwr ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'CANWAT', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      wr(1:nxm,1:nym) = dum2d(:,:)
      wr(nx,:) = wr(nxm,:)
      wr(:,ny) = wr(:,nym)
      WRITE (*,f6000) 'CANWAT   ', wr(lprt_metx, lprt_mety), 'kg m^-2'
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
        veg(nx,:) = veg(nxm,:)
        veg(:,ny) = veg(:,nym)
        WRITE (*,f6000) 'VEGF_PX  ', veg(lprt_metx, lprt_mety), 'area/area'
      ELSE
        CALL get_var_2d_real_cdf (cdfid, 'VEGFRA', dum2d, it, rcode)
        IF ( rcode == nf90_noerr ) THEN
          veg(1:nxm,1:nym) = dum2d(:,:) * 0.01
          veg(nx,:) = veg(nxm,:)
          veg(:,ny) = veg(:,nym)
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
        veg(nx,:) = veg(nxm,:)
        veg(:,ny) = veg(:,nym)
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
      isltyp(nx,:) = isltyp(nxm,:)
      isltyp(:,ny) = isltyp(:,nym)
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
      wg(nx,:) = wg(nxm,:)
      wg(:,ny) = wg(:,nym)
      WRITE (*,f6000) 'SMOIS 1  ', wg(lprt_metx, lprt_mety), 'm^3 m^-3'
      w2(1:nxm,1:nym) = dum3d_s(:,:,2)
      w2(nx,:) = w2(nxm,:)
      w2(:,ny) = w2(:,nym)
      WRITE (*,f6000) 'SMOIS 2  ', w2(lprt_metx, lprt_mety), 'm^3 m^-3'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'SMOIS', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'TSLB', dum3d_s, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      soilt1(1:nxm,1:nym) = dum3d_s(:,:,1)
      soilt1(nx,:) = soilt1(nxm,:)
      soilt1(:,ny) = soilt1(:,nym)
      WRITE (*,f6000) 'TSLB 1   ', soilt1(lprt_metx, lprt_mety), 'K'
      soilt2(1:nxm,1:nym) = dum3d_s(:,:,2)
      soilt2(nx,:) = soilt2(nxm,:)
      soilt2(:,ny) = soilt2(:,nym)
      WRITE (*,f6000) 'TSLB 2   ', soilt2(lprt_metx, lprt_mety), 'K'
    ELSE
      WRITE (*,f9400) TRIM(pname), 'TSLB', TRIM(nf90_strerror(rcode))
      CALL graceful_stop (pname)
    ENDIF

  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'TSK', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    groundt(1:nxm,1:nym) = dum2d(:,:)
    groundt(nx,:) = groundt(nxm,:)
    groundt(:,ny) = groundt(:,nym)
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
    albedo(nx,:) = albedo(nxm,:)
    albedo(:,ny) = albedo(:,nym)
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
          lufrac(1:nxm,1:nym,:) = dum3d_l(:,:,:)
          lufrac(  nx,  :,   :) = lufrac(nxm,:,:)
          lufrac( :,     ny, :) = lufrac(:,nym,:)
          WRITE (*,ifmt2) 'LANDUSEF ', (lufrac(lprt_metx,lprt_mety,k),k=1,nummetlu)
        ELSE
          WRITE (*,f9400) TRIM(pname), 'LANDUSEF', TRIM(nf90_strerror(rcode))
          CALL graceful_stop (pname)
        ENDIF
      ELSE  ! land use fractions in GEOGRID file from WPS
        flg = file_ter
        rcode = nf90_open (flg, nf90_nowrite, cdfidg)
        IF ( rcode /= nf90_noerr ) THEN
          WRITE (*,f9900) TRIM(pname)
          CALL graceful_stop (pname)
        ENDIF
        CALL get_var_3d_real_cdf (cdfidg, 'LANDUSEF', dum3d_l, 1, rcode)
        IF ( rcode == nf90_noerr ) THEN
          lufrac(1:nxm,1:nym,:) = dum3d_l(:,:,:)
          lufrac(  nx,  :,   :) = lufrac(nxm,:,:)
          lufrac( :,     ny, :) = lufrac(:,nym,:)
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
        frc_urb(nx,:) = frc_urb(nxm,:)
        frc_urb(:,ny) = frc_urb(:,nym)
        WRITE (*,f6000) 'FRC_URB2D', frc_urb(lprt_metx, lprt_mety), 'fraction'
      ELSE
        WRITE (*,f9400) TRIM(pname), 'FRC_URB2D', TRIM(nf90_strerror(rcode))
      ENDIF
    ENDIF
    IF ( lpv > 0 ) THEN
      CALL get_var_2d_real_cdf (cdfid, 'F', dum2d, it, rcode)
      IF ( rcode == nf90_noerr ) THEN
        coriolis(1:nxm,1:nym) = dum2d(:,:)
        coriolis(nx,:) = coriolis(nxm,:)
        coriolis(:,ny) = coriolis(:,nym)
        WRITE (*,f6000) 'F        ', coriolis(lprt_metx, lprt_mety), 's-1'
      ELSE
        WRITE (*,f9400) TRIM(pname), 'F      ', TRIM(nf90_strerror(rcode))
        CALL graceful_stop (pname)
      ENDIF
    ENDIF
  ENDIF

  IF ( ifznt ) THEN  ! expecting roughness length in file
    CALL get_var_2d_real_cdf (cdfid, 'ZNT', dum2d, it, rcode)
    IF ( rcode == nf90_noerr ) THEN
      znt(1:nxm,1:nym) = dum2d(:,:)
      znt(nx,:) = znt(nxm,:)
      znt(:,ny) = znt(:,nym)
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
    snowcovr(nx,:) = snowcovr(nxm,:)
    snowcovr(:,ny) = snowcovr(:,nym)
    WRITE (*,f6000) 'SNOWC    ', snowcovr(lprt_metx, lprt_mety), 'category'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SNOWC', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'SEAICE', dum2d, it, rcode)
  IF ( rcode == nf90_noerr ) THEN
    seaice(1:nxm,1:nym) = dum2d(:,:)
    seaice(nx,:) = seaice(nxm,:)
    seaice(:,ny) = seaice(:,nym)
    gotseaice = .TRUE.
    WRITE (*,f6000) 'SEAICE   ', seaice(lprt_metx, lprt_mety), 'fraction'
  ELSE
    WRITE (*,f9400) TRIM(pname), 'SEAICE', TRIM(nf90_strerror(rcode))
    CALL graceful_stop (pname)
  ENDIF

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

  rcode = nf90_close (cdfid)
  IF ( rcode /= nf90_noerr ) THEN
    WRITE (*,f9950) TRIM(pname)
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

        DO j = 1, ny
          DO i = 1, nx

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

          DO j = 1, ny  ! do all of Y to fill array; last row is outside domain
            DO i = 1, nx

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

          DO j = 1, ny
            DO i = 1, nx  ! do all of X to fill array; last col outside domain

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

        DO j = 1, ny
          DO i = 1, nx

            ! Use four-point interpolation here for latitude and longitude.
            ! Because CMAQ will never use outermost row and column from WRF
            ! due to location of CMAQ boundaries, inexact values in the
            ! outermost row and column will not matter.

            ii = MAX(i,nxm)
            jj = MAX(j,nym)

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

          DO j = 1, ny
            DO i = 1, nx

              ! Use linear interpolation here for latitude and longitude.
              ! Because CMAQ will never use outermost row and column from WRF
              ! due to location of CMAQ boundaries, inexact values in the
              ! outermost row and column will not matter.

              ii = MAX(i,nxm)
              jj = MAX(j,nym)

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

        DO j = 1, ny
          DO i = 1, nx

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

          DO j = 1, ny  ! do all of Y to fill array; last row is outside domain
            DO i = 1, nx

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

          DO j = 1, ny
            DO i = 1, nx  ! do all of X to fill array; last col outside domain

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

      znt(:,ny) = znt(:,nym)
      znt(nx,:) = znt(nxm,:)

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
! DEALLOCATE ( dum3d_l )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_p )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_t )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_u )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_v )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_w )  ! commented out to avoid memory fragmentation
 
END SUBROUTINE rdwrfem
