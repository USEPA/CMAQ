!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

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
!-------------------------------------------------------------------------------

  USE date_pack
  USE file
  USE metinfo, nx => met_nx, ny => met_ny, nz => met_nz, ns => met_ns
  USE metvars
  USE mcipparm
  USE wrf_netcdf

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       SAVE          :: cdfid
  INTEGER                      :: cdfidg
  REAL,    SAVE, ALLOCATABLE   :: dum2d      ( : , : )
  INTEGER, SAVE, ALLOCATABLE   :: dum2d_i    ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum2d_u    ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum2d_v    ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_l    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_p    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_s    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_t    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_u    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_v    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_w    ( : , : , : )
  CHARACTER*24                 :: endseas
  LOGICAL,       SAVE          :: first      = .TRUE.
  CHARACTER*256                :: fl
  CHARACTER*256                :: flg
  LOGICAL                      :: gotfaces   = .TRUE.
  LOGICAL                      :: gotznt
  INTEGER                      :: i
  INTEGER                      :: idts_end
  INTEGER                      :: idts_start
  INTEGER                      :: idtsec
  LOGICAL                      :: iffl
  CHARACTER*64                 :: ifmt1
  CHARACTER*64                 :: ifmt1a
  CHARACTER*64                 :: ifmt2
  INTEGER                      :: it
  INTEGER,       SAVE          :: it_start
  INTEGER                      :: itm1
  INTEGER                      :: j
  INTEGER                      :: k1
  INTEGER                      :: k2
  REAL,          EXTERNAL      :: mapfac_lam
  REAL,          EXTERNAL      :: mapfac_merc
  REAL,          EXTERNAL      :: mapfac_ps
  INTEGER,       PARAMETER     :: max_times  = 1000
  CHARACTER*24,  INTENT(IN)    :: mcip_now
  CHARACTER*24                 :: mcip_previous
  INTEGER                      :: m1count    = 1
  INTEGER,       SAVE          :: mmcount    = 1
  INTEGER,       SAVE          :: n_times
  LOGICAL,       SAVE          :: newfile    = .TRUE.
  LOGICAL                      :: newfilem1  = .TRUE.
  INTEGER                      :: nxm
  INTEGER                      :: nym
  INTEGER                      :: nzp
  CHARACTER*16,  PARAMETER     :: pname      = 'RDWRFEM'
  INTEGER                      :: rcode
  REAL,          PARAMETER     :: rdovcp     = 2.0 / 7.0
  REAL                         :: sfz0mod    ( 33, 2 )
  REAL                         :: sfz0nlc    ( 50, 2 )
  REAL                         :: sfz0old    ( 13, 2 )
  REAL                         :: sfz0sib    ( 16, 2 )
  REAL                         :: sfz0usgs   ( 33, 2 )
  REAL,          PARAMETER     :: smallnum   = 1.0e-7
  CHARACTER*24                 :: startseas
  CHARACTER*2                  :: str1
  CHARACTER*2                  :: str2
  CHARACTER*80,  SAVE          :: times      ( max_times )
  INTEGER,       PARAMETER     :: ttol       = 300  ! [sec]
  REAL                         :: xoff
  REAL                         :: xxin
  REAL                         :: yoff
  REAL                         :: yyin

  ! Define roughness length as functions of land use and season in case
  ! it is not available in WRF output.

  DATA (sfz0old(i,1),i=1,13)  / 50.0,  15.0,  12.0,  50.0,  50.0,  40.0,   &
                                 0.01, 20.0,  10.0,  10.0,   5.0,  50.0,   &
                                15.0 /  ! summer [cm]

  DATA (sfz0old(i,2),i=1,13)  / 50.0,   5.0,  10.0,  50.0,  50.0,  40.0,   &
                                 0.01, 20.0,  10.0,  10.0,   5.0,  50.0,   &
                                15.0 /  ! winter [cm]

  DATA (sfz0mod(i,1),i=1,33)  / 50.0,  50.0,  50.0,  50.0,  50.0,   5.0,   &
                                 6.0,   5.0,  15.0,  12.0,  30.0,  15.0,   &
                                80.0,  14.0,   0.1,   1.0,   0.01, 30.0,   &
                                15.0,  10.0,  80.0,  80.0,  80.0,  80.0,   &
                                80.0,  80.0,  80.0,  80.0,  80.0,  80.0,   &
                                80.0,  80.0,  80.0 /  ! summer [cm]

  DATA (sfz0mod(i,2),i=1,33)  / 50.0,  50.0,  50.0,  50.0,  20.0,   1.0,   &
                                 1.0,   1.0,  15.0,  50.0,  30.0,   5.0,   &
                                80.0,   5.0,   0.1,   1.0,   0.01, 10.0,   &
                                30.0,  15.0,  80.0,  80.0,  80.0,  80.0,   &
                                80.0,  80.0,  80.0,  80.0,  80.0,  80.0,   &
                                80.0,  80.0,  80.0 /  ! winter [cm]

  DATA (sfz0nlc(i,1),i=1,50)  /  0.1,   1.2,  30.0,  40.0,  60.0, 100.0,   &
                                 5.0,   5.0, 100.0, 100.0, 100.0,  10.0,   &
                                30.0,   7.0,   7.0,   5.0,   5.0,   5.0,   &
                                 7.0,  10.0,  55.0,  80.0,  30.0,  60.0,   &
                                30.0,  11.0,  11.0,  11.0,   5.0,   5.0,   &
                                 0.1, 100.0,  90.0, 100.0, 100.0, 100.0,   &
                                30.0,  20.0,  25.0,  15.0,   7.0,  20.0,   &
                                10.0,  80.0,  10.0,   1.2,   5.0,   0.1,   &
                                 0.1,   0.1 /  ! summer [cm]

  DATA (sfz0nlc(i,2),i=1,50)  /  0.1,   1.2,  30.0,  40.0,  60.0, 100.0,   &
                                 5.0,   5.0, 100.0, 100.0, 100.0,  10.0,   &
                                30.0,   7.0,   7.0,   5.0,   5.0,   5.0,   &
                                 7.0,  10.0,  55.0,  80.0,  30.0,  60.0,   &
                                30.0,  11.0,  11.0,  11.0,   5.0,   5.0,   &
                                 0.1, 100.0,  90.0, 100.0, 100.0, 100.0,   &
                                30.0,  20.0,  25.0,  15.0,   7.0,  20.0,   &
                                10.0,  80.0,  10.0,   1.2,   5.0,   0.1,   &
                                 0.1,   0.1 /  ! winter [cm]

  DATA (sfz0sib(i,1),i=1,16)  / 50.0,  50.0,  40.0,  50.0,  50.0,  15.0,   &
                                12.0,  12.0,  12.0,  10.0,  10.0,  15.0,   &
                                20.0,  12.0,   0.01,  5.0 /  ! summer [cm]

  DATA (sfz0sib(i,2),i=1,16)  / 50.0,  50.0,  40.0,  50.0,  50.0,  15.0,   &
                                10.0,  10.0,  10.0,  10.0,  10.0,   5.0,   &
                                20.0,  10.0,   0.01,  5.0 /  ! winter [cm]

  DATA (sfz0usgs(i,1),i=1,33) / 80.0,  15.0,  10.0,  15.0,  14.0,  20.0,   &
                                12.0,   5.0,   6.0,  15.0,  50.0,  50.0,   &
                                50.0,  50.0,  50.0,   0.01, 20.0,  40.0,   &
                                 1.0,  10.0,  30.0,  15.0,  10.0,   5.0,   &
                                 1.0,  15.0,   1.0,  80.0,  80.0,  80.0,   &
                                80.0,  80.0,  80.0 /  ! summer [cm]

  DATA (sfz0usgs(i,2),i=1,33) / 80.0,   5.0,   2.0,   5.0,   5.0,  20.0,   &
                                10.0,   1.0,   1.0,  15.0,  50.0,  50.0,   &
                                50.0,  50.0,  20.0,   0.01, 20.0,  40.0,   &
                                 1.0,  10.0,  30.0,  15.0,   5.0,   5.0,   &
                                 1.0,  15.0,   1.0,  80.0,  80.0,  80.0,   &
                                80.0,  80.0,  80.0 /  ! winter [cm]

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
      ifmt1 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)),/,10x,"   &
         &    // str2 // "(1x,f12.4))"
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
      ifmt1a = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)),/,10x,"   &
         &     // str2 // "(1x,f12.4))"
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
      ifmt2 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)),/,10x,"   &
         &    // str2 // "(1x,f12.4))"
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
! If not processing the first WRF output time, retrieve accumulated
! precipitation totals from time increment before first MCIP step so that
! first incremental precipitation "rates" can be computed.  This step ensures
! that the "hold" values for convective and non-convective precipitation are
! correctly set with last accumulated total.
!-------------------------------------------------------------------------------

  IF ( ( first ) .AND. ( mcip_now > met_startdate ) ) THEN

    CALL geth_newdate (mcip_previous, mcip_now, intvl*(-60))

    fl = file_mm(m1count)

    rcode = nf_open (fl, nf_nowrite, cdfid)
    IF ( rcode /= nf_noerr ) THEN
      WRITE (6,9900)
      GOTO 1001
    ENDIF

    findprev: DO
      IF ( newfilem1 ) THEN
        rcode = nf_close (cdfid)
        IF ( rcode /= nf_noerr ) THEN
          WRITE (6,9950)
          GOTO 1001
        ENDIF
        rcode = nf_open (fl, nf_nowrite, cdfid)
        IF ( rcode /= nf_noerr ) THEN
          WRITE (6,9900)
          GOTO 1001
        ENDIF
        CALL chkwrfhdr (fl, cdfid)
        CALL get_times_cdf (cdfid, times, n_times, max_times, rcode)
        IF ( rcode == nf_noerr ) THEN
          newfilem1  = .FALSE.
        ELSE
          WRITE (6,9000) rcode
          GOTO 1001
        ENDIF
      ENDIF
      DO i = 1, n_times
        CALL geth_idts (times(i), mcip_previous, idtsec)
        IF ( ABS(idtsec) < ttol ) THEN  ! found MCIP_PREVIOUS in WRF output
          itm1 = i
          EXIT findprev
        ENDIF
      ENDDO
      IF ( i > n_times ) THEN
        newfilem1 = .TRUE.
        m1count   = m1count + 1
        IF ( m1count > max_mm ) THEN
          WRITE (6,9100) mcip_previous
          GOTO 1001
        ENDIF
        fl = file_mm(m1count)
        IF ( fl(1:10) == '          ' ) THEN
          WRITE (6,9200) mcip_previous, m1count
          GOTO 1001
        ENDIF
        INQUIRE (FILE=fl, EXIST=iffl)
        IF ( .NOT. iffl ) THEN
          WRITE (6,9300) mcip_previous, TRIM(fl)
          GOTO 1001
        ENDIF
      ENDIF
    ENDDO findprev

    CALL get_var_2d_real_cdf (cdfid, 'RAINC',    dum2d, nxm, nym, itm1, rcode)
    IF ( rcode == nf_noerr ) THEN
      rcold(1:nxm,1:nym) = dum2d(:,:)
    ELSE
      WRITE (6,9400) 'RAINC', rcode
      GOTO 1001
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'RAINNC',   dum2d, nxm, nym, itm1, rcode)
    IF ( rcode == nf_noerr ) THEN
      rnold(1:nxm,1:nym) = dum2d(:,:)
    ELSE
      WRITE (6,9400) 'RAINNC', rcode
      GOTO 1001
    ENDIF

  ENDIF

!-------------------------------------------------------------------------------
! Find time index (IT) for MCIP_NOW in WRF output file.
!-------------------------------------------------------------------------------

  fl = file_mm(mmcount)

  rcode = nf_open (fl, nf_nowrite, cdfid)
  IF ( rcode /= nf_noerr ) THEN
    WRITE (6,9900)
    GOTO 1001
  ENDIF

  findit: DO
    IF ( newfile ) THEN
      rcode = nf_close (cdfid)
      IF ( rcode /= nf_noerr ) THEN
        WRITE (6,9950)
        GOTO 1001
      ENDIF
      rcode = nf_open (fl, nf_nowrite, cdfid)
      IF ( rcode /= nf_noerr ) THEN
        WRITE (6,9900)
        GOTO 1001
      ENDIF
      CALL chkwrfhdr (fl, cdfid)
      CALL get_times_cdf (cdfid, times, n_times, max_times, rcode)
      IF ( rcode == nf_noerr ) THEN
        newfile  = .FALSE.
        it_start = 1
      ELSE
        WRITE (6,9000) rcode
        GOTO 1001
      ENDIF
    ENDIF
    DO i = it_start, n_times
      CALL geth_idts (times(i), mcip_now, idtsec)
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
        WRITE (6,9100) mcip_now
        GOTO 1001
      ENDIF
      fl = file_mm(mmcount)
      IF ( fl(1:10) == '          ' ) THEN
        WRITE (6,9200) mcip_now, mmcount
        GOTO 1001
      ENDIF
      INQUIRE (FILE=fl, EXIST=iffl)
      IF ( .NOT. iffl ) THEN
        WRITE (6,9300) mcip_now, TRIM(fl)
        GOTO 1001
      ENDIF
    ENDIF
  ENDDO findit

!-------------------------------------------------------------------------------
! Read WRF data for this domain.
!-------------------------------------------------------------------------------

  CALL get_var_3d_real_cdf (cdfid, 'U', dum3d_u, nx, nym, nz, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    ua(:,1:nym,:) = dum3d_u(:,:,:)
    ua(:,  ny, :) = ua(:,nym,:)
    WRITE (*,ifmt1) 'U        ', ua(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'U', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'V', dum3d_v, nxm, ny, nz, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    va(1:nxm,:,:) = dum3d_v(:,:,:)
    va(  nx, :,:) = va(nxm,:,:)
    WRITE (*,ifmt1) 'V        ', va(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'V', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'W', dum3d_w, nxm, nym, nzp, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    wa(1:nxm,1:nym,:) = dum3d_w(:,:,:)
    wa(  nx,  :,   :) = wa(nxm,:,:)
    wa( :,     ny, :) = wa(:,nym,:)
    WRITE (*,ifmt1a) 'W        ', wa(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'W', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'PH', dum3d_w, nxm, nym, nzp ,it, rcode)
  IF ( rcode == nf_noerr ) THEN
    ph(1:nxm,1:nym,:) = dum3d_w(:,:,:)
    ph(  nx,  :,   :) = ph(nxm,:,:)
    ph( :,     ny, :) = ph(:,nym,:)
    WRITE (*,ifmt1a) 'PH       ', ph(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'PH', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'PHB', dum3d_w, nxm, nym, nzp, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    phb(1:nxm,1:nym,:) = dum3d_w(:,:,:)
    phb(  nx,  :,   :) = phb(nxm,:,:)
    phb( :,     ny, :) = phb(:,nym,:)
    WRITE (*,ifmt1a) 'PHB      ', phb(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'PHB', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'P', dum3d_p, nxm, nym, nz, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    pp(1:nxm,1:nym,:) = dum3d_p(:,:,:)
    pp(  nx,  :,   :) = pp(nxm,:,:)
    pp( :,     ny, :) = pp(:,nym,:)
    WRITE (*,ifmt1) 'P        ', pp(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'P', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'PB', dum3d_p, nxm, nym, nz, it, rcode)
  IF ( rcode == 0 ) THEN
    pb(1:nxm,1:nym,:) = dum3d_p(:,:,:)
    pb(  nx,  :,   :) = pb(nxm,:,:)
    pb( :,     ny, :) = pb(:,nym,:)
    WRITE (*,ifmt1) 'PB       ', pb(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'PB', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'T', dum3d_t, nxm, nym, nz, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    dum3d_p(:,:,:)    = dum3d_p(:,:,:) + pp(1:nxm,1:nym,:)   ! pressure [Pa]
    dum3d_t(:,:,:)    = dum3d_t(:,:,:) + 300.0               ! theta [K]
    IF ( lpv > 0 ) THEN  ! will calculate potential vorticity; need theta
      theta(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      theta(  nx,  :,   :) = theta(nxm,:,:)
      theta( :,     ny, :) = theta(:,nym,:)
      WRITE (*,ifmt1) 'THETA    ', theta(lprt_metx,lprt_mety,:)
    ENDIF
    ta(1:nxm,1:nym,:) = dum3d_t(:,:,:) * (dum3d_p(:,:,:)/100000.0)**rdovcp
    ta(  nx,  :,   :) = ta(nxm,:,:)
    ta( :,     ny, :) = ta(:,nym,:)
    WRITE (*,ifmt1) 'T        ', ta(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'T', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'QVAPOR', dum3d_t, nxm, nym, nz, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    qva(1:nxm,1:nym,:) = dum3d_t(:,:,:)
    qva(  nx,  :,   :) = qva(nxm,:,:)
    qva( :,     ny, :) = qva(:,nym,:)
    WRITE (*,ifmt1) 'QVAPOR   ', qva(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'QVAPOR', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'QCLOUD', dum3d_t, nxm, nym, nz, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    qca(1:nxm,1:nym,:) = dum3d_t(:,:,:)
    qca(  nx,  :,   :) = qca(nxm,:,:)
    qca( :,     ny, :) = qca(:,nym,:)
    WRITE (*,ifmt1) 'QCLOUD   ', qca(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'QCLOUD', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (cdfid, 'QRAIN', dum3d_t, nxm, nym, nz, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    qra(1:nxm,1:nym,:) = dum3d_t(:,:,:)
    qra(  nx,  :,   :) = qra(nxm,:,:)
    qra( :,     ny, :) = qra(:,nym,:)
    WRITE (*,ifmt1) 'QRAIN    ', qra(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'QRAIN', rcode
    GOTO 1001
  ENDIF

  rcode = nf_inq_varid (cdfid, 'QICE', rcode)
  IF ( rcode == nf_noerr ) THEN
    CALL get_var_3d_real_cdf (cdfid, 'QICE', dum3d_t, nxm, nym, nz, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      qia(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      qia(  nx,  :,   :) = qia(nxm,:,:)
      qia( :,     ny, :) = qia(:,nym,:)
      WRITE (*,ifmt1) 'QICE     ', qia(lprt_metx,lprt_mety,:)
    ELSE
      WRITE (6,9400) 'QICE', rcode
      GOTO 1001
    ENDIF
  ELSE
    qia(:,:,:) = 0.0
  ENDIF

  rcode = nf_inq_varid (cdfid, 'QSNOW', rcode)
  IF ( rcode == nf_noerr ) THEN
    CALL get_var_3d_real_cdf (cdfid, 'QSNOW', dum3d_t, nxm, nym, nz, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      qsa(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      qsa(  nx,  :,   :) = qsa(nxm,:,:)
      qsa( :,     ny, :) = qsa(:,nym,:)
      WRITE (*,ifmt1) 'QSNOW    ', qsa(lprt_metx,lprt_mety,:)
    ELSE
      WRITE (6,9400) 'QSNOW', rcode
      GOTO 1001
    ENDIF
  ELSE
    qsa(:,:,:) = 0.0
  ENDIF

  rcode = nf_inq_varid (cdfid, 'QGRAUP', rcode)
  IF ( rcode == nf_noerr ) THEN
    CALL get_var_3d_real_cdf (cdfid, 'QGRAUP', dum3d_t, nxm, nym, nz, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      qga(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      qga(  nx,  :,   :) = qga(nxm,:,:)
      qga( :,     ny, :) = qga(:,nym,:)
      WRITE (*,ifmt1) 'QGRAUP   ', qga(lprt_metx,lprt_mety,:)
    ELSE
      WRITE (6,9400) 'QGRAUP', rcode
      GOTO 1001
    ENDIF
  ELSE
    qga(:,:,:) = 0.0
  ENDIF

  IF ( ( iftke ) .AND. ( iftkef ) ) THEN  ! TKE on full-levels
    CALL get_var_3d_real_cdf (cdfid, 'TKE', dum3d_w, nxm, nym, nzp, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      tke(1:nxm,1:nym,:) = dum3d_w(:,:,:)
      tke(  nx,  :,   :) = tke(nxm,:,:)
      tke( :,     ny, :) = tke(:,nym,:)
      WRITE (*,ifmt1a) 'TKE      ', tke(lprt_metx,lprt_mety,:)
    ELSE
      WRITE (6,9400) 'TKE', rcode
      GOTO 1001
    ENDIF
  ELSE IF ( ( iftke ) .AND. ( .NOT. iftkef ) ) THEN  ! TKE on half-layers
    CALL get_var_3d_real_cdf (cdfid, 'TKE_MYJ', dum3d_t, nxm, nym, nz, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      tke(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      tke(  nx,  :,   :) = tke(nxm,:,:)
      tke( :,     ny, :) = tke(:,nym,:)
      WRITE (*,ifmt1) 'TKE_MYJ  ', tke(lprt_metx,lprt_mety,:)
    ELSE
      WRITE (6,9400) 'TKE_MYJ', rcode
      GOTO 1001
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MU', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    mu(1:nxm,1:nym) = dum2d(:,:)
    mu(nx,:) = mu(nxm,:)
    mu(:,ny) = mu(:,nym)
    WRITE (*,6000) 'MU       ', mu(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (6,9400) 'MU', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MUB', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    mub(1:nxm,1:nym) = dum2d(:,:)
    mub(nx,:) = mub(nxm,:)
    mub(:,ny) = mub(:,nym)
    WRITE (*,6000) 'MUB      ', mub(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (6,9400) 'MUB', rcode
    GOTO 1001
  ENDIF

  IF ( ift2m ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'T2', dum2d, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      t2(1:nxm,1:nym) = dum2d(:,:)
      t2(nx,:) = t2(nxm,:)
      t2(:,ny) = t2(:,nym)
      WRITE (*,6000) 'T2       ', t2(lprt_metx, lprt_mety), 'K'
    ELSE
      WRITE (6,9400) 'T2', rcode
      GOTO 1001
    ENDIF
  ENDIF

  IF ( ifq2m ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'Q2', dum2d, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      q2(1:nxm,1:nym) = dum2d(:,:)
      q2(nx,:) = t2(nxm,:)
      q2(:,ny) = t2(:,nym)
      WRITE (*,6000) 'Q2       ', q2(lprt_metx, lprt_mety), 'kg/kg'
    ELSE
      WRITE (6,9400) 'Q2', rcode
      GOTO 1001
    ENDIF
  ENDIF

  IF ( ifw10m ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'U10', dum2d, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      u10(1:nxm,1:nym) = dum2d(:,:)
      u10(nx,:) = u10(nxm,:)
      u10(:,ny) = u10(:,nym)
      WRITE (*,6000) 'U10      ', u10(lprt_metx, lprt_mety), 'm/s'
    ELSE
      WRITE (6,9400) 'U10', rcode
      GOTO 1001
    ENDIF
    CALL get_var_2d_real_cdf (cdfid, 'V10', dum2d, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      v10(1:nxm,1:nym) = dum2d(:,:)
      v10(nx,:) = v10(nxm,:)
      v10(:,ny) = v10(:,nym)
      WRITE (*,6000) 'V10      ', v10(lprt_metx, lprt_mety), 'm/s'
    ELSE
      WRITE (6,9400) 'V10', rcode
      GOTO 1001
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'PSFC', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    psa(1:nxm,1:nym) = dum2d(:,:)
    psa(nx,:) = psa(nxm,:)
    psa(:,ny) = psa(:,nym)
    WRITE (*,6000) 'PSFC     ', psa(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (6,9400) 'PSFC', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MAPFAC_M', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    mapcrs(1:nxm,1:nym) = dum2d(:,:)
    mapcrs(nx,:) = mapcrs(nxm,:)
    mapcrs(:,ny) = mapcrs(:,nym)
    WRITE (*,6000) 'MAPFAC_M ', mapcrs(lprt_metx, lprt_mety), 'dimensionless'
  ELSE
    WRITE (6,9400) 'MAPFAC_M', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MAPFAC_U', dum2d_u, nx, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    mapu(:,1:nym) = dum2d_u(:,:)
    mapu(:,  ny ) = mapu(:,nym)
    WRITE (*,6000) 'MAPFAC_U ', mapu(lprt_metx, lprt_mety), 'dimensionless'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'MAPFAC_V', dum2d_v, nxm, ny, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    mapv(1:nxm,:) = dum2d_v(:,:)
    mapv(  nx, :) = mapv(nxm,:)
    WRITE (*,6000) 'MAPFAC_V ', mapv(lprt_metx, lprt_mety), 'dimensionless'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'HGT', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    terrain(1:nxm,1:nym) = dum2d(:,:)
    terrain(nx,:) = terrain(nxm,:)
    terrain(:,ny) = terrain(:,nym)
    WRITE (*,6000) 'HGT      ', terrain(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (6,9400) 'HGT', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'RAINC', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    raincon(1:nxm,1:nym) = MAX(0.0, (dum2d(:,:) - rcold(1:nxm,1:nym))/10.0 )
    raincon(nx,:) = raincon(nxm,:)
    raincon(:,ny) = raincon(:,nym)
    rcold(1:nxm,1:nym) = dum2d(:,:)
    WRITE (*,6000) 'RAINC    ', raincon(lprt_metx, lprt_mety), 'cm'
  ELSE
    WRITE (6,9400) 'RAINC', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'RAINNC', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    rainnon(1:nxm,1:nym) = MAX(0.0, (dum2d(:,:) - rnold(1:nxm,1:nym))/10.0 )
    rainnon(nx,:) = rainnon(nxm,:)
    rainnon(:,ny) = rainnon(:,nym)
    rnold(1:nxm,1:nym) = dum2d(:,:)
    WRITE (*,6000) 'RAINNC   ', rainnon(lprt_metx, lprt_mety), 'cm'
  ELSE
    WRITE (6,9400) 'RAINNC', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'SWDOWN', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    rgrnd(1:nxm,1:nym) = dum2d(:,:)
    rgrnd(nx,:) = rgrnd(nxm,:)
    rgrnd(:,ny) = rgrnd(:,nym)
    WRITE (*,6000) 'SWDOWN   ', rgrnd(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (6,9400) 'SWDOWN', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'GLW', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    glw(1:nxm,1:nym) = dum2d(:,:)
    glw(nx,:) = glw(nxm,:)
    glw(:,ny) = glw(:,nym)
    WRITE (*,6000) 'GLW      ', glw(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (6,9400) 'GLW', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLAT', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    latcrs(1:nxm,1:nym) = dum2d(:,:)
    latcrs(nx,:) = latcrs(nxm,:)
    latcrs(:,ny) = latcrs(:,nym)
    WRITE (*,6000) 'XLAT     ', latcrs(lprt_metx, lprt_mety), 'degrees'
  ELSE
    WRITE (6,9400) 'XLAT', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLAT_U', dum2d_u, nx, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    latu(:,1:nym) = dum2d_u(:,:)
    latu(:,  ny ) = latu(:,nym)
    WRITE (*,6000) 'XLAT_U   ', latu(lprt_metx, lprt_mety), 'degrees'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLAT_V', dum2d_v, nxm, ny, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    latv(1:nxm,:) = dum2d_v(:,:)
    latv(  nx, :) = latv(nxm,:)
    WRITE (*,6000) 'XLAT_V   ', latv(lprt_metx, lprt_mety), 'degrees'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLONG', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    loncrs(1:nxm,1:nym) = dum2d(:,:)
    loncrs(nx,:) = loncrs(nxm,:)
    loncrs(:,ny) = loncrs(:,nym)
    WRITE (*,6000) 'XLONG    ', loncrs(lprt_metx, lprt_mety), 'degrees'
  ELSE
    WRITE (6,9400) 'XLONG', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLONG_U', dum2d_u, nx, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    lonu(:,1:nym) = dum2d_u(:,:)
    lonu(:,  ny ) = lonu(:,nym)
    WRITE (*,6000) 'XLONG_U  ', lonu(lprt_metx, lprt_mety), 'degrees'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'XLONG_V', dum2d_v, nxm, ny, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    lonv(1:nxm,:) = dum2d_v(:,:)
    lonv(  nx, :) = lonv(nxm,:)
    WRITE (*,6000) 'XLONG_V  ', lonv(lprt_metx, lprt_mety), 'degrees'
  ELSE
    gotfaces = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'LU_INDEX', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    IF ( MAXVAL(dum2d) > nummetlu ) THEN
      WRITE (6,9500) met_lu_src, MAXVAL(dum2d)
      GOTO 1001
    ENDIF
    landuse(1:nxm,1:nym) = NINT(dum2d(:,:))
    landuse(nx,:) = landuse(nxm,:)
    landuse(:,ny) = landuse(:,nym)
    WRITE (*,6100) 'LU_INDEX ', landuse(lprt_metx, lprt_mety), 'category'
  ELSE
    WRITE (6,9400) 'LU_INDEX', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'HFX', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    hfx(1:nxm,1:nym) = dum2d(:,:)
    hfx(nx,:) = hfx(nxm,:)
    hfx(:,ny) = hfx(:,nym)
    WRITE (*,6000) 'HFX      ', hfx(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (6,9400) 'HFX', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'LH', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    qfx(1:nxm,1:nym) = dum2d(:,:)
    qfx(nx,:) = qfx(nxm,:)
    qfx(:,ny) = qfx(:,nym)
    WRITE (*,6000) 'LH       ', qfx(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (6,9400) 'LH', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'UST', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    ust(1:nxm,1:nym) = dum2d(:,:)
    ust(nx,:) = ust(nxm,:)
    ust(:,ny) = ust(:,nym)
    WRITE (*,6000) 'UST      ', ust(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (6,9400) 'UST', rcode
    GOTO 1001
  ENDIF

  IF ( ifmol ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'RMOL', dum2d, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      mol(1:nxm,1:nym) = 1.0 / dum2d(:,:)
      mol(nx,:) = mol(nxm,:)
      mol(:,ny) = mol(:,nym)
    ELSE
      WRITE (6,9400) 'RMOL', rcode
      GOTO 1001
    ENDIF
    IF ( met_urban_phys >= 1 ) THEN  ! UCM used; get MOL above urban canopy
      CALL get_var_2d_real_cdf (cdfid, 'XXXC_URB', dum2d, nxm, nym, it, rcode)
      IF ( rcode == nf_noerr ) THEN  ! blend urban M-O length with RMOL
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
          WRITE (6,9800) 'XXXC_URB (URBAN MOL)', met_lu_src(1:4)
          GOTO 1001
        ENDIF
      ELSE
!~~~    Just use RMOL to fill Monin-Obukhov length without extra urban field
      ENDIF
    ENDIF
    WRITE (*,6000) 'MOL      ', mol(lprt_metx, lprt_mety), 'm'
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'PBLH', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    zpbl(1:nxm,1:nym) = dum2d(:,:)
    zpbl(nx,:) = zpbl(nxm,:)
    zpbl(:,ny) = zpbl(:,nym)
    WRITE (*,6000) 'PBLH     ', zpbl(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (6,9400) 'PBLH', rcode
    GOTO 1001
  ENDIF

  IF ( ifresist ) THEN

    CALL get_var_2d_real_cdf (cdfid, 'RA', dum2d, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      ra(1:nxm,1:nym) = dum2d(:,:)
      ra(nx,:) = ra(nxm,:)
      ra(:,ny) = ra(:,nym)
      IF ( ABS(MAXVAL(ra)) < smallnum ) THEN
        ifresist = .FALSE.
      ENDIF
      WRITE (*,6000) 'RA       ', ra(lprt_metx, lprt_mety), 's m^-1'
    ELSE
      WRITE (6,9400) 'RA', rcode
      GOTO 1001
    ENDIF

    CALL get_var_2d_real_cdf (cdfid, 'RS', dum2d, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      rstom(1:nxm,1:nym) = dum2d(:,:)
      rstom(nx,:) = rstom(nxm,:)
      rstom(:,ny) = rstom(:,nym)
      IF ( ABS(MAXVAL(rstom)) < smallnum ) THEN
        ifresist = .FALSE.
      ENDIF
      WRITE (*,6000) 'RS       ', rstom(lprt_metx, lprt_mety), 's m^-1'
    ELSE
      WRITE (6,9400) 'RS', rcode
      GOTO 1001
    ENDIF

  ENDIF

  IF ( iflai ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'LAI', dum2d, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      lai(1:nxm,1:nym) = dum2d(:,:)
      lai(nx,:) = lai(nxm,:)
      lai(:,ny) = lai(:,nym)
      IF ( ABS(MAXVAL(lai)) < smallnum ) THEN
        IF ( met_soil_lsm == 2 ) THEN  ! NOAH LSM
          lai(:,:) = 4.0
        ENDIF
      ENDIF
      WRITE (*,6000) 'LAI      ', lai(lprt_metx, lprt_mety), 'area/area'
    ELSE
      WRITE (6,9400) 'LAI', rcode
      GOTO 1001
    ENDIF
  ENDIF

  IF ( ifwr ) THEN
    CALL get_var_2d_real_cdf (cdfid, 'CANWAT', dum2d, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      wr(1:nxm,1:nym) = dum2d(:,:)
      wr(nx,:) = wr(nxm,:)
      wr(:,ny) = wr(:,nym)
      WRITE (*,6000) 'CANWAT   ', wr(lprt_metx, lprt_mety), 'kg m^-2'
    ELSE
      WRITE (6,9400) 'CANWAT', rcode
      GOTO 1001
    ENDIF
  ENDIF

  IF ( ifveg ) THEN
    IF ( met_soil_lsm == 7 ) THEN  ! Pleim-Xiu land-surface model
      CALL get_var_2d_real_cdf (cdfid, 'VEGF_PX', dum2d, nxm, nym, it, rcode)
      IF ( rcode == nf_noerr ) THEN
        veg(1:nxm,1:nym) = dum2d(:,:)
        veg(nx,:) = veg(nxm,:)
        veg(:,ny) = veg(:,nym)
        WRITE (*,6000) 'VEGF_PX  ', veg(lprt_metx, lprt_mety), 'area/area'
      ELSE
        CALL get_var_2d_real_cdf (cdfid, 'VEGFRA', dum2d, nxm, nym, it, rcode)
        IF ( rcode == nf_noerr ) THEN
          veg(1:nxm,1:nym) = dum2d(:,:) * 0.01
          veg(nx,:) = veg(nxm,:)
          veg(:,ny) = veg(:,nym)
          WRITE (*,6000) 'VEGFRA   ', veg(lprt_metx, lprt_mety), 'fraction'
        ELSE
          WRITE (6,9400) 'VEGFRA', rcode
          GOTO 1001
        ENDIF
      ENDIF
    ELSE
      CALL get_var_2d_real_cdf (cdfid, 'VEGFRA', dum2d, nxm, nym, it, rcode)
      IF ( rcode == nf_noerr ) THEN
        veg(1:nxm,1:nym) = dum2d(:,:) * 0.01
        veg(nx,:) = veg(nxm,:)
        veg(:,ny) = veg(:,nym)
        WRITE (*,6000) 'VEGFRA   ', veg(lprt_metx, lprt_mety), 'fraction'
      ELSE
        WRITE (6,9400) 'VEGFRA', rcode
        GOTO 1001
      ENDIF
    ENDIF
  ENDIF

  IF ( ifsoil ) THEN

    CALL get_var_2d_int_cdf (cdfid, 'ISLTYP', dum2d_i, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      isltyp(1:nxm,1:nym) = dum2d_i(:,:)
      isltyp(nx,:) = isltyp(nxm,:)
      isltyp(:,ny) = isltyp(:,nym)
      WRITE (*,6100) 'ISLTYP   ', isltyp(lprt_metx, lprt_mety), 'category'
    ELSE
      WRITE (6,9400) 'ISLTYP', rcode
      GOTO 1001
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'SMOIS', dum3d_s, nxm, nym, ns, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      wg(1:nxm,1:nym) = dum3d_s(:,:,1)
      wg(nx,:) = wg(nxm,:)
      wg(:,ny) = wg(:,nym)
      WRITE (*,6000) 'SMOIS 1  ', wg(lprt_metx, lprt_mety), 'm^3 m^-3'
      w2(1:nxm,1:nym) = dum3d_s(:,:,2)
      w2(nx,:) = w2(nxm,:)
      w2(:,ny) = w2(:,nym)
      WRITE (*,6000) 'SMOIS 2  ', w2(lprt_metx, lprt_mety), 'm^3 m^-3'
    ELSE
      WRITE (6,9400) 'SMOIS', rcode
      GOTO 1001
    ENDIF

    CALL get_var_3d_real_cdf (cdfid, 'TSLB', dum3d_s, nxm, nym, ns, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      soilt1(1:nxm,1:nym) = dum3d_s(:,:,1)
      soilt1(nx,:) = soilt1(nxm,:)
      soilt1(:,ny) = soilt1(:,nym)
      WRITE (*,6000) 'TSLB 1   ', soilt1(lprt_metx, lprt_mety), 'K'
      soilt2(1:nxm,1:nym) = dum3d_s(:,:,2)
      soilt2(nx,:) = soilt2(nxm,:)
      soilt2(:,ny) = soilt2(:,nym)
      WRITE (*,6000) 'TSLB 2   ', soilt2(lprt_metx, lprt_mety), 'K'
    ELSE
      WRITE (6,9400) 'TSLB', rcode
      GOTO 1001
    ENDIF

  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'TSK', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    groundt(1:nxm,1:nym) = dum2d(:,:)
    groundt(nx,:) = groundt(nxm,:)
    groundt(:,ny) = groundt(:,nym)
    WRITE (*,6000) 'TSK      ', groundt(lprt_metx, lprt_mety), 'K'
  ELSE
    IF ( ifsoil ) THEN
      groundt(:,:) = soilt1(:,:)
      WRITE (*,6000) 'TSK      ', groundt(lprt_metx, lprt_mety), 'K'
    ELSE
      WRITE (6,9400) 'TSK', rcode
      GOTO 1001
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'ALBEDO', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    albedo(1:nxm,1:nym) = dum2d(:,:)
    albedo(nx,:) = albedo(nxm,:)
    albedo(:,ny) = albedo(:,nym)
    WRITE (*,6000) 'ALBEDO   ', albedo(lprt_metx, lprt_mety), 'fraction'
  ELSE
    WRITE (6,9400) 'ALBEDO', rcode
    GOTO 1001
  ENDIF

  IF ( first ) THEN
    IF ( iflufrc ) THEN
      IF ( ifluwrfout ) THEN  ! land use fractions in WRF history file
        CALL get_var_3d_real_cdf (cdfid, 'LANDUSEF', dum3d_l, nxm, nym,  &
                                  nummetlu, it, rcode)
        IF ( rcode == nf_noerr ) THEN
          lufrac(1:nxm,1:nym,:) = dum3d_l(:,:,:)
          lufrac(  nx,  :,   :) = lufrac(nxm,:,:)
          lufrac( :,     ny, :) = lufrac(:,nym,:)
          WRITE (*,ifmt2) 'LANDUSEF ', lufrac(lprt_metx,lprt_mety,:)
        ELSE
          WRITE (6,9400) 'LANDUSEF', rcode
          GOTO 1001
        ENDIF
      ELSE  ! land use fractions in GEOGRID file from WPS
        flg = file_ter
        rcode = nf_open (flg, nf_nowrite, cdfidg)
        IF ( rcode /= nf_noerr ) THEN
          WRITE (6,9900)
          GOTO 1001
        ENDIF
        CALL get_var_3d_real_cdf (cdfidg, 'LANDUSEF', dum3d_l, nxm, nym,  &
                                  nummetlu, 1, rcode)
        IF ( rcode == nf_noerr ) THEN
          lufrac(1:nxm,1:nym,:) = dum3d_l(:,:,:)
          lufrac(  nx,  :,   :) = lufrac(nxm,:,:)
          lufrac( :,     ny, :) = lufrac(:,nym,:)
          WRITE (*,ifmt2) 'LANDUSEF ', lufrac(lprt_metx,lprt_mety,:)
        ELSE
          WRITE (6,9400) 'LANDUSEF', rcode
          GOTO 1001
        ENDIF
        rcode = nf_close (cdfidg)
        IF ( rcode /= nf_noerr ) THEN
          WRITE (6,9950)
          GOTO 1001
        ENDIF
      ENDIF
    ENDIF
    IF ( met_urban_phys >= 1 ) THEN  ! urban canopy model used
      CALL get_var_2d_real_cdf (cdfid, 'FRC_URB', dum2d, nxm, nym, it, rcode)
      IF ( rcode == nf_noerr ) THEN
        frc_urb(1:nxm,1:nym) = dum2d(:,:)
        frc_urb(nx,:) = frc_urb(nxm,:)
        frc_urb(:,ny) = frc_urb(:,nym)
        WRITE (*,6000) 'FRC_URB  ', frc_urb(lprt_metx, lprt_mety), 'fraction'
      ELSE
        WRITE (6,9400) 'FRC_URB', rcode
        GOTO 1001
      ENDIF
    ENDIF
    IF ( lpv > 0 ) THEN
      CALL get_var_2d_real_cdf (cdfid, 'F', dum2d, nxm, nym, it, rcode)
      IF ( rcode == nf_noerr ) THEN
        coriolis(1:nxm,1:nym) = dum2d(:,:)
        coriolis(nx,:) = coriolis(nxm,:)
        coriolis(:,ny) = coriolis(:,nym)
        WRITE (*,6000) 'F        ', coriolis(lprt_metx, lprt_mety), 's-1'
      ELSE
        WRITE (6,9400) 'F      ', rcode
        GOTO 1001
      ENDIF
    ENDIF
  ENDIF

  IF ( ifznt ) THEN  ! expecting roughness length in file
    CALL get_var_2d_real_cdf (cdfid, 'ZNT', dum2d, nxm, nym, it, rcode)
    IF ( rcode == nf_noerr ) THEN
      znt(1:nxm,1:nym) = dum2d(:,:)
      znt(nx,:) = znt(nxm,:)
      znt(:,ny) = znt(:,nym)
      WRITE (*,6000) 'ZNT      ', znt(lprt_metx, lprt_mety),    'm'
      gotznt = .TRUE.
    ELSE
      WRITE (6,9400) 'ZNT    ', rcode
      GOTO 1001
    ENDIF
  ELSE
    gotznt = .FALSE.
  ENDIF

  CALL get_var_2d_real_cdf (cdfid, 'SNOWC', dum2d, nxm, nym, it, rcode)
  IF ( rcode == nf_noerr ) THEN
    snowcovr(1:nxm,1:nym) = dum2d(:,:)
    snowcovr(nx,:) = snowcovr(nxm,:)
    snowcovr(:,ny) = snowcovr(:,nym)
    WRITE (*,6000) 'SNOWC    ', snowcovr(lprt_metx, lprt_mety), 'category'
  ELSE
    WRITE (6,9400) 'SNOWC', rcode
    GOTO 1001
  ENDIF

  CALL get_var_1d_real_cdf (cdfid, 'ZNU', sigmah, nz,  it, rcode)
  IF ( rcode /= nf_noerr ) THEN
    WRITE (6,9400) 'ZNU', rcode
    GOTO 1001
  ENDIF

  CALL get_var_1d_real_cdf (cdfid, 'ZNW', sigmaf, nzp, it, rcode)
  IF ( rcode /= nf_noerr ) THEN
    WRITE (6,9400) 'ZNW', rcode
    GOTO 1001
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

        xoff = 0.0  ! dot-point grid: no offset from dot-point center value
        yoff = 0.0  ! dot-point grid: no offset from dot-point center value

        DO j = 1, ny
          DO i = 1, nx

            xxin = met_xxctr -  &
                   ( met_rictr_dot - (FLOAT(i) + xoff) ) * met_resoln

            yyin = met_yyctr -  &
                   ( met_rjctr_dot - (FLOAT(j) + yoff) ) * met_resoln

            CALL xy2ll_ps (xxin, yyin, met_tru1, met_proj_clon,  &
                           latdot(i,j), londot(i,j))

            mapdot(i,j) = mapfac_ps (latdot(i,j), met_tru1)

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

              CALL xy2ll_ps (xxin, yyin, met_tru1, met_proj_clon,  &
                             latu(i,j), lonu(i,j))

              mapu(i,j) = mapfac_ps (latu(i,j), met_tru1)

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

              CALL xy2ll_ps (xxin, yyin, met_tru1, met_proj_clon,  &
                             latv(i,j), lonv(i,j))

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

    startseas = met_startdate(1:4) // "-04-15-00:00:00.0000"
    endseas   = met_startdate(1:4) // "-10-15-00:00:00.0000"

    CALL geth_idts (met_startdate, startseas,     idts_start)
    CALL geth_idts (endseas,       met_startdate, idts_end)

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

      DO j = 1, nym
        DO i = 1, nxm
          IF ( ( met_lu_src(1:4) == "USGS" ) .AND.  &
               ( met_lu_water == 16 ) ) THEN
            znt(i,j) = sfz0usgs(landuse(i,j),met_season) * 0.01  ! cm --> m
          ELSE IF ( ( met_lu_src(1:3) == "OLD" ) .AND.  &
                    ( met_lu_water == 7 ) ) THEN
            znt(i,j) = sfz0old(landuse(i,j),met_season)  * 0.01  ! cm --> m
          ELSE IF ( met_lu_src(1:3) == "NLC" ) THEN
            znt(i,j) = sfz0nlc(landuse(i,j),met_season)  * 0.01  ! cm --> m
          ELSE IF ( met_lu_src(1:3) == "SIB" ) THEN
            znt(i,j) = sfz0sib(landuse(i,j),met_season)  * 0.01  ! cm --> m
          ELSE IF ( met_lu_src(1:3) == "MOD" ) THEN
            znt(i,j) = sfz0mod(landuse(i,j),met_season)  * 0.01  ! cm --> m
          ELSE
            WRITE (6,9700) met_lu_src, met_lu_water
            GOTO 1001
          ENDIF
        ENDDO
      ENDDO

      znt(:,ny) = znt(:,nym)
      znt(nx,:) = znt(nxm,:)

      IF ( met_urban_phys < 1 ) THEN  ! if UCM, write after urban update
        WRITE (*,6000) 'ZNT      ', znt   (lprt_metx, lprt_mety), 'm'
      ENDIF

    ENDIF

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (1x, a, 1x, f12.4, 2x, a)
 6100 FORMAT (1x, a, 1x, i12,   2x, a)

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
 
  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   ERROR RETRIEVING TIMES FROM WRF FILE',     &
              /, 1X, '***   RCODE = ', i3,                             &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a,        &
              /, 1x, '***   NO MORE INPUT WRF FILES',                  &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a,        &
              /, 1x, '***   INPUT FILE NUMBER ', i3, ' IS BLANK',      &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a,        &
              /, 1x, '***   COULD NOT FIND FILE ', a,                  &
              /, 1x, '***   FILE MAY NOT EXIST',                       &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE',  &
              /, 1x, '***   VARIABLE = ', a,                           &
              /, 1x, '***   RCODE = ', i3,                             &
              /, 1x, 70('*'))

 9500 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   UNKNOWN LAND USE CLASSIFICATION SYSTEM',   &
              /, 1x, '***   LAND USE SOURCE = ', a,                    &
              /, 1x, '***   HIGHEST INDEX FOUND = ', i4,               &
              /, 1x, 70('*'))

 9700 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   UNABLE TO SET ZNT FROM LOOKUP TABLE',      &
              /, 1x, '***   LAND USE SOURCE = ', a,                    &
              /, 1x, '***   NUMBER OF LAND USE CATEGORIES = ', i3,     &
              /, 1x, 70('*'))

 9800 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   UNABLE TO BLEND ', a, ' FOR UCM',          &
              /, 1x, '***   UNKNOWN LAND USE SOURCE',                  &
              /, 1x, '***   LAND USE SOURCE = ', a,                    &
              /, 1x, 70('*'))

 9900 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_WRFEM',                        &
              /, 1x, '***   ERROR OPENING WRF NETCDF FILE',                &
              /, 1x, 70('*'))

 9950 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_WRFEM',                        &
              /, 1x, '***   ERROR CLOSING WRF NETCDF FILE',                &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE rdwrfem
