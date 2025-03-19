SUBROUTINE aqprep (grid, config_flags, t_phy_wrf, p_phy_wrf, rho_wrf,     &
                   z_at_w_wrf, dz8w_wrf, p8w_wrf, t8w_wrf,  &
                   numlu, release_version,                  &
                   wrf_cmaq_option, wrf_cmaq_freq,          &
                   ids, ide, jds, jde, kds, kde,            &
                   ims, ime, jms, jme, kms, kme,            &
                   its, ite, jts, jte, kts, kte,            &
                   qv_curr_wrf,                             &  ! optional
                   qc_curr_wrf,                             &  ! optional
                   qr_curr_wrf,                             &  ! optional
                   qi_curr_wrf,                             &  ! optional
                   qs_curr_wrf,                             &  ! optional
                   qg_curr_wrf                         )  ! optional

!===============================================================================
! Name:     AQ Prep
! Purpose:  Prepare meteorological fields for air quality simulation, including
!           meteorologically dependent emissions and chemical transport
!           modeling.
! Notes:    Some algorithms taken from the Community Multiscale Air Quality
!           (CMAQ) Modeling System's Meteorology-Chemistry Interface Processor.
! Revised:  06 Apr 2007  Original version.  (T. Otte)
!           10 Apr 2007  (David Wong)
!              -- define header_constant_data_record in header_data_module to
!                 capture header constant info for met. and grid buffered files.
!              -- redefine ioffset and joffset which are the distance, in number
!                 of grid cells, between the wrf and cmaq domain at the lower
!                 left corner. Note: ioffset >= 5 and joffset >= 5
!           11 Aug 2011  (David Wong)
!              -- updated to comply with CMAQ 5.0
!           10 Jun 2013  (David Wong)
!              -- updated to NLCD40
!           24 Sep 2013  (David Wong)
!              -- consolidated x- and y-cent calculation
!           17 Jan 2014  (David Wong)
!              -- refomulated the xorig and yorig calculation regardless of odd 
!                 or even number of grid cells
!           10 Mar 2014  (David Wong)
!              -- fixed bug in the refomulated the xorig and yorig calculation
!           14 May 2014  (David Wong)
!              -- made a distinction between USGS 24 and USGS 33
!           21 Jul 2014  (David Wong)
!              -- add new LU type: MODIFIED_IGBP_MODIS_NOAH and made a distinction 
!                 between NLCD, NLCD50 and NLCD40
!           25 Sep 2015  (David Wong
!              -- replaced SUBST_MODULES with SE_MOdULES
!           14 Dec 2015  (David Wong)
!              -- added assignment mminlu
!              -- updated how ioapi_header%vglvs was set w.r.t. znw's dimension 
!                 change
!           28 Dec 2015  (David Wong)
!              -- added optional PV calculation which is dictated by an environment
!                 variable CTM_PVO3 with default .false. value
!           11 Jan 2016  (David Wong)
!              -- removed mminlu
!              -- resized the first dimension of the following arrays:
!                 wrf_cmaq_c_send_to,
!                 wrf_cmaq_c_recv_from,
!                 wrf_cmaq_c_send_index_g,
!                 wrf_cmaq_c_send_index_l,
!                 wrf_cmaq_c_recv_index_g,
!                 wrf_cmaq_c_recv_index_l,
!                 wrf_cmaq_d_send_to,
!                 wrf_cmaq_d_recv_from,
!                 wrf_cmaq_d_send_index_g,
!                 wrf_cmaq_d_send_index_l,
!                 wrf_cmaq_d_recv_index_g,
!                 wrf_cmaq_d_recv_index_l,
!                 wrf_cmaq_ce_send_to,
!                 wrf_cmaq_ce_recv_from,
!                 wrf_cmaq_ce_send_index_g,
!                 wrf_cmaq_ce_send_index_l,
!                 wrf_cmaq_ce_recv_index_g,
!                 wrf_cmaq_ce_recv_index_l,
!                 wrf_cmaq_de_send_to,
!                 wrf_cmaq_de_recv_from,
!                 wrf_cmaq_de_send_index_g,
!                 wrf_cmaq_de_send_index_l,
!                 wrf_cmaq_de_recv_index_g,
!                 wrf_cmaq_de_recv_index_l
!           26 Feb 2016  (David Wong)
!              -- transformed the call pio_re_init to pio_init as routines
!                 pio_re_init and pio_init have been merged into one pio_init
!           07 Apr 2016  (David Wong)
!              -- reversed the decision of removing mminlu
!              -- removed obsolete NLCD50
!              -- set lwater = 17 and lice = 15 for NLCD40
!           05 May 2016  (David Wong)
!              -- Calculated and output the rainfall (convective and
!                 non-convect) information according to the output file time step
!                 rather than the two-way model time step
!           30 Aug 2016  (David Wong)
!              -- fixed a bug in outputing MET_CRO_2D physical file
!           11 Jan 2017  (David Wong)
!              -- fixed a bug to handle simulation with convective scheme or not
!           11 Jan 2018  (David Wong)
!              -- Added convective_scheme to set rainc accordingly
!           31 Jan 2019  (David Wong)
!              -- adopted the idea to process all twoway related environment
!                 variables in one place
!           04 Feb 2019  (Tanya Spero)
!              -- updated Jacobian calculation for hybrid vertical coordinate
!           04 Mar 2019  (Rob G. David Wong)
!              -- logic for WRF version, hybrid coord, PX variables
!              -- updated to work with PX LSM changed in WRFV4.1 that has
!                 additional soil texture info and lai name change to lai_px
!           01 Aug 2019  (David Wong)
!              -- made nprocs available for CMAQ
!              -- made two new variables, UWIND and VWIND as the wind component
!                 on the mass point
!           26 Jul 2022  (David Wong)
!              -- Added a prefix tw_ for these variables: sc, ec, sr, er sc_d, ec_d,
!                 sr_d, and er_d to avoid naming conflicts
!           16 Mar 2023  (David Wong)
!              -- fixed a bug in creating u and v components
!           30 Apr 2024  (Tanya Spero)
!              -- Changed constraint on XORIG and YORIG for Lambert conformal
!                 projections. Original constraint of 500 meters introduced an
!                 error in calculating the lower-left corner that is more
!                 noticeable at fine resolutions. Now using a constraint of
!                 5 meters to allow for "neater" XORIG and YORIG values across
!                 compilers.
!===============================================================================

  USE module_domain                                ! WRF module
  USE module_model_constants                       ! WRF module
  USE module_configure                             ! WRF module
  USE module_gfs_physcons, only : con_rerth        ! WRF module
  USE module_state_description, only : PXLSMSCHEME ! WRF module

  USE twoway_util_module
  USE twoway_header_data_module
  USE twoway_met_param_module
  USE twoway_data_module
  USE HGRD_DEFN
  USE SE_MODULES

  use se_comm_info_ext
  use utilio_defn

  IMPLICIT NONE

  INCLUDE SUBST_CONST
  INCLUDE SUBST_MPI

  TYPE(domain), INTENT(IN)                :: grid
  TYPE (grid_config_rec_type), INTENT(IN) :: config_flags

  REAL,    INTENT(IN)           :: t_phy_wrf    ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN)           :: p_phy_wrf    ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN)           :: rho_wrf      ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN)           :: z_at_w_wrf   ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN)           :: dz8w_wrf     ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN)           :: p8w_wrf      ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN)           :: t8w_wrf      ( ims:ime, kms:kme, jms:jme )

  REAL,    INTENT(IN), OPTIONAL :: qv_curr_wrf  ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN), OPTIONAL :: qc_curr_wrf  ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN), OPTIONAL :: qr_curr_wrf  ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN), OPTIONAL :: qi_curr_wrf  ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN), OPTIONAL :: qs_curr_wrf  ( ims:ime, kms:kme, jms:jme )
  REAL,    INTENT(IN), OPTIONAL :: qg_curr_wrf  ( ims:ime, kms:kme, jms:jme )

  INTEGER, INTENT(IN)           :: numlu
  CHARACTER(LEN=*), INTENT(IN)  :: release_version

  INTEGER, INTENT(IN)           :: wrf_cmaq_option     ! WRF-CMAQ coupled model option
                                                       ! 0 = only run WRF
                                                       ! 1 = run WRF-CMAQ coupled model to produce
                                                       !     GRID and MET files only
                                                       ! 2 = run WRF-CMAQ coupled model w/o producing
                                                       !     GRID and MET files
                                                       ! 3 = run WRF-CMAQ coupled model w producing
                                                       !     GRID and MET files
  INTEGER, INTENT(IN)           :: wrf_cmaq_freq

  INTEGER, INTENT(IN)           :: ids, ide, jds, jde, kds, kde
  INTEGER, INTENT(IN)           :: ims, ime, jms, jme, kms, kme
  INTEGER, INTENT(IN)           :: its, ite, jts, jte, kts, kte

  LOGICAL, PARAMETER            :: def_false = .false.
  LOGICAL, SAVE                 :: first = .TRUE.

  INTEGER, SAVE :: nlays, nvars
  INTEGER, SAVE :: tstep = 0

  INTEGER                       :: ii, jj, kk, ll, iim1, jjm1, v
  INTEGER                       :: c, r, lcm1, lrm1, kp1
  INTEGER                       :: ioffset, joffset
  INTEGER                       :: stat, temp
  REAL,    PARAMETER            :: gravi = 1 / g
  REAL                          :: tf, qf, densf

  INTEGER :: loc_wrf_c_domain_map(3, 2)

  CHARACTER( 2 ) :: COLROW = 'CR'  ! col/row arg list order for pio_init

  CHARACTER (LEN = 16), PARAMETER :: pname = 'aq_prep         '

  CHARACTER (LEN = 16) :: fname, pfname

! Calc for PV

  REAL,    SAVE, ALLOCATABLE  :: xuu_s      ( : , : , : )
  REAL,    SAVE, ALLOCATABLE  :: xvv_t      ( : , : , : )
  REAL,    SAVE, ALLOCATABLE  :: xuu_d      ( : , : , : )
  REAL,    SAVE, ALLOCATABLE  :: xvv_d      ( : , : , : )
  REAL,    SAVE, ALLOCATABLE  :: xtheta     ( : , : , : )
  REAL,    SAVE, ALLOCATABLE  :: xmapc      ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: xmapc2     ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: xcorl      ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: dtds       ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: dtdx       ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: dtdy       ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: duds       ( : , : )
  REAL,    SAVE, ALLOCATABLE  :: dvds       ( : , : )
  REAL                        :: f0
  REAL                        :: f1
  REAL                        :: f2
  INTEGER                     :: k
  INTEGER                     :: k0
  INTEGER                     :: k1
  INTEGER                     :: k2
  REAL,    SAVE, ALLOCATABLE  :: sigma      ( : )
  REAL                        :: t00
  REAL                        :: t1
  REAL                        :: t2
  REAL                        :: t3
  INTEGER                     :: rp1
  INTEGER                     :: cp1
  REAL                        :: vor
  REAL                        :: dsx
  REAL                        :: dsy
  REAL                        :: dx
  REAL                        :: dy

! metcro3d temporary storage

  REAL, ALLOCATABLE, SAVE :: densq   ( : , : , : )
  REAL, ALLOCATABLE, SAVE :: zf      ( : , : , : )
  REAL, ALLOCATABLE, SAVE :: dzf     ( : , : , : )
  REAL, ALLOCATABLE, SAVE :: presf   ( : , : , : )
  REAL                    :: muhybf                 ! for hybrid vertical coord
  REAL                    :: muhybh                 ! for hybrid vertical coord

! metdot3d temporary storage

  REAL, ALLOCATABLE, SAVE :: jdenm   ( : , : )

! metcro2d temporary storage

  REAL, ALLOCATABLE, SAVE :: u10     ( : , : )
  REAL, ALLOCATABLE, SAVE :: v10     ( : , : )
  REAL, ALLOCATABLE, SAVE :: albedo  ( : , : )

  real, allocatable, save :: gridcro2d_data_wrf (:,:,:)
  real, allocatable, save :: griddot2d_data_wrf (:,:)
  real, allocatable, save :: metcro3d_data_wrf (:,:,:,:)
  real, allocatable, save :: metdot3d_data_wrf (:,:,:,:)
  real, allocatable, save :: metcro2d_data_wrf (:,:,:)

  real, allocatable       :: gridcro2d_data_cmaq (:,:,:)
  real, allocatable       :: griddot2d_data_cmaq (:,:)
  real, allocatable, save :: metcro3d_data_cmaq (:,:,:,:)
  real, allocatable, save :: metdot3d_data_cmaq (:,:,:,:)
  real, allocatable, save :: metcro2d_data_cmaq (:,:,:)
! real, allocatable, save :: previous_rain_rec(:,:,:)
  real, allocatable, save :: temp_rainnc(:,:)
  real, allocatable, save :: temp_rainc(:,:)

  integer :: east_adjustment, north_adjustment

    integer, save :: jdate, jtime, sdate, stime, loc_logdev, nstep
    integer       :: wrf_halo_x_l, wrf_halo_x_r
    integer       :: wrf_halo_y_l, wrf_halo_y_u

    logical, save :: write_to_physical_file,                                 &
                     north_bdy_pe, south_bdy_pe, east_bdy_pe, west_bdy_pe
    integer, save :: file_time_step_in_sec

    integer :: i, j, status(MPI_STATUS_SIZE)
    character (len = 50) :: myfmt

!   character (len = 4), save :: pe_str

    logical, parameter :: debug = .true.

    integer, save :: cmaq_tstep

    TYPE(WRFU_Time) :: current_wrf_time
    integer :: rc
    character (len = 2), save :: data_ori = 'cr'

    integer, save :: tsc_c, tec_c, tsr_c, ter_c,     &
                     tsc_d, tec_d, tsr_d, ter_d,     &
                     tsc_e, tec_e, tsr_e, ter_e

    integer, save :: lwater, lice
    real, allocatable :: land_use_index(:,:)

    character(len=10) :: wrf_version
    logical   :: hybrid_vert, px_modis
    real      :: wrfv

    logical, save :: file_opened = .false.

    interface
      SUBROUTINE bcldprc_ak (wrf_ncols, wrf_nrows, nlays,                &
                             zf, ta, pres, qv, pbl, dzf, presf,  &
                             cfrac, cldb, cldt, wbar)
        INTEGER,       INTENT(IN)    :: wrf_ncols
        INTEGER,       INTENT(IN)    :: wrf_nrows
        INTEGER,       INTENT(IN)    :: nlays
        REAL,          INTENT(IN)    :: zf         ( : , : , : )
        REAL,          INTENT(IN)    :: ta         ( : , : , : )
        REAL,          INTENT(IN)    :: pres       ( : , : , : )
        REAL,          INTENT(IN)    :: qv         ( : , : , : )
        REAL,          INTENT(IN)    :: pbl        ( : , : )
        REAL,          INTENT(IN)    :: dzf        ( : , : , : )
        REAL,          INTENT(IN)    :: presf      ( : , : , : )
        REAL,          INTENT(OUT)   :: cfrac      ( : , : )
        REAL,          INTENT(OUT)   :: cldb       ( : , : )
        REAL,          INTENT(OUT)   :: cldt       ( : , : )
        REAL,          INTENT(OUT)   :: wbar       ( : , : )
      END SUBROUTINE bcldprc_ak
    end interface

!-------------------------------------------------------------------------------
! Set switches/logic that rely on WRF versions and/or namelist settings
  hybrid_vert = .false.
  px_modis    = .false.
  wrf_version = TRIM(release_version)
  read (wrf_version(2:4),'(F3.1)') wrfv

  ! Check version for hybrid coord configuration option
  IF(wrfv >= 3.9) THEN
    if (config_flags%hybrid_opt == 2) then
      hybrid_vert = .true.
    end if
  END IF

  ! Check version for WRFV4.1 PX MODIS and SOIL implementation
  IF(wrfv >= 4.1) THEN
    if (config_flags%sf_surface_physics == 7) then
      px_modis = .true.
    end if
  END IF

  print *, 'WRF Version ', wrf_version
  print *, 'WRF Version ', wrfv
  print *, 'Hybrid option number ', config_flags%hybrid_opt
  print *, 'Hybrid vertical coordinate (T/F) ', hybrid_vert
  print *, 'PX MODIS (T/F)', px_modis

  if (config_flags%cu_physics == 0) then
     convective_scheme = .false.
  else
     convective_scheme = .true.
  end if
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Define horizontal bounds for CMAQ processing.
!
! Note:  May want to have a set of four input variables that define the
!        AQ window:  IOFFSET, JOFFSET, NCOLS, NROWS.  Define SC, EC, SR, and ER
!        from those variables and the horizontal dimensions of the WRF domain.
!
! Note:  Not sure how lateral boundary cells (formerly found in METBDY3D) are
!        handled in indexing and in two-way system yet.
!-------------------------------------------------------------------------------

! call WRFU_ClockGet (grid%domain_clock, CurrTime=current_wrf_time, rc=rc )

  tstep = tstep + 1

  IF ( first ) THEN

     CALL TWOWAY_INIT_ENV_VARS

     call mpi_comm_rank (mpi_comm_world, twoway_mype, stat)

     wrf_halo_x_l = abs(its - ims)
     wrf_halo_x_r = abs(ite - ime)
     wrf_halo_y_l = abs(jts - jms)
     wrf_halo_y_u = abs(jte - jme)

     nprocs = grid%nproc_x * grid%nproc_y
     twoway_nprocs = nprocs

     north_adjustment = 0
     if (twoway_mype >= (twoway_nprocs - grid%nproc_x)) then
        north_bdy_pe = .true.
        north_adjustment = -1
     else
        north_bdy_pe = .false.
     end if

     if (twoway_mype < grid%nproc_x) then
        south_bdy_pe = .true.
     else
        south_bdy_pe = .false.
     end if

     east_adjustment = 0
     if (mod(twoway_mype, grid%nproc_x) == (grid%nproc_x - 1)) then
        east_bdy_pe = .true.
        east_adjustment = -1
     else
        east_bdy_pe = .false.
     end if

     if (mod(twoway_mype, grid%nproc_x) == 0) then
        west_bdy_pe = .true.
     else
        west_bdy_pe = .false.
     end if

     allocate (wrf_c_domain_map(3, 2, 0:twoway_nprocs-1), cmaq_c_domain_map(3, 2, 0:twoway_nprocs-1),           &
               wrf_d_domain_map(3, 2, 0:twoway_nprocs-1), cmaq_d_domain_map(3, 2, 0:twoway_nprocs-1),           &
                                                   cmaq_ce_domain_map(3, 2, 0:twoway_nprocs-1),          &
                                                   cmaq_de_domain_map(3, 2, 0:twoway_nprocs-1), stat=stat)
     if (stat .ne. 0) then
        print *, ' Error: Allocating domain_maps'
        stop
     end if

     loc_wrf_c_domain_map(1, 1) = its
     loc_wrf_c_domain_map(2, 1) = ite + east_adjustment
     loc_wrf_c_domain_map(3, 1) = ite - its + 1
     loc_wrf_c_domain_map(1, 2) = jts
     loc_wrf_c_domain_map(2, 2) = jte + north_adjustment
     loc_wrf_c_domain_map(3, 2) = jte - jts + 1

     call mpi_allgather (loc_wrf_c_domain_map, 6, mpi_integer, wrf_c_domain_map, 6, &
                         mpi_integer, mpi_comm_world, stat)

     tw_sc = ims + wrf_halo_x_l
     tw_ec = ime - wrf_halo_x_r + east_adjustment
     tw_sr = jms + wrf_halo_y_l
     tw_er = jme - wrf_halo_y_u + north_adjustment

     tw_sc_d = tw_sc
     tw_ec_d = tw_ec + 1
     tw_sr_d = tw_sr
     tw_er_d = tw_er + 1

     wrf_c_ncols = ime - ims + 1 - wrf_halo_x_l - wrf_halo_x_r + east_adjustment
     wrf_c_nrows = jme - jms + 1 - wrf_halo_y_l - wrf_halo_y_u + north_adjustment
     wrf_d_ncols = wrf_c_ncols + 1
     wrf_d_nrows = wrf_c_nrows + 1
     nlays = kme - 1             ! wrf is using layer, znw contains level values (D. Wong 5/22/07)

     wrf_c_col_dim = ide - ids + 1
     wrf_c_row_dim = jde - jds + 1

     cmaq_c_col_dim = envint ('CMAQ_COL_DIM', ' ', wrf_c_col_dim-10, stat)
     cmaq_c_row_dim = envint ('CMAQ_ROW_DIM', ' ', wrf_c_row_dim-10, stat)

     loc_logdev = init3 ()

     stime = cmaq_stime
     sdate = cmaq_sdate

     cmaq_tstep = sec2time(grid%time_step*wrf_cmaq_freq)

     jdate = sdate
     jtime = stime

     nstep = ((grid%run_days * 24 + grid%run_hours) * 3600 + grid%run_minutes * 60 + grid%run_seconds) / &
             (grid%time_step * wrf_cmaq_freq)

!-------------------------------------------------------------------------------
! Allocate arrays for CCTM...to mimic MCIP output arrays.
!-------------------------------------------------------------------------------

! Fields from METCRO3D

     ALLOCATE ( densq   (wrf_c_ncols,   wrf_c_nrows,   nlays) )  ! new output variable
     ALLOCATE ( zf      (wrf_c_ncols,   wrf_c_nrows,   0:nlays) )

     ALLOCATE ( dzf     (wrf_c_ncols,   wrf_c_nrows,   nlays) )  ! used in calcs, not output
     ALLOCATE ( presf   (wrf_c_ncols,   wrf_c_nrows,   nlays) )  ! used in calcs, not output

! Fields from METDOT3D

     ALLOCATE ( jdenm   (wrf_d_ncols, wrf_d_nrows)        )

! Fields from METCRO2D.

     ALLOCATE ( u10     (wrf_c_ncols,   wrf_c_nrows)          )
     ALLOCATE ( v10     (wrf_c_ncols,   wrf_c_nrows)          )
     ALLOCATE ( albedo  (wrf_c_ncols,   wrf_c_nrows)          )

!-------------------------------------------------------------------------------
! Fill M3IO header variables.
!
! Note:  The M3IO header variables are currently local to AQ_HEADER, but they
!        should fill variables that are available in COORD.EXT (or whatever
!        replaced it).
!-------------------------------------------------------------------------------

     npcol = grid%nproc_x
     nprow = grid%nproc_y

     se_twoway_npcol = npcol
     se_twoway_nprow = nprow

     twoway_nprocs = npcol * nprow

     wrf_d_domain_map(1,:,:) = wrf_c_domain_map(1,:,:)
     wrf_d_domain_map(2,:,:) = wrf_c_domain_map(2,:,:) + 1
     wrf_d_domain_map(3,:,:) = wrf_c_domain_map(3,:,:) + 1

! cmaq cross point domain
     call compute_decomp (cmaq_c_col_dim, npcol, nprow, 'cmaq', 'c', cmaq_c_domain_map(:,1,:), delta_x)
     call compute_decomp (cmaq_c_row_dim, nprow, npcol, 'cmaq', 'r', cmaq_c_domain_map(:,2,:), delta_y)

! cmaq dot point domain
     cmaq_d_domain_map(1,:,:) = cmaq_c_domain_map(1,:,:)
     cmaq_d_domain_map(2,:,:) = cmaq_c_domain_map(2,:,:) + 1
     cmaq_d_domain_map(3,:,:) = cmaq_c_domain_map(3,:,:) + 1

! cmaq cross point extended domain
     cmaq_ce_domain_map(1,:,:) = cmaq_c_domain_map(1,:,:) - 1
     cmaq_ce_domain_map(2,:,:) = cmaq_c_domain_map(2,:,:) + 1
     cmaq_ce_domain_map(3,:,:) = cmaq_c_domain_map(3,:,:) + 2

! cmaq dot point extended domain
     cmaq_de_domain_map(1,:,:) = cmaq_d_domain_map(1,:,:) - 1
     cmaq_de_domain_map(2,:,:) = cmaq_d_domain_map(2,:,:) + 1
     cmaq_de_domain_map(3,:,:) = cmaq_d_domain_map(3,:,:) + 2

     cmaq_c_ncols = cmaq_c_domain_map(3, 1, twoway_mype)
     cmaq_c_nrows = cmaq_c_domain_map(3, 2, twoway_mype)
     cmaq_d_ncols = cmaq_d_domain_map(3, 1, twoway_mype)
     cmaq_d_nrows = cmaq_d_domain_map(3, 2, twoway_mype)

! the reason for twoway_nprocs*3 is in the worst scenario, the entire cmaq domain is inside one wrf processor domain
     allocate (wrf_cmaq_c_send_to(0:9, 0:twoway_nprocs-1),               &
               wrf_cmaq_c_recv_from(0:9, 0:twoway_nprocs-1),             &
               wrf_cmaq_c_send_index_g(9*3, 2, 0:twoway_nprocs-1),       &    ! starting and ending dimension, dimenionality
               wrf_cmaq_c_send_index_l(9*3, 2, 0:twoway_nprocs-1),       &    ! starting and ending dimension, dimenionality
               wrf_cmaq_c_recv_index_g(9*3, 2, 0:twoway_nprocs-1),       &    ! starting and ending dimension, dimenionality
               wrf_cmaq_c_recv_index_l(9*3, 2, 0:twoway_nprocs-1),       &    ! starting and ending dimension, dimenionality
               wrf_cmaq_d_send_to(0:9, 0:twoway_nprocs-1),               &
               wrf_cmaq_d_recv_from(0:9, 0:twoway_nprocs-1),             &
               wrf_cmaq_d_send_index_g(9*3, 2, 0:twoway_nprocs-1),       &    ! starting and ending dimension, dimenionality
               wrf_cmaq_d_send_index_l(9*3, 2, 0:twoway_nprocs-1),       &    ! starting and ending dimension, dimenionality
               wrf_cmaq_d_recv_index_g(9*3, 2, 0:twoway_nprocs-1),       &    ! starting and ending dimension, dimenionality
               wrf_cmaq_d_recv_index_l(9*3, 2, 0:twoway_nprocs-1),       &    ! starting and ending dimension, dimenionality
               wrf_cmaq_ce_send_to(0:9, 0:twoway_nprocs-1),              &
               wrf_cmaq_ce_recv_from(0:9, 0:twoway_nprocs-1),            &
               wrf_cmaq_ce_send_index_g(9*3, 2, 0:twoway_nprocs-1),      &    ! starting and ending dimension, dimenionality
               wrf_cmaq_ce_send_index_l(9*3, 2, 0:twoway_nprocs-1),      &    ! starting and ending dimension, dimenionality
               wrf_cmaq_ce_recv_index_g(9*3, 2, 0:twoway_nprocs-1),      &    ! starting and ending dimension, dimenionality
               wrf_cmaq_ce_recv_index_l(9*3, 2, 0:twoway_nprocs-1),      &    ! starting and ending dimension, dimenionality
               wrf_cmaq_de_send_to(0:9, 0:twoway_nprocs-1),              &
               wrf_cmaq_de_recv_from(0:9, 0:twoway_nprocs-1),            &
               wrf_cmaq_de_send_index_g(9*3, 2, 0:twoway_nprocs-1),      &    ! starting and ending dimension, dimenionality
               wrf_cmaq_de_send_index_l(9*3, 2, 0:twoway_nprocs-1),      &    ! starting and ending dimension, dimenionality
               wrf_cmaq_de_recv_index_g(9*3, 2, 0:twoway_nprocs-1),      &    ! starting and ending dimension, dimenionality
               wrf_cmaq_de_recv_index_l(9*3, 2, 0:twoway_nprocs-1),      &    ! starting and ending dimension, dimenionality
               stat=stat) 
     if (stat .ne. 0) then
        print *, ' Error: Allocating communication indices arrays'
        stop
     end if

     call compute_comm_indices (twoway_nprocs, wrf_c_domain_map, cmaq_c_domain_map,        &
                                wrf_cmaq_c_send_to, wrf_cmaq_c_recv_from,                  &
                                wrf_cmaq_c_send_index_g, wrf_cmaq_c_send_index_l,          &
                                wrf_cmaq_c_recv_index_g, wrf_cmaq_c_recv_index_l   )

     call compute_comm_indices (twoway_nprocs, wrf_d_domain_map, cmaq_d_domain_map,        &
                                wrf_cmaq_d_send_to, wrf_cmaq_d_recv_from,                  &
                                wrf_cmaq_d_send_index_g, wrf_cmaq_d_send_index_l,          &
                                wrf_cmaq_d_recv_index_g, wrf_cmaq_d_recv_index_l   )

     call compute_comm_indices (twoway_nprocs, wrf_c_domain_map, cmaq_ce_domain_map,       &
                                wrf_cmaq_ce_send_to, wrf_cmaq_ce_recv_from,                &
                                wrf_cmaq_ce_send_index_g, wrf_cmaq_ce_send_index_l,        &
                                wrf_cmaq_ce_recv_index_g, wrf_cmaq_ce_recv_index_l   )

     call compute_comm_indices (twoway_nprocs, wrf_d_domain_map, cmaq_de_domain_map,       &
                                wrf_cmaq_de_send_to, wrf_cmaq_de_recv_from,                &
                                wrf_cmaq_de_send_index_g, wrf_cmaq_de_send_index_l,        &
                                wrf_cmaq_de_recv_index_g, wrf_cmaq_de_recv_index_l   )

    CALL aq_header (cmaq_c_ncols, cmaq_c_nrows, wrf_c_col_dim, wrf_c_row_dim, nlays,       &
                    sdate, stime, grid%dx, grid%dy, delta_x, delta_y,                      & 
                    config_flags%map_proj, config_flags%truelat1, config_flags%truelat2,   &
                    config_flags%cen_lat, config_flags%cen_lon, config_flags%stand_lon,    &
                    grid%p_top, grid%znw, grid%xlat(tw_sc,tw_sr), grid%xlong(tw_sc,tw_sr), &
                    wrf_lc_ref_lat)

     CALL setup_griddesc_file (cmaq_c_col_dim, cmaq_c_row_dim)

     if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
        file_time_step_in_sec = time2sec (file_time_step)

        if (.not.  pio_init (colrow, cmaq_c_col_dim, cmaq_c_row_dim,    &
                             nlays, 1, cmaq_c_ncols, cmaq_c_nrows,      &
                             npcol, nprow, twoway_nprocs, twoway_mype, wflg=.false.) ) then
           print *, ' Error: in invoking pio_init'
           stop
        end if
     end if

     if (config_flags%cu_physics == 0) then
        wrf_convective_scheme = .false.
     else
        wrf_convective_scheme = .true.
     end if

!-------------------------------------------------------------------------------
! Fill time-independent arrays for GRIDCRO2D and GRIDDOT2D.
! only need to do this once per run, not each step
!-------------------------------------------------------------------------------

     if (wrf_cmaq_option .gt. 1) then
        fname = 'GRID_CRO_2D'
     end if
     if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
        pfname = 'PGRID_CRO_2D'
     end if

     if (.not. file_opened) then
        call aq_set_ioapi_header ('C', cmaq_c_ncols, cmaq_c_nrows)

        mxrec3d = 1
        nlays3d = 1

        vname3d(1:n_gridcro2d_var) = gridcro2d_vlist
        units3d(1:n_gridcro2d_var) = gridcro2d_units

        num_land_cat = config_flags%num_land_cat

        do v = 1, numlu
           write (vname3d(v+n_gridcro2d_var), '(a7, i2.2)') 'LUFRAC_', v
           units3d(v+n_gridcro2d_var) = '1'
        end do

! this is particular for m3dry LUFRAC_01
        units3d(1+n_gridcro2d_var) = '1'

        nvars3d = numlu+n_gridcro2d_var
        tstep3d = 0
        vtype3d = ioapi_header%vtype

        allocate ( gridcro2d_data_wrf (wrf_c_ncols, wrf_c_nrows, nvars3d), stat=stat)
        allocate ( gridcro2d_data_cmaq (cmaq_c_ncols, cmaq_c_nrows, nvars3d), stat=stat)

        if (wrf_cmaq_option .gt. 1) then
           if ( .not. open3 (fname, FSRDWR3, pname) ) then
              print *, ' Error: Could not open file ', fname, 'for update'
              if ( .not. open3 (fname, FSNEW3, pname) ) then
                 print *, ' Error: Could not open file ', fname
              end if
           end if
        end if
        if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
           if (twoway_mype == 0) then
              ncols3d = cmaq_c_col_dim
              nrows3d = cmaq_c_row_dim
              if ( .not. open3 (pfname, FSRDWR3, pname) ) then
                 print *, ' Error: Could not open file ', pfname, 'for update'
                 if ( .not. open3 (pfname, FSNEW3, pname) ) then
                    print *, ' Error: Could not open file ', pfname
                 end if
              end if
           end if
        end if

        if (config_flags%mminlu == 'USGS') then
           lwater = 16
           lice   = 24
           if (config_flags%num_land_cat == 33) then
              mminlu = 'USGS33'
           else if (config_flags%num_land_cat == 24) then
              mminlu = 'USGS24'
           else if (config_flags%num_land_cat == 28) then
              mminlu = 'USGS28'
           end if 
        else if (config_flags%mminlu == 'NLCD-MODIS') then
           lwater = 17
           lice   = 15
           mminlu = config_flags%mminlu
        else if ((config_flags%mminlu == 'MODIS') .or. (config_flags%mminlu == 'MODIFIED_IGBP_MODIS_NOAH')) then
           lwater = 17
           lice   = 15
           mminlu = config_flags%mminlu
        else if (config_flags%mminlu(1:4) == 'NLCD') then
           if (config_flags%num_land_cat == 40) then
              lwater = 17
              lice   = 15
              mminlu = 'NLCD40'
           else
              lwater = 1
              lice   = 2
              mminlu = config_flags%mminlu
           end if
        else
           print *, ' Warning: Unknow landuse type ', config_flags%mminlu, grid%num_land_cat
        end if
     end if

     allocate ( land_use_index (wrf_c_ncols, wrf_c_nrows), stat=stat)
     land_use_index = grid%lu_index (tw_sc:tw_ec, tw_sr:tw_er)

    !---------------------------------------------------------------------------
    ! Fill scalar-point arrays of latitude (LAT), longitude (LON), terrain
    ! elevation (HT), land-water mask (LWMASK), and fractional land use (LUFRAC)
    ! directly from WRF arrays.
    !---------------------------------------------------------------------------

     gridcro2d_data_wrf (:,:,1) = grid%xlat (tw_sc:tw_ec, tw_sr:tw_er)
     gridcro2d_data_wrf (:,:,2) = grid%xlong (tw_sc:tw_ec, tw_sr:tw_er)
     gridcro2d_data_wrf (:,:,4) = grid%ht (tw_sc:tw_ec, tw_sr:tw_er)

     gridcro2d_data_wrf (:,:,5) = grid%landmask (tw_sc:tw_ec, tw_sr:tw_er)

     gridcro2d_data_wrf (:,:,7) = grid%lu_index (tw_sc:tw_ec, tw_sr:tw_er)

!    where ( ( nint(land_use_index(:,:)) == lwater ) .or. ( nint(land_use_index(:,:)) == lice ) )  ! water
!      gridcro2d_data_wrf(:,:,5) = 0.0
!    elsewhere  ! land
!      gridcro2d_data_wrf(:,:,5) = 1.0
!    end where

     do i = 1, numlu
        gridcro2d_data_wrf (:,:,n_gridcro2d_var+i) = grid%landusef     (tw_sc:tw_ec, i, tw_sr:tw_er)
     end do

    !---------------------------------------------------------------------------
    ! Compute squared scalar-point map-scale factors (MSFX2).
    !
    ! Note:  The scalar-point map-scale factors (MSFX), which are filled
    !        directly from a WRF array, are needed in the UHAT_JD and VHAT_JD
    !        calculations below.
    !---------------------------------------------------------------------------

     gridcro2d_data_wrf (:,:,3) = grid%msftx (tw_sc:tw_ec, tw_sr:tw_er) * grid%msftx (tw_sc:tw_ec, tw_sr:tw_er)

    !---------------------------------------------------------------------------
    ! Compute percentage of urban area per land in grid cell (PURB) using
    ! algorithm from MCIP.
    !---------------------------------------------------------------------------

     jj = tw_sr - 1
     do r = 1, wrf_c_nrows
        jj = jj + 1
        ii = tw_sc - 1
        do c = 1, wrf_c_ncols
           ii = ii + 1
           if ( nint(land_use_index(c,r)) == lwater ) then  ! water is dominant
              gridcro2d_data_wrf(c,r,6) = 0.0
           else  ! land is dominant over water in cell
              if ( grid%landusef(ii,lwater,jj) < 1.0 ) then
!                if ((config_flags%mminlu == 'USGS') .and. (config_flags%num_land_cat == 33)) then
                 if (mminlu == 'USGS33') then
                    gridcro2d_data_wrf(c,r,6) = ( ( grid%landusef(ii,1,jj)  + grid%landusef(ii,31,jj) +    &
                                                    grid%landusef(ii,32,jj) + grid%landusef(ii,33,jj) ) /  &
                                                  (1.0 - grid%landusef(ii,lwater,jj)) ) * 100.0
                 else if (mminlu == 'USGS24') then
                    gridcro2d_data_wrf(c,r,6) = ( grid%landusef(ii,1,jj) /  &
                                                (1.0 - grid%landusef(ii,lwater,jj)) ) * 100.0
                 else if ((mminlu == 'MODIS') .or. (mminlu == 'MODIFIED_IGBP_MODIS_NOAH')) then
                    gridcro2d_data_wrf(c,r,6) = ( grid%landusef(ii,13,jj) /  &
                                                (1.0 - grid%landusef(ii,lwater,jj)) ) * 100.0
                 else if ((mminlu == 'NLCD') .or. (mminlu == 'NLCD50')) then
                    gridcro2d_data_wrf(c,r,6) = ( ( grid%landusef(ii,3,jj) * 0.10 +    &
                                                    grid%landusef(ii,4,jj) * 0.35 +    &
                                                    grid%landusef(ii,5,jj) * 0.65 +    &
                                                    grid%landusef(ii,6,jj) * 0.90 +    &
                                                    grid%landusef(ii,44,jj)       ) /  &
                                                  (1.0 - grid%landusef(ii,lwater,jj)) ) * 100.0
                 else if (mminlu == 'NLCD40') then
                    gridcro2d_data_wrf(c,r,6) = ( ( grid%landusef(ii,23,jj) * 0.10 +    &
                                                    grid%landusef(ii,24,jj) * 0.35 +    &
                                                    grid%landusef(ii,25,jj) * 0.65 +    &
                                                    grid%landusef(ii,26,jj) * 0.90 +    &
                                                    grid%landusef(ii,13,jj)       ) /  &
                                                  (1.0 - grid%landusef(ii,lwater,jj)) ) * 100.0
                 else
                    print *, ' Warning:: Unknow Land Use type'
                    stop
                 end if
              else
                 gridcro2d_data_wrf(c,r,6) = 0.0
              end if
           end if
        end do
     end do

     deallocate (land_use_index)

     call se_wrf_cmaq_comm (twoway_mype, gridcro2d_data_wrf, gridcro2d_data_cmaq,     &
                            wrf_cmaq_c_send_to, wrf_cmaq_c_recv_from,                 &
                            wrf_cmaq_c_send_index_l, wrf_cmaq_c_recv_index_l, 1)

     if (wrf_cmaq_option .gt. 1) then
        if ( .not. buf_write3 (fname, allvar3, jdate, jtime, gridcro2d_data_cmaq ) ) then
           print *, ' Error: Could not write to file ', fname
           stop
        end if
     end if
     if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
        if ( .not. write3 (pfname, allvar3, jdate, jtime, gridcro2d_data_cmaq ) ) then
           print *, ' Error: Could not write to file ', pfname
           stop
        end if
     end if

    !---------------------------------------------------------------------------
    ! Compute sqaured dot-point map-scale factors (MSFD2).
    !
    ! The correct method would be to use the grid projection information and
    ! call a routine like gridgeometry from MCIP.  Here, for simplicity, 
    ! approximate dot-point map-scale factors from flux-point map-scale
    ! factors that are readily available in WRF using four-point interpolation.
    !---------------------------------------------------------------------------

     if (wrf_cmaq_option .gt. 1) then
        fname = 'GRID_DOT_2D'
     end if
     if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
        pfname = 'PGRID_DOT_2D'
     end if

     if (.not. file_opened) then
        call aq_set_ioapi_header ('D', cmaq_d_ncols, cmaq_d_nrows)
 
        mxrec3d = 1
        nlays3d = 1

        nvars3d = n_griddot2d_var
        vname3d(1:nvars3d) = griddot2d_vlist
        units3d(1:nvars3d) = griddot2d_units
        tstep3d = 0
        vtype3d = ioapi_header%vtype

        if (wrf_cmaq_option .gt. 1) then
           if ( .not. open3 (fname, FSRDWR3, pname) ) then
              print *, ' Error: Could not open file ', fname, 'for update'
              if ( .not. open3 (fname, FSNEW3, pname) ) then
                 print *, ' Error: Could not open file ', fname
              end if
           end if
        end if
        if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
           if (twoway_mype == 0) then
              ncols3d = cmaq_c_col_dim + 1
              nrows3d = cmaq_c_row_dim + 1
              if ( .not. open3 (pfname, FSRDWR3, pname) ) then
                 print *, ' Error: Could not open file ', pfname, 'for update'
                 if ( .not. open3 (pfname, FSNEW3, pname) ) then
                    print *, ' Error: Could not open file ', pfname
                 end if
              end if
           end if
        end if

        allocate ( griddot2d_data_wrf (wrf_d_ncols, wrf_d_nrows), stat=stat)
        allocate ( griddot2d_data_cmaq (cmaq_d_ncols, cmaq_d_nrows), stat=stat)
     end if

     jj = tw_sr_d - 1
     DO r = 1, wrf_d_nrows
        jj = min (jj+1, tw_er_d)
        jjm1 = MAX( jj-1, 1 )
        ii = tw_sc_d - 1
        DO c = 1, wrf_d_ncols
           ii = min (ii+1, tw_ec_d)
           iim1 = MAX ( ii-1, 1 )

           griddot2d_data_wrf(c,r) = 0.25 * ( grid%msfux(ii,jjm1) + grid%msfux(ii,jj) +  &
                                              grid%msfvx(iim1,jj) + grid%msfvx(ii,jj) )

           griddot2d_data_wrf(c,r) = griddot2d_data_wrf(c,r) * griddot2d_data_wrf(c,r)

        ENDDO
     ENDDO

     call se_wrf_cmaq_comm (twoway_mype, griddot2d_data_wrf, griddot2d_data_cmaq,     &
                            wrf_cmaq_d_send_to, wrf_cmaq_d_recv_from,                 &
                            wrf_cmaq_d_send_index_l, wrf_cmaq_d_recv_index_l, 2)

     if (wrf_cmaq_option .gt. 1) then
        if ( .not. buf_write3 (fname, allvar3, jdate, jtime, griddot2d_data_cmaq ) ) then
           print *, ' Error: Could not write to file ', fname
           stop
        end if
     end if
     if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
        tsc_d = 1
        if (east_bdy_pe) then
           tec_d = cmaq_d_domain_map(3,1,twoway_mype)
        else
           tec_d = cmaq_d_domain_map(3,1,twoway_mype) - 1
        end if
        tsr_d = 1
        if (north_bdy_pe) then
           ter_d = cmaq_d_domain_map(3,2,twoway_mype)
        else
           ter_d = cmaq_d_domain_map(3,2,twoway_mype) - 1
        end if

        if ( .not. write3 (pfname, allvar3, jdate, jtime, griddot2d_data_cmaq(tsc_d:tec_d,tsr_d:ter_d) ) ) then
           print *, ' Error: Could not write to file ', pfname
           stop
        end if
     end if

     first = .false.

  ENDIF  ! first

  if (wrf_cmaq_option .gt. 1) then
     fname = 'MET_CRO_3D'
  end if
  if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
     pfname = 'PMET_CRO_3D'
  end if

  if (.not. file_opened) then
     call aq_set_ioapi_header ('C', cmaq_ce_domain_map(3,1,twoway_mype), cmaq_ce_domain_map(3,2,twoway_mype))

     mxrec3d = nstep

     xorig3d = ioapi_header%xorig - ioapi_header%xcell
     yorig3d = ioapi_header%yorig - ioapi_header%ycell

     nlays3d = ioapi_header%nlays
     nvars3d = n_metcro3d_var
     vname3d(1:nvars3d) = metcro3d_vlist
     units3d(1:nvars3d) = metcro3d_units
     tstep3d = cmaq_tstep
     vtype3d = ioapi_header%vtype

     if (.not. allocated(metcro3d_data_wrf)) then
        allocate ( metcro3d_data_wrf (wrf_c_ncols, wrf_c_nrows, nlays, nvars3d), stat=stat)
        allocate ( metcro3d_data_cmaq (cmaq_ce_domain_map(3,1,twoway_mype), &
                                       cmaq_ce_domain_map(3,2,twoway_mype), nlays, nvars3d), stat=stat)

        metcro3d_data_wrf = 0.0

        tsc_c = 2
        tec_c = cmaq_ce_domain_map(3,1,twoway_mype) - 1
        tsr_c = 2
        ter_c = cmaq_ce_domain_map(3,2,twoway_mype) - 1

        tsc_e = 2
        tec_e = cmaq_ce_domain_map(3,1,twoway_mype) - 1
        tsr_e = 2
        ter_e = cmaq_ce_domain_map(3,2,twoway_mype) - 1

        if (west_bdy_pe) then
           tsc_e = 1
        end if
        if (east_bdy_pe) then
           tec_e = tec_e + 1
        end if
        if (south_bdy_pe) then
           tsr_e = 1
        end if
        if (north_bdy_pe) then
           ter_e = ter_e + 1
        end if

     end if

     if (wrf_cmaq_option .gt. 1) then
        if ( .not. open3 (fname, FSRDWR3, pname) ) then
           print *, ' Error: Could not open file ', fname, 'for update'
           if ( .not. open3 (fname, FSNEW3, pname) ) then
              print *, ' Error: Could not open file ', fname
           end if
        end if
     end if

     if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
        if (twoway_mype == 0) then
           ncols3d = cmaq_c_col_dim + 2
           nrows3d = cmaq_c_row_dim + 2
           tstep3d = file_time_step

           if ( .not. open3 (pfname, FSRDWR3, pname) ) then
              print *, ' Error: Could not open file ', pfname, 'for update'
              if ( .not. open3 (pfname, FSNEW3, pname) ) then
                 print *, ' Error: Could not open file ', pfname
              end if
           end if
        end if
     end if
  end if

!-------------------------------------------------------------------------------
! Fill time-dependent arrays for METCRO3D.
!
! Note:  Cannot just assign pointers for 3D variables because order of indices
!        is different in WRF (i,k,j) vs. CMAQ (i,j,k).
!-------------------------------------------------------------------------------

  IF (turn_on_pv) THEN

     IF ( .NOT. ALLOCATED ( sigma ) ) ALLOCATE ( sigma ( nlays ) )

     sigma = grid%znu(1:nlays)
!    sigma = grid%znu

     IF ( .NOT. ALLOCATED ( xuu_s ) ) ALLOCATE ( xuu_s ( wrf_d_ncols, wrf_d_nrows, nlays) )
     IF ( .NOT. ALLOCATED ( xvv_t ) ) ALLOCATE ( xvv_t ( wrf_d_ncols, wrf_d_nrows, nlays) )

     IF ( .NOT. ALLOCATED ( xuu_d ) ) ALLOCATE ( xuu_d ( wrf_d_ncols, wrf_d_nrows, nlays) )
     IF ( .NOT. ALLOCATED ( xvv_d ) ) ALLOCATE ( xvv_d ( wrf_d_ncols, wrf_d_nrows, nlays) )

     DO kk = 1, nlays
        jj = tw_sr_d - 1
        DO r = 1, wrf_d_nrows
           jj  = jj + 1
           ii = tw_sc_d - 1
           DO c = 1, wrf_d_ncols
              ii  = ii + 1

              xuu_s(c,r,kk)  = grid%u_2 (ii,kk,jj)
              xvv_t(c,r,kk)  = grid%v_2 (ii,kk,jj)

           ENDDO ! c
        ENDDO  ! r

        xvv_d(2:wrf_d_ncols-1,:,kk) = 0.5 * (xvv_t(1:wrf_d_ncols-2,:,kk) + xvv_t(2:wrf_d_ncols-1,:,kk))
        IF (west_bdy_pe) THEN
             xvv_d(1,        :,kk) = xvv_t(1,:,kk)
        ELSE
             xvv_d(1,        :,kk) = 0.5 * (xvv_t(1,:,kk) + grid%v_2 (tw_sc_d-1,kk,tw_sr_d:tw_er_d))
        ENDIF
        IF (east_bdy_pe) THEN
             xvv_d(wrf_d_ncols,:,kk) = xvv_t(wrf_d_ncols-1,:,kk)
        ELSE
             xvv_d(wrf_d_ncols,:,kk) = 0.5 * (xvv_t(wrf_d_ncols-1,:,kk) + xvv_t(wrf_d_ncols,:,kk))
        ENDIF

        xuu_d(:,2:wrf_d_nrows-1,kk) = 0.5 * (xuu_s(:,1:wrf_d_nrows-2,kk) + xuu_s(:,2:wrf_d_nrows-1,kk))
        IF (south_bdy_pe) THEN
             xuu_d(:,1,        kk) = xuu_s(:,1,kk)
        ELSE
             xuu_d(:,1,        kk) =  0.5 * (xuu_s(:,1,kk) + grid%u_2 (tw_sc_d:tw_ec_d,kk,tw_sr_d-1))
        ENDIF
        IF (north_bdy_pe) THEN
             xuu_d(:,wrf_d_nrows,kk) = xuu_s(:,wrf_d_nrows-1,kk)
        ELSE
             xuu_d(:,wrf_d_nrows,kk) = 0.5 * (xuu_s(:,wrf_d_nrows-1,kk) + xuu_s(:,wrf_d_nrows,kk))
        ENDIF
     ENDDO ! kk

     IF ( .NOT. ALLOCATED ( xtheta ) ) ALLOCATE ( xtheta ( wrf_c_ncols, wrf_c_nrows, nlays) )
  END IF  ! turn_on_pv

  zf (:,:,0) = 0.0
  DO kk = 1, nlays
     kp1 = kk + 1
     jj = tw_sr - 1
     DO r = 1, wrf_c_nrows
        jj = jj + 1
        ii = tw_sc - 1
        DO c = 1, wrf_c_ncols
           ii = ii + 1

        !-----------------------------------------------------------------------
        ! Fill "required" 3D scalar-point arrays of temperature (TA), water
        ! vapor mixing ratio (QV), pressure (PRES), density (DENS), density
        ! including contribution from water vapor (DENSQ), height of the
        ! half-layers (mid-layers) (ZH), height of the full levels (ZF),
        ! pressure on full levels (PRESF), height difference of full levels
        ! (DZF), cloud mixing ratio (QC), and rain water mixing ratio (QR)
        ! directly from WRF arrays.
        !
        ! Note:  QV, QC, and QR are "optional" variables in the WRF model
        !        because they are not output in some configurations of the
        !        model.  It may be better to ensure they are defined before
        !        this routine is called and make them required rather than
        !        optional on the calling statement.
        !
        ! Note:  DENSQ is new for output.  It may be preferred over DENS in
        !        some CMAQ calculations.
        !
        ! Note:  PRESF and DZF are filled to support other calculations, and
        !        they are not part of the output.
        !
        ! Note:  None of the full-level arrays are dimensioned to full levels.
        !        All arrays are filled using CMAQ's (:,:,0:nlays) convention.
        !        PRESF(:,:0) is PRSFC, which is filled in the 2D arrays.
        !        ZF(:,:,0) is 0.0 for all grid cells.  DZF(:,:,0) does not
        !        exist because there are only NLAYS differences between
        !        NLAYS+1 levels.
        !-----------------------------------------------------------------------

           metcro3d_data_wrf (c,r,kk,15) = grid%u_phy(ii,kk,jj)   ! store u wind component on mass point
           metcro3d_data_wrf (c,r,kk,16) = grid%v_phy(ii,kk,jj)   ! store v wind component on mass point

           metcro3d_data_wrf (c,r,kk,4) = t_phy_wrf   (ii,kk,jj)  ! ta

           if (turn_on_pv) then
              xtheta(c,r,kk) = grid%t_2 (ii,kk,jj) + t0
           end if

           IF ( PRESENT (qv_curr_wrf) .AND. f_qv ) THEN
              metcro3d_data_wrf (c,r,kk,5) = qv_curr_wrf (ii,kk,jj)   ! qv
           ELSE
              print *, ' Error: CMAQ *needs* QV to run. '
              stop
           ENDIF

           metcro3d_data_wrf   (c,r,kk,11) = p_phy_wrf   (ii,kk,jj)      ! pres

           metcro3d_data_wrf (c,r,kk,12) = 1.0 / grid%alt(ii,kk,jj)      ! den

           densq  (c,r,kk) = rho_wrf    (ii,kk,jj)  ! includes QV: 1/dens*(1+QV)

           zf     (c,r,kk) = z_at_w_wrf (ii,kp1,jj) - z_at_w_wrf (ii,1,jj)    ! adjust for 0: indexing in AQ
           metcro3d_data_wrf (c,r,kk,13) = 0.5 * (zf(c,r,kk) + zf(c,r,kk-1))  ! zh

           presf  (c,r,kk) = p8w_wrf    (ii,kp1,jj) ! adjust for 0: indexing in AQ
           dzf    (c,r,kk) = dz8w_wrf   (ii,kk,jj)  ! no kp1: dz8w=0 at model top

           IF ( PRESENT (qc_curr_wrf) .AND. f_qc ) THEN
              metcro3d_data_wrf (c,r,kk,6) = qc_curr_wrf(ii,kk,jj)   ! qc
           ELSE
             print *, ' Error: CMAQ *needs* QC to run. '
             stop
           ENDIF

           IF ( PRESENT (qr_curr_wrf) .AND. f_qr ) THEN
              metcro3d_data_wrf (c,r,kk,7) = qr_curr_wrf(ii,kk,jj)   ! qr
           ELSE
             print *, ' Error: CMAQ *needs* QR to run. '
             stop
           ENDIF

        !-----------------------------------------------------------------------
        ! Fill "optional" moisture variables:  ice mixing ratio (QI), snow
        ! mixing ratio (QS), and graupel mixing ratio (QG).
        !
        ! Note:  QI, QS, and QG are not required for CMAQ, but will be used
        !        if they are available.  They are not written to output if
        !        they are not available...so do not fill with 0.0 or BADVAL3.
        !-----------------------------------------------------------------------

           IF ( PRESENT (qi_curr_wrf) .AND. f_qi ) THEN
              metcro3d_data_wrf (c,r,kk,8) = qi_curr_wrf(ii,kk,jj)    ! qi
           ELSE
              metcro3d_data_wrf (c,r,kk,8) = 0.0
           ENDIF

           IF ( PRESENT (qs_curr_wrf) .AND. f_qs ) THEN
              metcro3d_data_wrf (c,r,kk,9) = qs_curr_wrf(ii,kk,jj)    ! qs
           ELSE
              metcro3d_data_wrf (c,r,kk,9) = 0.0
           ENDIF

           IF ( PRESENT (qg_curr_wrf) .AND. f_qg ) THEN
              metcro3d_data_wrf (c,r,kk,10) = qg_curr_wrf(ii,kk,jj)   ! qg
           ELSE
              metcro3d_data_wrf (c,r,kk,10) = 0.0
           ENDIF

        !-----------------------------------------------------------------------
        ! Compute Jacobian on full levels and mid-layers, and compute
        ! coupled density and Jacobian on mid-layers.  Need to compute
        ! full-level density (in WRF coordinate) for Jacobian.
        !
        ! Jacobian is from WRF relation:
        !   J*g = d(phi)/d(eta) = d(g z)/d(eta) = -mu alpha = -mu/rho
        !
        ! Note:  In QF calculation, use "raw" WRF variable QV_CURR_WRF instead
        !        of QV here because QV(:,kp1,:) is not yet filled.
        !
        ! Note:  JACOBF at the surface (i.e., JACOBS) is not filled because
        !        it is not used in CMAQ.
        !-----------------------------------------------------------------------

           tf    = t8w_wrf(ii,kp1,jj)  ! kp1 to adjust for 0: indexing in AQ model

           qf    = 0.5 * ( qv_curr_wrf(ii,kk,jj) + qv_curr_wrf(ii,kp1,jj) )
           densf = presf(c,r,kk) / ( r_d * tf * (1.0 + r_v*qf/r_d) )

        !-----------------------------------------------------------------------
        ! Update calculation of Jacobian for hybrid vertical coordinate.
        ! TLS 1 Feb 19
        !
        ! Calculate new variables MUHYBF and MUHYBH (mu hybrid on full and half
        ! levels).  Note that full level indexing in vertical differs by 1 from
        ! WRF because CMAQ's arrays are zero-based.
        !
        ! Replace MUT in the Jacobian calculations, below, with MUHYBF or
        ! MUHYBH, depending on the level where we want the Jacobian.
        !-----------------------------------------------------------------------
           if (hybrid_vert) then
             muhybf = grid%c1f(kp1) * grid%mut(ii,jj) + grid%c2f(kp1)
             muhybh = grid%c1h(kk)  * grid%mut(ii,jj) + grid%c2h(kk)
           else
             muhybf = grid%mut(ii,jj)
             muhybh = grid%mut(ii,jj)
           endif

           if (turn_on_pv) then
              metcro3d_data_wrf (c,r,kk,1) = tf*2
           else
              metcro3d_data_wrf (c,r,kk,1) = gravi * muhybf / (densf * gridcro2d_data_wrf (c,r,3)) 
           end if

           metcro3d_data_wrf (c,r,kk,2) = gravi * muhybh / (metcro3d_data_wrf(c,r,kk,12) * gridcro2d_data_wrf (c,r,3)) 
           metcro3d_data_wrf (c,r,kk,3) = gravi * muhybh / gridcro2d_data_wrf (c,r,3)   

        ENDDO
     ENDDO
  ENDDO

  metcro3d_data_wrf (:,:,1:nlays,14) = zf (:,:,1:nlays)

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
!           05 Sep 2012  Embeded in two-way model from Mcip4.0 (J. XING)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Compute vertical gradients using 2nd order polynomials at all levels.
! Gradients obtained at model sigma levels,
!   not at sigma=.5*(sigma(K+1)+sigma(K-1))
!-------------------------------------------------------------------------------

  IF (TURN_ON_PV) THEN
     IF ( .NOT. ALLOCATED ( xmapc ) ) ALLOCATE ( xmapc ( wrf_c_ncols, wrf_c_nrows) )
     xmapc(:,:) = grid%msftx (tw_sc:tw_ec, tw_sr:tw_er)
     IF ( .NOT. ALLOCATED ( xmapc2 ) ) ALLOCATE ( xmapc2 ( wrf_c_ncols, wrf_c_nrows))
     xmapc2(:,:) = grid%msftx (tw_sc:tw_ec, tw_sr:tw_er) * grid%msftx (tw_sc:tw_ec, tw_sr:tw_er)
     IF ( .NOT. ALLOCATED ( xcorl ) ) ALLOCATE ( xcorl ( wrf_c_ncols, wrf_c_nrows) )
     xcorl(:,:) = grid%f (tw_sc:tw_ec, tw_sr:tw_er)

     dx  = grid%dx
     dy  = grid%dy

     dsx = 2.0 * dx
     dsy = 2.0 * dy

     DO k = 1, nlays
        IF ( k == 1 ) THEN
           k0 = k
           k1 = k + 1
           k2 = k + 2

           f0 = -1.0 / (sigma(k1) - sigma(k0)) - 1.0 / (sigma(k2) - sigma(k0))
           f1 =  1.0 / (sigma(k1) - sigma(k0)) + 1.0 / (sigma(k2) - sigma(k1))
           f2 = -1.0 * ( (sigma(k1) - sigma(k0)) /   &
                     ( (sigma(k2) - sigma(k0)) * (sigma(k2) - sigma(k1)) ) )

        ELSE IF ( k == nlays ) THEN

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

        IF ( .NOT. ALLOCATED ( duds  ) ) ALLOCATE ( duds ( wrf_c_ncols,   wrf_c_nrows) )
        IF ( .NOT. ALLOCATED ( dvds  ) ) ALLOCATE ( dvds ( wrf_c_ncols,   wrf_c_nrows) )
        IF ( .NOT. ALLOCATED ( dtdx  ) ) ALLOCATE ( dtdx ( wrf_c_ncols,   wrf_c_nrows) )
        IF ( .NOT. ALLOCATED ( dtdy  ) ) ALLOCATE ( dtdy ( wrf_c_ncols,   wrf_c_nrows) )
        IF ( .NOT. ALLOCATED ( dtds  ) ) ALLOCATE ( dtds ( wrf_c_ncols,   wrf_c_nrows) )

        DO r = 1, wrf_c_nrows
           rp1 = r + 1
           DO c = 1, wrf_c_ncols
              cp1 = c + 1

              duds(c,r) = 0.5 * ( f0 * ( xuu_s(cp1,r  ,k0) + xuu_s(c,r,k0) ) +  &
                                  f1 * ( xuu_s(cp1,r  ,k1) + xuu_s(c,r,k1) ) +  &
                                  f2 * ( xuu_s(cp1,r  ,k2) + xuu_s(c,r,k2) ) )

              dvds(c,r) = 0.5 * ( f0 * ( xvv_t(c  ,rp1,k0) + xvv_t(c,r,k0) ) +  &
                                  f1 * ( xvv_t(c  ,rp1,k1) + xvv_t(c,r,k1) ) +  &
                                  f2 * ( xvv_t(c  ,rp1,k2) + xvv_t(c,r,k2) ) )

           ENDDO
        ENDDO

        DO r = 1, wrf_c_nrows
           DO c = 1, wrf_c_ncols

              t00 = xtheta(c,r,k0)
              t1  = xtheta(c,r,k1)
              t2  = xtheta(c,r,k2)

              dtds(c,r) = f0*t00 + f1*t1 + f2*t2

           ENDDO
        ENDDO

        jj = tw_sr - 1
        DO r = 1, wrf_c_nrows
           jj = jj + 1
           DO c = 2, wrf_c_ncols-1
              t1        = xtheta(c-1,r,k) / xmapc(c-1,r)
              t2        = xtheta(c+1,r,k) / xmapc(c+1,r)                    
              dtdx(c,r) = xmapc2(c,r) * (t2-t1) / dsx
           ENDDO

           IF (west_bdy_pe) THEN
              t1        = xtheta(1,r,k) / xmapc(1,r)
              t2        = xtheta(2,r,k) / xmapc(2,r)
              t3        = xtheta(3,r,k) / xmapc(3,r)
              dtdx(1,r) = xmapc2(1,r) * (-1.5*t1 + 2.0*t2 - 0.5*t3) / dx
           ELSE
              t1        = (grid%t_2(tw_sc-1,k,jj) + t0) / grid%msftx(tw_sc-1,jj)
              t2        = xtheta(2,r,k) / xmapc(2,r)
              dtdx(1,r) = xmapc2(1,r) * (t2-t1) / dsx
           ENDIF

           IF (east_bdy_pe) THEN
              t00             = xtheta(wrf_c_ncols-2,r,k) / xmapc(wrf_c_ncols-2,r)
              t1              = xtheta(wrf_c_ncols-1,r,k) / xmapc(wrf_c_ncols-1,r)
              t2              = xtheta(wrf_c_ncols,  r,k) / xmapc(wrf_c_ncols,  r)
              dtdx(wrf_c_ncols,r) = xmapc2(wrf_c_ncols,r) * (0.5*t00 - 2.0*t1 + 1.5*t2) / dx
           ELSE
              t1        = xtheta(c-1,r,k) / xmapc(c-1,r)
              t2        = (grid%t_2(tw_ec+1,k,jj) + t0) / grid%msftx(tw_ec+1,jj)
              dtdx(wrf_c_ncols,r) = xmapc2(wrf_c_ncols,r) * (t2-t1) / dsx
           ENDIF

        ENDDO

        ii = tw_sc - 1
        DO c = 1, wrf_c_ncols
           ii = ii + 1
            DO r = 2, wrf_c_nrows-1
               t1        = xtheta(c,r-1,k) / xmapc(c,r-1)
               t2        = xtheta(c,r+1,k) / xmapc(c,r+1)
               dtdy(c,r) = xmapc2(c,r) * (t2-t1) / dsy
            ENDDO ! r

            IF (south_bdy_pe) THEN
              t1        = xtheta(c,1,k) / xmapc(c,1)
              t2        = xtheta(c,2,k) / xmapc(c,2)
              t3        = xtheta(c,3,k) / xmapc(c,3)
              dtdy(c,1) = xmapc2(c,1) * (-1.5*t1 + 2.0*t2 - 0.5*t3) / dy
           ELSE
              t1        = (grid%t_2(ii,k,tw_sr-1) + t0) / grid%msftx(ii,tw_sr-1)
              t2        = xtheta(c,2,k) / xmapc(c,2)
              dtdy(c,1) = xmapc2(c,1) * (t2-t1) / dsy
           ENDIF

           IF (north_bdy_pe) THEN
              t00       = xtheta(c,wrf_c_nrows-2,k) / xmapc(c,wrf_c_nrows-2)
              t1        = xtheta(c,wrf_c_nrows-1,k) / xmapc(c,wrf_c_nrows-1)
              t2        = xtheta(c,wrf_c_nrows,  k) / xmapc(c,wrf_c_nrows)
              dtdy(c,wrf_c_nrows) = xmapc2(c,wrf_c_nrows) * (0.5*t00 - 2.0*t1 + 1.5*t2) / dy
           ELSE
              t1        = xtheta(c,r-1,k) / xmapc(c,r-1)
              t2        = (grid%t_2(ii,k,tw_er+1) + t0)/ grid%msftx(ii,tw_er+1)
              dtdy(c,wrf_c_nrows) = xmapc2(c,wrf_c_nrows) * (t2-t1) / dsy
           ENDIF

        ENDDO

!-------------------------------------------------------------------------------
! Compute slab absolute vorticity, and store potential vorticity in XPVC.
!
!     1. Because we use X3 instead of SIGMA in equation,
!        GRAV/PSB is replaced by 1.0/XRHOJM (density * Jacobian).
!
!     2. As a shortcut, 1.0/XRHOJM is not included in XPVC here;
!        it will be included in subroutine METCRO before PV is output.
!     3. Added RHOJ
!-------------------------------------------------------------------------------

        DO r = 1, wrf_c_nrows
           rp1 = r + 1

           DO c = 1, wrf_c_ncols
              cp1 = c + 1

              vor = xmapc2(c,r) * ((xvv_d(cp1,r,  k) + xvv_d(cp1,rp1,k) -          &
                                    xvv_d(c,  r,  k) - xvv_d(c,  rp1,k)) / dsx  -  &
                                   (xuu_d(c,  rp1,k) + xuu_d(cp1,rp1,k) -          &
                                    xuu_d(c,  r,  k) - xuu_d(cp1,r,  k)) / dsy) +  &
                    xcorl(c,r)

              metcro3d_data_wrf (c,r,k,n_metcro3d_var) = -1.0e6 * ( vor * dtds(c,r)   &
                                                         - dvds(c,r) * dtdx(c,r)      &
                                                         + duds(c,r) * dtdy(c,r) )    &
                         / ( metcro3d_data_wrf (c,r,k,3) * gridcro2d_data_wrf (c,r,3))
           ENDDO
        ENDDO
     ENDDO
  END IF  ! turn on pv

  call se_wrf_cmaq_comm (twoway_mype, metcro3d_data_wrf, metcro3d_data_cmaq,         &
                         wrf_cmaq_ce_send_to, wrf_cmaq_ce_recv_from,                 &
                         wrf_cmaq_ce_send_index_l, wrf_cmaq_ce_recv_index_l, 3)

  if (wrf_cmaq_option .gt. 1) then
     if ( .not. buf_write3 (fname, allvar3, jdate, jtime, metcro3d_data_cmaq ) ) then
        print *, ' Error: Could not write to file ', fname
        stop
     end if
  end if
  if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
     if (mod(time2sec(jtime), file_time_step_in_sec) == 0) then
        write_to_physical_file = .true.
        if ( .not. write3 (pfname, allvar3, jdate, jtime, metcro3d_data_cmaq(tsc_e:tec_e, tsr_e:ter_e, :, :) ) ) then
           print *, ' Error: Could not write to file ', pfname
           stop
        end if
     else
        write_to_physical_file = .false.
     end if
  end if

! --------------------------

  if (wrf_cmaq_option .gt. 1) then
     fname = 'MET_DOT_3D'
  end if
  if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
     pfname = 'PMET_DOT_3D'
  end if

  if (.not. file_opened) then
     call aq_set_ioapi_header ('D', cmaq_de_domain_map(3,1,twoway_mype), cmaq_de_domain_map(3,2,twoway_mype))

     mxrec3d = nstep

     nlays3d = ioapi_header%nlays
     nvars3d = n_metdot3d_var
     vname3d(1:nvars3d) = metdot3d_vlist
     units3d(1:nvars3d) = metdot3d_units
     tstep3d = cmaq_tstep
     vtype3d = ioapi_header%vtype

     if (.not. allocated(metdot3d_data_wrf)) then
        allocate ( metdot3d_data_wrf (wrf_d_ncols, wrf_d_nrows, nlays, nvars3d), stat=stat)
        allocate ( metdot3d_data_cmaq (cmaq_de_domain_map(3,1,twoway_mype), &
                                       cmaq_de_domain_map(3,2,twoway_mype), nlays, nvars3d), stat=stat)
        tsc_d = 2
        if (east_bdy_pe) then
           tec_d = cmaq_de_domain_map(3,1,twoway_mype) - 1
        else
           tec_d = cmaq_de_domain_map(3,1,twoway_mype) - 2
        end if
        tsr_d = 2
        if (north_bdy_pe) then
           ter_d = cmaq_de_domain_map(3,2,twoway_mype) - 1
        else
           ter_d = cmaq_de_domain_map(3,2,twoway_mype) - 2
        end if
     end if

     if (wrf_cmaq_option .gt. 1) then
        if ( .not. open3 (fname, FSRDWR3, pname) ) then
           print *, ' Error: Could not open file ', fname, 'for update'
           if ( .not. open3 (fname, FSNEW3, pname) ) then
              print *, ' Error: Could not open file ', fname
           end if
        end if
     end if

     if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
        if (twoway_mype == 0) then
           ncols3d = cmaq_c_col_dim + 1
           nrows3d = cmaq_c_row_dim + 1
           tstep3d = file_time_step
           if ( .not. open3 (pfname, FSRDWR3, pname) ) then
              print *, ' Error: Could not open file ', pfname, 'for update'
              if ( .not. open3 (pfname, FSNEW3, pname) ) then
                 print *, ' Error: Could not open file ', pfname
              end if
           end if
        end if
     end if
  end if

!-------------------------------------------------------------------------------
! Fill time-dependent arrays in METDOT3D.
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Fill UWINDC and VWINDC directly from WRF arrays.
  !
  ! Note:  UWIND and VWIND changed to UWINDC and VWINDC to remind us that
  !        they are on the Arakawa C staggering, not the Arakawa B staggering
  !        that is in MCIP output and is expected in community SMOKE and CMAQ
  !        codes.
  !
  ! Note:  If trying to combine triply nested loops for UWINDC/VWINDC with
  !        UHAT_JD/VHAT_JD, the ends for R and C loop counters are different.
  !-----------------------------------------------------------------------------

  DO kk = 1, nlays
     jj = tw_sr_d - 1
     DO r = 1, wrf_d_nrows
        jj  = jj + 1
        ii = tw_sc_d - 1
        DO c = 1, wrf_d_ncols
           ii  = ii + 1

           metdot3d_data_wrf (c,r,kk,1) = grid%u_2 (ii,kk,jj)     ! C grid staggering, uwindc
           metdot3d_data_wrf (c,r,kk,2) = grid%v_2 (ii,kk,jj)     ! C grid staggering, vwindc

        ENDDO
     ENDDO

  !-----------------------------------------------------------------------------
  ! Compute UHAT_JD and VHAT_JD.
  !
  ! Note:  If order of loops is changed so that K is not on outside, JDENM
  !        needs to be allocated and filled in three dimensions.
  !
  ! Note:  If trying to combine triply nested loops for UWINDC/VWINDC with
  !        UHAT_JD/VHAT_JD, the ends for R and C loop counters are different.
  !-----------------------------------------------------------------------------

     jj = tw_sr - 1
     DO r = 1, wrf_d_nrows
        lrm1 = MAX( r-1, 1 )
        jj = jj + 1
        ii = tw_sc - 1
        DO c = 1, wrf_d_ncols
           ii = ii + 1
           lcm1 = MAX( c-1, 1 )

           jdenm  (c,r) = gravi * grid%mut(ii,jj) / grid%msftx (ii,jj)

           metdot3d_data_wrf(c,r,kk,3) = 0.5 * ( jdenm(lcm1,r) + jdenm(c,r) ) * metdot3d_data_wrf(c,r,kk,1)

           metdot3d_data_wrf(c,r,kk,4) = 0.5 * ( jdenm(c,lrm1) + jdenm(c,r) ) * metdot3d_data_wrf(c,r,kk,2)

        ENDDO
     ENDDO

    !---------------------------------------------------------------------------
    ! Note:  Use of NCOLS and NROWS for JDENM in the outermost column/row is
    !        deliberate in UHAT_JD and VHAT_JD calculations.  Original code in
    !        MCIP has JDENM defined to NCOLS+1, NROWS+1, but outer row and
    !        column are simply persisted from NCOLS, NROWS.
    !---------------------------------------------------------------------------

  ENDDO

  call se_wrf_cmaq_comm (twoway_mype, metdot3d_data_wrf, metdot3d_data_cmaq,       &
                         wrf_cmaq_de_send_to, wrf_cmaq_de_recv_from,               &
                         wrf_cmaq_de_send_index_l, wrf_cmaq_de_recv_index_l, 4)

  if (wrf_cmaq_option .gt. 1) then
     if ( .not. buf_write3 (fname, allvar3, jdate, jtime, metdot3d_data_cmaq ) ) then
       print *, ' Error: Could not write to file ', fname
       stop
     end if
  end if
  if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
     if (write_to_physical_file) then
        if ( .not. write3 (pfname, allvar3, jdate, jtime, metdot3d_data_cmaq(tsc_d:tec_d, tsr_d:ter_d, :, :) ) ) then
           print *, ' Error: Could not write to file ', pfname
           stop
        end if
     end if
  end if

! ------------------

  if (wrf_cmaq_option .gt. 1) then
     fname = 'MET_CRO_2D'
  end if
  if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
     pfname = 'PMET_CRO_2D'
  end if

  if (.not. file_opened) then
     call aq_set_ioapi_header ('C', cmaq_ce_domain_map(3,1,twoway_mype), cmaq_ce_domain_map(3,2,twoway_mype))

     nlays3d = 1
     mxrec3d = nstep
     nvars3d = n_metcro2d_var
     vname3d(1:nvars3d) = metcro2d_vlist
     units3d(1:nvars3d) = metcro2d_units
     tstep3d = cmaq_tstep
     vtype3d = ioapi_header%vtype

     if (.not. allocated(metcro2d_data_wrf)) then
        allocate ( metcro2d_data_wrf (wrf_c_ncols, wrf_c_nrows, nvars3d), stat=stat)
        allocate ( metcro2d_data_cmaq (cmaq_ce_domain_map(3,1,twoway_mype),           &
                                       cmaq_ce_domain_map(3,2,twoway_mype), nvars3d), &
                   temp_rainnc (cmaq_ce_domain_map(3,1,twoway_mype),                  &
                                cmaq_ce_domain_map(3,2,twoway_mype)),                 &
                   temp_rainc (cmaq_ce_domain_map(3,1,twoway_mype),                   &
                               cmaq_ce_domain_map(3,2,twoway_mype)),                  &
                   stat=stat)
        temp_rainnc = 0.0
        temp_rainc  = 0.0
     end if

     if (wrf_cmaq_option .gt. 1) then
        if ( .not. open3 (fname, FSRDWR3, pname) ) then
           print *, ' Error: Could not open file ', fname, 'for update'
           if ( .not. open3 (fname, FSNEW3, pname) ) then
              print *, ' Error: Could not open file ', fname
           end if
        end if
     end if

     if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
        if (twoway_mype == 0) then
           ncols3d = cmaq_c_col_dim
           nrows3d = cmaq_c_row_dim
           tstep3d = file_time_step
           if ( .not. open3 (pfname, FSRDWR3, pname) ) then
              print *, ' Error: Could not open file ', pfname, 'for update'
              if ( .not. open3 (pfname, FSNEW3, pname) ) then
                 print *, ' Error: Could not open file ', pfname
              end if
           end if
        end if
     end if
     file_opened = .true.
  end if

!-------------------------------------------------------------------------------
! Fill time-dependent arrays for METCRO2D.
!-------------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Fill friction velocity (USTAR), planetary boundary layer height (PBL),
  ! roughness length (ZRUF), inverse Monin-Obukhov length (MOLI), sensible
  ! heat flux (HFX), aerodynamic resistance (RA), stomatal resistance (RS),
  ! ground temperature (TEMPG), shortwave radiation (GSW), accumulated
  ! non-convective precipitation (RNA), accumulated convective precipitation
  ! (RCA), snow cover flag (SNOCOV), leaf area index (LAI), 2-m temperature
  ! (TEMP2), and canopy moisture content (WR) directly from WRF arrays.
  !
  ! Note:  RA and RS are the reciprocals of RADYNI and RSTOMI, respectively.
  !        RA and RS are directly available in WRF, and they are actually
  !        used in CMAQ, rather than the reciprocals RADYNI and RSTOMI, which
  !        are typically part of WRF output.
  !
  ! Note:  RNA and RCA are accumulated non-convective and convective precip,
  !        respectively.  RN and RC are MCIP "time step" buckets (typically
  !        hourly).  Can use RNA and RCA from current and previous CMAQ time
  !        steps in two-way model to compute rainfall rates needed in CMAQ.
  !
  ! Note:  For rainfall:  biogenics code uses cm/h, CMAQ ultimately needs mm/h.
  !-----------------------------------------------------------------------------
  metcro2d_data_wrf  (:,:,2) =  grid%ust   (tw_sc:tw_ec, tw_sr:tw_er)   ! ustar
  metcro2d_data_wrf  (:,:,4) =  grid%pblh  (tw_sc:tw_ec, tw_sr:tw_er)   ! pbl
  metcro2d_data_wrf  (:,:,5) =  grid%znt   (tw_sc:tw_ec, tw_sr:tw_er)   ! zruf
  metcro2d_data_wrf  (:,:,6) =  grid%rmol  (tw_sc:tw_ec, tw_sr:tw_er)   ! moli
  metcro2d_data_wrf  (:,:,7) =  grid%hfx   (tw_sc:tw_ec, tw_sr:tw_er)   ! hfx
  metcro2d_data_wrf  (:,:,8) =  grid%ra    (tw_sc:tw_ec, tw_sr:tw_er)   ! RA = 1/RADNYI
  metcro2d_data_wrf  (:,:,9) =  grid%rs    (tw_sc:tw_ec, tw_sr:tw_er)   ! RA = 1/RSTOMI
  metcro2d_data_wrf (:,:,11) =  grid%gsw   (tw_sc:tw_ec, tw_sr:tw_er)   ! gsw

  metcro2d_data_wrf (:,:,13) =  (grid%rainnc(tw_sc:tw_ec, tw_sr:tw_er) - grid%prev_rainnc(tw_sc:tw_ec,tw_sr:tw_er)) * 0.1  ! RNA = SUM(RN), in cm
  if (wrf_convective_scheme) then
     metcro2d_data_wrf (:,:,14) = (grid%rainc (tw_sc:tw_ec, tw_sr:tw_er) - grid%prev_rainc(tw_sc:tw_ec,tw_sr:tw_er)) * 0.1   ! RCA = SUM(RC), in cm
  else
     metcro2d_data_wrf (:,:,14) = 0.0
  end if

  metcro2d_data_wrf (:,:,19) =  grid%snowc (tw_sc:tw_ec, tw_sr:tw_er)           ! snowcov
  metcro2d_data_wrf (:,:,21) =  grid%t2    (tw_sc:tw_ec, tw_sr:tw_er)           ! temp2
  metcro2d_data_wrf (:,:,22) =  grid%canwat(tw_sc:tw_ec, tw_sr:tw_er) * 0.001   ! wr (in meter)
  metcro2d_data_wrf (:,:,23) =  grid%tsk   (tw_sc:tw_ec, tw_sr:tw_er)           ! tempg
  metcro2d_data_wrf (:,:,25) =  grid%isltyp(tw_sc:tw_ec, tw_sr:tw_er)           ! soil type
  metcro2d_data_wrf (:,:,26) =  grid%q2    (tw_sc:tw_ec, tw_sr:tw_er)           ! Q2
  metcro2d_data_wrf (:,:,27) =  grid%xice  (tw_sc:tw_ec, tw_sr:tw_er)           ! seaice
  metcro2d_data_wrf (:,:,28) =  grid%smois (tw_sc:tw_ec, 1, tw_sr:tw_er)        ! SOIM1
  metcro2d_data_wrf (:,:,29) =  grid%smois (tw_sc:tw_ec, 2, tw_sr:tw_er)        ! SOIM2
  metcro2d_data_wrf (:,:,30) =  grid%tslb  (tw_sc:tw_ec, 1, tw_sr:tw_er)        ! SOIT1
  metcro2d_data_wrf (:,:,31) =  grid%tslb  (tw_sc:tw_ec, 2, tw_sr:tw_er)        ! SOIT2

  metcro2d_data_wrf (:,:,32) =  grid%lh   (tw_sc:tw_ec, tw_sr:tw_er)            ! lh (qfx)

  metcro2d_data_wrf (:,:,33) =  grid%wwlt_px  (tw_sc:tw_ec, tw_sr:tw_er)        ! WWLT_PX
  metcro2d_data_wrf (:,:,34) =  grid%wfc_px   (tw_sc:tw_ec, tw_sr:tw_er)        ! WFC_PX
  metcro2d_data_wrf (:,:,35) =  grid%wsat_px  (tw_sc:tw_ec, tw_sr:tw_er)        ! WSAT_PX
  metcro2d_data_wrf (:,:,36) =  grid%clay_px  (tw_sc:tw_ec, tw_sr:tw_er)        ! CLAY_PX
  metcro2d_data_wrf (:,:,37) =  grid%csand_px (tw_sc:tw_ec, tw_sr:tw_er)        ! CSAND_PX
  metcro2d_data_wrf (:,:,38) =  grid%fmsand_px(tw_sc:tw_ec, tw_sr:tw_er)        ! FMSAND_PX


  where (metcro2d_data_wrf (:,:,13) .lt. 0.0)
    metcro2d_data_wrf (:,:,13) = 0.0
  end where

  where (metcro2d_data_wrf (:,:,14) .lt. 0.0)
    metcro2d_data_wrf (:,:,14) = 0.0
  end where

  !-----------------------------------------------------------------------------
  ! Assign surface pressure (PRSFC) from WRF array P8W (i.e., "p at w levels").
  !-----------------------------------------------------------------------------

  jj = tw_sr - 1
  DO r = 1, wrf_c_nrows
     jj = jj + 1
     ii = tw_sc - 1
     DO c = 1, wrf_c_ncols
        ii = ii + 1
        metcro2d_data_wrf(c,r,1) = p8w_wrf(ii,1,jj)   ! prsfc
     ENDDO
  ENDDO

  !-----------------------------------------------------------------------------
  ! Compute convective velocity scale (WSTAR) using a algorithm from MCIP.
  !
  ! Note:  KARMAN is defined in WRF module_model_constants.  It is the same
  !        value (0.4) that is used in MCIP.
  !-----------------------------------------------------------------------------

  jj = tw_sr - 1
  DO r = 1, wrf_c_nrows
     jj = jj + 1
     ii = tw_sc - 1
     DO c = 1, wrf_c_ncols
        ii = ii + 1
        IF ( grid%rmol(ii,jj) < 0.0 ) THEN
           ! wstart = ustar * (pbl * ABS(grid%rmol / karman )) ** 0.3333333
           metcro2d_data_wrf(c,r,3) = metcro2d_data_wrf(c,r,2) * ( metcro2d_data_wrf(c,r,4) * ABS(grid%rmol(ii, jj) / karman ) ) ** 0.3333333
        ELSE
           metcro2d_data_wrf(c,r,3) = 0.0
        END IF
     END DO
  END DO

  !-----------------------------------------------------------------------------
  ! Compute WSPD10 from WRF components U10 and V10.  WSPD10 should be on
  ! scalar points.  Assume here that U10 and V10 are on scalar points in WRF.
  !-----------------------------------------------------------------------------

  u10     (:,:) =  grid%u10   (tw_sc:tw_ec, tw_sr:tw_er)
  u10     (:,:) =  u10(:,:) * u10(:,:)
  v10     (:,:) =  grid%v10   (tw_sc:tw_ec, tw_sr:tw_er)
  v10     (:,:) =  v10(:,:) * v10(:,:)
  metcro2d_data_wrf (:,:,10) =  SQRT( u10(:,:) + v10(:,:) )   ! components already squared, wspd10

  !-----------------------------------------------------------------------------
  ! Compute solar radiation reaching the ground (RGRND) from ALBEDO and GSW.
  !
  ! Note:  RGRND may not be needed depending on how it is used by biogenics.
  !-----------------------------------------------------------------------------

  albedo  (:,:) =  grid%albedo(tw_sc:tw_ec, tw_sr:tw_er)
  metcro2d_data_wrf   (:,:,12) =  metcro2d_data_wrf(:,:,11) / (1.0 - albedo(:,:))

  !-----------------------------------------------------------------------------
  ! Get VEG and LAI from WRF dependent on WRF LSM option and WRF version number.
  ! Also if WRF VEGFRA is used, it's in percent. Convert to fraction.
  ! Also if PX MODIS version (WRFv4.1+) is used, add PX soil properties to MCIP
  ! file for updated dust model. If PX MODIS is not used set to missing value
  ! that will trigger old soil category based calculations in DUST_EMIS.F
  !-----------------------------------------------------------------------------
  if (config_flags%sf_surface_physics == 7) then
     metcro2d_data_wrf     (:,:,20) =  grid%vegf_px (tw_sc:tw_ec, tw_sr:tw_er)
  else
     metcro2d_data_wrf     (:,:,20) =  grid%vegfra (tw_sc:tw_ec, tw_sr:tw_er) * 0.01
  end if

  if(px_modis) then                                                         
     metcro2d_data_wrf (:,:,24) =  grid%lai_px(tw_sc:tw_ec, tw_sr:tw_er) 
     metcro2d_data_wrf (:,:,33) =  grid%wwlt_px  (tw_sc:tw_ec, tw_sr:tw_er)
     metcro2d_data_wrf (:,:,34) =  grid%wfc_px   (tw_sc:tw_ec, tw_sr:tw_er)
     metcro2d_data_wrf (:,:,35) =  grid%wsat_px  (tw_sc:tw_ec, tw_sr:tw_er)   
     metcro2d_data_wrf (:,:,36) =  grid%clay_px  (tw_sc:tw_ec, tw_sr:tw_er)
     metcro2d_data_wrf (:,:,37) =  grid%csand_px (tw_sc:tw_ec, tw_sr:tw_er)
     metcro2d_data_wrf (:,:,38) =  grid%fmsand_px(tw_sc:tw_ec, tw_sr:tw_er)
  else 
     metcro2d_data_wrf (:,:,24) =  grid%lai(tw_sc:tw_ec, tw_sr:tw_er)
     metcro2d_data_wrf (:,:,33) =  -9999.
     metcro2d_data_wrf (:,:,34) =  -9999.
     metcro2d_data_wrf (:,:,35) =  -9999.
     metcro2d_data_wrf (:,:,36) =  -9999.
     metcro2d_data_wrf (:,:,37) =  -9999.
     metcro2d_data_wrf (:,:,38) =  -9999.
  end if  

  !-----------------------------------------------------------------------------
  ! Compute total cloud fraction (CFRAC), cloud top layer height (CLDT), 
  ! cloud bottom layer height (CLDB), and average liquid water content of
  ! cloud (WBAR) using a modified MCIP subroutine.
  !-----------------------------------------------------------------------------

  CALL bcldprc_ak (wrf_c_ncols, wrf_c_nrows, nlays, zf, metcro3d_data_wrf(:,:,:,4),                   &
                   metcro3d_data_wrf(:,:,:,11), metcro3d_data_wrf(:,:,:,5), metcro2d_data_wrf(:,:,4), &
                   dzf, presf, metcro2d_data_wrf(:,:,15),                                             &
                   metcro2d_data_wrf(:,:,17), metcro2d_data_wrf(:,:,16), metcro2d_data_wrf(:,:,18))

  call se_wrf_cmaq_comm (twoway_mype, metcro2d_data_wrf, metcro2d_data_cmaq,       &
                         wrf_cmaq_ce_send_to, wrf_cmaq_ce_recv_from,                 &
                         wrf_cmaq_ce_send_index_l, wrf_cmaq_ce_recv_index_l, 5)

  temp_rainnc = temp_rainnc + metcro2d_data_cmaq(:,:,13)
  if (wrf_convective_scheme) then
     temp_rainc  = temp_rainc  + metcro2d_data_cmaq(:,:,14)
  end if

  if (wrf_cmaq_option .gt. 1) then
     if ( .not. buf_write3 (fname, allvar3, jdate, jtime, metcro2d_data_cmaq ) ) then
       print *, ' Error: Could not write to file ', fname
       stop
     end if
  end if
  if ((wrf_cmaq_option == 1) .or. (wrf_cmaq_option == 3)) then
     if (write_to_physical_file) then
        do v = 1, n_metcro2d_var   
           if (v == 13) then
              if ( .not. write3 (pfname, metcro2d_vlist(v), jdate, jtime, temp_rainnc(tsc_c:tec_c, tsr_c:ter_c) ) ) then
                 print *, ' Error: Could not write to file ', pfname
                 stop
              end if
           else if (v == 14) then
              if ( .not. write3 (pfname, metcro2d_vlist(v), jdate, jtime, temp_rainc(tsc_c:tec_c, tsr_c:ter_c) ) ) then
                 print *, ' Error: Could not write to file ', pfname
                 stop
              end if
           else
              if ( .not. write3 (pfname, metcro2d_vlist(v), jdate, jtime, metcro2d_data_cmaq(tsc_c:tec_c, tsr_c:ter_c, v) ) ) then
                 print *, ' Error: Could not write to file ', pfname
                 stop
              end if
           end if
        end do
        write_to_physical_file = .false.
        temp_rainnc = 0.0
        temp_rainc  = 0.0
     end if
  end if

  CALL NEXTIME( JDATE, JTIME, cmaq_tstep)

!-------------------------------------------------------------------------------

CONTAINS

SUBROUTINE aq_header (ncols, nrows, gncols, gnrows, nlays, sdate, stime, dx, dy,    &
                      delta_x, delta_y, map_proj, truelat1, truelat2, moad_cen_lat, &
                      cen_lon, stand_lon, ptop, znw, lat_llc, lon_llc, wrf_lc_ref_lat)

!-------------------------------------------------------------------------------
! Name:     AQ Header
! Purpose:  Fill M3IO header variables for CMAQ in WRF-CMAQ two-way system.
! Revised:  02 Apr 2007  Original version.  (T. Otte)
!           11 Apr 2007  (David Wong)
!              -- store info in variable ioapi_header
!-------------------------------------------------------------------------------

  USE twoway_header_data_module

  use utilio_defn

  IMPLICIT NONE

  REAL,          INTENT(IN)    :: dx                ! [m]
  REAL,          INTENT(IN)    :: dy                ! [m]
  REAL,          INTENT(IN)    :: lat_llc
  REAL,          INTENT(IN)    :: lon_llc
  INTEGER,       INTENT(IN)    :: map_proj, delta_x, delta_y
  REAL,          INTENT(IN)    :: moad_cen_lat
  REAL,          INTENT(IN)    :: cen_lon
  INTEGER,       INTENT(IN)    :: ncols, gncols
  INTEGER,       INTENT(IN)    :: nrows, gnrows
  INTEGER,       INTENT(IN)    :: nlays
  INTEGER,       INTENT(IN)    :: sdate
  INTEGER,       INTENT(IN)    :: stime
  REAL,          INTENT(IN)    :: ptop              ! model top [Pa]
  REAL,          INTENT(IN)    :: stand_lon
  REAL,          INTENT(IN)    :: truelat1
  REAL,          INTENT(IN)    :: truelat2
  REAL,          INTENT(IN)    :: znw        ( : )
  REAL,          INTENT(IN)    :: wrf_lc_ref_lat

  CHARACTER*16                 :: coordnam_gd
  INTEGER                      :: gdtyp_gd
  INTEGER                      :: kvert
  REAL(8)                      :: p_alp_gd
  REAL(8)                      :: p_bet_gd
  REAL(8)                      :: p_gam_gd
  REAL                         :: vgtop_gd
  CHARACTER*16                 :: vgtpun_gd
  REAL,          ALLOCATABLE   :: vglvs_gd   ( : )
  CHARACTER*16                 :: vglvun_gd
  REAL,          ALLOCATABLE   :: x3face_gd  ( : )
  REAL                         :: xorig             ! X-origin [m]
  REAL(8)                      :: xxx
  REAL                         :: yorig             ! Y-origin [m]
  REAL(8)                      :: yyy
  REAL                         :: ref_lat
  REAL                         :: cntrx, cntry
  REAL                         :: xtemp, ytemp

  INTEGER                      :: GRID_SIZE, DOMAIN_TYPE, STAT

  logical                      :: use_hard_code_values

!-------------------------------------------------------------------------------
! Define map projection identifiers for M3IO.
!-------------------------------------------------------------------------------

  ioapi_header%ncols = ncols
  ioapi_header%nrows = nrows
  ioapi_header%nlays = nlays
  ioapi_header%nthik = 1
  ioapi_header%sdate = sdate
  ioapi_header%stime = stime

  SELECT CASE ( map_proj )

    CASE (1)  ! Lambert conformal
      gdtyp_gd = lamgrd3  ! in PARMS3
      p_alp_gd = DBLE( MIN(truelat1, truelat2) )
      p_bet_gd = DBLE( MAX(truelat1, truelat2) )
      p_gam_gd = DBLE( stand_lon )

    CASE (2)  ! polar stereographic
      gdtyp_gd = polgrd3  ! in PARMS3
      p_alp_gd = DBLE( SIGN(1.0, moad_cen_lat) )
      p_bet_gd = DBLE( truelat1 )
      p_gam_gd = DBLE( stand_lon )

    CASE (3)  ! Mercator
      gdtyp_gd = EQMGRD3  ! in PARMS3
      p_alp_gd = 0.0
      p_bet_gd = 0.0
      p_gam_gd = DBLE( stand_lon )

  END SELECT

  ioapi_header%vtype = m3real

  ioapi_header%gdtyp = gdtyp_gd
  ioapi_header%p_alp = p_alp_gd
  ioapi_header%p_bet = p_bet_gd
  ioapi_header%p_gam = p_gam_gd

!-------------------------------------------------------------------------------
! Define grid location for M3IO.  Use center of projection as reference point
! for XCENT and YCENT.  Compute XORIG and YORIG with respect to XCENT and YCENT.
!
! Note:  After the call to LATLON_TO_XY, the REAL coordinates of the center of
!        the projection (XXX, YYY) may not be "exact".  Will need to adjust the
!        values of XORIG and YORIG such that they are multiples of 0.5*DX and
!        0.5*DY.  (This may not be a universal assumption, but it is a practical
!        assumption for two-way modeling...at least initially.)
!
! Note:  DX and DY are the same for our application.
!
! (XCENT_GD, YCENT_GD):
! For most projections, these are the longitude, -180 < X <= 180, and the
!   latitude, -90 <= Y <= 90, for the center of the grid's respective Cartesian
!   coordinate system.  Units are meters.
! For UTM:  UTM coordinates of the origin for offset UTM coordinates (or are
!           (0,0) for Equator-based UTM coordinates).
! For Lat-Lon:  unused
!-------------------------------------------------------------------------------

  IF ( map_proj == 1 ) THEN
     ioapi_header%xcent = stand_lon
     IF ( wrf_lc_ref_lat > 0.0) THEN
       ref_lat  = wrf_lc_ref_lat
     ELSE
       ref_lat  = ( truelat1 + truelat2 ) * 0.5
     ENDIF
     ioapi_header%ycent = ref_lat

     CALL ll2xy_lam (moad_cen_lat, cen_lon, truelat1, truelat2, stand_lon, ref_lat, xxx, yyy)

  ELSE IF ( map_proj == 2 ) THEN
     ioapi_header%xcent = stand_lon
     ioapi_header%ycent = moad_cen_lat
     CALL ll2xy_ps (moad_cen_lat, cen_lon, truelat1, cen_lon, xxx, yyy)
  ELSE IF ( map_proj == 3 ) THEN
     ioapi_header%xcent = stand_lon
     ioapi_header%ycent = 0.0
  ELSE
     write (6, *) ' Unknown projection '
  END IF

  nthik = 1
  cntrx = FLOAT(gncols - 1)/2.0 + 1.0
  cntry = FLOAT(gnrows - 1)/2.0 + 1.0

  xorig = xxx - DBLE( cntrx - FLOAT(delta_x+nthik) ) * DBLE(dx)
  yorig = yyy - DBLE( cntry - FLOAT(delta_y+nthik) ) * DBLE(dy)
! xorig = xxx - DBLE( cntrx - 0.5 ) * DBLE(dx)
! yorig = yyy - DBLE( cntry - 0.5 ) * DBLE(dy)

! IF ( wrf_lc_ref_lat > -999.0 ) THEN  ! adjust XORIG and YORIG
  IF ( moad_cen_lat > -999.0 ) THEN  ! adjust XORIG and YORIG

    xtemp = xorig / 5.0
    ytemp = yorig / 5.0
    xtemp = FLOAT(NINT(xtemp))
    ytemp = FLOAT(NINT(ytemp))
    xorig = xtemp * 5.0
    yorig = ytemp * 5.0

  ENDIF

  ioapi_header%xorig = xorig
  ioapi_header%yorig = yorig

!-------------------------------------------------------------------------------
! Fill horizontal grid spacing.
!-------------------------------------------------------------------------------

  ioapi_header%xcell = dx
  ioapi_header%ycell = dy

!-------------------------------------------------------------------------------
! Define vertical grid.
!-------------------------------------------------------------------------------

  ioapi_header%vgtyp = vgwrfem  ! in PARMS3

  ioapi_header%vgtop = ptop

  ioapi_header%vglvs(1:nlays+1) = znw
! ioapi_header%vglvs(1:nlays) = znw
! ioapi_header%vglvs(nlays+1) = 0.0

!-------------------------------------------------------------------------------
! Define other identifiers.
!-------------------------------------------------------------------------------

  ioapi_header%gdnam = "WRF-CMAQ"

END SUBROUTINE aq_header

!-------------------------------------------------------------------------------

SUBROUTINE ll2xy_lam (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)

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
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          INTENT(IN)    :: lambda  ! longitude [deg]
  REAL,          INTENT(IN)    :: lambda0 ! standard longitude [deg]
  REAL,          INTENT(IN)    :: phi     ! latitude [deg]
  REAL,          INTENT(IN)    :: phi0    ! reference latitude [deg]
  REAL,          INTENT(IN)    :: phi1    ! true latitude 1 [deg]
  REAL,          INTENT(IN)    :: phi2    ! true latitude 2 [deg]
  REAL(8),       INTENT(OUT)   :: xx      ! X-coordinate from origin
  REAL(8),       INTENT(OUT)   :: yy      ! Y-coordinate from origin

  REAL,          PARAMETER     :: phitol  = 0.001  ! tolerance [deg]

  CHARACTER*16,  PARAMETER     :: pname   = 'LL2XY_LAM'

!-------------------------------------------------------------------------------
! Determine whether Lambert conformal is tangent or secant.
!-------------------------------------------------------------------------------

  IF ( ABS( phi1 - phi2 ) < phitol ) THEN  ! tangent case
    WRITE (6,9000) phi1, phi2
    PRINT *, 'Error: Lambert conformal is tangent '
    STOP
  ELSE  ! secant case
    CALL ll2xy_lam_sec (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                                  &
              /, 1x, '*** SUBROUTINE: LL2XY_LAM',                              &
              /, 1x, '***   TANGENT LAMBERT CONFORMAL PROJECTION DETECTED',    &
              /, 1x, '***   TRUE LATITUDES = ', f8.3, 2x, f8.3,                &
              /, 1x, '***   MAY NOT WORK PROPERLY IN SPATIAL ALLOCATOR',       &
              /, 1x, '***   ...PLEASE SUBMIT BUGZILLA TICKET TO INVESTIGATE',  &
              /, 1x, 70('*'))

END SUBROUTINE ll2xy_lam

!-------------------------------------------------------------------------------

SUBROUTINE ll2xy_lam_sec (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Lambert Conformal Projection
! Purpose:  Calcluates (X,Y) from origin for a given latitude-longitude pair
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
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          INTENT(IN)    :: lambda  ! longitude [deg]
  REAL,          INTENT(IN)    :: lambda0 ! standard longitude [deg]
  REAL,          INTENT(IN)    :: phi     ! latitude [deg]
  REAL,          INTENT(IN)    :: phi0    ! reference latitude [deg]
  REAL,          INTENT(IN)    :: phi1    ! true latitude 1 [deg]
  REAL,          INTENT(IN)    :: phi2    ! true latitude 2 [deg]
  REAL(8),       INTENT(OUT)   :: xx      ! X-coordinate from origin
  REAL(8),       INTENT(OUT)   :: yy      ! Y-coordinate from origin

  REAL, PARAMETER              :: rearth = 6370000.0  ! [m]

  REAL(8)                      :: deg2rad ! convert degrees to radians
  REAL(8)                      :: dlambda ! delta lambda
  REAL(8)                      :: drearth ! double-precision radius of earth [m]
  REAL(8)                      :: phirad  ! latitude [rad]
  REAL(8)                      :: phi0rad ! reference latitude [rad]
  REAL(8)                      :: phi1rad ! true latitude 1 [rad]
  REAL(8)                      :: phi2rad ! true latitude 2 [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover4 ! pi/4
  REAL(8)                      :: psi     ! auxiliary function
  REAL(8)                      :: rho     ! polar radius to latitude phi
  REAL(8)                      :: rho0    ! polar radius to origin
  REAL(8)                      :: term
  REAL(8)                      :: term0
  REAL(8)                      :: term1
  REAL(8)                      :: term2
  REAL(8)                      :: theta   ! polar angle
  REAL(8)                      :: sinphi0 ! cone constant

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = DATAN(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

  drearth = DBLE(rearth)

!-------------------------------------------------------------------------------
! Compute cone constant, SINPHI0.
! Note:  PHI0 is the reference latitude, which is user-defined.  It is NOT
!        used in the calculation of SINPHI0, which is the cone constant.
!-------------------------------------------------------------------------------

  phi0rad = DBLE(phi0) * deg2rad  ! convert PHI0 from degrees to radians
  phi1rad = DBLE(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phi2rad = DBLE(phi2) * deg2rad  ! convert PHI2 from degrees to radians

  term0 = DTAN (piover4 - phi0rad/2.0d0)
  term1 = DTAN (piover4 - phi1rad/2.0d0)
  term2 = DTAN (piover4 - phi2rad/2.0d0)

  sinphi0 = DLOG ( DCOS(phi1rad) / DCOS(phi2rad) )
  sinphi0 = sinphi0 / DLOG (term1 / term2)

!-------------------------------------------------------------------------------
! Compute polar angle, THETA.
!-------------------------------------------------------------------------------

  dlambda = DBLE(lambda - lambda0) * deg2rad
  theta   = dlambda * sinphi0

!-------------------------------------------------------------------------------
! Compute polar radius to origin, RHO0, where origin is at PHI0.
!-------------------------------------------------------------------------------

  psi  = drearth * DCOS(phi1rad) / sinphi0 / (term1**sinphi0)
  rho0 = psi * (term0**sinphi0)

!-------------------------------------------------------------------------------
! Compute polar radius to latitude PHI, RHO.
!-------------------------------------------------------------------------------

  phirad = DBLE(phi) * deg2rad  ! convert PHI from degrees to radians
  term   = DTAN (piover4 - phirad/2.0d0)
  rho    = psi * (term**sinphi0)

!-------------------------------------------------------------------------------
! Compute Cartesian coordinates, XX and YY.
!-------------------------------------------------------------------------------

  xx =        rho * DSIN(theta)
  yy = rho0 - rho * DCOS(theta)

END SUBROUTINE ll2xy_lam_sec

SUBROUTINE ll2xy_ps (phi, lambda, phi1, lambda0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Polar Stereographic Projection
! Purpose:  Calcluates (X,Y) from origin for a given latitude-longitude pair
!           and polar stereographic projection information.
! Notes:    Adapted from equations found at http://starbase.jpl.nasa.gov/
!           mgn-v-rdrs-5-dvdr-v1.0/gvdr0001/catalog/dsmp.lbl.
! Revised:  28 Sep 2009  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          INTENT(IN)    :: lambda  ! longitude [deg]
  REAL,          INTENT(IN)    :: lambda0 ! standard longitude [deg]
  REAL,          INTENT(IN)    :: phi     ! latitude [deg]
  REAL,          INTENT(IN)    :: phi1    ! true latitude 1 [deg]
  REAL(8),       INTENT(OUT)   :: xx      ! X-coordinate from origin
  REAL(8),       INTENT(OUT)   :: yy      ! Y-coordinate from origin

  REAL, PARAMETER              :: rearth = 6370000.0  ! [m]

  REAL(8)                      :: deg2rad ! convert degrees to radians
  REAL(8)                      :: drearth ! earth radius [m]
  REAL(8)                      :: hemi    ! +/-1 for Northern/Southern Hemis
  REAL(8)                      :: phirad  ! latitude [rad]
  REAL(8)                      :: phi1rad ! true latitude 1 [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover4 ! pi/4
  REAL(8)                      :: scalefac
  REAL(8)                      :: sigma   ! image scale
  REAL(8)                      :: theta   ! polar angle
  REAL(8)                      :: tt

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = DATAN(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

  drearth = DBLE(rearth)

!-------------------------------------------------------------------------------
! Compute image scale, SIGMA.
!-------------------------------------------------------------------------------

  hemi = DSIGN (1.0d0, DBLE(phi1))

  phi1rad = DBLE(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phirad  = DBLE(phi)  * deg2rad  ! convert PHI  from degrees to radians

!!!TLO  sigma   = (1.0d0 + DSIN(phi1rad)) / (1.0d0 + DSIN(pi))  ! at pole
  sigma   = (1.0d0 + DSIN(phi1rad)) / 2.0d0 * hemi

  scalefac = drearth / sigma

  tt = DTAN ( piover4 - phirad/2.0d0)

!-------------------------------------------------------------------------------
! Compute polar angle, THETA.
!-------------------------------------------------------------------------------

  theta = DBLE(lambda - lambda0) * deg2rad

!-------------------------------------------------------------------------------
! Compute Cartesian coordinates, XX and YY.
!-------------------------------------------------------------------------------

  xx =         2.0d0 * scalefac * tt * DSIN(theta)
  yy = -hemi * 2.0d0 * scalefac * tt * DCOS(theta)

END SUBROUTINE ll2xy_ps

END SUBROUTINE aqprep
