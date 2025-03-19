!===============================================================================
! Purpose:  Define general data structure for the twoway model
!
! Revised:  April 2007  Original version.  David Wong
! Revised:  April 7, 2016 David Wong: Added variable mminlu
! Revised:  Jan. 11, 2018 David Wong: Added variable convective_scheme
!           31 Jan 2019  (David Wong)
!              -- adopted the idea to process all twoway related environment
!                 variables in one place
!           01 Aug 2019  (David Wong)
!              -- renamed convective_scheme to wrf_convective_scheme
!           26 Jul 2022  (David Wong)
!              -- Added a prefix tw_ for these variables: sc, ec, sr, er sc_d, ec_d,
!                 sr_d, and er_d to avoid naming conflicts
!===============================================================================

  module twoway_data_module

    LOGICAL :: indirect_effect

    CHARACTER (LEN = 40) :: mminlu

    INTEGER :: num_land_cat
    INTEGER :: twoway_mype, twoway_nprocs

    INTEGER :: wrf_c_ncols, wrf_c_nrows, cmaq_c_ncols, cmaq_c_nrows
    INTEGER :: wrf_d_ncols, wrf_d_nrows, cmaq_d_ncols, cmaq_d_nrows

    INTEGER, ALLOCATABLE :: wrf_c_domain_map (:, :, :)
    INTEGER, ALLOCATABLE :: wrf_d_domain_map (:, :, :)
    INTEGER, ALLOCATABLE :: cmaq_c_domain_map (:, :, :)
    INTEGER, ALLOCATABLE :: cmaq_d_domain_map (:, :, :)
    INTEGER, ALLOCATABLE :: cmaq_ce_domain_map (:, :, :)
    INTEGER, ALLOCATABLE :: cmaq_de_domain_map (:, :, :)

    integer :: cmaq_c_col_dim
    integer :: cmaq_c_row_dim
    integer :: wrf_c_col_dim
    integer :: wrf_c_row_dim
    integer :: delta_x, delta_y
!   integer :: wrf_cmaq_freq
    integer :: cmaq_sdate, cmaq_stime, file_time_step

! cmaq_c stands for cmaq cross grid
! cmaq_d stands for cmaq dot grid
! cmaq_ce stands for cmaq cross grid extension, +1 in four directions
! cmaq_de stands for cmaq dot grid extension, +1 in four directions

    integer, pointer :: wrf_cmaq_c_send_to (:,:), wrf_cmaq_c_recv_from (:,:)
    integer, pointer :: wrf_cmaq_c_send_index_g (:,:,:), wrf_cmaq_c_recv_index_g (:,:,:)
    integer, pointer :: wrf_cmaq_c_send_index_l (:,:,:), wrf_cmaq_c_recv_index_l (:,:,:)
    integer, pointer :: wrf_cmaq_d_send_to (:,:), wrf_cmaq_d_recv_from (:,:)
    integer, pointer :: wrf_cmaq_d_send_index_g (:,:,:), wrf_cmaq_d_recv_index_g (:,:,:)
    integer, pointer :: wrf_cmaq_d_send_index_l (:,:,:), wrf_cmaq_d_recv_index_l (:,:,:)
    integer, pointer :: wrf_cmaq_ce_send_to (:,:), wrf_cmaq_ce_recv_from (:,:)
    integer, pointer :: wrf_cmaq_ce_send_index_g (:,:,:), wrf_cmaq_ce_recv_index_g (:,:,:)
    integer, pointer :: wrf_cmaq_ce_send_index_l (:,:,:), wrf_cmaq_ce_recv_index_l (:,:,:)
    integer, pointer :: wrf_cmaq_de_send_to (:,:), wrf_cmaq_de_recv_from (:,:)
    integer, pointer :: wrf_cmaq_de_send_index_g (:,:,:), wrf_cmaq_de_recv_index_g (:,:,:)
    integer, pointer :: wrf_cmaq_de_send_index_l (:,:,:), wrf_cmaq_de_recv_index_l (:,:,:)

    integer, pointer :: cmaq_wrf_c_send_to (:,:), cmaq_wrf_c_recv_from (:,:)
    integer, pointer :: cmaq_wrf_c_send_index_g (:,:,:), cmaq_wrf_c_recv_index_g (:,:,:)
    integer, pointer :: cmaq_wrf_c_send_index_l (:,:,:), cmaq_wrf_c_recv_index_l (:,:,:)

    INTEGER :: tw_sc, tw_ec, tw_sr, tw_er
    INTEGER :: tw_sc_d, tw_ec_d, tw_sr_d, tw_er_d

    real :: WRF_LC_REF_LAT

    logical :: wrf_convective_scheme,     &
               cmaq_wrf_feedback,         &    ! flag to indicate CMAQ provides aerosol information back to WRF
               sd_time_series,            &
!              create_physical_file,      &
!              run_cmaq_driver,           &
!              wrf_restart,               &
               turn_on_pv

    logical :: wrf_lightning_assim = .false.

    character (len = 16)  :: grid_name_str
    character (len = 500) :: griddesc_fname

  end module twoway_data_module
