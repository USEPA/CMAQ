!===============================================================================
! Purpose:  Define general data structure for the twoway model
!
! Revised:  April 2007  Original version.  David Wong
! Revised:  April 7, 2016 David Wong: Added variable mminlu
!           Jan 11, 2018 David Wong: Added variable convective_scheme
!===============================================================================

  module twoway_data_module

    CHARACTER (LEN = 40) :: mminlu

    INTEGER, SAVE :: num_land_cat
    INTEGER, SAVE :: twoway_mype, nprocs

    INTEGER, SAVE :: wrf_c_ncols, wrf_c_nrows, cmaq_c_ncols, cmaq_c_nrows
    INTEGER, SAVE :: wrf_d_ncols, wrf_d_nrows, cmaq_d_ncols, cmaq_d_nrows

    INTEGER, ALLOCATABLE, SAVE :: wrf_c_domain_map (:, :, :)
    INTEGER, ALLOCATABLE, SAVE :: wrf_d_domain_map (:, :, :)
    INTEGER, ALLOCATABLE, SAVE :: cmaq_c_domain_map (:, :, :)
    INTEGER, ALLOCATABLE, SAVE :: cmaq_d_domain_map (:, :, :)
    INTEGER, ALLOCATABLE, SAVE :: cmaq_ce_domain_map (:, :, :)
    INTEGER, ALLOCATABLE, SAVE :: cmaq_de_domain_map (:, :, :)

    integer :: cmaq_c_col_dim
    integer :: cmaq_c_row_dim
    integer :: wrf_c_col_dim
    integer :: wrf_c_row_dim
    integer :: delta_x, delta_y

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

    INTEGER, SAVE :: sc, ec, sr, er
    INTEGER, SAVE :: sc_d, ec_d, sr_d, er_d

    LOGICAL :: convective_scheme

  end module twoway_data_module
