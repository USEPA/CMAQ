       module coupler_module

         use mydata_module

         implicit none

! for 2d variable
         integer, parameter :: n2d_data   = 35

         integer, parameter :: cfrac2dr_ind =  1
         integer, parameter :: chlo_ind     =  2
         integer, parameter :: dms_ind      =  3
         integer, parameter :: hfx_ind      =  4
         integer, parameter :: ht_ind       =  5
         integer, parameter :: lai_ind      =  6
         integer, parameter :: lat_ind      =  7
         integer, parameter :: lh_ind       =  8
         integer, parameter :: lon_ind      =  9
         integer, parameter :: lwmask_ind   = 10
         integer, parameter :: open_ind     = 11
         integer, parameter :: pbl_ind      = 12
         integer, parameter :: prsfc_ind    = 13
         integer, parameter :: purb_ind     = 14
         integer, parameter :: q2_ind       = 15
         integer, parameter :: ra_ind       = 16     ! aerodynamic resistance
         integer, parameter :: rainc_ind    = 17     ! time-step convective precipitation
         integer, parameter :: rgrnd_ind    = 18
         integer, parameter :: rainnc_ind   = 19     ! time-step total grid-scale precipitation
         integer, parameter :: rs_ind       = 20     ! surface resistance
         integer, parameter :: seaice_ind   = 21
         integer, parameter :: sltyp_ind    = 22
         integer, parameter :: snocov_ind   = 23
         integer, parameter :: soit1_ind    = 24
         integer, parameter :: surf_ind     = 25
         integer, parameter :: temp2_ind    = 26
         integer, parameter :: tempg_ind    = 27
         integer, parameter :: ustar_ind    = 28
         integer, parameter :: vegpx_ind    = 29
         integer, parameter :: canwat_ind   = 30 
         integer, parameter :: wspd10_ind   = 31     ! 2m wind speed
         integer, parameter :: znt_ind      = 32
         integer, parameter :: cellArea_ind = 33     ! cell area, m**2
         integer, parameter :: cfrac2dt_ind = 34
         integer, parameter :: rmol_ind     = 35

! alphabetical order and upper case letter goes first than lower case letter
         character (20), parameter :: vname_2d(n2d_data) =                       &   ! in ascending order
            (/ 'CFRAC   ',  'CHLO    ',  'DMS     ',  'HFX     ',  'HT      ',   &
               'LAI     ',  'LAT     ',  'LH      ',  'LON     ',  'LWMASK  ',   &
               'OPEN    ',  'PBL     ',  'PRSFC   ',  'PURB    ',  'Q2      ',   &
               'RA      ',  'RC      ',  'RGRND   ',  'RN      ',  'RS      ',   &
               'SEAICE  ',  'SLTYP   ',  'SNOCOV  ',  'SOIT1   ',  'SURF    ',   &
               'TEMP2   ',  'TEMPG   ',  'USTAR   ',  'VEG     ',  'WR      ',   &
               'WSPD10  ',  'ZRUF    ',  'cellArea',  'cfrac2dt',  'rmol    '    &
             /)

! for 3d variable
         integer, parameter :: n3d_data     = 18

         integer, parameter :: cfrac3d_ind    =  1
         integer, parameter :: dens_ind       =  2
         integer, parameter :: densa_j_ind    =  3
         integer, parameter :: pres_ind       =  4
         integer, parameter :: qc_ind         =  5
         integer, parameter :: qg_ind         =  6
         integer, parameter :: qi_ind         =  7
         integer, parameter :: qr_ind         =  8
         integer, parameter :: qs_ind         =  9
         integer, parameter :: qv_ind         = 10
         integer, parameter :: temp_ind       = 11
         integer, parameter :: wspd_ind       = 12
         integer, parameter :: zf_ind         = 13
         integer, parameter :: zh_ind         = 14
         integer, parameter :: cldfracwcu_ind = 15
         integer, parameter :: eddy_ind       = 16
         integer, parameter :: qc_cu_ind      = 17
         integer, parameter :: qi_cu_ind      = 18

! alphabetical order and upper case letter goes first than lower case letter
         character (20), parameter :: vname_3d(n3d_data) =                &
            (/ 'CFRAC_3D  ',  'DENS      ',   'DENSA_J   ',  'PRES      ',  'QC        ',          &
               'QG        ',        'QI        ',     'QR        ',       'QS        ',    'QV        ',          &
               'TA        ',        'WSPD      ',   'ZF        ',       'ZH        ',    'cldfracwcu',  &
               'eddy      ',      'qc_cu     ',  'qi_cu     '                             &
             /)

! for cgrid data, CMAQ v53
         integer, parameter :: my_n_gc_cb6r3m_ae7 = 127
         integer, parameter :: my_n_ae_cb6r3m_ae7 =  84
         integer, parameter :: my_n_nr_cb6r3m_ae7 =  11

         integer, parameter :: num_cmaq_species_cb6r3m_ae7 =   &
                                  my_n_gc_cb6r3m_ae7           &
                                + my_n_ae_cb6r3m_ae7           &
                                + my_n_nr_cb6r3m_ae7

         integer, parameter :: my_n_gc_cb6r3m_ae7_aq = 162
         integer, parameter :: my_n_ae_cb6r3m_ae7_aq =  86
         integer, parameter :: my_n_nr_cb6r3m_ae7_aq =  11

         integer, parameter :: num_cmaq_species_cb6r3m_ae7_aq =  &
                                  my_n_gc_cb6r3m_ae7_aq          &
                                + my_n_ae_cb6r3m_ae7_aq          &
                                + my_n_nr_cb6r3m_ae7_aq

         integer :: num_cmaq_species

         integer  :: my_gc_adj
         integer  :: my_ae_adj
         integer  :: my_nr_adj

         real, allocatable :: g2ddata(:,:,:)
         real, allocatable :: g3ddata(:,:,:,:)
         real, allocatable :: lufrac_data(:,:)
         real, allocatable :: smois_data(:,:,:)                  ! surface layer
         real, allocatable :: cmaq_species(:,:,:,:)
         real, allocatable :: cell_area(:,:)                   ! cell area
         real, allocatable :: cell_vol(:,:,:)                  ! cell volume
         real, allocatable :: inv_cell_vol(:,:,:)              ! reciprical of cell volume
         real, allocatable :: cell_thickness(:,:,:)            ! cell thickness full level
         real, allocatable :: inv_cell_thickness(:,:,:)        ! reciprical of cell thickness
         real, allocatable :: inv_mlvl_cell_thickness(:,:,:)   ! reciprical of cell thickness mid level
!        real, allocatable :: gc_species(:,:,:,:)
!        real, allocatable :: ae_species(:,:,:,:)
!        real, allocatable :: nr_species(:,:,:,:)
!        real, allocatable :: tr_species(:,:,:,:)

         real, allocatable :: my_emis_buffer (:, :, :, :, :)

         integer :: my_emis_buffer_ind, my_emis_tstep 

         logical :: mpas_cmaq_last_step = .false.
         logical :: mpas_diag

         character (16) :: mminlu_mpas

         character (19) :: ctm_out_clock

       end module coupler_module
