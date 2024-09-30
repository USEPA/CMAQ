! Purpose: Setup constant values

      module mio_parameter_module

        use netcdf

        implicit none

! data type
        integer, parameter :: mio_byte   = 1
        integer, parameter :: mio_char   = 2
        integer, parameter :: mio_short  = 3
        integer, parameter :: mio_int    = 4
        integer, parameter :: mio_real   = 5
        integer, parameter :: mio_double = 6

! I/O mode
        integer, parameter :: mio_read_only  = 1
        integer, parameter :: mio_read_write = 2
        integer, parameter :: mio_new        = 3

! netCDF file format
        integer, parameter :: mio_ioapi3_format = 1
        integer, parameter :: mio_wrf_format    = 2
        integer, parameter :: mio_mpas_format   = 3
        integer, parameter :: mio_epa_format    = 4

! parallelism
        integer, parameter :: mio_serial        = 1
        integer, parameter :: mio_pseudo        = 2
        integer, parameter :: mio_true_parallel = 3

! character string parameter
        integer, parameter :: mio_max_global_att_extra   = 100
        integer, parameter :: mio_max_num_att            = 20
        integer, parameter :: mio_max_num_var_att        = 10
        integer, parameter :: mio_max_att_value_char_len = 500
        integer, parameter :: mio_max_str_len            = 256
!       integer, parameter :: mio_max_file_full_name_len = 512
        integer, parameter :: mio_max_filename_len       = 512
        integer, parameter :: mio_max_varname_len        = 64
        integer, parameter :: mio_max_time_length        = 64    ! WRF is 19 and MPAS is 64
        integer, parameter :: mio_ioapi_3_str_len        = 16
        integer, parameter :: mio_max_outfile_def_list   = 500   ! max number of outfile definitions
        integer, parameter :: mio_preset_num_tsteps      = 100   ! pre-set number of time step in timestamp
        integer, parameter :: mio_max_att_cval_len       = 80000 ! max attribute cval length

        integer, parameter :: mio_iunit = 168   ! file that contains input and output data information

        integer, parameter :: mio_n_basic_mpas_ivars = 4
        integer, parameter :: mio_n_basic_mpas_dvars = 5
        integer, parameter :: mio_n_basic_mpas_vars = mio_n_basic_mpas_ivars + mio_n_basic_mpas_dvars

! MPAS use

        character (20), dimension(mio_n_basic_mpas_vars), parameter ::    &
          mio_basic_mpas_vnames_l = [ character(20)                 ::    &
             'indextocellid',                                             &
             'nedgesoncell',                                              &
             'indextovertexid',                                           &
             'verticesoncell',                                            &
             'latcell',                                                   &
             'loncell',                                                   &
!            'areacell',                                                  &
             'latvertex',                                                 &
             'lonvertex',                                                 &
             'zgrid'                                                      &
                                    ]

        character (20), dimension(mio_n_basic_mpas_vars), parameter ::    &
          mio_basic_mpas_vnames = [ character(20)                   ::    &
             'indexToCellID',                                             &
             'nEdgesOnCell',                                              &
             'indexToVertexID',                                           &
             'verticesOnCell',                                            &
             'latCell',                                                   &
             'lonCell',                                                   &
!            'areaCell',                                                  &
             'latVertex',                                                 &
             'lonVertex',                                                 &
             'Zgrid'                                                      &
                                  ]

        character (20), parameter :: mio_basic_mpas_vnames_dim(2, mio_n_basic_mpas_vars) = &
          reshape ((/ 'nCells       ',     '-            ',               &
                      'nCells       ',     '-            ',               &
                      'nVertices    ',     '-            ',               &
                      'maxEdges     ',     'nCells       ',               &
                      'nCells       ',     '-            ',               &
                      'nCells       ',     '-            ',               &
!                     'nCells       ',     '-            ',               &
                      'nVertices    ',     '-            ',               &
                      'nVertices    ',     '-            ',               &
                      'nVertLevelsP1',     'nCells       '                &
                    /), (/2, mio_n_basic_mpas_vars/))

      end module mio_parameter_module
