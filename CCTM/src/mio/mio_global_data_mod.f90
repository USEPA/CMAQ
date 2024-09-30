! Purpose: Define variables which are sharable among mio routines.

      module mio_global_data_module

        use mio_type_def_module

        implicit none

        integer :: mio_nfiles           ! total number of files
        integer :: mio_n_infiles        ! total number of input files
        integer :: mio_n_outfiles       ! total number of output files
        integer :: mio_cfile            ! current file pointer

        integer :: mio_mype             ! mype
        integer :: mio_mype_p1          ! mype + 1
        logical :: mio_io_pe_inclusive  ! an indicator of a PE handles output

        integer :: mio_logdev           ! log device number

        integer :: mio_parallelism      ! indicate I/O parallelism implemmentation

        integer :: mio_base_ncols   
        integer :: mio_base_nrows       ! # of columns and rows in base domain

        integer :: mio_nprocs           ! total # allocated processors
        integer :: mio_npcol            ! # allocated processor along column dimension
        integer :: mio_nprow            ! # allocated processor along row dimension

! mio_file_data will be allocated based on number of pre-defined number
! of input and output files. It also assumes that once a file open, it
! won't be closed until the very end.
        type(mio_file_record), allocatable :: mio_file_data(:)

! to store output file variable information defined in file_input.txt
        type(mio_outfile_def_record) :: mio_outfile_def_info

! for mpas
        character (len = 1000) :: mio_mpas_dmap_file

        integer, allocatable :: mio_mpas_dmap(:,:)

! Once mio_setfile is called the following 8 variables w.r.t. the file are available 
        integer :: mio_gl_ncols, mio_gl_nrows   ! # of columns and rows in global domain
        integer :: mio_ncols, mio_nrows         ! # of columns and rows in each processor
        integer :: mio_nbase_vars               ! # of base variables, which includes
                                                ! time variable and variable in MPAS that 
                                                ! defines a mesh
        integer :: mio_nlays                    ! # of layers
        integer :: mio_nvars                    ! # of varibles exclude time and basic MPAS variables 
        integer :: mio_time_var_ind             ! time dimension variable index
        integer :: mio_bndy_var_ind             ! boundary dimension variable index

      end module mio_global_data_module
