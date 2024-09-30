! Purpose: Define a data structure to hold various information about a file.

      module mio_type_def_module

        use netcdf
        use mio_parameter_module

        implicit none

        type mio_file_record
          character (mio_max_filename_len)   :: filename            ! file logical name
          character (mio_max_filename_len)   :: full_filename       ! full file name
          integer                            :: file_format         ! ioapi3 = 1, wrf = 2, mpas = 3
          character (1)                      :: grid_type           ! grid type: c (cross), d (dot), b (boundary),
                                                                    ! m (IOAPI3 stack group data for MPAS)
          integer                            :: link                ! pointer to the same physical file, -1 means
                                                                    ! unique file
          integer                            :: mode                ! i/o mode (r, rw, new)
          integer                            :: fileid              ! file id, -1 denotes is closed
          integer                            :: unlimited           ! dimension # with unlimited characteristic
          integer                            :: nsteps              ! number of time steps
          integer                            :: time_strlen_dim_loc ! location of the time strlen dimension
          integer                            :: time_dim_loc        ! location of time dimension
          integer                            :: layer_dim_loc       ! location of layer dimension
          integer, allocatable               :: tflag(:,:)          ! IOAPI-3 time stamp info
          integer                            :: tstep               ! time step size, only for IOAPI3
          character (mio_max_time_length), allocatable            & ! time stamp info
                                             :: timestamp(:)
          integer                            :: gl_ncols            ! global number of columns in the file
          integer                            :: gl_nrows            ! global number of rows in the file
          integer                            :: ncols               ! local number of columns
          integer                            :: nrows               ! local number of rows
          integer                            :: nlays               ! number of vertical layers
          integer                            :: nbndy_cells         ! number of boundary cells
          integer                            :: bndy_thickness      ! boundary thickness

! dimension section
          integer                            :: ndims               ! num of dimensions
          character (mio_max_varname_len), allocatable            &
                                             :: dim_name(:)         ! dimension name
          integer, allocatable               :: dim_value(:)        ! dimension value

! variable section
          integer                            :: nvars               ! num of variables w/o time step variable
                                                                    ! (w/o basic MPAS variables as well)
          integer                            :: nbvars              ! num of basic variables which are on the list
          integer                            :: fnvars              ! num of variables in the file
          logical, allocatable               :: var_time_dep(:)     ! variable time dependent or not
          character (mio_max_varname_len), allocatable            &
                                             :: var_name(:)         ! variable name
          character (mio_max_varname_len), allocatable            &
                                             :: lvar_name(:)        ! lower case variable name for MPAS basic variable comparison
          logical, allocatable               :: var_decomp(:)       ! variable decomposable or not
          character (mio_max_varname_len), allocatable            &
                                             :: units(:)            ! variable units
          integer, allocatable               :: var_type(:)         ! variable type, 1 = int, 2 = real, etc
          character, allocatable             :: var_grid_type(:)    ! c = cross, d = dot, ' ' = none
          integer, allocatable               :: var_id(:)           ! variable id
          integer, allocatable               :: var_ndims(:)        ! num of dimension of a variable
          integer, allocatable               :: var_dimids(:,:)     ! variable dimension id
          integer, allocatable               :: var_dimsize(:,:)    ! variable dimension size
          integer, allocatable               :: num_var_att(:)      ! num of variable attributes
          character (mio_max_varname_len), allocatable            &
                                             :: var_att_name(:,:)   ! variable attribute name (mio_max_num_var_att, fnvars)
          integer, allocatable               :: var_att_len(:,:)    ! variable attribute size
          integer, allocatable               :: var_att_type(:,:)   ! variable attribute type (same as var type)
          integer, allocatable               :: int_vatt_val(:,:)   ! value of integer type variable attribute
          real, allocatable                  :: real_vatt_val(:,:)  ! value of real type variable attribute
          character (mio_max_str_len), allocatable                &
                                             :: char_vatt_val(:,:)  ! value of character type variable attribute

! global attribute section
          integer                            :: n_global_atts       ! num of global attributes
          character (mio_max_varname_len), allocatable            &
                                             :: glo_att_name(:)     ! global attribute name
          integer, allocatable               :: glo_att_type(:)     ! global attribtue type
          integer, allocatable               :: glo_att_len(:)      ! global attribute size
          integer, allocatable               :: glo_att_crange(:)   ! starting and ending index of glo_att_cval
          integer, allocatable               :: glo_att_irange(:)   ! starting and ending index of glo_att_ival
          integer, allocatable               :: glo_att_rrange(:)   ! starting and ending index of glo_att_rval
          integer, allocatable               :: glo_att_drange(:)   ! starting and ending index of glo_att_dval
          character (mio_max_att_cval_len)   :: glo_att_cval        ! global character attribute value
          integer                            :: glo_att_ival(200)   ! global intever attribute value
          real                               :: glo_att_rval(200)   ! global real attribute value
          real*8                             :: glo_att_dval(200)   ! global double attribute value
! data and decomposition map
          integer, allocatable               :: ncols_pe(:,:)       ! nprocs, cross or dot, CMAQ or WRF
          integer, allocatable               :: nrows_pe(:,:)       ! nprocs, cross or dot, CMAQ or WRF
          integer, allocatable               :: colde_pe(:,:,:)     ! begin and end col, nprocs, cross or dot
          integer, allocatable               :: rowde_pe(:,:,:)     ! begin and end row, nprocs, cross or dot

! for circular buffer
          integer                            :: n_vars(6)           ! number of variables in each type
          character(mio_max_time_length), allocatable             & 
                                             :: cb_tstamp(:,:)      ! circular buffer time stamp for each variable
          integer, allocatable               :: data_index(:,:,:)   ! variable range for head, tail and stored data
          integer, allocatable               :: head_loc(:)         ! variable head loc
          integer, allocatable               :: tail_loc(:)         ! variable tail loc
          integer, allocatable               :: i_data(:)           ! integer data
          real, allocatable                  :: r_data(:)           ! real data
          real*8, allocatable                :: d_data(:)           ! double precision data
        end type mio_file_record

        type mio_outfile_def_var_record
          character (mio_max_filename_len)         :: fname
          character (mio_max_filename_len)         :: copy_from     ! copy output file format from a specific file
          character (mio_max_filename_len)         :: new_file_info ! dimension name info of a brand new file
          character (2)                            :: fmode
          integer                                  :: nvars         ! number of variables on vlist
          integer, allocatable                     :: plist(:)      ! list of printing variables, 0 (not print),
                                                                    ! 1 (print), and position 0 contains number of
                                                                    ! printing variables
          character (mio_max_str_len), allocatable :: vlist(:)      ! list of variable names
        end type mio_outfile_def_var_record

        type mio_outfile_def_record
          integer                          :: num_of_file_definitions
          type(mio_outfile_def_var_record) :: flist(mio_max_outfile_def_list)
        end type mio_outfile_def_record

      end module mio_type_def_module
