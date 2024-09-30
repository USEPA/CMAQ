      module mio_file_template_module

        integer, parameter :: n_dim_names             = 15
        integer, parameter :: n_att_names             =  4
        integer, parameter :: n_default_glo_att_names = 12

        character (17), parameter :: dim_names_list(n_dim_names) =    &
          (/ 'time             ',                                     &
             'str_len          ',                                     &
             'dimx             ',                                     &
             'dimy             ',                                     &
             'dimz             ',                                     &
             'dimx_p1          ',                                     &
             'dimy_p1          ',                                     &
             'dimz_p1          ',                                     &
             'soil_layers      ',                                     &
             'soil_layers_p1   ',                                     &
             'num_landuse_types',                                     &
             'num_pt_sources   ',                                     &
             'num_soil_types   ',                                     &
             'bndy_thickness   ',                                     &
             'num_bndy_cells   '                                      &
          /)

        character (13), parameter :: var_att_list(n_att_names) =      &
          (/ 'standard_name',                                         &
             'description  ',                                         &
             'units        ',                                         &
             'missing_value'                                          &
          /)

        character (21), parameter :: global_att_list(n_default_glo_att_names) =   &
          (/ 'model_name           ',                                             &
             'project_information  ',                                             &
             'dimx_resolution      ',                                             &
             'dimy_resolution      ',                                             &
             'simulation_start_date',                                             &
             'cen_lat              ',                                             &
             'cen_lon              ',                                             &
             'truelat1             ',                                             &
             'truelat2             ',                                             &
             'Projection           ',                                             &
             'Xorig                ',                                             &
             'yorig                '                                              &
          /)

          private :: setup_range_val

        contains

! ------------------------------------------------------------------------------
        subroutine mio_setup_new_file (fnum, n_in_char)

          use mio_type_def_module
          use mio_global_data_module
          use mio_search_module
          use mio_get_env_module

          implicit none

          integer, intent(in)      :: fnum
          character(*), intent(in) :: n_in_char

          character (mio_max_varname_len), allocatable :: temp(:)
          character (mio_max_varname_len) :: temp_dim_name, temp_str
          integer, allocatable :: loc_dim_value(:)
          integer :: temp_dim_value, n, i, stat, half_n, loc,     &
                     gl_ncols, gl_nrows, ndims, fnvars, start(6), &
                     leng(6), n_additional_global_att_names,      &
                     total_n_global_att_names
          character (2000) :: fname
          logical :: eof

          interface
            subroutine mio_setup_decomp (nprocs, npcol, nprow, ncols, nrows, &
                                         op_type, ncols_pe, nrows_pe,        &
                                         colde_pe, rowde_pe)
              integer, intent(in)  :: nprocs
              integer, intent(in)  :: npcol
              integer, intent(in)  :: nprow
              integer, intent(in)  :: ncols
              integer, intent(in)  :: nrows
              integer, intent(in)  :: op_type
              integer, intent(out) :: ncols_pe(:,:)
              integer, intent(out) :: nrows_pe(:,:)
              integer, intent(out) :: colde_pe(:,:,:)
              integer, intent(out) :: rowde_pe(:,:,:)
            end subroutine mio_setup_decomp
          end interface

          read (n_in_char, *) n

          half_n = n / 2

          allocate (temp(n),                     &
                    loc_dim_value(n_dim_names),  &
                    stat=stat)

          loc_dim_value = 0
          loc_dim_value(2) = 19

          gl_ncols = 0
          gl_nrows = 0

          read (mio_outfile_def_info%flist(fnum)%new_file_info, *) temp
          do i = 1, half_n
             temp_dim_name = temp((i-1)*2+1)
             read (temp(2*i), *) temp_dim_value
             loc = mio_search (temp_dim_name, dim_names_list, n_dim_names)
             loc_dim_value(loc) = temp_dim_value

             if ((temp_dim_name == 'dimx') .or. &
                 (temp_dim_name == 'dimy') .or. &
                 (temp_dim_name == 'dimz')) then

                if (temp_dim_name == 'dimx') then
                   gl_ncols = temp_dim_value
                else if (temp_dim_name == 'dimy') then
                   gl_nrows = temp_dim_value
                end if

!               temp_dim_name = trim(temp_dim_name) // '_p1' 
!               loc = mio_search (temp_dim_name, dim_names_list, n_dim_names)
!               loc_dim_value(loc) = temp_dim_value + 1
             end if
          end do

          mio_file_data(0)%nvars = mio_outfile_def_info%flist(fnum)%nvars
          mio_file_data(0)%file_format = mio_epa_format
          mio_file_data(0)%ndims = n_dim_names
          mio_file_data(0)%nbvars = 0
          mio_file_data(0)%unlimited = 0
          mio_file_data(0)%time_strlen_dim_loc = 2
          mio_file_data(0)%time_dim_loc = 1
          mio_file_data(0)%layer_dim_loc = 5

          ndims  = n_dim_names
          fnvars = mio_outfile_def_info%flist(fnum)%nvars + 1

          allocate (mio_file_data(0)%dim_name(ndims),                            &
                    mio_file_data(0)%dim_value(ndims),                           &
                    mio_file_data(0)%var_name(fnvars),                           &
                    mio_file_data(0)%lvar_name(fnvars),                          &
                    mio_file_data(0)%units(fnvars),                              &
                    mio_file_data(0)%var_time_dep(fnvars),                       &
                    mio_file_data(0)%var_type(fnvars),                           &
                    mio_file_data(0)%var_decomp(fnvars),                         &
                    mio_file_data(0)%var_grid_type(fnvars),                      &
                    mio_file_data(0)%var_id(fnvars),                             &
                    mio_file_data(0)%var_ndims(fnvars),                          &
                    mio_file_data(0)%var_dimids(ndims, fnvars),                  &
                    mio_file_data(0)%var_dimsize(ndims, fnvars),                 &
                    mio_file_data(0)%num_var_att(fnvars),                        &
                    mio_file_data(0)%var_att_name(mio_max_num_var_att, fnvars),  &
                    mio_file_data(0)%var_att_len(mio_max_num_var_att, fnvars),   &
                    mio_file_data(0)%var_att_type(mio_max_num_var_att, fnvars),  &
                    mio_file_data(0)%int_vatt_val(mio_max_num_var_att, fnvars),  &
                    mio_file_data(0)%real_vatt_val(mio_max_num_var_att, fnvars), &
                    mio_file_data(0)%char_vatt_val(mio_max_num_var_att, fnvars), &
                    mio_file_data(0)%ncols_pe(mio_nprocs, 2),                    &
                    mio_file_data(0)%nrows_pe(mio_nprocs, 2),                    &
                    mio_file_data(0)%colde_pe(2, mio_nprocs, 2),                 &
                    mio_file_data(0)%rowde_pe(2, mio_nprocs, 2),                 &
                    stat=stat)

          mio_file_data(0)%dim_name  = dim_names_list
          mio_file_data(0)%dim_value = loc_dim_value

          if ((gl_ncols > 0) .and. (gl_nrows > 0)) then
             call mio_setup_decomp (mio_nprocs,                      &
                                    mio_npcol,                       &
                                    mio_nprow,                       &
                                    gl_ncols,                        &
                                    gl_nrows,                        &
                                    mio_epa_format,                  &
                                    mio_file_data(0)%ncols_pe,       &
                                    mio_file_data(0)%nrows_pe,       &
                                    mio_file_data(0)%colde_pe,       &
                                    mio_file_data(0)%rowde_pe)
          else
             mio_file_data(0)%ncols_pe = 0
             mio_file_data(0)%nrows_pe = 0
             mio_file_data(0)%colde_pe = 0
             mio_file_data(0)%rowde_pe = 0
          end if

          write (temp(1), '(i0)') n
          mio_outfile_def_info%flist(fnum)%new_file_info = trim(temp(1)) // ' ' // trim(mio_outfile_def_info%flist(fnum)%new_file_info)

          deallocate (temp, loc_dim_value)

          ! check for additional global attribute input file
          call mio_get_env (fname, 'AGAL', ' ')
          n_additional_global_att_names = 0
          if (fname .ne. ' ') then
             open (unit=11, file=fname, status='old')
             eof = .false.
             do while (.not. eof)
                read (11, *, iostat=stat) temp_str
                if (stat .ne. 0) then
                   eof = .true.
                else
                   n_additional_global_att_names = n_additional_global_att_names + 1
                end if
             end do
             rewind (11)
          end if

          total_n_global_att_names = n_default_glo_att_names + n_additional_global_att_names

          mio_file_data(0)%n_global_atts = total_n_global_att_names

          allocate (mio_file_data(0)%glo_att_name(total_n_global_att_names),              &
                    mio_file_data(0)%glo_att_type(total_n_global_att_names),              &
                    mio_file_data(0)%glo_att_len(total_n_global_att_names),               &
                    mio_file_data(0)%glo_att_crange(total_n_global_att_names*2),          &
                    mio_file_data(0)%glo_att_irange(total_n_global_att_names*2),          &
                    mio_file_data(0)%glo_att_rrange(total_n_global_att_names*2),          &
                    mio_file_data(0)%glo_att_drange(total_n_global_att_names*2),          &
                    stat=stat)

          mio_file_data(0)%glo_att_name(1:n_default_glo_att_names) = global_att_list

          start = 0
          leng  = 1
          mio_file_data(0)%glo_att_crange = 0
          mio_file_data(0)%glo_att_irange = 0
          mio_file_data(0)%glo_att_rrange = 0
          mio_file_data(0)%glo_att_drange = 0

          open (unit=10, file='global.txt', status='old')

          call setup_range_val (n_default_glo_att_names, 10,       &
                                mio_file_data(0)%glo_att_len,      &
                                mio_file_data(0)%glo_att_type,     &
                                mio_file_data(0)%glo_att_crange,   &
                                mio_file_data(0)%glo_att_irange,   &
                                mio_file_data(0)%glo_att_rrange,   &
                                mio_file_data(0)%glo_att_drange,   &
                                mio_file_data(0)%glo_att_cval,     &
                                mio_file_data(0)%glo_att_ival,     &
                                mio_file_data(0)%glo_att_rval,     &
                                mio_file_data(0)%glo_att_dval,     &
                                leng, start)

          close (10)

          if (fname .ne. ' ') then

             call setup_range_val (n_additional_global_att_names, 11, &
                                   mio_file_data(0)%glo_att_len,      &
                                   mio_file_data(0)%glo_att_type,     &
                                   mio_file_data(0)%glo_att_crange,   &
                                   mio_file_data(0)%glo_att_irange,   &
                                   mio_file_data(0)%glo_att_rrange,   &
                                   mio_file_data(0)%glo_att_drange,   &
                                   mio_file_data(0)%glo_att_cval,     &
                                   mio_file_data(0)%glo_att_ival,     &
                                   mio_file_data(0)%glo_att_rval,     &
                                   mio_file_data(0)%glo_att_dval,     &
                                   leng, start,                       &
                                   mio_file_data(0)%glo_att_name)
             close (11)
          end if

        end subroutine mio_setup_new_file

! ------------------------------------------------------------------------------
        subroutine setup_range_val (n_names, iounit, att_len, att_type,  &
                                    crange, irange, rrange, drange,      &
                                    cval, ival, rval, dval,              &
                                    leng, start, name)

          use mio_global_data_module

          integer, intent(in)  :: n_names, iounit
          integer, intent(out) :: att_len(:), att_type(:),                     &
                                  crange(:), irange(:), rrange(:), drange(:),  &
                                  ival(200), leng(:), start(:)
          real, intent(out)    :: rval(200)
          real*8, intent(out)  :: dval(200)
          character (*), intent(out) :: cval
          character (*), intent(out), optional :: name(:)

          character (mio_max_att_value_char_len), allocatable :: temp_str(:)
          integer :: n, s, e, stat, loc, adj

          if (iounit == 10) then
             allocate (temp_str(2), stat=stat)
             loc = 2
             adj = 0
          else
             allocate (temp_str(3), stat=stat)
             loc = 3
             adj = n_default_glo_att_names
          end if

          do n = 1, n_names
             read (iounit, *) temp_str
             att_len(n+adj) = 1

             if (iounit == 11) then
                name(n + adj) = temp_str(2)
             end if

             if (temp_str(1) == 'char') then

                leng(mio_char) = len_trim(temp_str(loc))
                att_type(n+adj) = mio_char
                start(mio_char) = start(mio_char) + leng(mio_char)
                s = start(mio_char)
                e = s + leng(mio_char) - 1
                crange((n+adj-1)*2+1) = s
                crange(2*(n+adj)) = e
                cval(s:e) = temp_str(loc)

             else if (temp_str(1) == 'int') then

                att_type(n+adj) = mio_int
                start(mio_int) = start(mio_int) + 1
                irange((n+adj-1)*2+1) = start(mio_int)
                s = start(mio_int)
                e = s
                irange(2*(n+adj)) = e
                read (temp_str(loc), *) ival(s:e)

             else if (temp_str(1) == 'real') then

                att_type(n+adj) = mio_real
                start(mio_real) = start(mio_real) + 1
                rrange((n+adj-1)*2+1) = start(mio_real)
                s = start(mio_real)
                e = s
                rrange(2*(n+adj)) = e
                read (temp_str(loc), *) rval(s:e)

             else if (temp_str(1) == 'double') then

                att_type(n+adj) = mio_double
                start(mio_double) = start(mio_double) + 1
                drange((n+adj-1)*2+1) = start(mio_double)
                s = start(mio_double)
                e = s
                drange(2*(n+adj)) = e
                read (temp_str(loc), *) dval(s:e)

             end if
          end do

          deallocate (temp_str)

        end subroutine setup_range_val

      end module mio_file_template_module
