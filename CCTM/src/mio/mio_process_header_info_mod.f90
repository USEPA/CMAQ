! Purpose: retrieve and define dimension, variable, and global attribute information

        module mio_process_header_info_module

          use netcdf
          use mio_type_def_module
          use mio_global_data_module, only : mio_mype, mio_logdev

          implicit none

          private :: extraction, reorder

!         interface mio_define_dimension_information
!           module procedure mio_define_dimension_information_file,     &
!                            mio_define_dimension_information_template 
!         end interface

          contains

! ----------------------------------------------------------------------------
          subroutine mio_retrieve_dimension_information (file_data)

            use mio_util_func_module, only : mio_to_lower_case

            type (mio_file_record), intent(inout) :: file_data

            character (34), parameter :: pname = 'mio_retrieve_dimension_information'

            integer :: n, stat

            do n = 1, file_data%ndims
               stat = nf90_inquire_dimension (file_data%fileid,       &
                                              n,                      &
                                              file_data%dim_name(n),  &
                                              file_data%dim_value(n))
               if (stat .ne. nf90_noerr) then
                  write (mio_logdev, *) ' Abort in routine ', pname
                  write (mio_logdev, *) ' with error message ', trim(nf90_strerror(stat))
                  stop
               end if
            end do

          end subroutine mio_retrieve_dimension_information

! ----------------------------------------------------------------------------
          subroutine mio_retrieve_variable_information (file_data)

            use mio_global_data_module, only : mio_n_basic_mpas_vars,   &
                                               mio_basic_mpas_vnames_l, &
                                               mio_time_var_ind,        &
                                               mio_bndy_var_ind
            use mio_search_module
            use mio_util_func_module, only : mio_to_lower_case

            type (mio_file_record), intent(inout) :: file_data

            character (33), parameter :: pname = 'mio_retrieve_variable_information'

            integer :: v, t, stat, time_att_id, ndims,     &
                       loc, reg_var, basic_var, uloc
            integer, allocatable :: t_var_dimids(:,:),  &
                                    t_var_type(:),    &
                                    t_var_ndims(:),   &
                                    t_num_var_att(:), &
                                    m(:)
            character (mio_max_varname_len), allocatable :: t_var_name(:)
            logical :: error

            ndims = file_data%ndims

            allocate (t_var_dimids(ndims, file_data%fnvars),  &
                      t_var_name(file_data%fnvars),           &
                      t_var_type(file_data%fnvars),           &
                      t_var_ndims(file_data%fnvars),          &
                      t_num_var_att(file_data%fnvars),        &
                      m(file_data%fnvars),                    &
                      stat=stat)

! determine number of basic variable in the file w.r.t. the pre-defined basic variable list
            basic_var = 0
            do v = 1, file_data%fnvars
               error = .false.

               stat = nf90_inquire_variable (file_data%fileid,       &
                                             v,                      &
                                             t_var_name(v),          &
                                             t_var_type(v),          &
                                             t_var_ndims(v),         &
                                             t_var_dimids(:,v),      &
                                             t_num_var_att(v))
   
               if (stat .ne. nf90_noerr) then
                  error = .true.
                  write (mio_logdev, *) ' Calling nf90_inquire_variable in routine ', pname
                  write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
               end if

               ! determine how many mpas basic variables
               m(v) = mio_search (mio_to_lower_case(t_var_name(v)),  &
                                  mio_basic_mpas_vnames_l,           &
                                  mio_n_basic_mpas_vars)

               if (m(v) .gt. 0) then
                  basic_var = basic_var + 1
               end if
            end do
            file_data%nvars  = file_data%nvars - basic_var 
            file_data%nbvars = basic_var

            reg_var = 0
            basic_var = 0
            time_att_id = -1

            mio_bndy_var_ind = mio_search ('PERIM', file_data%dim_name, file_data%ndims)

            do v = 1, file_data%fnvars

! reorder the variable base on pre-defined sequence: regular variable,
! basic variable, and time step
               if (m(v) .gt. 0) then
                  basic_var = basic_var + 1
                  loc       = basic_var + file_data%nvars
               else if ((t_var_name(v) .eq. 'Times') .or.   &
                        (t_var_name(v) .eq. 'TFLAG') .or.   &
                        (t_var_name(v) .eq. 'xtime')) then
                  loc = file_data%fnvars
                  mio_time_var_ind = loc
                  time_att_id = t_var_dimids(t_var_ndims(v), v)
               else
                  reg_var = reg_var + 1
                  loc     = reg_var
               end if

               file_data%var_name(loc)     = t_var_name(v)
               file_data%lvar_name(loc)    = mio_to_lower_case(t_var_name(v))
               file_data%var_type(loc)     = t_var_type(v)
               file_data%var_ndims(loc)    = t_var_ndims(v)
               file_data%var_dimids(:,loc) = t_var_dimids(:,v)
               do t = 1, t_var_ndims(v)
                  file_data%var_dimsize(t,loc) = file_data%dim_value(t_var_dimids(t,v))
               end do
               file_data%num_var_att(loc)  = t_num_var_att(v)

               stat = nf90_inq_varid (file_data%fileid,           &
                                      file_data%var_name(loc),    &
                                      file_data%var_id(loc))

               if (stat .ne. nf90_noerr) then
                  error = .true.
                  write (mio_logdev, *) ' Calling nf90_inq_varid in routine ', pname
                  write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
               end if

               file_data%units(loc) = '-'

               if (file_data%num_var_att(loc) .gt. 0) then

                  do t = 1, file_data%num_var_att(loc)

                     stat = nf90_inq_attname (file_data%fileid,                     &
                                              file_data%var_id(loc),                &
                                              t,                                    &
                                              file_data%var_att_name(t,loc))

                     if (stat .ne. nf90_noerr) then
                        error = .true.
                        write (mio_logdev, *) ' Calling nf90_inq_attname in routine ', pname
                        write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                     end if

                     stat = nf90_inquire_attribute (file_data%fileid,               &
                                                    file_data%var_id(loc),          &
                                                    file_data%var_att_name(t,loc),  &
                                                    file_data%var_att_type(t,loc),  &
                                                    file_data%var_att_len(t,loc))

                     if (stat .ne. nf90_noerr) then
                        error = .true.
                        write (mio_logdev, *) ' Calling nf90_inquire_attribute in routine ', pname
                        write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                     end if

                     if (file_data%var_att_type(t,loc) .eq. nf90_char) then
                        stat = nf90_get_att (file_data%fileid,                      &
                                             file_data%var_id(loc),                 &
                                             file_data%var_att_name(t,loc),         &
                                             file_data%char_vatt_val(t,loc))

                        if (stat .ne. nf90_noerr) then
                           error = .true.
                           write (mio_logdev, *) ' Calling nf90_get_att for char type in routine ', pname
                           write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                        end if

                        uloc = index (file_data%var_att_name(t,loc), 'nits') 
                        if (uloc > 0) then
                           file_data%units(loc) = file_data%char_vatt_val(t,loc)
                        end if

                     else if (file_data%var_att_type(t,loc) .eq. nf90_int) then
                        stat = nf90_get_att (file_data%fileid,                      &
                                             file_data%var_id(loc),                 &
                                             file_data%var_att_name(t,loc),         &
                                             file_data%int_vatt_val(t,loc))

                        if (stat .ne. nf90_noerr) then
                           error = .true.
                           write (mio_logdev, *) ' Calling nf90_get_att for int type in routine ', pname
                           write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                        end if

                     else if (file_data%var_att_type(t,loc) .eq. nf90_float) then
                        stat = nf90_get_att (file_data%fileid,                      &
                                             file_data%var_id(loc),                 &
                                             file_data%var_att_name(t,loc),         &
                                             file_data%real_vatt_val(t,loc))

                        if (stat .ne. nf90_noerr) then
                           error = .true.
                           write (mio_logdev, *) ' Calling nf90_get_att for real type in routine ', pname
                           write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                        end if

                     else
                        print *, ' Error: Unknown attribute type '
                        stop
                     end if
                  end do
               end if

               if (error) then
                  write (mio_logdev, *) ' Abort in routine ', pname
                  stop
               end if

            end do

            deallocate (t_var_dimids, t_var_name, t_var_type, t_var_ndims, t_num_var_att)

            ! determine a variable is a time dependent one or not
            file_data%var_time_dep  = .false.
            file_data%var_grid_type = ' '
            do v = 1, file_data%nvars
               if (file_data%var_ndims(v) .gt. 0) then
                  if (file_data%var_dimids(file_data%var_ndims(v),v) .eq. time_att_id) then
                     file_data%var_time_dep(v) = .true.
                  end if
                  ! determine a variable is boundary variable
                  if (file_data%var_dimids(1, v) == mio_bndy_var_ind) then
                     file_data%var_grid_type(v) = 'b'
                  end if
               end if
            end do

          end subroutine mio_retrieve_variable_information

! ----------------------------------------------------------------------------
          subroutine mio_retrieve_global_attribute_information (file_data)

            type (mio_file_record), intent(inout) :: file_data

            character (41), parameter :: pname = 'mio_retrieve_global_attribute_information'

            character (50000) :: t_str
            integer :: n, stat, s, e, length, start(6), leng(6)    ! for 6 different types of data which is 
                                                                   ! defined in mio_parameter_mod.f90
            logical :: error

            start = 0
            leng = 1
            do n = 1, file_data%n_global_atts

               error = .false.

               stat = nf90_inq_attname (file_data%fileid,                &
                                        nf90_global,                     &
                                        n,                               &
                                        file_data%glo_att_name(n))

               if (stat .ne. nf90_noerr) then
                  error = .true.
                  write (mio_logdev, *) ' Calling nf90_inq_attname in routine ', pname
                  write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))

               end if

!        write (6, *) ' ==d== ret glo a ', n, trim(file_data%glo_att_name(n)), '=='

               stat = nf90_inquire_attribute (file_data%fileid,          &
                                              nf90_global,               &
                                              file_data%glo_att_name(n), &
                                              file_data%glo_att_type(n), &
                                              file_data%glo_att_len(n))

               if (stat .ne. nf90_noerr) then
                  error = .true.
                  write (mio_logdev, *) ' Calling nf90_inquire_attribute in routine ', pname
                  write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
               end if

               if (file_data%glo_att_type(n) .eq. nf90_char) then

                  stat = nf90_get_att (file_data%fileid,                 &
                                       nf90_global,                      &
                                       file_data%glo_att_name(n),        &
                                       t_str)

                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_get_att for char type in routine ', pname
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if

                  start(mio_char) = start(mio_char) + leng(mio_char)
                  file_data%glo_att_crange((n-1)*2+1) = start(mio_char)
                  s = start(mio_char)

                  if (ichar(t_str(1:1)) == 0) then   ! treatment for carriage return character 
                     e = s
                     length = 1
                     file_data%glo_att_cval(s:e) = t_str(1:1)
                  else
                     length = len_trim(t_str)
                     e = s + length - 1
                     file_data%glo_att_cval(s:e) = trim(t_str)
                  end if
                  file_data%glo_att_crange(2*n) = e

                  leng(mio_char) = length

               else if (file_data%glo_att_type(n) .eq. nf90_int) then

                  start(mio_int) = start(mio_int) + leng(mio_int)
                  file_data%glo_att_irange((n-1)*2+1) = start(mio_int)
                  s = start(mio_int)
                  e = s + file_data%glo_att_len(n) - 1
                  file_data%glo_att_irange(2*n) = e

                  stat = nf90_get_att (file_data%fileid,              &
                                       nf90_global,                   &
                                       file_data%glo_att_name(n),     &
                                       file_data%glo_att_ival(s:e))

                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_get_att for int type in routine ', pname
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if

                  if (file_data%glo_att_name(n) == 'TSTEP') then
                     file_data%tstep = file_data%glo_att_ival(s)
                  end if

                  leng(mio_int) = file_data%glo_att_len(n)

               else if (file_data%glo_att_type(n) .eq. nf90_float) then

                  start(mio_real) = start(mio_real) + leng(mio_real)
                  file_data%glo_att_rrange((n-1)*2+1) = start(mio_real)
                  s = start(mio_real)
                  e = s + file_data%glo_att_len(n) - 1
                  file_data%glo_att_rrange(2*n) = e

                  stat = nf90_get_att (file_data%fileid,              &
                                       nf90_global,                   &
                                       file_data%glo_att_name(n),     &
                                       file_data%glo_att_rval(s:e))

                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_get_att for real type in routine ', pname
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if

                  leng(mio_real) = file_data%glo_att_len(n)

               else if (file_data%glo_att_type(n) .eq. nf90_double) then

                  start(mio_double) = start(mio_double) + leng(mio_double)
                  file_data%glo_att_drange((n-1)*2+1) = start(mio_double)
                  s = start(mio_double)
                  e = s + file_data%glo_att_len(n) - 1
                  file_data%glo_att_drange(2*n) = e

                  stat = nf90_get_att (file_data%fileid,              &
                                       nf90_global,                   &
                                       file_data%glo_att_name(n),     &
                                       file_data%glo_att_dval(s:e))

                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_get_att for double type in routine ', pname
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if

                  leng(mio_double) = file_data%glo_att_len(n)

               end if

               if (error) then
                  write (mio_logdev, *) ' Abort in routine ', pname
                  stop
               end if

            end do

          end subroutine mio_retrieve_global_attribute_information

! ----------------------------------------------------------------------------
          subroutine mio_define_dimension_information (file_data, fnum)

            use mio_global_data_module

            type (mio_file_record), intent(in) :: file_data
            integer, intent(in), optional      :: fnum

            character (32), parameter :: pname = 'mio_define_dimension_information'

            integer :: stat, n, i, dim_id
            logical :: error
            character (mio_max_varname_len), allocatable :: temp(:)
            integer                                      :: temp_value

            error = .false.

            if (present(fnum)) then     ! this is for creating a file from a template
               read (mio_outfile_def_info%flist(fnum)%new_file_info, *) n
               allocate (temp(n+1), stat=stat)
               read (mio_outfile_def_info%flist(fnum)%new_file_info, *) temp
               stat = nf90_def_dim (file_data%fileid,         &
                                    'time',                   &
                                    nf90_unlimited,           &
                                    dim_id)

               stat = nf90_def_dim (file_data%fileid,         &
                                    'str_len',                &
                                    19,                       &
                                    dim_id)

               do i = 2, n, 2
                  read (temp(i+1), *) temp_value 
                  stat = nf90_def_dim (file_data%fileid,      &
                                       temp(i),               &
                                       temp_value,            &
                                       dim_id)
                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_def_dim in routine ', pname
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if
               end do
               deallocate (temp)
            else
               do n = 1, file_data%ndims

                  if (n .eq. file_data%unlimited) then
                     stat = nf90_def_dim (file_data%fileid,         &
                                          file_data%dim_name(n),    &
                                          nf90_unlimited,           &
                                          dim_id)
                     if (stat .ne. nf90_noerr) then
                        error = .true.
                        write (mio_logdev, *) ' Calling nf90_def_dim in routine ', pname
                        write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                     end if
                  else
                     stat = nf90_def_dim (file_data%fileid,         &
                                          file_data%dim_name(n),    &
                                          file_data%dim_value(n),   &
                                          dim_id)
                     if (stat .ne. nf90_noerr) then
                        error = .true.
                        write (mio_logdev, *) ' Calling nf90_def_dim in routine ', pname
                        write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                     end if
                  end if

               end do
            end if

            if (error) then
               write (mio_logdev, *) ' Abort in routine ', pname
               stop
            end if

          end subroutine mio_define_dimension_information

! ----------------------------------------------------------------------------
          subroutine mio_define_variable_information (file_data)

            type (mio_file_record), intent(inout) :: file_data

            character (31), parameter :: pname = 'mio_define_variable_information'

            integer :: v, stat
            logical :: error

            error = .false.

! take care the time step data
            v = file_data%fnvars

            stat = nf90_def_var (file_data%fileid,                                   &
                                 file_data%var_name(v),                              &
                                 file_data%var_type(v),                              &
                                 file_data%var_dimids(1:file_data%var_ndims(v),v),   &
                                 file_data%var_id(v))

            if (stat .ne. nf90_noerr) then
               error = .true.
               write (mio_logdev, *) ' Calling nf90_def_var in routine ', pname
               write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
            end if

! regular variable
            do v = 1, file_data%nvars

               stat = nf90_def_var (file_data%fileid,                                   &
                                    file_data%var_name(v),                              &
                                    file_data%var_type(v),                              &
                                    file_data%var_dimids(1:file_data%var_ndims(v),v),   &
                                    file_data%var_id(v))

               if (stat .ne. nf90_noerr) then
                  error = .true.
                  write (mio_logdev, *) ' Calling nf90_def_var in routine ', pname, ' for variable # ', v
                  write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
               end if
            end do

! take care base variables other than time variable (for MPAS only)
            do v = file_data%nvars+1, file_data%fnvars-1
               stat = nf90_def_var (file_data%fileid,                                   &
                                    file_data%var_name(v),                              &
                                    file_data%var_type(v),                              &
                                    file_data%var_dimids(1:file_data%var_ndims(v),v),   &
                                    file_data%var_id(v))

               if (stat .ne. nf90_noerr) then
                  error = .true.
                  write (mio_logdev, *) ' Calling nf90_def_var in routine ', pname, ' for MPAS base variable # ', v
                  write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
               end if
            end do

            if (error) then
               write (mio_logdev, *) ' Abort in routine ', pname
               stop
            end if

          end subroutine mio_define_variable_information

! ----------------------------------------------------------------------------
          subroutine mio_define_variable_attributes_information (file_data)

            type (mio_file_record), intent(in) :: file_data

            character (42), parameter :: pname = 'mio_define_variable_attributes_information'

            integer :: n, t, stat, vv
            logical :: error

! take care time step data attributes
            n = file_data%fnvars
            error = .false.

            do t = 1, file_data%num_var_att(n)
               if (file_data%var_att_type(t,n) .eq. nf90_char) then

                  stat = nf90_put_att (file_data%fileid,                   &
                                       file_data%var_id(n),                &
                                       trim(file_data%var_att_name(t,n)),  &
                                       trim(file_data%char_vatt_val(t,n)))
                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_put_att for temp step data in routine ', pname
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if
               else if (file_data%var_att_type(t,n) .eq. nf90_int) then
                  stat = nf90_put_att (file_data%fileid,                   &
                                       file_data%var_id(n),                &
                                       trim(file_data%var_att_name(t,n)),  &
                                       file_data%int_vatt_val(t,n))
                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_put_att for temp step data in routine ', pname
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if
               end if
            end do

            if (error) then
               write (mio_logdev, *) ' Error in defining time step data attributes'
               stop
            end if

! for other variables' data attributes
            do n = 1, file_data%nvars
               do t = 1, file_data%num_var_att(n)
                  if (file_data%var_att_type(t,n) .eq. nf90_char) then
                     stat = nf90_put_att (file_data%fileid,                   &
                                          file_data%var_id(n),                &
                                          trim(file_data%var_att_name(t,n)),  &
                                          trim(file_data%char_vatt_val(t,n)))
                     if (stat .ne. nf90_noerr) then
                        error = .true.
                        write (mio_logdev, *) ' Calling nf90_put_att for char type in routine ', pname, ' for variable # ', n
                        write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                     end if
                  else if (file_data%var_att_type(t,n) .eq. nf90_int) then
                     stat = nf90_put_att (file_data%fileid,                   &
                                          file_data%var_id(n),                &
                                          trim(file_data%var_att_name(t,n)),  &
                                          file_data%int_vatt_val(t,n))
                     if (stat .ne. nf90_noerr) then
                        error = .true.
                        write (mio_logdev, *) ' Calling nf90_put_att for int type in routine ', pname, ' for variable # ', n
                        write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                     end if
                  else if (file_data%var_att_type(t,n) .eq. nf90_float) then
                     stat = nf90_put_att (file_data%fileid,                   &
                                          file_data%var_id(n),                &
                                          trim(file_data%var_att_name(t,n)),  &
                                          file_data%real_vatt_val(t,n))
                     if (stat .ne. nf90_noerr) then
                        error = .true.
                        write (mio_logdev, *) ' Calling nf90_put_att for real type in routine ', pname, ' for variable # ', n
                        write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                     end if
                  end if
               end do
            end do

! for MPAS dataset, output basic data
            if (file_data%file_format .eq. mio_mpas_format) then
               if (mio_mype .eq. 0) then
                  do vv = 1, file_data%nbvars
                     n = vv + file_data%nvars
                     do t = 1, file_data%num_var_att(n)
                        if (file_data%var_att_type(t,n) .eq.  nf90_char) then
                           stat = nf90_put_att (file_data%fileid,                  &
                                                file_data%var_id(n),               &
                                                trim(file_data%var_att_name(t,n)), &
                                                trim(file_data%char_vatt_val(t,n)))
                           if (stat .ne. nf90_noerr) then
                              error = .true.
                              write (mio_logdev, *) ' Calling nf90_put_att for real type in routine ', pname, ' for MPAS basic variable # ', vv
                              write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                           end if
                        else if (file_data%var_att_type(t,n) .eq.  nf90_int) then
                           stat = nf90_put_att (file_data%fileid,                  &
                                                file_data%var_id(n),               &
                                                trim(file_data%var_att_name(t,n)), &
                                                file_data%int_vatt_val(t,n))
                           if (stat .ne. nf90_noerr) then
                              error = .true.
                              write (mio_logdev, *) ' Calling nf90_put_att for int type in routine ', pname, ' for MPAS basic variable # ', vv
                              write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                           end if
                        else if (file_data%var_att_type(t,n) .eq.  nf90_float) then
                           stat = nf90_put_att (file_data%fileid,                  &
                                                file_data%var_id(n),               &
                                                trim(file_data%var_att_name(t,n)), &
                                                file_data%real_vatt_val(t,n))
                           if (stat .ne. nf90_noerr) then
                              error = .true.
                              write (mio_logdev, *) ' Calling nf90_put_att for real type in routine ', pname, ' for MPAS basic variable # ', vv
                              write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                           end if
                        end if
                     end do
                  end do
               end if
            end if

            if (error) then
               write (mio_logdev, *) ' Abort in routine ', pname
               stop
            end if


          end subroutine mio_define_variable_attributes_information

! ----------------------------------------------------------------------------
!         subroutine mio_define_global_attribute_information (file_data, replacement)
          subroutine mio_define_global_attribute_information (file_data)

            type (mio_file_record), intent(in) :: file_data
!           character (*), intent(in) :: replacement

            character (39), parameter :: pname = 'mio_define_global_attribute_information'

            integer                          :: n, stat, s, e
            integer, allocatable             :: i_val(:)
            real, allocatable                :: r_val(:)
            real*8, allocatable              :: d_val(:)
            logical                          :: error
            character (5000), allocatable    :: loc_replacement(:)
            character (mio_max_att_cval_len) :: c_val
            integer                          :: n_loc_replacement, i

            allocate (loc_replacement(30), stat=stat)

!           call extraction (replacement, loc_replacement, n_loc_replacement)
            n_loc_replacement = 0

            if (n_loc_replacement > 0) then
               call reorder (file_data%n_global_atts, file_data%glo_att_name, &
                             loc_replacement, n_loc_replacement)
            end if

            i = 1
            error = .false.
            do n = 1, file_data%n_global_atts

               if (file_data%glo_att_type(n) .eq. nf90_char) then

                  s = file_data%glo_att_crange((n-1)*2+1)
                  e = file_data%glo_att_crange(2*n)

                  c_val(1:e-s+1) = file_data%glo_att_cval(s:e)
                  if (n_loc_replacement > 0) then
                     if (loc_replacement(i) == file_data%glo_att_name(n)) then
                        i = i + 1
                        read(loc_replacement(i), *) c_val
                        i = i + 1
                     end if
                  end if

                  stat = nf90_put_att (file_data%fileid,             &
                                       nf90_global,                  &
                                       file_data%glo_att_name(n),    &
                                       c_val(1:e-s+1))
!                                      file_data%glo_att_cval(s:e))

                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_put_att for char type in routine ', pname, ' for attribute # ', n
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if

               else if (file_data%glo_att_type(n) .eq. nf90_int) then

                  s = file_data%glo_att_irange((n-1)*2+1)
                  e = file_data%glo_att_irange(2*n)

                  allocate (i_val(e-s+1), stat=stat)
                  i_val = file_data%glo_att_ival(s:e)
                  if (n_loc_replacement > 0) then
                     if (loc_replacement(i) == file_data%glo_att_name(n)) then
                        i = i + 1
                        read(loc_replacement(i), *) i_val(1:1)
                        i = i + 1
                     end if
                  end if

                  stat = nf90_put_att (file_data%fileid,             &
                                       nf90_global,                  &
                                       file_data%glo_att_name(n),    &
                                       i_val)
!                                      file_data%glo_att_ival(s:e))

                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_put_att for int type in routine ', pname, ' for attribute # ', n
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if
                  deallocate (i_val)

               else if (file_data%glo_att_type(n) .eq. nf90_float) then

                  s = file_data%glo_att_rrange((n-1)*2+1)
                  e = file_data%glo_att_rrange(2*n)

                  allocate (r_val(e-s+1), stat=stat)

                  r_val = file_data%glo_att_rval(s:e)
                  if (n_loc_replacement > 0) then
                     if (loc_replacement(i) == file_data%glo_att_name(n)) then
                        i = i + 1
                        read(loc_replacement(i), *) r_val(1:1)
                        i = i + 1
                     end if
                  end if

                  stat = nf90_put_att (file_data%fileid,             &
                                       nf90_global,                  &
                                       file_data%glo_att_name(n),    &
                                       r_val)
!                                      file_data%glo_att_rval(s:e))

                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_put_att for real type in routine ', pname, ' for attribute # ', n
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if
                  deallocate (r_val)

               else if (file_data%glo_att_type(n) .eq. nf90_double) then

                  s = file_data%glo_att_drange((n-1)*2+1)
                  e = file_data%glo_att_drange(2*n)

                  allocate (d_val(e-s+1), stat=stat)
                  d_val = file_data%glo_att_dval(s:e)

                  if (n_loc_replacement > 0) then
                     if (loc_replacement(i) == file_data%glo_att_name(n)) then
                        i = i + 1
                        read(loc_replacement(i), *) d_val(1:1)
                        i = i + 1
                     end if
                  end if

                  stat = nf90_put_att (file_data%fileid,             &
                                       nf90_global,                  &
                                       file_data%glo_att_name(n),    &
                                       d_val)
!                                      file_data%glo_att_dval(s:e))

                  if (stat .ne. nf90_noerr) then
                     error = .true.
                     write (mio_logdev, *) ' Calling nf90_put_att for double type in routine ', pname, ' for attribute # ', n
                     write (mio_logdev, *) ' Error: ', trim(nf90_strerror(stat))
                  end if
                  deallocate (d_val)

               end if
            end do

            if (error) then
               write (mio_logdev, *) ' Abort in routine ', pname
               stop
            end if

            deallocate (loc_replacement)

          end subroutine mio_define_global_attribute_information

! ----------------------------------------------------------------------------
          subroutine extraction (instring, outstring_array, n)

            character (*), intent(in) :: instring
            character (*), intent(out) :: outstring_array(:)
            integer, intent(out) :: n

            integer, allocatable :: index(:)
            integer :: loc_n, slen, i, s, e, stat
            logical :: encountered_lexical

            allocate (index(size(outstring_array)*2), stat=stat)

            slen = len_trim(instring)
            if (slen == 1) then
               n = 0
            else
               loc_n = 0
               index = 0
               i = 0
               encountered_lexical = .false.
               do while (i < slen)
                  i = i + 1
                  if (encountered_lexical) then
                     if ((instring(i:i) == ' ') .or.    &
                         (instring(i:i) == ',') .or.    &
                         (instring(i:i) == '"')) then
                        encountered_lexical = .false.
                        loc_n = loc_n + 1
                        index(loc_n) = i - 1
                     end if
                  else
                     ! check the beginning of a lexical string
                     if ((('a' <= instring(i:i)) .and. (instring(i:i) <= 'z')) .or.   &
                         (('A' <= instring(i:i)) .and. (instring(i:i) <= 'Z')) .or.   &
                         (('0' <= instring(i:i)) .and. (instring(i:i) <= '9'))) then
                        encountered_lexical = .true.
                        loc_n = loc_n + 1
                        index(loc_n) = i
                     end if
                  end if
               end do
               if (encountered_lexical) then
                  loc_n = loc_n + 1
                  index(loc_n) = slen + 1
               end if

               do i = 1, loc_n, 2
                   s = index(i)
                   e = index(i+1)
                   outstring_array((i+1)/2) = instring(s:e)
               end do
               n = loc_n / 2
            end if

            deallocate (index)

          end subroutine extraction

! ----------------------------------------------------------------------------
          subroutine reorder (n_global_att, glo_att_name, &
                              replacement, n_replacement)

! reorder individual item in the new global attribute sub-list in the same 
! sequential order of the file global attribute list

            integer, intent(in) :: n_global_att, n_replacement
            character(*), intent(in) :: glo_att_name(:)
            character(*), intent(inout) :: replacement(:)

            integer :: i, j, n
            logical :: all_checked, found
            character (20) :: loc_replacement(20)

            j = 0
            n = 1
            all_checked = .false.
            do while ((j < n_global_att) .and. (.not. all_checked))
               j = j + 1
               i = 1
               found = .false.
               do while ((.not. found) .and. (i < n_replacement))
                  if (glo_att_name(j) == replacement(i)) then
                     found = .true.
                     loc_replacement(n)   = replacement(i)
                     loc_replacement(n+1) = replacement(i+1)
                     n = n + 2
                     i = i + 2
                     if (n > n_replacement) then
                        all_checked = .true.
                     end if
                  else
                     i = i + 2
                  end if
               end do
            end do

            replacement = loc_replacement

          end subroutine reorder

        end module mio_process_header_info_module
