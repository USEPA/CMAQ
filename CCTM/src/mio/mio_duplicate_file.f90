! Purpose: To copy file information such as dimension names, variable
! dimensions, variable attributes and global attributes, partially or
! wholly from source file to destination file

      subroutine mio_duplicate_file (source, dest)

        use mio_type_def_module
        use mio_global_data_module, only : mio_file_data
        use mio_search_module

        implicit none

        integer, intent(in) :: source, dest

        integer :: t, v, loc, reg_var, basic_var,  &
                   n, s, e, s1, e1, s2, e2, nvars
        character (50000) :: vlist

        reg_var = 0
        basic_var = 0

        do v = 1, mio_file_data(dest)%fnvars

           t = mio_search(mio_file_data(source)%lvar_name(v), &
                          mio_basic_mpas_vnames_l,            &
                          mio_n_basic_mpas_vars)

! reorder the variable base on pre-defined sequence: regular variable, basic variable, and time step
           if (t .gt. 0) then
              basic_var = basic_var + 1
              loc       = basic_var + mio_file_data(source)%nvars
           else if ((mio_file_data(source)%var_name(v) .eq. 'Times') .or.   &
                    (mio_file_data(source)%var_name(v) .eq. 'TFLAG') .or.   &
                    (mio_file_data(source)%var_name(v) .eq. 'xtime')) then
              loc = mio_file_data(dest)%fnvars
           else
              reg_var = reg_var + 1
              loc     = reg_var
           end if

           mio_file_data(dest)%var_name(loc)        = mio_file_data(source)%var_name(v)
           mio_file_data(dest)%lvar_name(loc)       = mio_file_data(source)%lvar_name(v)
           mio_file_data(dest)%units(loc)           = mio_file_data(source)%units(v)
           mio_file_data(dest)%var_type(loc)        = mio_file_data(source)%var_type(v)
           mio_file_data(dest)%var_decomp(loc)      = mio_file_data(source)%var_decomp(v)
           mio_file_data(dest)%var_grid_type(loc)   = mio_file_data(source)%var_grid_type(v)
           mio_file_data(dest)%var_time_dep(loc)    = mio_file_data(source)%var_time_dep(v)
           mio_file_data(dest)%var_ndims(loc)       = mio_file_data(source)%var_ndims(v)
           mio_file_data(dest)%var_dimids(:,loc)    = mio_file_data(source)%var_dimids(:,v)
           mio_file_data(dest)%var_dimsize(:,loc)   = mio_file_data(source)%var_dimsize(:,v)
           mio_file_data(dest)%num_var_att(loc)     = mio_file_data(source)%num_var_att(v)
           mio_file_data(dest)%var_att_name(:,loc)  = mio_file_data(source)%var_att_name(:,v)
           mio_file_data(dest)%var_att_len(:,loc)   = mio_file_data(source)%var_att_len(:,v)
           mio_file_data(dest)%var_att_type(:,loc)  = mio_file_data(source)%var_att_type(:,v)
           mio_file_data(dest)%int_vatt_val(:,loc)  = mio_file_data(source)%int_vatt_val(:,v)
           mio_file_data(dest)%real_vatt_val(:,loc) = mio_file_data(source)%real_vatt_val(:,v)
           mio_file_data(dest)%char_vatt_val(:,loc) = mio_file_data(source)%char_vatt_val(:,v)

        end do

! transfer global attribute information
        mio_file_data(dest)%time_dim_loc          = mio_file_data(source)%time_dim_loc
        mio_file_data(dest)%layer_dim_loc         = mio_file_data(source)%layer_dim_loc
        mio_file_data(dest)%n_global_atts         = mio_file_data(source)%n_global_atts

        n                                         = mio_file_data(dest)%n_global_atts
        mio_file_data(dest)%glo_att_len           = mio_file_data(source)%glo_att_len
        mio_file_data(dest)%glo_att_name(1:n)     = mio_file_data(source)%glo_att_name(1:n)
        mio_file_data(dest)%glo_att_type(1:n)     = mio_file_data(source)%glo_att_type(1:n)
        mio_file_data(dest)%glo_att_irange(1:2*n) = mio_file_data(source)%glo_att_irange(1:n*2)
        mio_file_data(dest)%glo_att_rrange(1:2*n) = mio_file_data(source)%glo_att_rrange(1:n*2)
        mio_file_data(dest)%glo_att_drange(1:2*n) = mio_file_data(source)%glo_att_drange(1:n*2)
        mio_file_data(dest)%glo_att_crange        = mio_file_data(source)%glo_att_crange
        mio_file_data(dest)%glo_att_ival          = mio_file_data(source)%glo_att_ival
        mio_file_data(dest)%glo_att_rval          = mio_file_data(source)%glo_att_rval
        mio_file_data(dest)%glo_att_dval          = mio_file_data(source)%glo_att_dval
        mio_file_data(dest)%glo_att_cval          = mio_file_data(source)%glo_att_cval

        if (mio_file_data(source)%file_format .eq. mio_ioapi3_format) then
           vlist = ' '
           nvars = mio_file_data(dest)%nvars
           s = 1
           e = 16
           do n = 1, nvars
              write (vlist(s:e), '(a16)') mio_file_data(dest)%var_name(n)
              s = s + 16
              e = e + 16
           end do

           s2 = 0
           e2 = 0
           do n = 1, mio_file_data(source)%n_global_atts
              if (mio_file_data(dest)%glo_att_type(n) .eq. nf90_char) then
                 if (n .eq. 31) then   ! VAR-LIST
                    s2 = e2 + 1
                    e2 = s2 + nvars * 16
                    mio_file_data(dest)%glo_att_cval(s2:e2) = vlist(1:nvars*16)
                    mio_file_data(dest)%glo_att_len(1:n)    = nvars * 16
                 else
                    s1 = mio_file_data(source)%glo_att_crange((n-1)*2+1)
                    e1 = mio_file_data(source)%glo_att_crange(2*n)
                    s2 = e2 + 1
                    e2 = s2 + (e1 - s1)
                    mio_file_data(dest)%glo_att_cval(s2:e2) = mio_file_data(source)%glo_att_cval(s1:e1)
                 end if
                 mio_file_data(dest)%glo_att_crange((n-1)*2+1) = s2
                 mio_file_data(dest)%glo_att_crange(2*n)       = e2
                 mio_file_data(dest)%glo_att_len(1:n)    = mio_file_data(source)%glo_att_len(1:n)
              else if (n .eq. 15) then    ! NVARS
                 s1 = mio_file_data(source)%glo_att_irange((n-1)*2+1)
                 e1 = mio_file_data(source)%glo_att_irange(2*n)
                 mio_file_data(dest)%glo_att_ival(s1:e1) = nvars
              end if
           end do

           mio_file_data(dest)%gl_ncols = mio_file_data(dest)%dim_value(6)
           mio_file_data(dest)%gl_nrows = mio_file_data(dest)%dim_value(5)
           mio_file_data(dest)%nlays    = mio_file_data(dest)%dim_value(3)

        else if ((mio_file_data(source)%file_format .eq. mio_wrf_format) .or.   &
                 (mio_file_data(source)%file_format .eq. mio_mpas_format)) then
           mio_file_data(dest)%glo_att_crange(1:2*n) = mio_file_data(source)%glo_att_crange(1:n*2)
           mio_file_data(dest)%glo_att_cval          = mio_file_data(source)%glo_att_cval
           if (mio_file_data(source)%file_format .eq. mio_mpas_format) then
              n = mio_search ('nCells', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
              mio_file_data(dest)%gl_ncols = mio_file_data(dest)%dim_value(n)
              n = mio_search ('nVertLevels', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
              if (n > 0) then
                 mio_file_data(dest)%nlays = mio_file_data(dest)%dim_value(n)
              else
                 mio_file_data(dest)%nlays = 1
              end if
              mio_file_data(dest)%gl_nrows = 1
           else
              n = mio_search ('west_east', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
              mio_file_data(dest)%gl_ncols = mio_file_data(dest)%dim_value(n)
              n = mio_search ('bottom_top', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
              if (n < 0) then
                 mio_file_data(dest)%nlays = 1
              else
                 mio_file_data(dest)%nlays = mio_file_data(dest)%dim_value(n)
              end if
              n = mio_search ('south_north', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
              mio_file_data(dest)%gl_nrows = mio_file_data(dest)%dim_value(n)
           end if
        end if

      end subroutine mio_duplicate_file

! -------------------------------------------------------------------------------------
      subroutine mio_duplicate_partial_file (source, dest, nvars)

        use mio_type_def_module
        use mio_global_data_module, only : mio_file_data,         &
                                           mio_outfile_def_info
        use mio_search_module
!       use mio_parameter_module, only: mio_iunit

        implicit none

        integer, intent(in) :: source, dest, nvars

        integer :: v, t, s, e, s1, e1, s2, e2, n, loc, fnum
        character (mio_max_str_len) :: t_vname
        character (50000) :: vlist
        logical :: found_mpas_basic_variable, found

        found = .false.
        fnum = 0
        do while ((.not. found) .and. (fnum < mio_outfile_def_info%num_of_file_definitions))
           fnum = fnum + 1
           if (mio_file_data(dest)%filename .eq. mio_outfile_def_info%flist(fnum)%fname) then
              found = .true.
           end if
        end do

        do v = 1, nvars

           t_vname = mio_outfile_def_info%flist(fnum)%vlist(v)

           t = mio_search(t_vname, mio_file_data(source)%var_name, mio_file_data(source)%fnvars)

! place the variable base on pre-defined sequence: regular variable, basic variable, and time step
           mio_file_data(dest)%var_name(v)        = mio_file_data(source)%var_name(t)
           mio_file_data(dest)%lvar_name(v)       = mio_file_data(source)%lvar_name(t)
           mio_file_data(dest)%units(v)           = mio_file_data(source)%units(t)
           mio_file_data(dest)%var_type(v)        = mio_file_data(source)%var_type(t)
           mio_file_data(dest)%var_decomp(v)      = mio_file_data(source)%var_decomp(t)
           mio_file_data(dest)%var_grid_type(v)   = mio_file_data(source)%var_grid_type(t)
           mio_file_data(dest)%var_time_dep(v)    = mio_file_data(source)%var_time_dep(t)
           mio_file_data(dest)%var_ndims(v)       = mio_file_data(source)%var_ndims(t)
           mio_file_data(dest)%var_dimids(:,v)    = mio_file_data(source)%var_dimids(:,t)
           mio_file_data(dest)%var_dimsize(:,v)   = mio_file_data(source)%var_dimsize(:,t)
           mio_file_data(dest)%num_var_att(v)     = mio_file_data(source)%num_var_att(t)
           mio_file_data(dest)%var_att_name(:,v)  = mio_file_data(source)%var_att_name(:,t)
           mio_file_data(dest)%var_att_len(:,v)   = mio_file_data(source)%var_att_len(:,t)
           mio_file_data(dest)%var_att_type(:,v)  = mio_file_data(source)%var_att_type(:,t)
           mio_file_data(dest)%int_vatt_val(:,v)  = mio_file_data(source)%int_vatt_val(:,t)
           mio_file_data(dest)%real_vatt_val(:,v) = mio_file_data(source)%real_vatt_val(:,t)
           mio_file_data(dest)%char_vatt_val(:,v) = mio_file_data(source)%char_vatt_val(:,t)

        end do

        found_mpas_basic_variable = .true.
        v = 0
        do while ((found_mpas_basic_variable) .and. (v .lt. mio_file_data(source)%nbvars))
           v = v + 1

           t = mio_search(mio_basic_mpas_vnames_l(v),       &
                          mio_file_data(source)%lvar_name,  &
                          mio_file_data(source)%fnvars)

           if (t .gt. 0) then
              loc = nvars + v
              mio_file_data(dest)%var_name(loc)        = mio_file_data(source)%var_name(t)
              mio_file_data(dest)%lvar_name(loc)       = mio_file_data(source)%lvar_name(t)
              mio_file_data(dest)%units(loc)           = mio_file_data(source)%units(t)
              mio_file_data(dest)%units(loc)           = mio_file_data(source)%units(t)
              mio_file_data(dest)%var_type(loc)        = mio_file_data(source)%var_type(t)
              mio_file_data(dest)%var_decomp(loc)      = mio_file_data(source)%var_decomp(t)
              mio_file_data(dest)%var_grid_type(loc)   = mio_file_data(source)%var_grid_type(t)
              mio_file_data(dest)%var_time_dep(loc)    = mio_file_data(source)%var_time_dep(t)
              mio_file_data(dest)%var_ndims(loc)       = mio_file_data(source)%var_ndims(t)
              mio_file_data(dest)%var_dimids(:,loc)    = mio_file_data(source)%var_dimids(:,t)
              mio_file_data(dest)%var_dimsize(:,loc)   = mio_file_data(source)%var_dimsize(:,t)
              mio_file_data(dest)%num_var_att(loc)     = mio_file_data(source)%num_var_att(t)
              mio_file_data(dest)%var_att_name(:,loc)  = mio_file_data(source)%var_att_name(:,t)
              mio_file_data(dest)%var_att_len(:,loc)   = mio_file_data(source)%var_att_len(:,t)
              mio_file_data(dest)%var_att_type(:,loc)  = mio_file_data(source)%var_att_type(:,t)
              mio_file_data(dest)%int_vatt_val(:,loc)  = mio_file_data(source)%int_vatt_val(:,t)
              mio_file_data(dest)%real_vatt_val(:,loc) = mio_file_data(source)%real_vatt_val(:,t)
              mio_file_data(dest)%char_vatt_val(:,loc) = mio_file_data(source)%char_vatt_val(:,t)
           else
              found_mpas_basic_variable = .false.
           end if

        end do

        if (mio_file_data(source)%file_format .eq. mio_ioapi3_format) then
           t_vname = 'TFLAG'
        else if (mio_file_data(source)%file_format .eq. mio_wrf_format) then
           t_vname = 'Times'
        else if (mio_file_data(source)%file_format .eq. mio_mpas_format) then
           t_vname = 'xtime'
        end if

        t = mio_search(t_vname,                         &
                       mio_file_data(source)%var_name,  &
                       mio_file_data(source)%fnvars)

        if (found_mpas_basic_variable) then
           loc = nvars + mio_file_data(source)%nbvars + 1
        else
           loc = nvars + 1
        end if

        mio_file_data(dest)%var_name(loc)        = mio_file_data(source)%var_name(t)
        mio_file_data(dest)%lvar_name(loc)       = mio_file_data(source)%lvar_name(t)
        mio_file_data(dest)%units(loc)           = mio_file_data(source)%units(t)
        mio_file_data(dest)%var_type(loc)        = mio_file_data(source)%var_type(t)
        mio_file_data(dest)%var_decomp(loc)      = mio_file_data(source)%var_decomp(t)
        mio_file_data(dest)%var_grid_type(loc)   = mio_file_data(source)%var_grid_type(t)
        mio_file_data(dest)%var_time_dep(loc)    = mio_file_data(source)%var_time_dep(t)
        mio_file_data(dest)%var_ndims(loc)       = mio_file_data(source)%var_ndims(t)
        mio_file_data(dest)%var_dimids(:,loc)    = mio_file_data(source)%var_dimids(:,t)
        mio_file_data(dest)%var_dimsize(:,loc)   = mio_file_data(source)%var_dimsize(:,t)
        mio_file_data(dest)%num_var_att(loc)     = mio_file_data(source)%num_var_att(t)
        mio_file_data(dest)%var_att_name(:,loc)  = mio_file_data(source)%var_att_name(:,t)
        mio_file_data(dest)%var_att_len(:,loc)   = mio_file_data(source)%var_att_len(:,t)
        mio_file_data(dest)%var_att_type(:,loc)  = mio_file_data(source)%var_att_type(:,t)
        mio_file_data(dest)%int_vatt_val(:,loc)  = mio_file_data(source)%int_vatt_val(:,t)
        mio_file_data(dest)%real_vatt_val(:,loc) = mio_file_data(source)%real_vatt_val(:,t)
        mio_file_data(dest)%char_vatt_val(:,loc) = mio_file_data(source)%char_vatt_val(:,t)

! transfer global attribute information
        mio_file_data(dest)%time_dim_loc          = mio_file_data(source)%time_dim_loc
        mio_file_data(dest)%layer_dim_loc         = mio_file_data(source)%layer_dim_loc
        mio_file_data(dest)%n_global_atts         = mio_file_data(source)%n_global_atts

        n                                         = mio_file_data(dest)%n_global_atts
        mio_file_data(dest)%glo_att_len           = mio_file_data(source)%glo_att_len
        mio_file_data(dest)%glo_att_name(1:n)     = mio_file_data(source)%glo_att_name(1:n)
        mio_file_data(dest)%glo_att_type(1:n)     = mio_file_data(source)%glo_att_type(1:n)
        mio_file_data(dest)%glo_att_irange(1:2*n) = mio_file_data(source)%glo_att_irange(1:n*2)
        mio_file_data(dest)%glo_att_rrange(1:2*n) = mio_file_data(source)%glo_att_rrange(1:n*2)
        mio_file_data(dest)%glo_att_drange(1:2*n) = mio_file_data(source)%glo_att_drange(1:n*2)
        mio_file_data(dest)%glo_att_crange        = mio_file_data(source)%glo_att_crange
        mio_file_data(dest)%glo_att_ival          = mio_file_data(source)%glo_att_ival
        mio_file_data(dest)%glo_att_rval          = mio_file_data(source)%glo_att_rval
        mio_file_data(dest)%glo_att_dval          = mio_file_data(source)%glo_att_dval
        mio_file_data(dest)%glo_att_cval          = mio_file_data(source)%glo_att_cval

        if (mio_file_data(source)%file_format .eq. mio_ioapi3_format) then
           vlist = ' '
           s = 1
           e = 16
           do n = 1, mio_file_data(dest)%nvars
              write (vlist(s:e), '(a16)') mio_file_data(dest)%var_name(n)
              s = s + 16
              e = e + 16
           end do

           s2 = 0
           e2 = 0
           do n = 1, mio_file_data(source)%n_global_atts
              if (mio_file_data(dest)%glo_att_type(n) .eq. nf90_char) then
                 if (n .eq. 31) then   ! VAR-LIST
                    s2 = e2 + 1
                    e2 = s2 + mio_file_data(dest)%nvars * 16
                    mio_file_data(dest)%glo_att_cval(s2:e2) = vlist(1:mio_file_data(dest)%nvars*16)
                    mio_file_data(dest)%glo_att_len(1:n)    = mio_file_data(dest)%nvars * 16
                 else
                    s1 = mio_file_data(source)%glo_att_crange((n-1)*2+1)
                    e1 = mio_file_data(source)%glo_att_crange(2*n)
                    s2 = e2 + 1
                    e2 = s2 + (e1 - s1)
                    mio_file_data(dest)%glo_att_cval(s2:e2) = mio_file_data(source)%glo_att_cval(s1:e1)
                 end if
                 mio_file_data(dest)%glo_att_crange((n-1)*2+1) = s2
                 mio_file_data(dest)%glo_att_crange(2*n)       = e2
                 mio_file_data(dest)%glo_att_len(1:n)    = mio_file_data(source)%glo_att_len(1:n)
              else if (n .eq. 15) then    ! NVARS
                 s1 = mio_file_data(source)%glo_att_irange((n-1)*2+1)
                 e1 = mio_file_data(source)%glo_att_irange(2*n)
                 mio_file_data(dest)%glo_att_ival(s1:e1) = mio_file_data(dest)%nvars
              end if
           end do

           mio_file_data(dest)%gl_ncols = mio_file_data(dest)%dim_value(6)
           mio_file_data(dest)%gl_nrows = mio_file_data(dest)%dim_value(5)
           mio_file_data(dest)%nlays    = mio_file_data(dest)%dim_value(3)

        else if ((mio_file_data(source)%file_format .eq. mio_wrf_format) .or.   &
                 (mio_file_data(source)%file_format .eq. mio_mpas_format)) then
           mio_file_data(dest)%glo_att_crange(1:2*n) = mio_file_data(source)%glo_att_crange(1:n*2)
           mio_file_data(dest)%glo_att_cval          = mio_file_data(source)%glo_att_cval
           if (mio_file_data(source)%file_format .eq. mio_mpas_format) then
              n = mio_search ('nCells', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
              mio_file_data(dest)%gl_ncols = mio_file_data(dest)%dim_value(n)
              n = mio_search ('nVertLevels', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
              mio_file_data(dest)%nlays = mio_file_data(dest)%dim_value(n)
              mio_file_data(dest)%gl_nrows = 1
           else
              n = mio_search ('west_east', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
              mio_file_data(dest)%gl_ncols = mio_file_data(dest)%dim_value(n)
              n = mio_search ('bottom_top', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
              if (n < 0) then
                 mio_file_data(dest)%nlays = 1
              else
                 mio_file_data(dest)%nlays = mio_file_data(dest)%dim_value(n)
              end if
              n = mio_search ('south_north', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
              mio_file_data(dest)%gl_nrows = mio_file_data(dest)%dim_value(n)
           end if
        end if

      end subroutine mio_duplicate_partial_file
