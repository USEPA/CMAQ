! Purpose: Create a new file

      module mio_fcreate_module

        use mio_parameter_module
        use mio_global_data_module
        use mio_search_module
        use mio_get_env_module
        use mio_process_header_info_module
        use mio_file_template_module

        implicit none

        logical :: subset_layer

        private :: mio_output_mpas_basic_var,             &
                   mio_process_wrf_var_att,               &
                   mio_process_ioapi3_var_att,            &
                   mio_transfer_global_attribute_info,    &
                   mio_transfer_mpas_base_variable_info,  &
                   mio_extract_wrf_var_info,              &
                   mio_extract_mpas_var_info,             &
                   extraction,                            &
                   subset_layer

        contains

        subroutine mio_fcreate (fname, mode, replacement)

          use mio_util_func_module, only : mio_to_lower_case

          character (*), intent(in) :: fname
          character (*), intent(in), optional :: replacement   ! replacing certain global attributes
          integer,       intent(in) :: mode

          integer :: source, dest, stat, ndims, nvars, fnvars,       &
                     n_global_atts, n, vv, vsource, vdest, tndims,   &
                     n_att, t_type, count, fnum, nlines, dim_loc,    &
                     new_file_dim_name_index(n_dim_names), ind, loc, &
                     index_mapping(n_dim_names), ind_count,          &
                     n_replacements, dim_value, layer_id1, layer_id2
          character (mio_max_varname_len) :: tvname, tdim_name(6), tunit_name
          character (500)                 :: t_type_str
          character (500)                 :: str
          character (100)                 :: var_desc, stagger, coordinates, memoryorder
          character (500)                 :: loc_replacement(2,100)
          character (30)                  :: missing_value
          logical :: entire_file, partial_file, skip_search, found,   &
                     cfile_is_an_input_file, use_new_file_template

#ifdef parallel
          include 'mpif.h'
#endif

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

          entire_file  = .false.
          partial_file = .false.
          subset_layer = .false.

          if (present(replacement)) then
             call extraction (replacement, loc_replacement, n_replacements)
          else
             n_replacements = 0
             loc_replacement = ' '
          end if

          dest = mio_search(fname)

          new_file_dim_name_index = 0

          if (dest .gt. 0) then
             write (mio_logdev, *) ' Abort in routine mio_fcreate due to output file is already existed'
             stop
          else

             found = .false.
             fnum = 0
             do while ((.not. found) .and. (fnum < mio_outfile_def_info%num_of_file_definitions))
                fnum = fnum + 1
                if (fname .eq. mio_outfile_def_info%flist(fnum)%fname) then
                   found = .true.
                end if
             end do
             if (.not. found) then
                write (mio_logdev, *) ' Abort in routine mio_fcreate due to file ', trim(fname)
                write (mio_logdev, *) ' does not have an output file definition '
                stop
             else

                if (mio_outfile_def_info%flist(fnum)%new_file_info .ne. ' ') then
                   use_new_file_template = .true.
                else
                   use_new_file_template = .false.
                end if

                if (use_new_file_template) then
                   source = 0
                   call mio_setup_new_file (fnum, mio_outfile_def_info%flist(fnum)%copy_from)
                else
                   if (mio_cfile < 0) then   ! bases on copied from information
                      source = mio_search(mio_outfile_def_info%flist(fnum)%copy_from)
                   else
                      source = mio_search(mio_file_data(mio_cfile)%filename)
                   end if

                   if (mio_file_data(source)%file_format .eq. mio_ioapi3_format) then
                      if (mio_nlays .ne. mio_file_data(source)%nlays) then
                         subset_layer = .true.
                      end if
                      layer_id1 = mio_search ('LAY', mio_file_data(source)%dim_name, mio_file_data(source)%ndims)
                   else if (mio_file_data(source)%file_format .eq.  mio_wrf_format) then
                      layer_id1 = mio_search ('bottom_top', mio_file_data(source)%dim_name, mio_file_data(source)%ndims)
                      layer_id2 = mio_search ('bottom_top_stag', mio_file_data(source)%dim_name, mio_file_data(source)%ndims)
                   else if (mio_file_data(source)%file_format .eq.  mio_mpas_format) then
                      layer_id1 = mio_search ('nVertLevels', mio_file_data(source)%dim_name, mio_file_data(source)%ndims)
                      layer_id2 = mio_search ('nVertLevelsP1', mio_file_data(source)%dim_name, mio_file_data(source)%ndims)
                   end if

                end if

                cfile_is_an_input_file = ((source <= mio_n_infiles) .and. (source > 0))

                if (((mio_cfile <= 0) .or. (.not. cfile_is_an_input_file)) .and. &
                    (mio_outfile_def_info%flist(fnum)%copy_from == '')) then
                   write (mio_logdev, *) ' Abort in routine mio_fcreate due to either mio_cfile has '
                   write (mio_logdev, *) ' not been set and/or copied from information is not known'
                   stop
                end if

             end if

!            if (mio_outfile_def_info%flist(fnum)%copy_from == ' ') then
!               source = mio_cfile
!            else
!               source = mio_search (mio_outfile_def_info%flist(fnum)%copy_from)
!            end if

             mio_nfiles = mio_nfiles + 1
             dest = mio_nfiles

             mio_file_data(dest)%filename = mio_outfile_def_info%flist(fnum)%fname

             mio_file_data(dest)%mode = mio_new

             call mio_get_env (mio_file_data(dest)%full_filename, mio_file_data(dest)%filename , ' ')

             inquire (file=mio_file_data(dest)%full_filename, exist=found)

! must synchronize before the file is created a few lines down in this subroutine
             call mio_set_barrier

             if (found) then
                if (mio_file_data(dest)%mode .eq. mio_new) then
                   print *, ' Abort in routine mio_fcreate due to output file ', trim(fname), ' is already exist '
                   stop
                end if
             end if

             if ((mio_parallelism .eq. mio_serial) .or.     &
                 (mio_parallelism .eq. mio_pseudo)) then

                if (mio_file_data(dest)%mode .eq. mio_new) then
                   if (mio_mype .eq. 0) then

                      stat = nf90_create (mio_file_data(dest)%full_filename, mode, mio_file_data(dest)%fileid)
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) 'Abort in routine mio_fcreate while creating file'
                         write (mio_logdev, *) trim(mio_file_data(dest)%filename), ' due to an error ', trim(nf90_strerror(stat))
                         stop
                      end if
                   end if
                else
                   call mio_fopen (fname, mio_file_data(dest)%mode)
                end if
             end if

         write (mio_logdev, '(a13, 3i5, 2x, a16)') ' ==d== check ', dest, mio_file_data(dest)%mode, mio_new, fname

             if (mio_file_data(dest)%mode .eq. mio_new) then

                nvars = mio_outfile_def_info%flist(fnum)%nvars

                if (nvars .eq. 0) then
                   entire_file = .true.
                   nvars = mio_file_data(source)%nvars
                else if (nvars .lt. 0) then
                   partial_file = .true.
                   nvars = nvars * (-1)
                end if

                if (mio_file_data(source)%file_format .eq. mio_mpas_format) then
                   fnvars = nvars + 1 + mio_file_data(source)%nbvars
                   mio_nbase_vars = 1 + mio_file_data(source)%nbvars
                else
                   fnvars = nvars + 1
                   mio_nbase_vars = 1
                end if

                mio_file_data(dest)%nsteps        = 0

                mio_file_data(dest)%ndims         = mio_file_data(source)%ndims
                mio_file_data(dest)%nvars         = nvars
                mio_file_data(dest)%fnvars        = fnvars
                mio_file_data(dest)%nbvars        = mio_file_data(source)%nbvars
                mio_file_data(dest)%n_global_atts = mio_file_data(source)%n_global_atts
                mio_file_data(dest)%unlimited     = mio_file_data(source)%unlimited
                mio_file_data(dest)%mode          = mode

                mio_file_data(dest)%time_strlen_dim_loc = mio_file_data(source)%time_strlen_dim_loc
                mio_file_data(dest)%time_dim_loc        = mio_file_data(source)%time_dim_loc
                mio_file_data(dest)%layer_dim_loc       = mio_file_data(source)%layer_dim_loc

                mio_file_data(dest)%link          = -1

                ndims         = mio_file_data(dest)%ndims
                n_global_atts = mio_file_data(dest)%n_global_atts

                allocate (mio_file_data(dest)%dim_name(ndims),                            &
                          mio_file_data(dest)%dim_value(ndims),                           &
                          mio_file_data(dest)%var_name(fnvars),                           &
                          mio_file_data(dest)%lvar_name(fnvars),                          &
                          mio_file_data(dest)%units(fnvars),                              &
                          mio_file_data(dest)%var_time_dep(fnvars),                       &
                          mio_file_data(dest)%var_type(fnvars),                           &
                          mio_file_data(dest)%var_decomp(fnvars),                         &
                          mio_file_data(dest)%var_grid_type(fnvars),                      &
                          mio_file_data(dest)%var_id(fnvars),                             &
                          mio_file_data(dest)%var_ndims(fnvars),                          &
                          mio_file_data(dest)%var_dimids(ndims, fnvars),                  &
                          mio_file_data(dest)%var_dimsize(ndims, fnvars),                 &
                          mio_file_data(dest)%num_var_att(fnvars),                        &
                          mio_file_data(dest)%var_att_name(mio_max_num_var_att, fnvars),  &
                          mio_file_data(dest)%var_att_len(mio_max_num_var_att, fnvars),   &
                          mio_file_data(dest)%var_att_type(mio_max_num_var_att, fnvars),  &
                          mio_file_data(dest)%int_vatt_val(mio_max_num_var_att, fnvars),  &
                          mio_file_data(dest)%real_vatt_val(mio_max_num_var_att, fnvars), &
                          mio_file_data(dest)%char_vatt_val(mio_max_num_var_att, fnvars), &
                          mio_file_data(dest)%glo_att_name(n_global_atts),                &
                          mio_file_data(dest)%glo_att_type(n_global_atts),                &
                          mio_file_data(dest)%glo_att_len(n_global_atts),                 &
                          mio_file_data(dest)%glo_att_crange(n_global_atts*2),            &
                          mio_file_data(dest)%glo_att_irange(n_global_atts*2),            &
                          mio_file_data(dest)%glo_att_rrange(n_global_atts*2),            &
                          mio_file_data(dest)%glo_att_drange(n_global_atts*2),            &
                          mio_file_data(dest)%ncols_pe(mio_nprocs, 2),                    &
                          mio_file_data(dest)%nrows_pe(mio_nprocs, 2),                    &
                          mio_file_data(dest)%colde_pe(2, mio_nprocs, 2),                 &
                          mio_file_data(dest)%rowde_pe(2, mio_nprocs, 2),                 &
                          stat=stat)

                mio_file_data(dest)%dim_name  = mio_file_data(source)%dim_name
                mio_file_data(dest)%dim_value = mio_file_data(source)%dim_value

                do n = 1, n_replacements
                   dim_loc = mio_search (loc_replacement(1,n), mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                   read (loc_replacement(2,n), *) dim_value
                   mio_file_data(dest)%dim_value(dim_loc) = dim_value
                end do

                mio_file_data(dest)%ncols_pe  = mio_file_data(source)%ncols_pe

                mio_file_data(dest)%nrows_pe  = mio_file_data(source)%nrows_pe
                mio_file_data(dest)%colde_pe  = mio_file_data(source)%colde_pe
                mio_file_data(dest)%rowde_pe  = mio_file_data(source)%rowde_pe

                if (mio_file_data(source)%file_format .eq. mio_ioapi3_format) then
                   mio_file_data(dest)%dim_value(4) = nvars
                end if

                mio_file_data(dest)%file_format = mio_file_data(source)%file_format

! setup variable information
                if (entire_file) then
                   call mio_duplicate_file (source, dest)
                else if (partial_file) then
                   call mio_duplicate_partial_file (source, dest, nvars)
                else   ! variable information is from file_input.txt
                   count = mio_file_data(source)%nbvars
                   nlines = 0
                   do vv = 1, fnvars
                      if (vv .le. nvars) then          ! regular variable
                         nlines = nlines + 1
                         str = mio_outfile_def_info%flist(fnum)%vlist(nlines)
                         if (mio_file_data(source)%file_format .eq. mio_wrf_format) then
                            call mio_extract_wrf_var_info (str, mio_file_data(dest)%ndims,      &
                                                           mio_file_data(dest)%dim_name,        &
                                                           tvname, t_type_str, tndims,          &
                                                           tdim_name, tunit_name, memoryorder,  &
                                                           var_desc, stagger, coordinates)
                         else if (mio_file_data(source)%file_format .eq. mio_ioapi3_format) then
                            read (str, *) tvname, t_type_str, tndims,                 &
                                          tdim_name(1:tndims), tunit_name, var_desc
                         else if (mio_file_data(source)%file_format .eq. mio_mpas_format) then

                            call mio_extract_mpas_var_info (str, tvname, t_type_str,  &
                                                            tndims, tdim_name,        &
                                                            tunit_name, var_desc      )
                         else if (mio_file_data(source)%file_format .eq. mio_epa_format) then
                            call mio_extract_epa_var_info (str, mio_file_data(dest)%ndims,      &
                                                           mio_file_data(dest)%dim_name,        &
                                                           tvname, t_type_str, tndims,          &
                                                           tdim_name, tunit_name,               &
                                                           var_desc, missing_value)
                            do ind = 1, tndims
                               loc = mio_search (tdim_name(ind), mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                               new_file_dim_name_index(loc) = new_file_dim_name_index(loc) + 1
                            end do
                         end if

                         if (('1' .le. t_type_str(1:1)) .and. (t_type_str(1:1) .le.  '9')) then
                            read (t_type_str(1:1), *) t_type
                         else if (t_type_str(1:3) .eq. 'int') then
                            t_type = mio_int
                         else if (t_type_str(1:4) .eq. 'char') then
                            t_type = mio_char
                         else if (t_type_str(1:4) .eq. 'real') then
                            t_type = mio_real
                         else if (t_type_str(1:6) .eq. 'double') then
                            t_type = mio_double
                         end if

                         vdest = vv
!                        mio_file_data(dest)%var_id(vdest) = vdest + 1
                         skip_search = .true.
                      else if (vv .eq. fnvars) then    ! time stamp variable
                         if (mio_file_data(source)%file_format .eq. mio_wrf_format) then
                            tvname = 'Times'
                         else if (mio_file_data(source)%file_format .eq. mio_ioapi3_format) then
                            tvname = 'TFLAG'
                         else if (mio_file_data(source)%file_format .eq. mio_mpas_format) then
                            tvname = 'xtime'
                         end if
                         vdest = fnvars
!                        mio_file_data(dest)%var_id(vdest) = 1
                         skip_search = .false.
                      else if (vv .ge. fnvars-mio_file_data(source)%nbvars) then   ! mpas basic variable
                         tvname = mio_file_data(source)%var_name(mio_file_data(source)%fnvars-count)
                         count = count - 1
                         vdest = vv
!                        mio_file_data(dest)%var_id(vdest) = vdest + 1
                         skip_search = .false.
                      end if

                      if (skip_search) then
                         vsource = 0
                      else
                         vsource = mio_search(mio_to_lower_case(tvname),           &
                                              mio_file_data(source)%lvar_name, &
                                              mio_file_data(source)%fnvars)
                      end if

                      if (vsource .gt. 0) then

! for time data
!                        if (vv .eq. fnvars) then
                            tndims = mio_file_data(source)%var_ndims(vsource)
!                        end if

                         mio_file_data(dest)%var_name(vdest)      = mio_file_data(source)%var_name(vsource)
                         mio_file_data(dest)%lvar_name(vdest)     = mio_file_data(source)%lvar_name(vsource)
                         mio_file_data(dest)%units(vdest)         = mio_file_data(source)%units(vsource)
                         mio_file_data(dest)%var_type(vdest)      = mio_file_data(source)%var_type(vsource)
                         mio_file_data(dest)%var_decomp(vdest)    = mio_file_data(source)%var_decomp(vsource)
                         mio_file_data(dest)%var_grid_type(vdest) = mio_file_data(source)%var_grid_type(vsource)
                         mio_file_data(dest)%var_ndims(vdest)     = mio_file_data(source)%var_ndims(vsource)

                         mio_file_data(dest)%var_time_dep(vdest)  = mio_file_data(source)%var_time_dep(vsource)

                         tndims                                   = mio_file_data(source)%var_ndims(vsource)

                         mio_file_data(dest)%var_dimids(1:tndims,vdest)  = mio_file_data(source)%var_dimids(1:tndims,vsource)
                         mio_file_data(dest)%var_dimsize(1:tndims,vdest) = mio_file_data(source)%var_dimsize(1:tndims,vsource)
                         mio_file_data(dest)%num_var_att(vdest)          = mio_file_data(source)%num_var_att(vsource)

                         n_att                                           = mio_file_data(source)%num_var_att(vsource)
                         mio_file_data(dest)%var_att_name(1:n_att,vdest) = mio_file_data(source)%var_att_name(1:n_att,vsource)
                         mio_file_data(dest)%var_att_len(1:n_att,vdest)  = mio_file_data(source)%var_att_len(1:n_att,vsource)
                         mio_file_data(dest)%var_att_type(1:n_att,vdest) = mio_file_data(source)%var_att_type(1:n_att,vsource)
                         mio_file_data(dest)%int_vatt_val(:,vdest)       = mio_file_data(source)%int_vatt_val(:,vsource)
                         mio_file_data(dest)%real_vatt_val(:,vdest)      = mio_file_data(source)%real_vatt_val(:,vsource)
                         mio_file_data(dest)%char_vatt_val(:,vdest)      = mio_file_data(source)%char_vatt_val(:,vsource)

! usually mpas variable does not have any variable attribute and this
! allows user to provide some basic variable attribute information
!                        if ((vv .le. nvars) .and. (mio_file_data(source)%file_format .eq. mio_mpas_format)) then
                         if (vv .le. nvars) then
                            if (mio_file_data(source)%file_format .eq. mio_mpas_format) then

                               if (len_trim(tunit_name) .gt. 0) then
                                  mio_file_data(dest)%num_var_att(vdest) = 2

                                  mio_file_data(dest)%var_att_name(1,vdest) = 'units'
                                  mio_file_data(dest)%var_att_name(2,vdest) = 'Variable Description'

                                  mio_file_data(dest)%var_att_type(1,vdest) = mio_char
                                  mio_file_data(dest)%var_att_type(2,vdest) = mio_char

                                  mio_file_data(dest)%char_vatt_val(1,vdest) = trim(tunit_name)
                                  mio_file_data(dest)%char_vatt_val(2,vdest) = trim(var_desc)
                               else
                                  mio_file_data(dest)%num_var_att(vdest) = 0
                               end if
                            else if (mio_file_data(source)%file_format .eq. mio_wrf_format) then
                               call mio_process_wrf_var_att (dest, vdest, coordinates,  &
                                                             memoryorder, stagger,      &
                                                             tunit_name, var_desc, 1)
                            else if (mio_file_data(source)%file_format .eq. mio_ioapi3_format) then
                               call mio_process_ioapi3_var_att (dest, vdest,  &
                                                                tunit_name,   &
                                                                var_desc, 1)
                            end if
                         end if

                      else   ! for a new variable

                         do n = 1, tndims
                            mio_file_data(dest)%var_dimids(tndims-n+1,vdest) = mio_search(tdim_name(n),                    &
                                                                                          mio_file_data(source)%dim_name,  &
                                                                                          mio_file_data(source)%ndims)
                            dim_loc = mio_file_data(dest)%var_dimids(tndims-n+1,vdest)
                            mio_file_data(dest)%var_dimsize(tndims-n+1,vdest) = mio_file_data(dest)%dim_value(dim_loc)
                         end do

                         ! make adjustment for layer subsetting case. Currently only for IOAPI3 file type
                         if (subset_layer .and. (mio_ioapi3_format == mio_file_data(source)%file_format)) then
                            mio_file_data(dest)%var_dimsize(3,vdest) = mio_nlays
                            mio_file_data(dest)%dim_value(layer_id1) = mio_nlays
                         end if

                         if ((vdest == fnvars) .and. (source == 0)) then   ! for time variable from file template
                            mio_file_data(dest)%var_name(vdest)      = 'timestamp'
                            mio_file_data(dest)%var_type(vdest)      = mio_char
                            mio_file_data(dest)%var_ndims(vdest)     = 2
                            mio_file_data(dest)%var_dimids(1, vdest) = 2
                            mio_file_data(dest)%var_dimids(2, vdest) = 1
                         else
                            mio_file_data(dest)%var_name(vdest)  = tvname
                            mio_file_data(dest)%lvar_name(vdest) = mio_to_lower_case(tvname)
                            mio_file_data(dest)%units(vdest)     = tunit_name
                            mio_file_data(dest)%var_type(vdest)  = t_type
                            mio_file_data(dest)%var_ndims(vdest) = tndims
                         end if

                         if (mio_file_data(dest)%file_format == mio_ioapi3_format) then
                            if ((mio_search('COL', tdim_name, tndims) > 0) .and.   &
                                (mio_search('ROW', tdim_name, tndims) > 0)) then
                               mio_file_data(dest)%var_decomp(vdest) = .true.
                            else
                               mio_file_data(dest)%var_decomp(vdest) = .false.
                            end if
                         else if (mio_file_data(dest)%file_format == mio_wrf_format) then
                            if ((mio_search('south_north', tdim_name, tndims) > 0) .and.   &
                                (mio_search('west_east', tdim_name, tndims) > 0)) then
                               mio_file_data(dest)%var_decomp(vdest) = .true.
                            else
                               mio_file_data(dest)%var_decomp(vdest) = .false.
                            end if
                         else if (mio_file_data(dest)%file_format == mio_mpas_format) then
                            if (mio_search('nCells', tdim_name, tndims) > 0) then
                               mio_file_data(dest)%var_decomp(vdest) = .true.
                            else
                               mio_file_data(dest)%var_decomp(vdest) = .false.
                            end if
                         else if (mio_file_data(dest)%file_format == mio_epa_format) then
                            if ((mio_search('dimx', tdim_name, tndims) > 0) .and.   &
                                (mio_search('dimy', tdim_name, tndims) > 0)) then
                               mio_file_data(dest)%var_decomp(vdest) = .true.
                            else
                               mio_file_data(dest)%var_decomp(vdest) = .false.
                            end if
                         end if

                         if (mio_file_data(source)%file_format .eq. mio_wrf_format) then

                            call mio_process_wrf_var_att (dest, vdest, coordinates,  &
                                                          memoryorder, stagger,      &
                                                          tunit_name, var_desc)

                         else if (mio_file_data(source)%file_format .eq. mio_ioapi3_format) then

                            call mio_process_ioapi3_var_att (dest, vdest,  &
                                                             tunit_name,   &
                                                             var_desc)

                         else if (mio_file_data(source)%file_format .eq. mio_mpas_format) then

                            if (len_trim(tunit_name) .gt. 0) then
                               mio_file_data(dest)%num_var_att(vdest) = 2

                               mio_file_data(dest)%var_att_name(1,vdest) = 'units'
                               mio_file_data(dest)%var_att_name(2,vdest) = 'long_name'

                               mio_file_data(dest)%var_att_type(1,vdest) = mio_char
                               mio_file_data(dest)%var_att_type(2,vdest) = mio_char

                               mio_file_data(dest)%char_vatt_val(1,vdest) = trim(tunit_name)
                               mio_file_data(dest)%char_vatt_val(2,vdest) = trim(var_desc)
                            else
                               mio_file_data(dest)%num_var_att(vdest) = 0
                            end if
                         else if (mio_file_data(source)%file_format .eq. mio_epa_format) then
                            if (vdest == fnvars) then
                               mio_file_data(dest)%num_var_att(vdest) = 0
                            else
                               mio_file_data(dest)%num_var_att(vdest) = 4
                            end if

                            mio_file_data(dest)%var_att_name(1,vdest) = 'Standard_Name'
                            mio_file_data(dest)%var_att_name(2,vdest) = 'Description'
                            mio_file_data(dest)%var_att_name(3,vdest) = 'Units'
                            mio_file_data(dest)%var_att_name(4,vdest) = 'Missing_Value'

                            mio_file_data(dest)%var_att_type(1,vdest) = mio_char
                            mio_file_data(dest)%var_att_type(2,vdest) = mio_char
                            mio_file_data(dest)%var_att_type(3,vdest) = mio_char
                            mio_file_data(dest)%var_att_type(4,vdest) = mio_char

                            mio_file_data(dest)%char_vatt_val(1,vdest) = trim(var_desc)
                            mio_file_data(dest)%char_vatt_val(2,vdest) = trim(var_desc)
                            mio_file_data(dest)%char_vatt_val(3,vdest) = trim(tunit_name)
                            mio_file_data(dest)%char_vatt_val(4,vdest) = trim(missing_value)
                         end if

                         n = mio_file_data(dest)%var_dimids(mio_file_data(dest)%var_ndims(vdest), vdest)
                         if ((mio_file_data(dest)%dim_name(n) .eq. 'Time') .or.  &
                             (mio_file_data(dest)%dim_name(n) .eq. 'time') .or.  &
                             (mio_file_data(dest)%dim_name(n) .eq. 'TSTEP')) then
                            mio_file_data(dest)%var_time_dep(vdest) = .true.
                         else
                            mio_file_data(dest)%var_time_dep(vdest) = .false.
                         end if
                      end if
                   end do

                   if (mio_file_data(source)%file_format .eq. mio_mpas_format) then
                      call mio_transfer_mpas_base_variable_info (source, dest)
                   end if

!                  if (source > 0) then
                      call mio_transfer_global_attribute_info (source, dest)
!                  end if

                end if

                call mio_setup_decomp (mio_nprocs, mio_npcol, mio_nprow,   &
                                       mio_file_data(dest)%gl_ncols,       &
                                       mio_file_data(dest)%gl_nrows,       &
                                       mio_file_data(dest)%file_format,    &
                                       mio_file_data(dest)%ncols_pe,       &
                                       mio_file_data(dest)%nrows_pe,       &
                                       mio_file_data(dest)%colde_pe,       &
                                       mio_file_data(dest)%rowde_pe)

                if (mio_file_data(source)%file_format == mio_mpas_format) then
                   mio_file_data(dest)%ncols    = mio_file_data(dest)%gl_ncols
                   mio_file_data(dest)%nrows    = 1
                else
                   mio_file_data(dest)%ncols = mio_file_data(dest)%ncols_pe(mio_mype+1,1)
                   mio_file_data(dest)%nrows = mio_file_data(dest)%nrows_pe(mio_mype+1,1)
                end if

! reset mpas tstep step size
!               if (mio_file_data(source)%file_format .eq. mio_mpas_format) then
                n = mio_file_data(dest)%time_dim_loc
                if (mio_file_data(dest)%dim_value(n) .gt. 0) then
                   mio_file_data(dest)%dim_value(n) = nf90_unlimited
                end if
!               end if

                mio_file_data(dest)%grid_type = 'c'

                if (mio_mype .eq. 0) then
                   if (source == 0) then    ! using a file template

                      ! form a subset of dimension variable name
                      if (new_file_dim_name_index(1) > 0) then
                         ind_count = 2
                      else
                         ind_count = 0
                      end if
                      index_mapping = 0
                      do ind = 3, n_dim_names
                         if (new_file_dim_name_index(ind) > 0) then
                            ind_count = ind_count + 1
                            mio_file_data(dest)%dim_name(ind_count)  = mio_file_data(dest)%dim_name(ind)
                            mio_file_data(dest)%dim_value(ind_count) = mio_file_data(dest)%dim_value(ind)
                            index_mapping(ind) = ind - ind_count
                         end if
                      end do
                      mio_file_data(dest)%ndims = ind_count

                      ! adjust the dimids according to the subset dimension variable names
                      do vv = 1, fnvars
                         tndims = mio_file_data(dest)%var_ndims(vv)
                         do n = 1, tndims
                            ind = mio_file_data(dest)%var_dimids(n,vv)
                            mio_file_data(dest)%var_dimids(n,vv) =   mio_file_data(dest)%var_dimids(n,vv) &
                                                                   - index_mapping(ind)
                         end do
                      end do

                      call mio_define_dimension_information (mio_file_data(dest), fnum)
                   else
                      call mio_define_dimension_information (mio_file_data(dest))
                   end if

                   call mio_define_variable_information (mio_file_data(dest))

                   call mio_define_variable_attributes_information (mio_file_data(dest))

!                  call mio_define_global_attribute_information (mio_file_data(dest), loc_replacement)
                   call mio_define_global_attribute_information (mio_file_data(dest))

                   stat = nf90_enddef (mio_file_data(dest)%fileid)

                   if (stat .ne. nf90_noerr) then
                      write (mio_logdev, *) 'Abort in routine mio_fcreate due to ending netCDF definition '
                      write (mio_logdev, *) ' section for file ', trim(fname), ' with an error ', trim(nf90_strerror(stat))
                      stop
                   end if
                end if

                if (mio_parallelism .ne. mio_serial) then
                   call mio_set_barrier
                end if

             end if
          end if

! for MPAS dataset, output basic data
          if (mio_file_data(source)%file_format .eq. mio_mpas_format) then
             if (mio_mype .eq. 0) then
                call mio_output_mpas_basic_var (source, dest, ndims)
             end if
          end if

        end subroutine mio_fcreate

! ---------------------------------------------------------------------------------------------
        subroutine mio_extract_wrf_var_info (str, ndims, dim_name,        &
                                             tvname, t_type_str, tndims,  &
                                             tdim_name, tunit_name,       &
                                             memoryorder, var_desc,       &
                                             stagger, coordinates)

          character (*), intent(in)  :: str
          integer, intent(in)        :: ndims
          character (*), intent(in)  :: dim_name(:)
          character (*), intent(out) :: tvname,         &
                                        tdim_name(6),   &
                                        tunit_name
          character (*), intent(out) :: t_type_str
          character (*), intent(out) :: var_desc,       &
                                        stagger,        &
                                        coordinates,    &
                                        memoryorder
          integer, intent(out)       :: tndims

          logical, save :: firstime = .true.
          logical       :: spatial, vertical, latlon
          logical, allocatable, save :: gstagger(:)
          logical, allocatable, save :: lstagger(:)
          integer :: n, str_len, stat, stagger_loc

          if (firstime) then
             firstime = .false.
             allocate (gstagger(ndims), lstagger(ndims), stat=stat)
             do n = 1, ndims
                str_len = len_trim(dim_name(n)) 
                if (dim_name(n)(str_len-3:str_len) .eq. "stag") then
                   gstagger(n) = .true.
                else
                   gstagger(n) = .false.
                end if
             end do
          end if

!         read (str, *) tvname, t_type_str, tndims
          read (str, *) tvname, t_type_str, tndims, tdim_name(1:tndims), tunit_name, var_desc
          coordinates = ""
          stagger = ""
          latlon = .false.
          if ((tndims .eq. 1) .and. (tdim_name(1) .eq. 'Time')) then
             MemoryOrder = "0  "
          else
             stagger_loc = 0
             spatial = .false.
             vertical = .false.
             do n = 1, tndims
                str_len = len_trim(tdim_name(n)) 
                if (str_len .gt. 4) then
                   if (tdim_name(n)(str_len-3:str_len) .eq. "stag") then
                      stagger_loc = n - 1
                   else if ((tdim_name(n)(str_len-3:str_len) .eq. "east") .or.   &
                            (tdim_name(n)(str_len-3:str_len) .eq. "orth")) then
                      spatial = .true.
                   else if (tdim_name(n)(1:4) .eq. "bott") then
                      vertical = .true.
                   end if

                   if ((tdim_name(n) .eq. 'XLAT') .or. (tdim_name(n) .eq. 'XLONG')) then
                      latlon = .true.
                   end if
                end if
             end do

             if (tndims .eq. 2) then
                if ((.not. vertical) .and. (.not. spatial)) then
                   MemoryOrder = "C  "
                else if (vertical) then
                   MemoryOrder = "Z  "
                   if (stagger_loc .gt. 0) then
                      stagger = "Z"
                   end if
                else if (spatial) then
                   MemoryOrder = "XY "
                   coordinates = "XLONG XLAT"
                else
                   MemoryOrder = "Z  "
                   if (stagger_loc .gt. 0) then
                      stagger = "Z"
                   end if
                end if
             else if (tndims .eq. 3) then
                if (spatial) then
                   MemoryOrder = "XY "
                   if (latlon) then
                      coordinates = "XLONG XLAT"
                   else
                      coordinates = "XLONG XLAT XTIME"
                   end if
                end if
             else if (tndims .eq. 4) then
                MemoryOrder = "XYZ"
                coordinates = "XLONG XLAT XTIME"

                if (stagger_loc .eq. 1) then
                   stagger = "Z"
                else if (stagger_loc .eq. 2) then
                   stagger = "Y"
                else if (stagger_loc .eq. 3) then
                   stagger = "X"
                end if
             end if
          end if

        end subroutine mio_extract_wrf_var_info

! ---------------------------------------------------------------------------------------------
        subroutine mio_extract_mpas_var_info (str, tvname, t_type_str,  &
                                              tndims, tdim_name,        &
                                              tunit_name, var_desc      )

          character (*), intent(in)  :: str
          character (*), intent(out) :: tvname,         &
                                        tdim_name(6),   &
                                        tunit_name
          character (*), intent(out) :: t_type_str
          character (*), intent(out) :: var_desc
          integer, intent(out)       :: tndims

          integer :: str_len, n_arguments, i, state, remaining

          n_arguments = 1
          str_len = len_trim(str)
          i = 0
          state = 0
          do while (i < str_len)
            i = i + 1
            if ((state .eq. 0) .and. (str(i:i) .eq. ' ')) then
               state = 1
               n_arguments = n_arguments + 1
            else if ((state .eq. 1) .and. (str(i:i) .ne. ' ')) then
               state = 0
            end if
          end do

          read (str, *) tvname, t_type_str, tndims,                    &
                        tdim_name(1:tndims)

          remaining = n_arguments - tndims - 3

          if (remaining .eq. 0) then
             tunit_name = '???'
             var_desc   = '???'
          else if (remaining .eq. 1) then
             read (str, *) tvname, t_type_str, tndims,                 &
                           tdim_name(1:tndims), tunit_name
             var_desc = tvname
          else
             read (str, *) tvname, t_type_str, tndims,                 &
                           tdim_name(1:tndims), tunit_name, var_desc
          end if

        end subroutine mio_extract_mpas_var_info

! ---------------------------------------------------------------------------------------------
        subroutine mio_extract_epa_var_info (str, ndims, dim_name,        &
                                             tvname, t_type_str, tndims,  &
                                             tdim_name, tunit_name,       &
                                             var_desc, missing_value)

          character (*), intent(in)    :: str
          integer, intent(inout)       :: ndims
          character (*), intent(inout) :: dim_name(:)
          character (*), intent(out)   :: tvname,         &
                                          tdim_name(6),   &
                                          tunit_name
          character (*), intent(out)   :: t_type_str
          character (*), intent(out)   :: var_desc
          integer, intent(out)         :: tndims
          character (*), intent(out)   :: missing_value

          read (str, *) tvname, t_type_str, tndims
          read (str, *) tvname, t_type_str, tndims, tdim_name(1:tndims), tunit_name, var_desc, missing_value

        end subroutine mio_extract_epa_var_info

! ---------------------------------------------------------------------------------------------
        subroutine mio_process_wrf_var_att (dest, v, coordinates,       &
                                            memoryorder, stagger,       &
                                            tunit_name, var_desc, flag)

          integer, intent(in)           :: dest, v
          character (*), intent(in)     :: var_desc, stagger, coordinates, memoryorder
          character (*), intent(in)     :: tunit_name
          integer, intent(in), optional :: flag

          integer :: unit_loc, desc_loc

          if (present(flag)) then
             unit_loc = mio_search ('units',                                &
                                    mio_file_data(dest)%var_att_name(:,v),  &
                                    mio_file_data(dest)%num_var_att(v))
             if (unit_loc .gt. 0) then
                mio_file_data(dest)%char_vatt_val(unit_loc,v) = trim(tunit_name)
             end if
             desc_loc = mio_search ('description',                          &
                                    mio_file_data(dest)%var_att_name(:,v),  &
                                    mio_file_data(dest)%num_var_att(v))
             if (desc_loc .gt. 0) then
                if (len_trim(var_desc) .eq. 0) then
                   mio_file_data(dest)%char_vatt_val(desc_loc,v) = '-'
                else
                   mio_file_data(dest)%char_vatt_val(desc_loc,v) = trim(var_desc)
                end if
             end if
          else  ! brand new variable
             if (len_trim(coordinates) .eq. 0) then
                mio_file_data(dest)%num_var_att(v) = 5
             else
                mio_file_data(dest)%num_var_att(v) = 6
                mio_file_data(dest)%var_att_name(6,v) = 'coordinates'
                mio_file_data(dest)%var_att_type(6,v) = mio_char
                mio_file_data(dest)%char_vatt_val(6,v) = coordinates
             end if

             mio_file_data(dest)%var_att_name(1,v) = 'FieldType'
             mio_file_data(dest)%var_att_name(2,v) = 'MemoryOrder'
             mio_file_data(dest)%var_att_name(3,v) = 'description'
             mio_file_data(dest)%var_att_name(4,v) = 'units'
             mio_file_data(dest)%var_att_name(5,v) = 'stagger'

             mio_file_data(dest)%var_att_type(1,v) = mio_int
             mio_file_data(dest)%var_att_type(2,v) = mio_char
             mio_file_data(dest)%var_att_type(3,v) = mio_char
             mio_file_data(dest)%var_att_type(4,v) = mio_char
             mio_file_data(dest)%var_att_type(5,v) = mio_char

             if (mio_file_data(dest)%var_type(v) .eq. mio_real) then
                mio_file_data(dest)%int_vatt_val(1,v) = 104
             else if (mio_file_data(dest)%var_type(v) .eq. mio_int) then
                mio_file_data(dest)%int_vatt_val(1,v) = 106
             end if

             mio_file_data(dest)%char_vatt_val(2,v) = memoryorder
             mio_file_data(dest)%char_vatt_val(5,v) = stagger
             mio_file_data(dest)%char_vatt_val(4,v) = trim(tunit_name)

             if (len_trim(var_desc) .eq. 0) then
                mio_file_data(dest)%char_vatt_val(3,v) = '-'
             else
                mio_file_data(dest)%char_vatt_val(3,v) = trim(var_desc)
             end if
          end if

        end subroutine mio_process_wrf_var_att

! ---------------------------------------------------------------------------------------------
        subroutine mio_process_ioapi3_var_att (dest, v, tunit_name, var_desc, flag)

          integer, intent(in)           :: dest, v
          character (*), intent(in)     :: tunit_name
          character (*), intent(in)     :: var_desc
          integer, intent(in), optional :: flag

          if (.not. present(flag)) then
             mio_file_data(dest)%var_att_name(1,v) = 'long_name'
             mio_file_data(dest)%var_att_name(2,v) = 'units'
             mio_file_data(dest)%var_att_name(3,v) = 'var_desc'

             mio_file_data(dest)%num_var_att(v) = 3

             mio_file_data(dest)%var_att_type(1,v) = mio_char
             mio_file_data(dest)%var_att_type(2,v) = mio_char
             mio_file_data(dest)%var_att_type(3,v) = mio_char

             mio_file_data(dest)%char_vatt_val(1,v) = mio_file_data(dest)%var_name(v)
          end if

          mio_file_data(dest)%char_vatt_val(2,v) = trim(tunit_name)

          if (len_trim(var_desc) .eq. 0) then
             mio_file_data(dest)%char_vatt_val(3,v) = '-'
          else
             mio_file_data(dest)%char_vatt_val(3,v) = trim(var_desc)
          end if

        end subroutine mio_process_ioapi3_var_att 

! ---------------------------------------------------------------------------------------------
        subroutine mio_transfer_mpas_base_variable_info (source, dest)

          integer, intent(in) :: source, dest

          integer :: v, s_ind, d_ind

          do v = 1, mio_n_basic_mpas_vars

             s_ind = mio_search (mio_basic_mpas_vnames_l(v),       &
                                 mio_file_data(source)%lvar_name,  &
                                 mio_file_data(source)%fnvars)

             d_ind = mio_search (mio_basic_mpas_vnames_l(v),       &
                                 mio_file_data(dest)%lvar_name,    &
                                 mio_file_data(dest)%fnvars)

             if ((d_ind .gt. 0) .and. (s_ind .gt. 0)) then
                mio_file_data(dest)%var_type(d_ind)      = mio_file_data(source)%var_type(s_ind)
                mio_file_data(dest)%var_decomp(d_ind)    = mio_file_data(source)%var_decomp(s_ind)
                mio_file_data(dest)%var_grid_type(d_ind) = mio_file_data(source)%var_grid_type(s_ind)
                mio_file_data(dest)%var_ndims(d_ind)     = mio_file_data(source)%var_ndims(s_ind)
                mio_file_data(dest)%var_dimids(:,d_ind)  = mio_file_data(source)%var_dimids(:,s_ind)
                mio_file_data(dest)%var_dimsize(:,d_ind) = mio_file_data(source)%var_dimsize(:,s_ind)
             end if

          end do

        end subroutine  mio_transfer_mpas_base_variable_info 

! ---------------------------------------------------------------------------------------------
        subroutine mio_transfer_global_attribute_info (source, dest)

          integer, intent(in) :: source, dest

          integer :: n, s, e, s1, e1, s2, e2, nvars
          character (50000) :: vlist

! transfer global attribute information
          n = mio_file_data(dest)%n_global_atts
          mio_file_data(dest)%glo_att_name(1:n)     = mio_file_data(source)%glo_att_name(1:n)
          mio_file_data(dest)%glo_att_type(1:n)     = mio_file_data(source)%glo_att_type(1:n)
          mio_file_data(dest)%glo_att_irange(1:2*n) = mio_file_data(source)%glo_att_irange(1:n*2)
          mio_file_data(dest)%glo_att_rrange(1:2*n) = mio_file_data(source)%glo_att_rrange(1:n*2)
          mio_file_data(dest)%glo_att_drange(1:2*n) = mio_file_data(source)%glo_att_drange(1:n*2)
          mio_file_data(dest)%glo_att_ival          = mio_file_data(source)%glo_att_ival
          mio_file_data(dest)%glo_att_rval          = mio_file_data(source)%glo_att_rval
          mio_file_data(dest)%glo_att_dval          = mio_file_data(source)%glo_att_dval

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

             if (subset_layer) then
                mio_file_data(dest)%nlays = mio_nlays
             else
                mio_file_data(dest)%nlays = mio_file_data(dest)%dim_value(3)
             end if

          else if ((mio_file_data(source)%file_format .eq. mio_wrf_format) .or.   &
                   (mio_file_data(source)%file_format .eq. mio_mpas_format) .or.  &
                   (mio_file_data(source)%file_format .eq. mio_epa_format)) then
             mio_file_data(dest)%glo_att_crange(1:2*n) = mio_file_data(source)%glo_att_crange(1:n*2)
             mio_file_data(dest)%glo_att_cval          = mio_file_data(source)%glo_att_cval
             if (mio_file_data(source)%file_format .eq. mio_mpas_format) then
                n = mio_search ('nCells', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                mio_file_data(dest)%gl_ncols = mio_file_data(dest)%dim_value(n)
                n = mio_search ('nVertLevels', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                if (n < 0) then
                   n = mio_search ('nVertLevelsP1', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                   if (n < 0) then
                      mio_file_data(dest)%nlays = 1
                   else
                      if (subset_layer) then
                         mio_file_data(dest)%nlays = mio_nlays
                      else
                         mio_file_data(dest)%nlays = mio_file_data(dest)%dim_value(n) - 1
                      end if
                   end if
                else
                   if (subset_layer) then
                      mio_file_data(dest)%nlays = mio_nlays
                   else
                      mio_file_data(dest)%nlays = mio_file_data(dest)%dim_value(n)
                   end if
                end if
                mio_file_data(dest)%gl_nrows = 1
             else if (mio_file_data(source)%file_format .eq. mio_wrf_format) then
                n = mio_search ('west_east', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                mio_file_data(dest)%gl_ncols = mio_file_data(dest)%dim_value(n)
                n = mio_search ('bottom_top', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                if (n > 0) then
                   if (subset_layer) then
                      mio_file_data(dest)%nlays = mio_nlays
                   else
                      mio_file_data(dest)%nlays = mio_file_data(dest)%dim_value(n)
                   end if
                else
                   mio_file_data(dest)%nlays = 1
                end if
                n = mio_search ('south_north', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                mio_file_data(dest)%gl_nrows = mio_file_data(dest)%dim_value(n)
             else if (mio_file_data(source)%file_format .eq. mio_epa_format) then
                n = mio_search ('dimx', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                mio_file_data(dest)%gl_ncols = mio_file_data(dest)%dim_value(n)
                n = mio_search ('dimz', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                if (n > 0) then
                   if (subset_layer) then
                      mio_file_data(dest)%nlays = mio_nlays
                   else
                      mio_file_data(dest)%nlays = mio_file_data(dest)%dim_value(n)
                   end if
                else
                   mio_file_data(dest)%nlays = 1
                end if
                n = mio_search ('dimy', mio_file_data(dest)%dim_name, mio_file_data(dest)%ndims)
                mio_file_data(dest)%gl_nrows = mio_file_data(dest)%dim_value(n)
             end if
          end if

        end subroutine  mio_transfer_global_attribute_info 

! ---------------------------------------------------------------------------------------------
        subroutine mio_output_mpas_basic_var (source, dest, ndims)

          integer, intent(in) :: source, dest, ndims

          integer :: vv, n, s_ind, d_ind, stat, loc_dim(2), in_vid, out_vid, &
                     adjustment, mystart(5), mycount(5)
          logical :: error

          integer, allocatable :: idata(:,:)
          real,    allocatable :: rdata(:,:)
          real(8), allocatable :: ddata(:,:)

          adjustment = mio_file_data(dest)%nvars

          do vv = 1, mio_file_data(dest)%nbvars
             do n = 1, 2
                if (mio_basic_mpas_vnames_dim(n,vv) .eq. '-') then
                   loc_dim(n) = 1
                else
                   s_ind = mio_search (mio_basic_mpas_vnames_dim(n,vv),     &
                                       mio_file_data(dest)%dim_name,        &
                                       ndims)
                   loc_dim(n) = mio_file_data(dest)%dim_value(s_ind) 
                end if
             end do

             ! look for the vid
             s_ind = mio_search (mio_basic_mpas_vnames_l(vv),       &
                                 mio_file_data(source)%lvar_name,   &
                                 mio_file_data(source)%fnvars)

             d_ind = mio_search (mio_basic_mpas_vnames_l(vv),       &
                                 mio_file_data(dest)%lvar_name,     &
                                 mio_file_data(dest)%fnvars)

             if ((s_ind .gt. 0) .and. (d_ind .gt. 0)) then
                in_vid  = mio_file_data(source)%var_id(s_ind)
                out_vid = mio_file_data(dest)%var_id(d_ind)

                if (mio_file_data(source)%var_type(s_ind) .eq. mio_int) then
                   allocate (idata(loc_dim(1), loc_dim(2)), stat=stat)
                else if (mio_file_data(source)%var_type(s_ind) .eq. mio_real) then
                   allocate (rdata(loc_dim(1), loc_dim(2)), stat=stat)
                else if (mio_file_data(source)%var_type(s_ind) .eq. mio_double) then
                   allocate (ddata(loc_dim(1), loc_dim(2)), stat=stat)
                end if

                if (stat .ne. 0) then
                   write (mio_logdev, *) ' Abort in routine mio_fcreate due to memory allocation error'
                   stop
                end if

                error = .false.
                mycount = 1
                mystart = 1
                if (loc_dim(2) .eq. 1) then
                   mycount(1) = loc_dim(1)
                   if (mio_file_data(source)%var_type(s_ind) .eq. mio_int) then
                      stat = nf90_get_var(mio_file_data(source)%fileid, in_vid, idata(:,1))
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if

                      stat = nf90_put_var(mio_file_data(dest)%fileid, out_vid, idata(:,1))

                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if
                   else if (mio_file_data(source)%var_type(s_ind) .eq. mio_real) then
                      stat = nf90_get_var(mio_file_data(source)%fileid, in_vid, rdata(:,1))
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if

                      stat = nf90_put_var(mio_file_data(dest)%fileid, out_vid, rdata(:,1))
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if
                   else if (mio_file_data(source)%var_type(s_ind) .eq. mio_double) then
                      stat = nf90_get_var(mio_file_data(source)%fileid, in_vid, ddata(:,1))
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if

                      stat = nf90_put_var(mio_file_data(dest)%fileid, out_vid, ddata(:,1))
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if
                   end if
                else
                   mycount(1) = loc_dim(1)
                   mycount(2) = loc_dim(2)
                   if (mio_file_data(source)%var_type(s_ind) .eq. mio_int) then
                      stat = nf90_get_var(mio_file_data(source)%fileid,     &
                                          in_vid,                           &
                                          idata,                            &
                                          start = mystart,                  &
                                          count = mycount)
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if

                      stat = nf90_put_var(mio_file_data(dest)%fileid,       &
                                          out_vid,                          &
                                          idata,                            &
                                          start = mystart,                  &
                                          count = mycount)
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if
                   else if (mio_file_data(source)%var_type(s_ind) .eq. mio_real) then
                      stat = nf90_get_var(mio_file_data(source)%fileid,     &
                                          in_vid,                           &
                                          rdata,                            &
                                          start = mystart,                  &
                                          count = mycount)
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if

                      stat = nf90_put_var(mio_file_data(dest)%fileid,       &
                                          out_vid,                          &
                                          rdata,                            &
                                          start = mystart,                  &
                                          count = mycount)
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if
                   else if (mio_file_data(source)%var_type(s_ind) .eq. mio_double) then
                      stat = nf90_get_var(mio_file_data(source)%fileid,     &
                                          in_vid,                           &
                                          ddata,                            &
                                          start = mystart,                  &
                                          count = mycount)
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if

                      stat = nf90_put_var(mio_file_data(dest)%fileid,       &
                                          out_vid,                          &
                                          ddata,                            &
                                          start = mystart,                  &
                                          count = mycount)
                      if (stat .ne. nf90_noerr) then
                         write (mio_logdev, *) trim(nf90_strerror(stat))
                         error = .true.
                      end if
                   end if
                end if

                if (error) then
                   write (mio_logdev, *) ' Abort in routine mio_fcreate due to netCDF reading/writing '
                   write (mio_logdev, *) ' variable, error is ', trim(mio_basic_mpas_vnames(vv))
                   stop
                end if

                if (mio_file_data(source)%var_type(s_ind) .eq. mio_int) then
                   deallocate (idata)
                else if (mio_file_data(source)%var_type(s_ind) .eq. mio_real) then
                   deallocate (rdata)
                else if (mio_file_data(source)%var_type(s_ind) .eq. mio_double) then
                   deallocate (ddata)
                end if
             end if

          end do

        end subroutine mio_output_mpas_basic_var

! ----------------------------------------------------------------------------
          subroutine extraction (instring, outstring_array, n)

            character (*), intent(in) :: instring
            character (*), intent(out) :: outstring_array(:,:)
            integer, intent(out) :: n

            integer :: index(200)
            integer :: loc_n, slen, i, j, k, m, s, e
            logical :: encountered_lexical, double_quote
            character (500) :: loc_instring

            loc_instring = adjustl(instring)

            slen = len_trim(loc_instring)
            if (slen == 1) then
               n = 0
            else
               loc_n = 0
               index = 0
               i = 0
               encountered_lexical = .false.
               double_quote = .false.
               do while (i < slen)
                  i = i + 1
                  if (loc_instring(i:i) == '"') then
                     if (double_quote) then
                        loc_n = loc_n + 1
                        index(loc_n) = i - 1
                        double_quote = .false.
                     else
                        double_quote = .true.
                        loc_n = loc_n + 1
                        index(loc_n) = i + 1
                     end if
                  else
                     if (double_quote) then
                        ! do nothing
                     else
                        if (encountered_lexical) then
                           if (loc_instring(i:i) == ' ') then
                              encountered_lexical = .false.
                              loc_n = loc_n + 1
                              index(loc_n) = i - 1
                           end if
                        else
                           ! check the beginning of a lexical string
                           if ((('a' <= loc_instring(i:i)) .and. (loc_instring(i:i) <= 'z')) .or.   &
                               (('A' <= loc_instring(i:i)) .and. (loc_instring(i:i) <= 'Z')) .or.   &
                               (('0' <= loc_instring(i:i)) .and. (loc_instring(i:i) <= '9'))) then
                              encountered_lexical = .true.
                              loc_n = loc_n + 1
                              index(loc_n) = i
                           end if
                        end if
                     end if
                  end if
               end do
               if (encountered_lexical) then
                  loc_n = loc_n + 1
                  index(loc_n) = slen + 1
               end if

               j = 1
               do i = 1, loc_n, 2
                   s = index(i)
                   e = index(i+1)
                   k = mod(j+1,2) + 1
                   j = j + 1
                   m = j / 2
                   outstring_array(k,m) = loc_instring(s:e)
               end do
               n = loc_n / 4
            end if

          end subroutine extraction

      end module mio_fcreate_module
