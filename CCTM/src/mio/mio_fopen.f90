! Purpose: open a file either with read only or read write mode.

      subroutine mio_fopen (fname, mode)

        use mio_parameter_module
        use mio_global_data_module
        use mio_get_env_module
        use mio_process_header_info_module
        use mio_search_module
        use mio_time_util_func_module, only : mio_julian_to_calendar

        implicit none

        character (*), intent(in) :: fname
        integer, intent(in)       :: mode

        character (mio_max_time_length), parameter :: zero_time = '0000-00-00_00:00:00'

        integer :: stat, fnvars, ndims, n_global_atts, unlimited,   &
                   nsteps, t, i, ins, iwe, year, month, day, hh, mm, ss,      &
                   fmode, time_strlen_dim_loc, floc
        character (mio_max_time_length) :: time_str
        character (mio_max_filename_len) :: full_name
        logical :: done
        character :: tt

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

        floc = mio_cfile

        call mio_get_env (full_name, fname, ' ')
        mio_file_data(floc)%full_filename = full_name

        t = mio_search (full_name, mio_file_data(:)%full_filename, mio_nfiles-1)

        if (t .gt. 0) then
           mio_file_data(floc)%link = t
        else 
           mio_file_data(floc)%link = -1
           if (mode .eq. mio_read_only) then
              fmode = nf90_nowrite
           else
              fmode = nf90_write
           end if

           stat = nf90_open (full_name, fmode, mio_file_data(floc)%fileid)

           if (stat == nf90_noerr) then
              if (mio_mype == 0) then
                 write (mio_logdev, '(a13, a)') 'File opened: ', trim(full_name)
              end if
           else
              write (mio_logdev, *) 'Abort in routine mio_fopen opening file ', trim(mio_file_data(floc)%filename)
              write (mio_logdev, *) '      due to an error ', trim(nf90_strerror(stat))
              stop
           end if

           stat = nf90_inquire (mio_file_data(floc)%fileid, ndims,    &
                                fnvars, n_global_atts, unlimited)

           mio_file_data(floc)%ndims         = ndims
           mio_file_data(floc)%fnvars        = fnvars
           mio_file_data(floc)%nvars         = fnvars - 1
           mio_file_data(floc)%n_global_atts = n_global_atts
           mio_file_data(floc)%mode          = mode

           allocate (mio_file_data(floc)%dim_name(ndims),                            &
                     mio_file_data(floc)%dim_value(ndims),                           &
                     mio_file_data(floc)%var_time_dep(fnvars),                       &
                     mio_file_data(floc)%var_name(fnvars),                           &
                     mio_file_data(floc)%lvar_name(fnvars),                          &
                     mio_file_data(floc)%units(fnvars),                              &
                     mio_file_data(floc)%var_type(fnvars),                           &
                     mio_file_data(floc)%var_decomp(fnvars),                         &
                     mio_file_data(floc)%var_grid_type(fnvars),                      &
                     mio_file_data(floc)%var_id(fnvars),                             &
                     mio_file_data(floc)%var_ndims(fnvars),                          &
                     mio_file_data(floc)%var_dimids(ndims, fnvars),                  &
                     mio_file_data(floc)%var_dimsize(ndims, fnvars),                 &
                     mio_file_data(floc)%num_var_att(fnvars),                        &
                     mio_file_data(floc)%var_att_name(mio_max_num_var_att, fnvars),  &
                     mio_file_data(floc)%var_att_len(mio_max_num_var_att, fnvars),   &
                     mio_file_data(floc)%var_att_type(mio_max_num_var_att, fnvars),  &
                     mio_file_data(floc)%int_vatt_val(mio_max_num_var_att, fnvars),  &
                     mio_file_data(floc)%real_vatt_val(mio_max_num_var_att, fnvars), &
                     mio_file_data(floc)%char_vatt_val(mio_max_num_var_att, fnvars), &
                     mio_file_data(floc)%glo_att_name(n_global_atts),                &
                     mio_file_data(floc)%glo_att_type(n_global_atts),                &
                     mio_file_data(floc)%glo_att_len(n_global_atts),                 &
                     mio_file_data(floc)%glo_att_crange(n_global_atts*2),            &
                     mio_file_data(floc)%glo_att_irange(n_global_atts*2),            &
                     mio_file_data(floc)%glo_att_rrange(n_global_atts*2),            &
                     mio_file_data(floc)%glo_att_drange(n_global_atts*2),            &
                     stat=stat)

           if (stat .ne. 0) then
              write (mio_logdev, *) 'Abort in routine mio_fopen due to memory allocation error'
              stop
           end if

           call mio_retrieve_dimension_information (mio_file_data(floc))

           i = mio_search ('nCells',                          &
                           mio_file_data(floc)%dim_name,      &
                           mio_file_data(floc)%ndims)

           if (i .gt. 0) then
              mio_file_data(floc)%file_format = mio_mpas_format
              mio_file_data(floc)%nvars       = fnvars - 1
           end if

           call mio_retrieve_variable_information (mio_file_data(floc))

           call mio_retrieve_global_attribute_information (mio_file_data(floc))

           i = mio_search ('TITLE',                            &
                           mio_file_data(floc)%glo_att_name,   &
                           mio_file_data(floc)%n_global_atts)

           if (i .gt. 0) then                                           !  WRF
              mio_file_data(floc)%file_format  = mio_wrf_format

              ! local WRF file DateStrLen dimension location
              time_strlen_dim_loc = mio_search ('DateStrLen',                        &
                                                mio_file_data(floc)%dim_name,   &
                                                mio_file_data(floc)%ndims)

              mio_file_data(floc)%time_strlen_dim_loc = time_strlen_dim_loc

              ins = mio_search ('south_north',                      &
                                mio_file_data(floc)%dim_name,  &
                                mio_file_data(floc)%ndims)
              if (ins .gt. 0) then
                 mio_file_data(floc)%gl_nrows = mio_file_data(floc)%dim_value(ins)
              else
                 mio_file_data(floc)%gl_nrows = 1
              end if

              iwe = mio_search ('west_east',                        &
                                mio_file_data(floc)%dim_name,  &
                                mio_file_data(floc)%ndims)
              if (iwe .gt. 0) then
                 mio_file_data(floc)%gl_ncols = mio_file_data(floc)%dim_value(iwe)
              else
                 mio_file_data(floc)%gl_ncols = 1
              end if

              i = mio_search ('bottom_top',                        &
                              mio_file_data(floc)%dim_name,   &
                              mio_file_data(floc)%ndims)
              if (i .gt. 0) then
                 mio_file_data(floc)%nlays = mio_file_data(floc)%dim_value(i)
              else
                 mio_file_data(floc)%nlays = 1
              end if

              mio_file_data(floc)%time_dim_loc = mio_search ('Time',                         &
                                                             mio_file_data(floc)%dim_name,   &
                                                             mio_file_data(floc)%ndims)

              mio_file_data(floc)%layer_dim_loc = mio_search ('bottom_top',                   &
                                                              mio_file_data(floc)%dim_name,   &
                                                              mio_file_data(floc)%ndims)

              mio_file_data(floc)%var_decomp = .false.
              do i = 1, fnvars
                 if ((mio_search (ins, mio_file_data(floc)%var_dimids(:,i),     &
                                  mio_file_data(floc)%var_ndims(i)) > 0) .and.  &
                     (mio_search (iwe, mio_file_data(floc)%var_dimids(:,i),     &
                                  mio_file_data(floc)%var_ndims(i)) > 0)) then
                    mio_file_data(floc)%var_decomp(i) = .true.
                 end if
              end do

           else if (mio_file_data(floc)%glo_att_name(1) == 'IOAPI_VERSION') then   !  IOAPI_3

              mio_file_data(floc)%file_format  = mio_ioapi3_format

              if (mio_bndy_var_ind == -1) then  ! a non-boundary file
                 mio_file_data(floc)%nbndy_cells = 0
                 mio_file_data(floc)%gl_ncols    = mio_file_data(floc)%dim_value(6)
                 mio_file_data(floc)%gl_nrows    = mio_file_data(floc)%dim_value(5)
              else
                 mio_file_data(floc)%nbndy_cells = mio_file_data(floc)%dim_value(5)
                 i = mio_search ('NCOLS',                            &
                                 mio_file_data(floc)%glo_att_name,   &
                                 mio_file_data(floc)%n_global_atts)
                 t = mio_file_data(floc)%glo_att_irange(2*i)
                 mio_file_data(floc)%gl_ncols    = mio_file_data(floc)%glo_att_ival(t)

                 i = mio_search ('NROWS',                            &
                                 mio_file_data(floc)%glo_att_name,   &
                                 mio_file_data(floc)%n_global_atts)
                 t = mio_file_data(floc)%glo_att_irange(2*i)
                 mio_file_data(floc)%gl_nrows    = mio_file_data(floc)%glo_att_ival(t)

                 i = mio_search ('NTHIK',                            &
                                 mio_file_data(floc)%glo_att_name,   &
                                 mio_file_data(floc)%n_global_atts)
                 t = mio_file_data(floc)%glo_att_irange(2*i)
                 mio_file_data(floc)%bndy_thickness = mio_file_data(floc)%glo_att_ival(t)
              end if

              mio_file_data(floc)%time_dim_loc = mio_search ('TSTEP',                        &
                                                             mio_file_data(floc)%dim_name,   &
                                                             mio_file_data(floc)%ndims)
 
              i = mio_search ('LAY',                                                         &
                              mio_file_data(floc)%dim_name,                                  &
                              mio_file_data(floc)%ndims)
              mio_file_data(floc)%layer_dim_loc = i

              mio_file_data(floc)%nlays = mio_file_data(floc)%dim_value(i)
 
              mio_file_data(floc)%var_decomp = .false.
              do i = 1, fnvars
                 if ((mio_search (5, mio_file_data(floc)%var_dimids(:,i),       &
                                  mio_file_data(floc)%var_ndims(i)) > 0) .and.  &
                     (mio_search (6, mio_file_data(floc)%var_dimids(:,i),       &
                                  mio_file_data(floc)%var_ndims(i)) > 0)) then
                    mio_file_data(floc)%var_decomp(i) = .true.
                 end if
              end do

           else if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then

              ! local MPAS file StrLen dimension location
              time_strlen_dim_loc = mio_search ('StrLen',                          &
                                                mio_file_data(floc)%dim_name, &
                                                mio_file_data(floc)%ndims)

              mio_file_data(floc)%time_strlen_dim_loc = time_strlen_dim_loc

              i = mio_search ('nCells',                     &
                              mio_file_data(floc)%dim_name, &
                              mio_file_data(floc)%ndims)

              mio_file_data(floc)%gl_ncols = mio_file_data(floc)%dim_value(i)
              mio_file_data(floc)%gl_nrows = 1

              if (mio_parallelism .eq. mio_serial) then
                 mio_file_data(floc)%ncols = mio_file_data(floc)%gl_ncols
!             else
!                need implementation
              end if
              mio_file_data(floc)%nrows = 1

              i = mio_search ('nVertLevels',                     &
                              mio_file_data(floc)%dim_name,      &
                              mio_file_data(floc)%ndims)
              if (i .gt. 0) then
                 mio_file_data(floc)%nlays = mio_file_data(floc)%dim_value(i)
              else
                 i = mio_search ('nVertLevelsP1',                &
                                 mio_file_data(floc)%dim_name,   &
                                 mio_file_data(floc)%ndims)
                 if (i .gt. 0) then
                    mio_file_data(floc)%nlays = mio_file_data(floc)%dim_value(i) - 1
                 else
                    ! # of layers information does not exist, set to 1 by default'
                    mio_file_data(floc)%nlays = 1
                 end if
              end if

              mio_file_data(floc)%time_dim_loc = mio_search ('Time',                         &
                                                             mio_file_data(floc)%dim_name,   &
                                                             mio_file_data(floc)%ndims)

              mio_file_data(floc)%layer_dim_loc = mio_search ('nVertLevels',                  &
                                                              mio_file_data(floc)%dim_name,   &
                                                              mio_file_data(floc)%ndims)

              ins = mio_search ('nCells',                      &
                                mio_file_data(floc)%dim_name,  &
                                mio_file_data(floc)%ndims)

              mio_file_data(floc)%var_decomp = .false.
              do i = 1, fnvars
                 if (mio_search (ins, mio_file_data(floc)%var_dimids(:,i),       &
                                  mio_file_data(floc)%var_ndims(i)) > 0) then
                    mio_file_data(floc)%var_decomp(i) = .true.
                 end if
              end do
           end if

!          if (unlimited .ge. 1) then
!             nsteps = mio_file_data(floc)%dim_value(mio_file_data(floc)%time_dim_loc)
!             mio_file_data(floc)%unlimited = unlimited
!          else
!             nsteps = 0
!          end if

           if (mio_file_data(floc)%time_dim_loc .gt. 0) then
              nsteps = mio_file_data(floc)%dim_value(mio_file_data(floc)%time_dim_loc)
!             if (mio_file_data(floc)%mode .eq. mio_read_write) then
!                allocate (mio_file_data(floc)%timestamp(nsteps+mio_preset_num_tsteps), stat=stat)
!             else
                 allocate (mio_file_data(floc)%timestamp(nsteps), stat=stat)
!             end if
           else
              allocate (mio_file_data(floc)%timestamp(1), stat=stat)
              mio_file_data(floc)%timestamp(1) = zero_time
              nsteps = 1
           end if

           mio_file_data(floc)%nsteps = nsteps

           ! setup domain decompostion mapping
           ! the last dimension indicates cross or dot grid
           allocate (mio_file_data(floc)%ncols_pe(mio_nprocs, 2),     &
                     mio_file_data(floc)%nrows_pe(mio_nprocs, 2),     &
                     mio_file_data(floc)%colde_pe(2, mio_nprocs, 2),  &
                     mio_file_data(floc)%rowde_pe(2, mio_nprocs, 2),  &
                     stat=stat)
 
           mio_file_data(floc)%colde_pe = 0
           mio_file_data(floc)%rowde_pe = 0

           call mio_setup_decomp (mio_nprocs, mio_npcol, mio_nprow,   &
                                  mio_file_data(floc)%gl_ncols,       &
                                  mio_file_data(floc)%gl_nrows,       &
                                  mio_file_data(floc)%file_format,    &
                                  mio_file_data(floc)%ncols_pe,       &
                                  mio_file_data(floc)%nrows_pe,       &
                                  mio_file_data(floc)%colde_pe,       &
                                  mio_file_data(floc)%rowde_pe)

           if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then         ! this is for IOAPI data 
              if (mio_parallelism .eq. mio_serial) then
                 mio_file_data(floc)%ncols = mio_file_data(floc)%gl_ncols
                 mio_file_data(floc)%nrows = mio_file_data(floc)%gl_nrows
              else
                 if (mio_file_data(floc)%ndims == 5) then                           ! boundary file 
                    mio_file_data(floc)%grid_type = 'b'
                    mio_file_data(floc)%ncols = -1
                    mio_file_data(floc)%nrows = -1
                 else
                    if ((mio_base_ncols .eq. mio_file_data(floc)%dim_value(6)) .and.         &
                        (mio_base_nrows .eq. mio_file_data(floc)%dim_value(5))) then
                       mio_file_data(floc)%grid_type = 'c'
                       mio_file_data(floc)%ncols = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                       mio_file_data(floc)%nrows = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                    else if ((mio_base_ncols .eq. mio_file_data(floc)%dim_value(6)-1) .and.  &
                             (mio_base_nrows .eq. mio_file_data(floc)%dim_value(5)-1)) then
                       mio_file_data(floc)%grid_type = 'd'
                       mio_file_data(floc)%ncols = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                       mio_file_data(floc)%nrows = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                    else if (mio_base_nrows .eq. 1) then   ! for MPAS stack group data in IOAPI3 format
                       mio_file_data(floc)%grid_type = 'm'
                       mio_file_data(floc)%ncols = mio_base_ncols
                       mio_file_data(floc)%nrows = mio_file_data(floc)%dim_value(5)
                    end if
                 end if
              end if

              allocate (mio_file_data(floc)%tflag(2,nsteps), stat=stat)
              do t = 1, nsteps
                 stat = nf90_get_var(mio_file_data(floc)%fileid,                    &
                                     mio_file_data(floc)%var_id(mio_time_var_ind),  &
                                     mio_file_data(floc)%tflag(:,t),                &
                                     start = (/ 1, 1, t /),                              &
                                     count = (/ 2, 1, 1 /))
                 if (stat .ne. nf90_noerr) then
                    write (mio_logdev, *) ' Abort in routine mio_fopen while getting time stamp info '
                    write (mio_logdev, *) ' for IOAPI_3 file due to an error ', trim(nf90_strerror(stat))
                    stop
                 end if

                 call mio_julian_to_calendar (mio_file_data(floc)%tflag(1,t), year, month, day)

                 mm = mio_file_data(floc)%tflag(2,t) / 100

                 hh = mm / 100
                 mm = mod(mm, 100)
                 ss = mod(mio_file_data(floc)%tflag(2,t), 100)

                 write (mio_file_data(floc)%timestamp(t),     &
                        '(i4.4, a1, i2.2, a1, i2.2, 1a, i2.2, 2(a1, i2.2))')     &
                        year, '-', month, '-', day, '_', hh, ':', mm, ':', ss
              end do
           else if ((mio_file_data(floc)%file_format .eq. mio_wrf_format) .or.   &   ! this is for WRF data 
                    (mio_file_data(floc)%file_format .eq. mio_mpas_format)) then     ! this is for MPAS data 

              if (mio_file_data(floc)%file_format .eq. mio_wrf_format) then          ! this is for WRF data 
                 if (mio_parallelism .eq. mio_serial) then
                    mio_file_data(floc)%ncols = mio_file_data(floc)%gl_ncols
                    mio_file_data(floc)%nrows = mio_file_data(floc)%gl_nrows
                 else
                    mio_file_data(floc)%ncols = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                    mio_file_data(floc)%nrows = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                 end if
              end if

!             allocate (mio_file_data(floc)%timestamp(nsteps), stat=stat)
              do t = 1, nsteps
                 time_str = ' '
                 done = .false.
                 i = 0
                 do while ((.not. done) .and.  (i .lt. mio_file_data(floc)%dim_value(time_strlen_dim_loc)))
                    i = i + 1
                    stat = nf90_get_var(mio_file_data(floc)%fileid,                      &
                                        mio_file_data(floc)%var_id(mio_time_var_ind),    &
                                        tt,                                                   &
                                        start = (/ i, t /),                                   &
                                        count = (/ 1, 1 /) )
                    if (stat .ne. nf90_noerr) then
                       write (mio_logdev, *) ' Abort in routine mio_fopen while getting time stamp info '
                       write (mio_logdev, *) ' due to an error ', trim(nf90_strerror(stat))
                       stop
                    end if
                    if ((tt .eq. ' ') .or. (ichar(tt) .eq. 0)) then
                       done = .true.
                    else
                       time_str(i:i) = tt
                    end if
                 end do
                 mio_file_data(floc)%timestamp(t) = time_str
              end do
!             if (nsteps > 1) then
!                mio_file_data(floc)%tstep = 0
!             else
!                mio_file_data(floc)%tstep = 0
!             end if

           end if
        end if

      end subroutine mio_fopen
