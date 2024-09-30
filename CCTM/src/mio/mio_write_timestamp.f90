! Purpose: write timestamp info to a file

      subroutine mio_write_timestamp (floc, timestamp, v, t, caller)

        use mio_global_data_module, only : mio_file_data, mio_logdev,    &
                                           mio_mype
        use mio_parameter_module
        use mio_time_util_func_module, only : mio_time_format_conversion
        use mio_get_env_module
        use netcdf

        implicit none

        character (19), parameter :: pname = 'mio_write_timestamp'

        integer, intent(in) :: floc, v, t
        character (*), intent(in) :: timestamp, caller
        logical :: error

        integer :: stat, i, tflag(2), loc, str_len, pre_set_length, n
        logical :: mpas_padding
        character (mio_max_time_length), allocatable :: loc_timestampa(:)

        call mio_get_env ( mpas_padding, 'MPAS_PADDING', .false. )
        if (mpas_padding) then
           pre_set_length = mio_max_time_length
        else
           pre_set_length = len_trim(timestamp)
        end if

        if (.not. allocated(mio_file_data(floc)%timestamp)) then
           allocate (mio_file_data(floc)%timestamp(mio_preset_num_tsteps), stat=stat)
           if (stat .ne. 0) then
              write (mio_logdev, *) ' Calling from: ', trim(caller)
              write (mio_logdev, *) ' Abort in ', pname, ' due to memory allocation error'
              stop
           end if
        else
           n = size(mio_file_data(floc)%timestamp)
           ! to expand memory allocation if necessary
           if (t > n) then
              allocate (loc_timestampa(n), stat=stat)
              loc_timestampa = mio_file_data(floc)%timestamp
              deallocate (mio_file_data(floc)%timestamp)
              allocate (mio_file_data(floc)%timestamp(mio_preset_num_tsteps+n), stat=stat)
              mio_file_data(floc)%timestamp(1:n) = loc_timestampa
              deallocate (loc_timestampa)
           end if
        end if

        error = .false.
        mio_file_data(floc)%timestamp(t) = timestamp
        loc = mio_file_data(floc)%fnvars

        if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
           call mio_time_format_conversion (timestamp, tflag(1), tflag(2))

           if (mio_mype .eq. 0) then
              stat = nf90_put_var(mio_file_data(floc)%fileid,          &
                                  mio_file_data(floc)%var_id(loc),     &
                                  tflag,                               &
                                  start = (/ 1, v, t /),               &
                                  count = (/ 2, 1, 1 /) )
              if (stat .ne. nf90_noerr) then
                 write (mio_logdev, *) ' Writing time stamp data for IOAPI type file encounter: ', &
                                       trim(nf90_strerror(stat))
                 error = .true.
              end if
           end if

        else if ((mio_file_data(floc)%file_format .eq. mio_wrf_format) .or.   &  ! WRF data
                 (mio_file_data(floc)%file_format .eq. mio_mpas_format) .or.  &  ! MPAS data
                 (mio_file_data(floc)%file_format .eq. mio_epa_format)) then    ! EPA data
           if (mio_mype .eq. 0) then
              error = .false.
              str_len = len_trim(timestamp)
              i = 0
              do while ((.not. error) .and. (i .lt. pre_set_length))
                 i = i + 1
                 if (i .le. str_len) then
                    stat = nf90_put_var(mio_file_data(floc)%fileid,       &
                                        mio_file_data(floc)%var_id(loc),  &
                                        timestamp(i:i),                   &
                                        start = (/ i, t /),               &
                                        count = (/ 1, 1 /) )
                 else
                    stat = nf90_put_var(mio_file_data(floc)%fileid,       &
                                        mio_file_data(floc)%var_id(loc),  &
                                        ' ',                              &
                                        start = (/ i, t /),               &
                                        count = (/ 1, 1 /) )
                 end if
                 if (stat .ne. nf90_noerr) then
                    write (mio_logdev, *) ' Writing time stamp data for WRF or MPAS file encounter: ', &
                                          trim(nf90_strerror(stat))
                    error = .true.
                 end if
              end do
           end if
        else
           write (mio_logdev, *) ' Calling from: ', trim(caller)
           write (mio_logdev, *) ' Abort in ', pname, ' due to unknown file type'
           stop
        end if

!       if (nf90_sync (mio_file_data(floc)%fileid) .ne.  nf90_noerr) then
!          write (mio_logdev, *) ' Error: in routine ', pname
!          write (mio_logdev, *) '        cannot sync timestamp to file ', trim(mio_file_data(floc)%filename)
!          error = .true.
!       end if

        if (error) then
           write (mio_logdev, *) ' Calling from: ', trim(caller)
           write (mio_logdev, *) ' Abort in ', pname, ' due to an error while '
           write (mio_logdev, *) ' writing timestamp for file ', trim(mio_file_data(floc)%filename)
           stop
        end if

      end subroutine mio_write_timestamp
