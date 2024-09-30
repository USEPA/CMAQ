! Purpose: To write a variable to a file either in serial mode, pseudo
!          parallel mode or true parallel mode depends on whether the
!          variable is domain decomposable or not.

      module mio_fwrite_module

        use mio_global_data_module, only : mio_file_data, mio_parallelism, &
                                           mio_mype, mio_logdev,           &
                                           mio_nbase_vars, mio_mpas_dmap
        use mio_parameter_module, only : mio_max_filename_len,             &
                                         mio_max_varname_len,              &
                                         mio_max_time_length, mio_serial,  &
                                         mio_pseudo, mio_ioapi3_format,    &
                                         mio_mpas_format
        use mio_search_module
        use mio_put_data_module
        use mio_gather_data_module

        implicit none

        interface mio_fwrite
          module procedure mio_fwrite_0d_real,     &   ! write a single real value
                           mio_fwrite_1d_real,     &   ! write a 1D real array
                           mio_fwrite_2d_real,     &   ! write a 2D real array
                           mio_fwrite_3d_real,     &   ! write a 3D real array
                           mio_fwrite_0d_double,   &   ! write a single double precision value
                           mio_fwrite_1d_double,   &   ! write a 1D double precision array
                           mio_fwrite_2d_double,   &   ! write a 2D double precision array
                           mio_fwrite_3d_double,   &   ! write a 3D double precision array
                           mio_fwrite_0d_int,      &   ! write a single integer value
                           mio_fwrite_1d_int,      &   ! write a 1D integer array
                           mio_fwrite_2d_int,      &   ! write a 2D integer array
                           mio_fwrite_3d_int,      &   ! write a 3D integer array
                           mio_fwrite_char             ! write a character type variable

        end interface

        contains

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_0d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(in)          :: data
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_0d_real'
          integer :: t, v, mystart(5), mycount(5), floc
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   ! in IOAPI3 case, each variable has an associated time stamp
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1
                   mystart(1) = t

                   if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                           mio_file_data(floc)%var_id(v), &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   else
                      if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                         write (mio_logdev, *) ' Error: in routine ', trim(pname)
                         write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_0d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_1d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(in)          :: data(:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_1d_real'
          integer :: t, v, mystart(5), mycount(5), floc, stat
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.
          real, allocatable :: mio_mpas_output_1d_data (:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1

                   mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
                   mystart(2) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      allocate (mio_mpas_output_1d_data(mycount(1)), stat=stat)

                      call mio_gather_data (data, mio_file_data(floc)%file_format,  &
                                            mio_file_data(floc)%var_decomp(v),      &
                                            mio_mpas_output_1d_data)

                      if (mio_mype .eq. 0) then
                         if (.not. mio_put_data (mio_file_data(floc)%fileid,      &
                                                 mio_file_data(floc)%var_id(v),   &
                                                 mystart, mycount,                &
                                                 mio_mpas_output_1d_data) ) then
                            write (mio_logdev, *) ' Error: Not able to write 1d ', trim(vname), ' to file ', trim(fname)
                            lerror = .true.
                         else
                            if (nf90_sync (mio_file_data(floc)%fileid) .ne.  nf90_noerr) then
                               write (mio_logdev, *) ' Error: Not able to sync 1d var to file ', trim(vname)
                               lerror = .true.
                            end if
                         end if
                      end if
                      deallocate (mio_mpas_output_1d_data)
                   else  ! not mpas format
                      if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                              mio_file_data(floc)%var_id(v), &
                                              mystart, mycount,              &
                                              data) ) then
                         lerror = .true.
                      else
                         if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                            write (mio_logdev, *) ' Error: in routine ', trim(pname)
                            write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                            lerror = .true.
                         end if
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_1d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_2d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(in)          :: data(:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_2d_real'
          integer :: t, v, first_dim, mystart(5),    &
                     mycount(5), floc, stat, s1, s2
          real, allocatable :: recv_buf(:,:) 
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.
          real, allocatable :: mio_mpas_output_2d_data(:,:)
          real, allocatable :: mio_output_3d_data(:,:,:)

          floc = mio_search (fname)

          if (mio_file_data(floc)%file_format == mio_ioapi3_format) then
             s1 = size(data,1)
             s2 = size(data,2)
             allocate (mio_output_3d_data(s1,s2,1), stat=stat)
             mio_output_3d_data (:,:,1) = data
             call mio_fwrite_3d_real (fname, vname, caller, mio_output_3d_data, timestamp)
             deallocate (mio_output_3d_data)
          else

             if (floc < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find file ', trim(fname)
                lerror = .true.
             else

                if (mio_file_data(floc)%mode == mio_read_only) then
                   write (mio_logdev, *) ' Calling from: ', trim(caller)
                   write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                         ' due to writing data to a read-only file ', trim(fname)
                   stop
                end if

                v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

                if (v < 0) then
                   write (mio_logdev, *) ' Error: in routine ', trim(pname)
                   write (mio_logdev, *) '        cannot find variable ', trim(vname)
                   lerror = .true.
                else
                   if (present(timestamp)) then
                      t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                      ! when t < 0 means current time step data has not been written out
                      if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                         if (t < 0) then
                            t = mio_file_data(floc)%nsteps + 1
                            mio_file_data(floc)%nsteps = t
                         end if
                         call mio_write_timestamp (floc, timestamp, v, t, pname)
                      end if
                   else
                      t = 1
                      if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                         ! special treatment for IOAPI time independent data
                         t_timestamp = '0'
                         call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                      end if
                   end if

                   if (.not. lerror) then
                      mystart = 1
                      mycount = 1
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                      mystart(3) = t

                      if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then

                         first_dim = size(data,1)

                         if (mio_parallelism .eq. mio_serial) then
                            if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                    mio_file_data(floc)%var_id(v), &
                                                    mystart, mycount,              &
                                                    data) ) then
                               lerror = .true.
                            else
                               if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                  write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                  write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                  lerror = .true.
                               end if
                            end if

                         else if (mio_parallelism .eq. mio_pseudo) then
                            allocate (mio_mpas_output_2d_data(mycount(1), mycount(2)), stat=stat)

                            call mio_gather_data (data,                               &
                                                  mio_file_data(floc)%file_format,    &
                                                  mio_file_data(floc)%grid_type,      &
                                                  mio_file_data(floc)%colde_pe,       &
                                                  mio_file_data(floc)%rowde_pe,       &
                                                  mio_file_data(floc)%ncols_pe,       &
                                                  mio_file_data(floc)%nrows_pe,       &
                                                  mio_mpas_output_2d_data             )

                            if (mio_mype .eq. 0) then
                               if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                       mio_file_data(floc)%var_id(v), &
                                                       mystart, mycount,              &
                                                       mio_mpas_output_2d_data) ) then
                                  lerror = .true.
                               else
                                  if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                     write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                     write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                     lerror = .true.
                                  end if
                               end if
                            end if
                            deallocate (mio_mpas_output_2d_data)
                         end if

                      else   ! non-MPAS case

                         if (mio_parallelism .eq. mio_serial) then
                            if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                    mio_file_data(floc)%var_id(v), &
                                                    mystart, mycount, data) ) then
                               lerror = .true.
                            else
                               if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                  write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                  write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                  lerror = .true.
                               end if
                            end if
                         else if (mio_parallelism .eq. mio_pseudo) then
                            if (mio_mype .eq. 0) then
                               allocate (recv_buf(mio_file_data(floc)%gl_ncols,  &
                                                  mio_file_data(floc)%gl_nrows), &
                                                  stat=stat)
                            end if

                            call mio_gather_data (data,                               &
                                                  mio_file_data(floc)%file_format,    &
                                                  mio_file_data(floc)%grid_type,      &
                                                  mio_file_data(floc)%colde_pe,       &
                                                  mio_file_data(floc)%rowde_pe,       &
                                                  mio_file_data(floc)%ncols_pe,       &
                                                  mio_file_data(floc)%nrows_pe,       &
                                                  recv_buf                        )

                            if (mio_mype .eq. 0) then
                               if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                       mio_file_data(floc)%var_id(v), &
                                                       mystart, mycount, recv_buf) ) then
                                  lerror = .true.
                               else
                                  if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                     write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                     write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                     lerror = .true.
                                  end if
                               end if
                               deallocate (recv_buf)
                            end if
                         end if
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_2d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_3d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(in)          :: data(:,:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_3d_real'
          integer :: t, v, third_dim, mystart(5),    &
                     mycount(5), floc, stat
          real, allocatable :: recv_buf(:,:,:) 
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   if (.not. allocated(mio_file_data(floc)%timestamp)) then
                      t = -1
                   else
                      t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   end if
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1
                   mystart(4) = t
                   mycount(1:3) = mio_file_data(floc)%var_dimsize(1:3,v)

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      ! do nothing, have not encountered any MPAS 3D variables
                   else
                      third_dim = size(data,3)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                 mio_file_data(floc)%var_id(v), &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         else
                            if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                               write (mio_logdev, *) ' Error: in routine ', trim(pname)
                               write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                               lerror = .true.
                            end if
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_mype .eq. 0) then
                            allocate (recv_buf(mio_file_data(floc)%gl_ncols,  &
                                               mio_file_data(floc)%gl_nrows,  &
                                               third_dim),                    &
                                               stat=stat)
                         end if

                         call mio_gather_data (data,                               &
                                               mio_file_data(floc)%grid_type,      &
                                               mio_file_data(floc)%colde_pe,       &
                                               mio_file_data(floc)%rowde_pe,       &
                                               mio_file_data(floc)%ncols_pe,       &
                                               mio_file_data(floc)%nrows_pe,       &
                                               recv_buf                        )

                         if (mio_mype .eq. 0) then
                            if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                    mio_file_data(floc)%var_id(v), &
                                                    mystart, mycount, recv_buf) ) then
                               lerror = .true.
                            else
                               if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                  write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                  write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                  lerror = .true.
                               end if
                            end if
                            deallocate (recv_buf)
                         end if
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_3d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_0d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(in)        :: data
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_0d_double'
          integer :: t, v, mystart(5), mycount(5), floc
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1
                   mystart(1) = t

                   if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                           mio_file_data(floc)%var_id(v), &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   else
                      if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                         write (mio_logdev, *) ' Error: in routine ', trim(pname)
                         write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_0d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_1d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(in)        :: data(:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_1d_double'
          integer :: t, v, mystart(5), mycount(5), floc, stat
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.
          real*8, allocatable :: mio_mpas_output_1d_data (:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1

                   mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
                   mystart(2) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      allocate (mio_mpas_output_1d_data(mycount(1)), stat=stat)

                      call mio_gather_data (data, mio_file_data(floc)%file_format,  &
                                            mio_file_data(floc)%var_decomp(v),      &
                                            mio_mpas_output_1d_data)

                      if (mio_mype .eq. 0) then
                         if (.not. mio_put_data (mio_file_data(floc)%fileid,      &
                                                 mio_file_data(floc)%var_id(v),   &
                                                 mystart, mycount,                &
                                                 mio_mpas_output_1d_data) ) then
                            write (mio_logdev, *) ' Error: Not able to write 1d ', trim(vname), ' to file ', trim(fname)
                            lerror = .true.
                         else
                            if (nf90_sync (mio_file_data(floc)%fileid) .ne.  nf90_noerr) then
                               write (mio_logdev, *) ' Error: Not able to sync 1d var to file ', trim(vname)
                               lerror = .true.
                            end if
                         end if
                      end if
                      deallocate (mio_mpas_output_1d_data)
                   else  ! not mpas format
                      if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                              mio_file_data(floc)%var_id(v), &
                                              mystart, mycount,              &
                                              data) ) then
                         lerror = .true.
                      else
                         if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                            write (mio_logdev, *) ' Error: in routine ', trim(pname)
                            write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                            lerror = .true.
                         end if
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_1d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_2d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(in)        :: data(:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_2d_double'
          integer :: t, v, first_dim, mystart(5),    &
                     mycount(5), floc, stat
          real*8, allocatable :: recv_buf(:,:) 
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.
          real*8, allocatable :: mio_mpas_output_2d_data(:,:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1
                   mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   mystart(3) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then

                      first_dim = size(data,1)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                 mio_file_data(floc)%var_id(v), &
                                                 mystart, mycount,              &
                                                 data) ) then
                            lerror = .true.
                         else
                            if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                               write (mio_logdev, *) ' Error: in routine ', trim(pname)
                               write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                               lerror = .true.
                            end if
                         end if

                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_output_2d_data(mycount(1), mycount(2)), stat=stat)

                         call mio_gather_data (data,                               &
                                               mio_file_data(floc)%file_format,    &
                                               mio_file_data(floc)%grid_type,      &
                                               mio_file_data(floc)%colde_pe,       &
                                               mio_file_data(floc)%rowde_pe,       &
                                               mio_file_data(floc)%ncols_pe,       &
                                               mio_file_data(floc)%nrows_pe,       &
                                               mio_mpas_output_2d_data             )

                         if (mio_mype .eq. 0) then
                            if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                    mio_file_data(floc)%var_id(v), &
                                                    mystart, mycount,              &
                                                    mio_mpas_output_2d_data) ) then
                               lerror = .true.
                            else
                               if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                  write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                  write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                  lerror = .true.
                               end if
                            end if
                         end if
                         deallocate (mio_mpas_output_2d_data)
                      end if

                   else   ! non-MPAS case

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                 mio_file_data(floc)%var_id(v), &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         else
                            if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                               write (mio_logdev, *) ' Error: in routine ', trim(pname)
                               write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                               lerror = .true.
                            end if
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_mype .eq. 0) then
                            allocate (recv_buf(mio_file_data(floc)%gl_ncols,  &
                                               mio_file_data(floc)%gl_nrows), &
                                               stat=stat)
                         end if

                         call mio_gather_data (data,                               &
                                               mio_file_data(floc)%file_format,    &
                                               mio_file_data(floc)%grid_type,      &
                                               mio_file_data(floc)%colde_pe,       &
                                               mio_file_data(floc)%rowde_pe,       &
                                               mio_file_data(floc)%ncols_pe,       &
                                               mio_file_data(floc)%nrows_pe,       &
                                               recv_buf                        )

                         if (mio_mype .eq. 0) then
                            if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                    mio_file_data(floc)%var_id(v), &
                                                    mystart, mycount, recv_buf) ) then
                               lerror = .true.
                            else
                               if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                  write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                  write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                  lerror = .true.
                               end if
                            end if
                            deallocate (recv_buf)
                         end if
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_2d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_3d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(in)        :: data(:,:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_3d_double'
          integer :: t, v, third_dim, mystart(5),    &
                     mycount(5), floc, stat
          real*8, allocatable :: recv_buf(:,:,:) 
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1
                   mystart(4) = t
                   mycount(1:3) = mio_file_data(floc)%var_dimsize(1:3,v)

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      ! do nothing, have not encountered any MPAS 3D variables
                   else
                      third_dim = size(data,3)
                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                 mio_file_data(floc)%var_id(v), &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         else
                            if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                               write (mio_logdev, *) ' Error: in routine ', trim(pname)
                               write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                               lerror = .true.
                            end if
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_mype .eq. 0) then
                            allocate (recv_buf(mio_file_data(floc)%gl_ncols,  &
                                               mio_file_data(floc)%gl_nrows,  &
                                               third_dim),                    &
                                               stat=stat)
                         end if

                         call mio_gather_data (data,                               &
                                               mio_file_data(floc)%grid_type,      &
                                               mio_file_data(floc)%colde_pe,       &
                                               mio_file_data(floc)%rowde_pe,       &
                                               mio_file_data(floc)%ncols_pe,       &
                                               mio_file_data(floc)%nrows_pe,       &
                                               recv_buf                        )

                         if (mio_mype .eq. 0) then
                            if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                    mio_file_data(floc)%var_id(v), &
                                                    mystart, mycount, recv_buf) ) then
                               lerror = .true.
                            else
                               if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                  write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                  write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                  lerror = .true.
                               end if
                            end if
                            deallocate (recv_buf)
                         end if
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_3d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_0d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(in)       :: data
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_0d_int'
          integer :: t, v, mystart(5), mycount(5), floc
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1
                   mystart(1) = t

                   if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                           mio_file_data(floc)%var_id(v), &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   else
                      if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                         write (mio_logdev, *) ' Error: in routine ', trim(pname)
                         write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_0d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_1d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(in)       :: data(:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_1d_int'
          integer :: t, v, mystart(5), mycount(5), floc, stat
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.
          integer, allocatable :: mio_mpas_output_1d_data (:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1

                   mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
                   mystart(2) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      allocate (mio_mpas_output_1d_data(mycount(1)), stat=stat)

                      call mio_gather_data (data, mio_file_data(floc)%file_format,  &
                                            mio_file_data(floc)%var_decomp(v),      &
                                            mio_mpas_output_1d_data)

                      if (mio_mype .eq. 0) then
                         if (.not. mio_put_data (mio_file_data(floc)%fileid,      &
                                                 mio_file_data(floc)%var_id(v),   &
                                                 mystart, mycount,                &
                                                 mio_mpas_output_1d_data) ) then
                            write (mio_logdev, *) ' Error: Not able to write 1d ', trim(vname), ' to file ', trim(fname)
                            lerror = .true.
                         else
                            if (nf90_sync (mio_file_data(floc)%fileid) .ne.  nf90_noerr) then
                               write (mio_logdev, *) ' Error: Not able to sync 1d var to file ', trim(vname)
                               lerror = .true.
                            end if
                         end if
                      end if
                      deallocate (mio_mpas_output_1d_data)
                   else  ! not mpas format
                      if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                              mio_file_data(floc)%var_id(v), &
                                              mystart, mycount,              &
                                              data) ) then
                         lerror = .true.
                      else
                         if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                            write (mio_logdev, *) ' Error: in routine ', trim(pname)
                            write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                            lerror = .true.
                         end if
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_1d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_2d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(in)       :: data(:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_2d_int'
          integer :: t, v, first_dim, mystart(5),    &
                     mycount(5), floc, stat
          integer, allocatable :: recv_buf(:,:) 
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.
          integer, allocatable :: mio_mpas_output_2d_data(:,:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1
                   mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   mystart(3) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then

                      first_dim = size(data,1)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                 mio_file_data(floc)%var_id(v), &
                                                 mystart, mycount,              &
                                                 data) ) then
                            lerror = .true.
                         else
                            if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                               write (mio_logdev, *) ' Error: in routine ', trim(pname)
                               write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                               lerror = .true.
                            end if
                         end if

                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_output_2d_data(mycount(1), mycount(2)), stat=stat)

                         call mio_gather_data (data,                               &
                                               mio_file_data(floc)%file_format,    &
                                               mio_file_data(floc)%grid_type,      &
                                               mio_file_data(floc)%colde_pe,       &
                                               mio_file_data(floc)%rowde_pe,       &
                                               mio_file_data(floc)%ncols_pe,       &
                                               mio_file_data(floc)%nrows_pe,       &
                                               mio_mpas_output_2d_data             )

                         if (mio_mype .eq. 0) then
                            if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                    mio_file_data(floc)%var_id(v), &
                                                    mystart, mycount,              &
                                                    mio_mpas_output_2d_data) ) then
                               lerror = .true.
                            else
                               if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                  write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                  write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                  lerror = .true.
                               end if
                            end if
                         end if
                         deallocate (mio_mpas_output_2d_data)
                      end if

                   else   ! non-MPAS case

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                 mio_file_data(floc)%var_id(v), &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         else
                            if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                               write (mio_logdev, *) ' Error: in routine ', trim(pname)
                               write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                               lerror = .true.
                            end if
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_mype .eq. 0) then
                            allocate (recv_buf(mio_file_data(floc)%gl_ncols,  &
                                               mio_file_data(floc)%gl_nrows), &
                                               stat=stat)
                         end if

                         call mio_gather_data (data,                               &
                                               mio_file_data(floc)%file_format,    &
                                               mio_file_data(floc)%grid_type,      &
                                               mio_file_data(floc)%colde_pe,       &
                                               mio_file_data(floc)%rowde_pe,       &
                                               mio_file_data(floc)%ncols_pe,       &
                                               mio_file_data(floc)%nrows_pe,       &
                                               recv_buf                        )

                         if (mio_mype .eq. 0) then
                            if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                    mio_file_data(floc)%var_id(v), &
                                                    mystart, mycount, recv_buf) ) then
                               lerror = .true.
                            else
                               if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                  write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                  write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                  lerror = .true.
                               end if
                            end if
                            deallocate (recv_buf)
                         end if
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_2d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_3d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(in)       :: data(:,:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_3d_int'
          integer :: t, v, third_dim, mystart(5),    &
                     mycount(5), floc, stat
          integer, allocatable :: recv_buf(:,:,:) 
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1
                   mystart(4) = t
                   mycount(1:3) = mio_file_data(floc)%var_dimsize(1:3,v)

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      ! do nothing, have not encountered any MPAS 3D variables
                   else
                      third_dim = size(data,3)
                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                 mio_file_data(floc)%var_id(v), &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         else
                            if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                               write (mio_logdev, *) ' Error: in routine ', trim(pname)
                               write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                               lerror = .true.
                            end if
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_mype .eq. 0) then
                            allocate (recv_buf(mio_file_data(floc)%gl_ncols,  &
                                               mio_file_data(floc)%gl_nrows,  &
                                               third_dim),                    &
                                               stat=stat)
                         end if

                         call mio_gather_data (data,                               &
                                               mio_file_data(floc)%grid_type,      &
                                               mio_file_data(floc)%colde_pe,       &
                                               mio_file_data(floc)%rowde_pe,       &
                                               mio_file_data(floc)%ncols_pe,       &
                                               mio_file_data(floc)%nrows_pe,       &
                                               recv_buf                        )

                         if (mio_mype .eq. 0) then
                            if (.not. mio_put_data (mio_file_data(floc)%fileid,    &
                                                    mio_file_data(floc)%var_id(v), &
                                                    mystart, mycount, recv_buf) ) then
                               lerror = .true.
                            else
                               if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                                  write (mio_logdev, *) ' Error: in routine ', trim(pname)
                                  write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                                  lerror = .true.
                               end if
                            end if
                            deallocate (recv_buf)
                         end if
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_3d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fwrite_char (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          character (*), intent(in) :: data
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fwrite_char'
          integer :: t, v, mystart(5), mycount(5), floc
          character (mio_max_time_length) :: t_timestamp
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

             if (mio_file_data(floc)%mode == mio_read_only) then
                write (mio_logdev, *) ' Calling from: ', trim(caller)
                write (mio_logdev, *) ' Abort in routine ', trim(pname),                        &
                                      ' due to writing data to a read-only file ', trim(fname)
                stop
             end if

             if (mio_file_data(floc)%link .ne. -1) then
                floc = mio_file_data(floc)%link
             end if

             v = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%fnvars)

             if (v < 0) then
                write (mio_logdev, *) ' Error: in routine ', trim(pname)
                write (mio_logdev, *) '        cannot find variable ', trim(vname)
                lerror = .true.
             else
                if (present(timestamp)) then
                   t = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
                   ! when t < 0 means current time step data has not been written out
                   if ((t < 0) .or. (mio_file_data(floc)%file_format .eq. mio_ioapi3_format)) then
                      if (t < 0) then
                         t = mio_file_data(floc)%nsteps + 1
                         mio_file_data(floc)%nsteps = t
                      end if
                      call mio_write_timestamp (floc, timestamp, v, t, pname)
                   end if
                else
                   t = 1
                   if (mio_file_data(floc)%file_format .eq. mio_ioapi3_format) then
                      ! special treatment for IOAPI time independent data
                      t_timestamp = '0'
                      call mio_write_timestamp (floc, t_timestamp, v, t, pname)
                   end if
                end if

                if (.not. lerror) then
                   mystart = 1
                   mycount = 1
                   mystart(2) = t
                   mycount(1) = mio_file_data(floc)%var_dimsize(1,v)

                   if (.not. mio_put_data (mio_file_data(floc)%fileid,     &
                                           mio_file_data(floc)%var_id(v),  &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   else
                      if (nf90_sync (mio_file_data(floc)%fileid) .ne. nf90_noerr) then
                         write (mio_logdev, *) ' Error: in routine ', trim(pname)
                         write (mio_logdev, *) '        cannot sync ', trim(vname), ' to file ', trim(fname)
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from: ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to an error in writing ',  &
                                     trim(vname), ' to file ', trim(fname)
             stop
          end if

        end subroutine mio_fwrite_char

      end module mio_fwrite_module
