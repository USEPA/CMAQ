! Purpose: read a variable from a file

      module mio_fread_module

        use mio_global_data_module, only : mio_file_data, mio_mype_p1,      &
                                           mio_parallelism, mio_logdev,     &
                                           mio_mype, mio_mpas_dmap
        use mio_parameter_module, only : mio_max_filename_len,              &
                                         mio_max_varname_len,               &
                                         mio_max_time_length, mio_serial,   &
                                         mio_pseudo, mio_mpas_format,       &
                                         mio_wrf_format,                    &
                                         mio_ioapi3_format
        use mio_search_module
        use mio_get_data_module

        implicit none

        interface mio_fread
          module procedure mio_fread_0d_real,         &
                           mio_fread_1d_real,         &
                           mio_fread_2d_real,         &
                           mio_fread_3d_real,         &
                           mio_fread_3d_lay_real,     &
                           mio_fread_0d_double,       &
                           mio_fread_1d_double,       &
                           mio_fread_2d_double,       &
                           mio_fread_3d_double,       &
                           mio_fread_3d_lay_double,   &
                           mio_fread_0d_int,          &
                           mio_fread_1d_int,          &
                           mio_fread_2d_int,          &
                           mio_fread_3d_int,          &
                           mio_fread_3d_lay_int,      &
                           mio_fread_char
        end interface

        contains

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_0d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (17), parameter :: pname = 'mio_fread_0d_real'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(1) = t

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_0d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_1d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data(:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (17), parameter :: pname = 'mio_fread_1d_real'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          real, allocatable :: mio_mpas_input_1d_data (:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mystart(2) = t
                      mycount(1) = mio_file_data(floc)%var_dimsize(1,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                                 mio_file_data(floc)%var_id(v),   &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_1d_data(mycount(1)), stat=stat)

!       write (mio_logdev, '(a18, a16, 2x, a16, 6i5, i10, 10i5)') ' ==d== stk mio 1d ', trim(fname), trim(vname)
!       write (mio_logdev, '(a18, 5i5, i10, 10i5)') ' ==d== stk mio 1d ', mystart, mycount
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                   &
                                                 mio_file_data(floc)%var_id(v),                &
                                                 mystart, mycount, mio_mpas_input_1d_data) ) then
                            lerror = .true.
                         else
!       write (mio_logdev, '(a20, 5i8, 2e15.8)') ' ==d== stk mio 1d a ', mio_mype, mio_mpas_dmap(0, mio_mype), &
!        size(mio_mpas_input_1d_data), &
!        minval(mio_mpas_dmap(1:mio_mpas_dmap(0, mio_mype), mio_mype)), &
!        maxval(mio_mpas_dmap(1:mio_mpas_dmap(0, mio_mype), mio_mype)), &
!        minval(mio_mpas_input_1d_data), maxval(mio_mpas_input_1d_data) 
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(i) = mio_mpas_input_1d_data(mio_mpas_dmap(i, mio_mype))
                            end do
!       write (mio_logdev, '(a20, 8e15.8)') ' ==d== stk mio 1d b ', &
!         minval(data(1:mio_mpas_dmap(0, mio_mype))), &
!         maxval(data(1:mio_mpas_dmap(0, mio_mype))), &
!         sum(data(1:mio_mpas_dmap(0, mio_mype)))
                         end if
                         deallocate (mio_mpas_input_1d_data)
                      end if
                   else
                      ! this is to deal to stack group data where col = 1 and row = number of groups
                      if (mio_file_data(floc)%var_dimsize(1,v) == 1) then
                         mycount(2) = mio_file_data(floc)%var_dimsize(2,v)
                         mystart(4) = t
                      else
                         mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
                         mystart(2) = t
                      end if

!                     if (mio_parallelism .eq. mio_serial) then
!                        mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
!                     else if (mio_parallelism .eq. mio_pseudo) then
!                        if (mio_file_data(floc)%grid_type .eq. 'c') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
!                        else if (mio_file_data(floc)%grid_type .eq.  'd') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
!                        else if (mio_file_data(floc)%grid_type .eq. 'm') then  ! MPAS stack group data in IOAPI3 format (serial)
!                           mycount(2) = mio_file_data(floc)%nrows
!                           mycount(2) = size(data)
!                           mystart(4) = t
!                        end if
!                     end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,    &
                                              mio_file_data(floc)%var_id(v), &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
!       write (mio_logdev, '(a20, 20e15.8)') ' ==d== stk mio 1d h ', minval(data), maxval(data), sum(data)
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_1d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_2d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data(:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (17), parameter :: pname = 'mio_fread_2d_real'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          real, allocatable :: mio_mpas_input_2d_data (:,:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(3) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,       &
                                                 mio_file_data(floc)%var_id(v),    &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_2d_data(mycount(1), mycount(2)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                  &
                                                 mio_file_data(floc)%var_id(v),               &
                                                 mystart, mycount, mio_mpas_input_2d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(:, i) = mio_mpas_input_2d_data(:, mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_2d_data)
                      end if
                   else
                      if (mio_parallelism .eq. mio_serial) then
                         mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_file_data(floc)%grid_type .eq. 'c') then
                            mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                            mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                            mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                            mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                         else if (mio_file_data(floc)%grid_type .eq. 'd') then
                            mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                            mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                            mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                            mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                         end if
                      end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                              mio_file_data(floc)%var_id(v),   &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_2d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_real (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data(:,:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (17), parameter :: pname = 'mio_fread_3d_real'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

! write (6, *) ' ==d== fread 3d e '
                   mystart = 1
                   mycount = 1
                   mystart(4) = t

!   write (6, '(a18, i3, a16, i6, a16, i6, 3i5, 2a2)') ' ==d== fread 3d m ', v, trim(fname), mio_file_data(floc)%fileid, &
!    trim(vname), mio_file_data(floc)%var_id(v), mio_parallelism, mio_serial, mio_pseudo, mio_file_data(floc)%grid_type, '=='
                   if (mio_parallelism .eq. mio_serial) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mycount(3) = mio_file_data(floc)%var_dimsize(3,v)

!   write (6, '(a18, 10i5)') ' ==d== fread 3d r ', size(data,1), size(data,2), size(data,3)

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
! if (size(data,3) == 1) then
!   write (6, '(a18, 10e15.8)') ' ==d== fread 3d t ', minval(data(:,:,1)), maxval(data(:,:,1))
! else
!   write (6, '(a18, 10e15.8)') ' ==d== fread 3d u ', minval(data(:,:,1)), maxval(data(:,:,1)), &
! minval(data(:,:,3)), maxval(data(:,:,3)), minval(data(:,:,5)), maxval(data(:,:,5))
! end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_lay_real (fname, vname, caller, data, beg_lay, end_lay, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real, intent(out)         :: data(:,:,:)
          integer, intent(in)       :: beg_lay, end_lay
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (21), parameter :: pname = 'mio_fread_3d_lay_real'
          integer :: t, v, mystart(5), mycount(5), floc, loc_slay, loc_nlays
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

                   loc_slay  = 1
                   loc_nlays = mio_file_data(floc)%var_dimsize(1,v)

                   if (mio_file_data(floc)%var_dimids(3,v) == mio_file_data(floc)%layer_dim_loc) then
                      loc_slay  = beg_lay
                      loc_nlays = end_lay - beg_lay + 1
                   else
                      write (mio_logdev, *) ' Warning: variable ', trim(vname), ' does not have layer'
                      write (mio_logdev, *) '          structure, so layer informat is ignored'
                   end if

                   if (mio_parallelism .eq. mio_serial) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mystart(3) = loc_slay
                   mycount(3) = loc_nlays

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_lay_real

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_0d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(out)       :: data
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (19), parameter :: pname = 'mio_fread_0d_double'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(1) = t

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_0d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_1d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(out)       :: data(:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (19), parameter :: pname = 'mio_fread_1d_double'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          real*8, allocatable :: mio_mpas_input_1d_data (:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mystart(2) = t
                      mycount(1) = mio_file_data(floc)%var_dimsize(1,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                                 mio_file_data(floc)%var_id(v),   &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_1d_data(mycount(1)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                   &
                                                 mio_file_data(floc)%var_id(v),                &
                                                 mystart, mycount, mio_mpas_input_1d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(i) = mio_mpas_input_1d_data(mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_1d_data)
                      end if
                   else
                      ! this is to deal to stack group data where col = 1 and row = number of groups
                      if (mio_file_data(floc)%var_dimsize(1,v) == 1) then
                         mycount(2) = mio_file_data(floc)%var_dimsize(2,v)
                         mystart(4) = t
                      else
                         mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
                         mystart(2) = t
                      end if

!                     if (mio_parallelism .eq. mio_serial) then
!                        mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
!                     else if (mio_parallelism .eq. mio_pseudo) then
!                        if (mio_file_data(floc)%grid_type .eq. 'c') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
!                        else if (mio_file_data(floc)%grid_type .eq.  'd') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
!                        end if
!                     end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,    &
                                              mio_file_data(floc)%var_id(v), &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_1d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_2d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(out)       :: data(:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (19), parameter :: pname = 'mio_fread_2d_double'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          real*8, allocatable :: mio_mpas_input_2d_data (:,:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(3) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,       &
                                                 mio_file_data(floc)%var_id(v),    &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_2d_data(mycount(1), mycount(2)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                  &
                                                 mio_file_data(floc)%var_id(v),               &
                                                 mystart, mycount, mio_mpas_input_2d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(:, i) = mio_mpas_input_2d_data(:, mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_2d_data)
                      end if
                   else
                      if (mio_parallelism .eq. mio_serial) then
                         mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_file_data(floc)%grid_type .eq. 'c') then
                            mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                            mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                            mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                            mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                         else if (mio_file_data(floc)%grid_type .eq. 'd') then
                            mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                            mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                            mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                            mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                         end if
                      end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                              mio_file_data(floc)%var_id(v),   &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_2d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_double (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(out)       :: data(:,:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (19), parameter :: pname = 'mio_fread_3d_double'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

                   if (mio_parallelism .eq. mio_serial) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mycount(3) = mio_file_data(floc)%var_dimsize(3,v)

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_lay_double (fname, vname, caller, data, beg_lay, end_lay, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          real*8, intent(out)       :: data(:,:,:)
          integer, intent(in)       :: beg_lay, end_lay
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (23), parameter :: pname = 'mio_fread_3d_lay_double'
          integer :: t, v, mystart(5), mycount(5), floc, loc_slay, loc_nlays
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

                   loc_slay  = 1
                   loc_nlays = mio_file_data(floc)%var_dimsize(1,v)

                   if (mio_file_data(floc)%var_dimids(3,v) == mio_file_data(floc)%layer_dim_loc) then
                      loc_slay  = beg_lay
                      loc_nlays = end_lay - beg_lay + 1
                   else
                      write (mio_logdev, *) ' Warning: variable ', trim(vname), ' does not have layer'
                      write (mio_logdev, *) '          structure, so layer informat is ignored'
                   end if

                   if (mio_parallelism .eq. mio_serial) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mystart(3) = loc_slay
                   mycount(3) = loc_nlays

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_lay_double

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_0d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(out)      :: data
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (16), parameter :: pname = 'mio_fread_0d_int'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(1) = t

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_0d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_1d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(out)      :: data(:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (16), parameter :: pname = 'mio_fread_1d_int'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          integer, allocatable :: mio_mpas_input_1d_data (:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mystart(2) = t
                      mycount(1) = mio_file_data(floc)%var_dimsize(1,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                                 mio_file_data(floc)%var_id(v),   &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_1d_data(mycount(1)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                   &
                                                 mio_file_data(floc)%var_id(v),                &
                                                 mystart, mycount, mio_mpas_input_1d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(i) = mio_mpas_input_1d_data(mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_1d_data)
                      end if
                   else
                      ! this is to deal to stack group data where col = 1 and row = number of groups
                      if (mio_file_data(floc)%var_dimsize(1,v) == 1) then
                         mycount(2) = mio_file_data(floc)%var_dimsize(2,v)
                         mystart(4) = t
                      else
                         mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
                         mystart(2) = t
                      end if

!                     if (mio_parallelism .eq. mio_serial) then
!                        mycount(1) = mio_file_data(floc)%var_dimsize(1,v)
!                     else if (mio_parallelism .eq. mio_pseudo) then
!                        if (mio_file_data(floc)%grid_type .eq. 'c') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
!                        else if (mio_file_data(floc)%grid_type .eq.  'd') then
!                           mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
!                           mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
!                        else if (mio_file_data(floc)%grid_type .eq. 'm') then  ! MPAS stack group data in IOAPI3 format (serial)
!                           mycount(2) = mio_file_data(floc)%nrows
!                           mycount(2) = size(data)
!                           mystart(4) = t
!                        end if
!                     end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,    &
                                              mio_file_data(floc)%var_id(v), &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_1d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_2d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(out)      :: data(:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (16), parameter :: pname = 'mio_fread_2d_int'
          integer :: i, t, v, mystart(5), mycount(5), floc, stat
          logical :: lerror = .false.
          integer, allocatable :: mio_mpas_input_2d_data (:,:)

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(3) = t

                   if (mio_file_data(floc)%file_format .eq. mio_mpas_format) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)

                      if (mio_parallelism .eq. mio_serial) then
                         if (.not. mio_get_data (mio_file_data(floc)%fileid,       &
                                                 mio_file_data(floc)%var_id(v),    &
                                                 mystart, mycount, data) ) then
                            lerror = .true.
                         end if
                      else if (mio_parallelism .eq. mio_pseudo) then
                         allocate (mio_mpas_input_2d_data(mycount(1), mycount(2)), stat=stat)

                         if (.not. mio_get_data (mio_file_data(floc)%fileid,                  &
                                                 mio_file_data(floc)%var_id(v),               &
                                                 mystart, mycount, mio_mpas_input_2d_data) ) then
                            lerror = .true.
                         else
                            do i = 1, mio_mpas_dmap(0, mio_mype)
                               data(:, i) = mio_mpas_input_2d_data(:, mio_mpas_dmap(i, mio_mype))
                            end do
                         end if
                         deallocate (mio_mpas_input_2d_data)
                      end if
                   else
                      if (mio_parallelism .eq. mio_serial) then
                         mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                      else if (mio_parallelism .eq. mio_pseudo) then
                         if (mio_file_data(floc)%grid_type .eq. 'c') then
                            mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                            mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                            mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                            mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                         else if (mio_file_data(floc)%grid_type .eq. 'd') then
                            mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                            mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                            mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                            mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                         end if
                      end if

                      if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                              mio_file_data(floc)%var_id(v),   &
                                              mystart, mycount, data) ) then
                         lerror = .true.
                      end if
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_2d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_int (fname, vname, caller, data, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(out)      :: data(:,:,:)
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (16), parameter :: pname = 'mio_fread_3d_int'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

                   if (mio_parallelism .eq. mio_serial) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mycount(3) = mio_file_data(floc)%var_dimsize(3,v)

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_3d_lay_int (fname, vname, caller, data, beg_lay, end_lay, timestamp)

          character (*), intent(in) :: fname
          character (*), intent(in) :: vname
          integer, intent(out)      :: data(:,:,:)
          integer, intent(in)       :: beg_lay, end_lay
          character (*), intent(in) :: caller
          character (*), intent(in), optional :: timestamp

          character (20), parameter :: pname = 'mio_fread_3d_lay_int'
          integer :: t, v, mystart(5), mycount(5), floc, loc_slay, loc_nlays
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(4) = t

                   loc_slay  = 1
                   loc_nlays = mio_file_data(floc)%var_dimsize(1,v)

                   if (mio_file_data(floc)%var_dimids(3,v) == mio_file_data(floc)%layer_dim_loc) then
                      loc_slay  = beg_lay
                      loc_nlays = end_lay - beg_lay + 1
                   else
                      write (mio_logdev, *) ' Warning: variable ', trim(vname), ' does not have layer'
                      write (mio_logdev, *) '          structure, so layer informat is ignored'
                   end if

                   if (mio_parallelism .eq. mio_serial) then
                      mycount(1:2) = mio_file_data(floc)%var_dimsize(1:2,v)
                   else if (mio_parallelism .eq. mio_pseudo) then
                      if (mio_file_data(floc)%grid_type .eq. 'c') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 1)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 1)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 1)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 1)
                      else if (mio_file_data(floc)%grid_type .eq. 'd') then
                         mycount(1) = mio_file_data(floc)%ncols_pe(mio_mype_p1, 2)
                         mycount(2) = mio_file_data(floc)%nrows_pe(mio_mype_p1, 2)
                         mystart(1) = mio_file_data(floc)%colde_pe(1, mio_mype_p1, 2)
                         mystart(2) = mio_file_data(floc)%rowde_pe(1, mio_mype_p1, 2)
                      end if
                   end if

                   mystart(3) = loc_slay
                   mycount(3) = loc_nlays

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_3d_lay_int

! --------------------------------------------------------------------------------------------
        subroutine mio_fread_char (fname, vname, caller, data, timestamp)

          character (*), intent(in)  :: fname
          character (*), intent(in)  :: vname
          character (*), intent(out) :: data
          character (*), intent(in)  :: caller
          character (*), intent(in), optional :: timestamp

          character (14), parameter :: pname = 'mio_fread_char'
          integer :: t, v, mystart(5), mycount(5), floc
          logical :: lerror = .false.

          floc = mio_search (fname)

          if (floc < 0) then
             write (mio_logdev, *) ' Error: in routine ', trim(pname)
             write (mio_logdev, *) '        cannot find file ', trim(fname)
             lerror = .true.
          else

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
                else
                   t = 1
                end if

                if (t < 0) then
                   write (mio_logdev, *) ' Error: requested timestamp ', trim(timestamp), ' does not exist'
                   lerror = .true.
                else

                   mystart = 1
                   mycount = 1
                   mystart(2) = t

                   mycount(1) = mio_file_data(floc)%var_dimsize(1,v)

                   if (.not. mio_get_data (mio_file_data(floc)%fileid,      &
                                           mio_file_data(floc)%var_id(v),   &
                                           mystart, mycount, data) ) then
                      lerror = .true.
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Calling from ', trim(caller)
             write (mio_logdev, *) ' Abort in routine ', trim(pname), ' due to'
             write (mio_logdev, *) ' an error in reading ', trim(vname), ' from file ', trim(fname)
             stop
          end if

        end subroutine mio_fread_char

      end module mio_fread_module
