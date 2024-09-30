! Purpose: 

! Implementation note:
!
!   -- procedure mio_locate_time is used to determine an interpolating time lies in between which two
!      time steps, head_time_loc and tail_time_loc, respectively. If interpolating time matches with a
!      particular time step on file, head_time_loc points to that time step and tail_time_loc points to 
!      the next step on file. If tail_time_loc = -1, it means next time step does not exist on file
!
!   -- Due to compiler incapable of handling conditional shortcut as shown below
!
!        if ((mio_file_data(floc)%head_loc(vloc) == -1) .or.
!            ((mio_file_data(floc)%tail_loc(vloc) .ne -1) .and.
!             (mio_file_data(floc)%cb_tstamp(loc_tail,vloc) < mio_file_data(floc)%timestamp(head_time_loc)))) then
!
!      different cases are determined first and then calculation is done accordingly
!
!   -- There are four cases:
!
!      case 1  (cb_tstamp does not have anything)
!
!                     head    tail
!        cb_tstamp     -1      -1
!
!      case 2  (cb_tstamp and timestamp are in sync)
!
!                     head    tail
!        cb_tstamp      |       |
!        timestamp      |       |
!
!      case 3  (cb_tstamp and timestamp are off by one time step)
!
!                     head    tail
!        cb_tstamp      |       |
!        timestamp              |       |
!                             head     tail
!
!      case 4  (cb_tstamp and timestamp are out of sync, latter case should not
!               happend in real simulation since time is marching forward, but
!               this implemenation is able to handle it)
!
!                     head    tail
!        cb_tstamp      |       |
!        timestamp                   |       |
!                                  head     tail
!
!        or
!
!                                  head    tail
!        cb_tstamp                   |       |
!        timestamp      |       |
!                     head     tail

      module mio_interpolation_module

        use mio_parameter_module
        use mio_global_data_module
        use mio_search_module
        use mio_time_util_func_module
        use mio_fread_module

        implicit none

        interface mio_interpolate_var
          module procedure mio_interpolate_var_1d_real_jdatetime,    &
                           mio_interpolate_var_1d_real_time_str,     &
                           mio_interpolate_var_2d_real_jdatetime,    &
                           mio_interpolate_var_2d_real_time_str,     &
                           mio_interpolate_var_2db_real_jdatetime,   &
                           mio_interpolate_var_2db_real_time_str,    &
                           mio_interpolate_var_3d_real_jdatetime,    &
                           mio_interpolate_var_3d_real_time_str
        end interface

        contains

! ------------------------------------------------------------------------------
! Purpose: To initialize variable interpolation capability by setting up head 
!          and tail index for each variable; starting and ending memory location 
!          of the head, tail, and stored data; circular time stamp buffer, count
!          the number of real variable in the file in order to reserve correct 
!          amount of space

        subroutine mio_interpolation_init (num_of_infiles)

          integer, intent(in) :: num_of_infiles

          integer :: stat, file, t, v, ndim, vcount(6), &
                     start(6), end(6), tsize, n, vtype

          do file = 1, num_of_infiles

             allocate (mio_file_data(file)%head_loc(mio_file_data(file)%nvars),            &
                       mio_file_data(file)%tail_loc(mio_file_data(file)%nvars),            &
                       mio_file_data(file)%data_index(2, 0:2, mio_file_data(file)%nvars),  &
                       mio_file_data(file)%cb_tstamp(0:2, mio_file_data(file)%nvars),      &
                       stat=stat)

             mio_file_data(file)%head_loc = -1
             mio_file_data(file)%tail_loc = -1

             vcount = 0
             start  = 1
             end    = 0
             do v = 1, mio_file_data(file)%nvars
                vtype = mio_file_data(file)%var_type(v)
                vcount(vtype) = vcount(vtype) + 1

                if (mio_file_data(file)%var_time_dep(v)) then
                   ndim = mio_file_data(file)%var_ndims(v) - 1
                else
                   ndim = mio_file_data(file)%var_ndims(v)
                end if

                tsize = 1
                if (mio_file_data(file)%var_decomp(v)) then
                   if (mio_file_data(file)%file_format == mio_mpas_format) then
                      do t = 1, ndim-1
                         tsize = tsize * mio_file_data(file)%var_dimsize(t,v) 
                      end do
                      if (allocated(mio_mpas_dmap)) then
                         tsize = tsize * mio_mpas_dmap(0, mio_mype)
                      else
                         tsize = tsize
                      end if
                   else
                      ! for the spatial domain portion
                      tsize = tsize * mio_file_data(file)%ncols_pe(mio_mype_p1, 1)  &
                                    * mio_file_data(file)%nrows_pe(mio_mype_p1, 1)

                      ! for the non-spatial domain portion
                      do t = 3, ndim
                         tsize = tsize * mio_file_data(file)%var_dimsize(t,v) 
                      end do
                   end if
                else
                   do t = 1, ndim
                      tsize = tsize * mio_file_data(file)%var_dimsize(t,v) 
                   end do
                end if

                do n = 0, 2
                   mio_file_data(file)%data_index(1, n, v) = start(vtype)
                   end(vtype) = start(vtype) + tsize - 1
                   mio_file_data(file)%data_index(2, n, v) = end(vtype)
                   start(vtype) = end(vtype) + 1
                end do

! for checking purposes
!               write (6, '(a13, i2, i3, 2l, i2, i8, i2, 1x, a16, 6i10, 2i8, 10i3)') ' ==d== interp ', &
!                     file, v, mio_file_data(file)%var_time_dep(v), mio_file_data(file)%var_decomp(v), &
!                     mio_file_data(file)%var_ndims(v), tsize, vtype,                                  &
!                     mio_file_data(file)%var_name(v),                                                 &
!                     mio_file_data(file)%data_index(:,0,v),                                           &
!                     mio_file_data(file)%data_index(:,1,v),                                           &
!                     mio_file_data(file)%data_index(:,2,v),                                           &
!                     mio_file_data(file)%var_dimsize(1:ndim,v)

             end do

             mio_file_data(file)%n_vars = vcount

! no need to interpolate integer variable
!            if (vcount(mio_int) > 0) then
!               allocate (mio_file_data(file)%i_data(end(mio_int)), &
!                         stat=stat)
!            end if

             if (vcount(mio_real) > 0) then
                allocate (mio_file_data(file)%r_data(end(mio_real)), &
                          stat=stat)
             end if

! no need to interpolate double precision variable since input is 32-bits
!            if (vcount(mio_double) > 0) then
!               allocate (mio_file_data(file)%d_data(end(mio_double)), &
!                         stat=stat)
!            end if

          end do

        end subroutine mio_interpolation_init

! ------------------------------------------------------------------------------
! Purpose: To interpolate a 1D real variable

        subroutine mio_interpolate_var_1d_real_jdatetime (fname, vname, date, time, data)

          character (*), intent(in) :: fname, vname
          integer, intent(in)       :: date, time
          real, intent(inout)       :: data(:)

          character (19) :: time_stamp

          call mio_time_format_conversion (date, time, time_stamp)

          call  mio_interpolate_var_1d_real_time_str (fname, vname, time_stamp, data)

        end subroutine mio_interpolate_var_1d_real_jdatetime

! ------------------------------------------------------------------------------
        subroutine mio_interpolate_var_1d_real_time_str (fname, vname, tstamp, data)

          character (*), intent(in) :: fname, vname, tstamp
          real, intent(out)         :: data(:)

          character(27), parameter :: pname = 'mio_interpolate_var_1d_real'

          integer :: head_time_loc, tail_time_loc, vtype, dsize, floc, vloc, &
                     head_beg_index, head_end_index,                         &
                     tail_beg_index, tail_end_index,                         &
                     stored_beg_index, stored_end_index, dim1,               &
                     loc_head, loc_tail, case
          logical :: lerror
          real    :: diff1, diff2, ratio1, ratio2

          lerror = .false.
          floc = mio_search (fname) 
          if (floc < 0) then
             write (mio_logdev, '(a19)') 'File does not exist'
             lerror = .true. 
          else
             vloc = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%nvars)
             vtype = mio_file_data(floc)%var_type(vloc)
             if (vloc < 0) then
                write (mio_logdev, '(a23)') 'Variable does not exist'
                lerror = .true. 
             else if (vtype .ne. mio_real) then
                write (mio_logdev, '(a19)') 'Wrong variable type'
                lerror = .true. 
             else
                call mio_locate_time (floc, tstamp, head_time_loc, tail_time_loc)

                if ((head_time_loc == -1) .and. (tail_time_loc == -1)) then
                   write (mio_logdev, '(a71)') 'Interpolating time is outside time interval in the file'
                   lerror = .true. 
                else if (tail_time_loc == -1) then
                   write (mio_logdev, '(a71)') 'Interpolation requires two time steps but the ending one does not exist'
                   lerror = .true. 
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, '(a19)') 'Abort in ', pname, ' due to above error'
             stop
          else
             dim1 = size(data,1)

             dsize = dim1

             loc_head = mio_file_data(floc)%head_loc(vloc)
             loc_tail = mio_file_data(floc)%tail_loc(vloc)

             if (loc_head == -1) then
                case = 1
             else if (mio_file_data(floc)%cb_tstamp(loc_head,vloc) == mio_file_data(floc)%timestamp(head_time_loc)) then
                case = 2
             else if (mio_file_data(floc)%cb_tstamp(loc_tail,vloc) == mio_file_data(floc)%timestamp(head_time_loc)) then
                case = 3
             else
                case = 4
             end if

             if ((case == 1) .or. (case == 4)) then

                mio_file_data(floc)%head_loc(vloc) = 0
                mio_file_data(floc)%tail_loc(vloc) = 1

                loc_head = mio_file_data(floc)%head_loc(vloc)
                loc_tail = mio_file_data(floc)%tail_loc(vloc)

                mio_file_data(floc)%cb_tstamp(loc_head,vloc) = mio_file_data(floc)%timestamp(head_time_loc)
                mio_file_data(floc)%cb_tstamp(loc_tail,vloc) = mio_file_data(floc)%timestamp(tail_time_loc)

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                call mio_fread (fname, vname, pname, data, mio_file_data(floc)%timestamp(head_time_loc))
                mio_file_data(floc)%r_data(head_beg_index:head_end_index) = reshape (data, (/ dsize /))

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

                call mio_fread (fname, vname, pname, data, mio_file_data(floc)%timestamp(tail_time_loc))
                mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) = reshape (data, (/ dsize /))

             else if (case == 2) then

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

             else if (case == 3) then

                loc_head = mod(loc_head+1,2)
                loc_tail = mod(loc_tail+1,2)

                mio_file_data(floc)%head_loc(vloc) = loc_head
                mio_file_data(floc)%tail_loc(vloc) = loc_tail

                mio_file_data(floc)%cb_tstamp(loc_tail,vloc) = mio_file_data(floc)%timestamp(tail_time_loc)

                call mio_fread (fname, vname, pname, data, mio_file_data(floc)%timestamp(tail_time_loc))

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

                mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) = reshape (data, (/ dsize /))

             end if

             stored_beg_index = mio_file_data(floc)%data_index(1,2,vloc)
             stored_end_index = mio_file_data(floc)%data_index(2,2,vloc)

             if (mio_file_data(floc)%cb_tstamp(2,vloc) .ne. tstamp) then
                mio_file_data(floc)%cb_tstamp(2,vloc) = tstamp

                diff1 = real(mio_time_diff (tstamp, mio_file_data(floc)%cb_tstamp(loc_head,vloc)), 4)
                diff2 = real(mio_time_diff (mio_file_data(floc)%cb_tstamp(loc_tail,vloc), tstamp), 4)

                ratio2 = diff1 / (diff1 + diff2)
                ratio1 = 1.0 - ratio2

                mio_file_data(floc)%r_data(stored_beg_index:stored_end_index) =           &
                    mio_file_data(floc)%r_data(head_beg_index:head_end_index) * ratio1    &
                  + mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) * ratio2
             end if

             data = mio_file_data(floc)%r_data(stored_beg_index:stored_end_index)

          end if

        end subroutine mio_interpolate_var_1d_real_time_str

! ------------------------------------------------------------------------------
! Purpose: To interpolate a 2D real variable

        subroutine mio_interpolate_var_2d_real_jdatetime (fname, vname, date, time, data)

          character (*), intent(in) :: fname, vname
          integer, intent(in)       :: date, time
          real, intent(inout)       :: data(:,:)

          character (19) :: time_stamp

          call mio_time_format_conversion (date, time, time_stamp)

          call  mio_interpolate_var_2d_real_time_str (fname, vname, time_stamp, data)

        end subroutine mio_interpolate_var_2d_real_jdatetime

! ------------------------------------------------------------------------------
        subroutine mio_interpolate_var_2d_real_time_str (fname, vname, tstamp, data)

          character (*), intent(in) :: fname, vname, tstamp
          real, intent(out)         :: data(:,:)

          character(27), parameter :: pname = 'mio_interpolate_var_2d_real'

          integer :: head_time_loc, tail_time_loc, vtype, dsize, floc, vloc, &
                     head_beg_index, head_end_index,                         &
                     tail_beg_index, tail_end_index,                         &
                     stored_beg_index, stored_end_index, dim1, dim2,         &
                     loc_head, loc_tail, case
          logical :: lerror
          real    :: diff1, diff2, ratio1, ratio2

          lerror = .false.
          floc = mio_search (fname) 
          if (floc < 0) then
             write (mio_logdev, '(a19)') 'File does not exist'
             lerror = .true. 
          else
             vloc = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%nvars)
             vtype = mio_file_data(floc)%var_type(vloc)
             if (vloc < 0) then
                write (mio_logdev, '(a23)') 'Variable does not exist'
                lerror = .true. 
             else if (vtype .ne. mio_real) then
                write (mio_logdev, '(a19)') 'Wrong variable type'
                lerror = .true. 
             else
                call mio_locate_time (floc, tstamp, head_time_loc, tail_time_loc)

                if ((head_time_loc == -1) .and. (tail_time_loc == -1)) then
                   write (mio_logdev, '(a71)') 'Interpolating time is outside time interval in the file'
                   lerror = .true. 
                else if (tail_time_loc == -1) then
                   write (mio_logdev, '(a71)') 'Interpolation requires two time steps but the ending one does not exist'
                   lerror = .true. 
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, '(a19)') 'Abort in ', pname, ' due to above error'
             stop
          else
             dim1 = size(data,1)
             dim2 = size(data,2)

             dsize = dim1 * dim2

             loc_head = mio_file_data(floc)%head_loc(vloc)
             loc_tail = mio_file_data(floc)%tail_loc(vloc)

             if (loc_head == -1) then
                case = 1
             else if (mio_file_data(floc)%cb_tstamp(loc_head,vloc) == mio_file_data(floc)%timestamp(head_time_loc)) then
                case = 2
             else if (mio_file_data(floc)%cb_tstamp(loc_tail,vloc) == mio_file_data(floc)%timestamp(head_time_loc)) then
                case = 3
             else
                case = 4
             end if

             if ((case == 1) .or. (case == 4)) then

                mio_file_data(floc)%head_loc(vloc) = 0
                mio_file_data(floc)%tail_loc(vloc) = 1

                loc_head = mio_file_data(floc)%head_loc(vloc)
                loc_tail = mio_file_data(floc)%tail_loc(vloc)

                mio_file_data(floc)%cb_tstamp(loc_head,vloc) = mio_file_data(floc)%timestamp(head_time_loc)
                mio_file_data(floc)%cb_tstamp(loc_tail,vloc) = mio_file_data(floc)%timestamp(tail_time_loc)

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                call mio_fread (fname, vname, pname, data, mio_file_data(floc)%timestamp(head_time_loc))
                mio_file_data(floc)%r_data(head_beg_index:head_end_index) = reshape (data, (/ dsize /))

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

                call mio_fread (fname, vname, pname, data, mio_file_data(floc)%timestamp(tail_time_loc))
                mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) = reshape (data, (/ dsize /))

             else if (case == 2) then

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

             else if (case == 3) then

                loc_head = mod(loc_head+1,2)
                loc_tail = mod(loc_tail+1,2)

                mio_file_data(floc)%head_loc(vloc) = loc_head
                mio_file_data(floc)%tail_loc(vloc) = loc_tail

                mio_file_data(floc)%cb_tstamp(loc_tail,vloc) = mio_file_data(floc)%timestamp(tail_time_loc)

                call mio_fread (fname, vname, pname, data, mio_file_data(floc)%timestamp(tail_time_loc))

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

                mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) = reshape (data, (/ dsize /))

             end if

             stored_beg_index = mio_file_data(floc)%data_index(1,2,vloc)
             stored_end_index = mio_file_data(floc)%data_index(2,2,vloc)

             if (mio_file_data(floc)%cb_tstamp(2,vloc) .ne. tstamp) then
                mio_file_data(floc)%cb_tstamp(2,vloc) = tstamp

                diff1 = real(mio_time_diff (tstamp, mio_file_data(floc)%cb_tstamp(loc_head,vloc)), 4)
                diff2 = real(mio_time_diff (mio_file_data(floc)%cb_tstamp(loc_tail,vloc), tstamp), 4)

                ratio2 = diff1 / (diff1 + diff2)
                ratio1 = 1.0 - ratio2

                mio_file_data(floc)%r_data(stored_beg_index:stored_end_index) =           &
                    mio_file_data(floc)%r_data(head_beg_index:head_end_index) * ratio1    &
                  + mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) * ratio2
             end if

             data = reshape(mio_file_data(floc)%r_data(stored_beg_index:stored_end_index), (/dim1,dim2/))

          end if

        end subroutine mio_interpolate_var_2d_real_time_str

! ------------------------------------------------------------------------------
! Purpose: To interpolate a 2D boundary real variable
!          Right now it is for IOAPI boundary file only and nthik is set to 1

        subroutine mio_interpolate_var_2db_real_jdatetime (fname, vname, date, time, type, data, lvl)

          character (*), intent(in) :: fname, vname
          integer, intent(in)       :: date, time
          character, intent(in)     :: type
          real, intent(inout)       :: data(:,:)
          integer, intent(in), optional :: lvl

          character (19) :: time_stamp

          call mio_time_format_conversion (date, time, time_stamp)

          if (present(lvl)) then
             call  mio_interpolate_var_2db_real_time_str (fname, vname, time_stamp, type, data, lvl)
          else
             call  mio_interpolate_var_2db_real_time_str (fname, vname, time_stamp, type, data)
          end if

        end subroutine mio_interpolate_var_2db_real_jdatetime

! ------------------------------------------------------------------------------
        subroutine mio_interpolate_var_2db_real_time_str (fname, vname, tstamp, type, data, lvl)

          character (*), intent(in) :: fname, vname, tstamp
          character, intent(in)     :: type
          real, intent(inout)       :: data(:,:)
          integer, intent(in), optional :: lvl

          integer, parameter :: nthik = 1

          character(28), parameter :: pname = 'mio_interpolate_var_2db_real'

          integer :: head_time_loc, tail_time_loc, vtype, dsize, floc, vloc, &
                     head_beg_index, head_end_index,                         &
                     tail_beg_index, tail_end_index,                         &
                     stored_beg_index, stored_end_index, dim1, dim2,         &
                     loc_head, loc_tail, case, stat, k, beg_k, end_k,        &
                     starting_pt, ib, m, size_b2d
          logical :: lerror
          real    :: diff1, diff2, ratio1, ratio2
          real, allocatable :: loc_data(:,:)
          integer, save :: lns_size, lew_size, gns_size, gew_size,   &
                           ls_start, ls_end, ln_start, ln_end,       &
                           le_start, le_end, lw_start, lw_end,       &
                           gs_skip, ge_skip, gn_skip, gw_skip
          logical, save :: loc_firstime = .true.
          logical, save :: east_pe, south_pe, west_pe, north_pe

          lerror = .false.
          floc = mio_search (fname) 
          if (floc < 0) then
             write (mio_logdev, '(a19)') 'File does not exist'
             lerror = .true. 
          else
             if (mio_file_data(floc)%file_format .ne. mio_ioapi3_format) then
                write (mio_logdev, '(a23)') 'Only IOAPI boundary file is supported'
                lerror = .true. 
             else
                vloc = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%nvars)
                vtype = mio_file_data(floc)%var_type(vloc)
                if (vloc < 0) then
                   write (mio_logdev, '(a23)') 'Variable does not exist'
                   lerror = .true. 
                else if (vtype .ne. mio_real) then
                   write (mio_logdev, '(a19)') 'Wrong variable type'
                   lerror = .true. 
                else
                   call mio_locate_time (floc, tstamp, head_time_loc, tail_time_loc)
   
                   if ((head_time_loc == -1) .and. (tail_time_loc == -1)) then
                      write (mio_logdev, '(a71)') 'Interpolating time is outside time interval in the file'
                      lerror = .true. 
                   else if (tail_time_loc == -1) then
                      write (mio_logdev, '(a71)') 'Interpolation requires two time steps but the ending one does not exist'
                      lerror = .true. 
                   end if
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, '(a19)') 'Abort in ', pname, ' due to above error'
             stop
          else
             if (loc_firstime) then
                loc_firstime = .false.

                lns_size = nthik * ( mio_file_data(floc)%ncols_pe(mio_mype_p1,1) + nthik )
                lew_size = nthik * ( mio_file_data(floc)%nrows_pe(mio_mype_p1,1) + nthik )

! write (6, *) ' ==d== inter aa ', lns_size, mio_file_data(floc)%gl_ncols, nthik
! write (6, *) ' ==d== inter bb ', lew_size, mio_file_data(floc)%gl_nrows, nthik

                ls_start = 1
                ls_end   = lns_size
                le_start = ls_end + 1
                le_end   = le_start + lew_size - 1
                ln_start = le_end + 1
                ln_end   = ln_start + lns_size - 1
                lw_start = ln_end + 1
                lw_end   = lw_start + lew_size - 1

                gns_size = nthik * ( mio_file_data(floc)%gl_ncols + nthik )
                gew_size = nthik * ( mio_file_data(floc)%gl_nrows + nthik )

                gs_skip = nthik * ( mio_file_data(floc)%colde_pe( 1, mio_mype_p1, 1 ) - 1 ) - ls_start + 1
                ge_skip = gns_size + nthik * ( mio_file_data(floc)%rowde_pe( 1, mio_mype_p1, 1 ) - 1) - le_start + 1
                gn_skip = gns_size + gew_size + nthik * ( mio_file_data(floc)%colde_pe( 1, mio_mype_p1, 1 ) - 1 ) - ln_start + 1
                gw_skip = 2 * gns_size + gew_size + nthik * ( mio_file_data(floc)%rowde_pe( 1, mio_mype_p1, 1 ) - 1 ) - lw_start + 1

                east_pe = (mod(mio_mype, mio_npcol) .eq. mio_npcol - 1)
                west_pe = (mod(mio_mype, mio_npcol) .eq. 0)
                north_pe = (mio_mype .ge. mio_npcol * (mio_nprow - 1))
                south_pe = (mio_mype .lt. mio_npcol)

! write (6, '(a19, 20i5)') ' ==d== inter 2db a ', mio_mype, size(data,1), size(data,2), &
!    ls_start, ls_end, le_start, le_end, ln_start, ln_end, lw_start, lw_end, &
!    gs_skip, ge_skip, gn_skip, gw_skip

             end if

             dim1 = mio_file_data(floc)%var_dimsize(1,vloc)
             dim2 = mio_file_data(floc)%var_dimsize(2,vloc)

             size_b2d = dim1

! write (6, '(a19, 20i5)') ' ==d== inter 2db a ', dim1, dim2, mio_mype,  &
! mio_file_data(floc)%colde_pe( :, mio_mype_p1, 1 ), mio_file_data(floc)%rowde_pe( :, mio_mype_p1, 1 ), &
! gs_skip, ge_skip, gn_skip, gw_skip

! write (6, '(a19, 20i5)') ' ==d== inter 2db b ', dim1, dim2

             allocate (loc_data(dim1, dim2), stat=stat)

             dsize = dim1 * dim2

             loc_head = mio_file_data(floc)%head_loc(vloc)
             loc_tail = mio_file_data(floc)%tail_loc(vloc)

             if (loc_head == -1) then
                case = 1
             else if (mio_file_data(floc)%cb_tstamp(loc_head,vloc) == mio_file_data(floc)%timestamp(head_time_loc)) then
                case = 2
             else if (mio_file_data(floc)%cb_tstamp(loc_tail,vloc) == mio_file_data(floc)%timestamp(head_time_loc)) then
                case = 3
             else
                case = 4
             end if

             if ((case == 1) .or. (case == 4)) then

                mio_file_data(floc)%head_loc(vloc) = 0
                mio_file_data(floc)%tail_loc(vloc) = 1

                loc_head = mio_file_data(floc)%head_loc(vloc)
                loc_tail = mio_file_data(floc)%tail_loc(vloc)

                mio_file_data(floc)%cb_tstamp(loc_head,vloc) = mio_file_data(floc)%timestamp(head_time_loc)
                mio_file_data(floc)%cb_tstamp(loc_tail,vloc) = mio_file_data(floc)%timestamp(tail_time_loc)

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                call mio_fread (fname, vname, pname, loc_data, mio_file_data(floc)%timestamp(head_time_loc))
!   write (6, '(a14, 2i7, i3, a19, 2e15.8)') ' ==d== 2db a0 ', mio_mype, dsize, head_time_loc, &
!      trim(mio_file_data(floc)%timestamp(head_time_loc)), minval(loc_data), maxval(loc_data)
                mio_file_data(floc)%r_data(head_beg_index:head_end_index) = reshape (loc_data, (/ dsize /))
!   write (6, '(a14, 3i10, 2e15.8)') ' ==d== 2db a1 ', mio_mype, head_beg_index, head_end_index, &
!     minval(mio_file_data(floc)%r_data(head_beg_index:head_end_index)), &
!     maxval(mio_file_data(floc)%r_data(head_beg_index:head_end_index))

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

                call mio_fread (fname, vname, pname, loc_data, mio_file_data(floc)%timestamp(tail_time_loc))
!   write (6, '(a14, 2i7, i3, a19, 2e15.8)') ' ==d== 2db a2 ', mio_mype, dsize, tail_time_loc, &
!      trim(mio_file_data(floc)%timestamp(tail_time_loc)), minval(loc_data), maxval(loc_data)
                mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) = reshape (loc_data, (/ dsize /))
!   write (6, '(a14, 3i10, 2e15.8)') ' ==d== 2db a3 ', mio_mype, tail_beg_index, tail_end_index, &
!     minval(mio_file_data(floc)%r_data(tail_beg_index:tail_end_index)), &
!     maxval(mio_file_data(floc)%r_data(tail_beg_index:tail_end_index))

             else if (case == 2) then

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

             else if (case == 3) then

                loc_head = mod(loc_head+1,2)
                loc_tail = mod(loc_tail+1,2)

                mio_file_data(floc)%head_loc(vloc) = loc_head
                mio_file_data(floc)%tail_loc(vloc) = loc_tail

                mio_file_data(floc)%cb_tstamp(loc_tail,vloc) = mio_file_data(floc)%timestamp(tail_time_loc)

                call mio_fread (fname, vname, pname, loc_data, mio_file_data(floc)%timestamp(tail_time_loc))

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

                mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) = reshape (loc_data, (/ dsize /))

             end if

             stored_beg_index = mio_file_data(floc)%data_index(1,2,vloc)
             stored_end_index = mio_file_data(floc)%data_index(2,2,vloc)

             if (mio_file_data(floc)%cb_tstamp(2,vloc) .ne. tstamp) then
                mio_file_data(floc)%cb_tstamp(2,vloc) = tstamp

                diff1 = real(mio_time_diff (tstamp, mio_file_data(floc)%cb_tstamp(loc_head,vloc)), 4)
                diff2 = real(mio_time_diff (mio_file_data(floc)%cb_tstamp(loc_tail,vloc), tstamp), 4)

                ratio2 = diff1 / (diff1 + diff2)
                ratio1 = 1.0 - ratio2

                mio_file_data(floc)%r_data(stored_beg_index:stored_end_index) =           &
                    mio_file_data(floc)%r_data(head_beg_index:head_end_index) * ratio1    &
                  + mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) * ratio2
             end if

             if (present(lvl)) then
                beg_k = lvl
                end_k = lvl
             else
                beg_k = 1
                end_k = mio_file_data(floc)%nlays
             end if

! write (6, '(a19, i3, 4l)') ' ==d== inter 2db b ', mio_mype, north_pe, east_pe, south_pe, west_pe
! write (6, '(a19, l)') ' ==d== inter 2db h ', allocated(mio_file_data(floc)%r_data)
! write (6, '(a19, 4i10, l)') ' ==d== inter 2db k ', floc, size(mio_file_data(floc)%r_data), stored_beg_index,stored_end_index

! write (6, '(a19, 2i10, 22e15.8)') ' ==d== inter 2db m ', stored_beg_index, stored_end_index
!   minval(mio_file_data(floc)%r_data(stored_beg_index:stored_end_index)), &
!   maxval(mio_file_data(floc)%r_data(stored_beg_index:stored_end_index))

             data = 0.0
             do k = beg_k, end_k
                starting_pt = stored_beg_index + (k - 1) * size_b2d - 1
! Construct SOUTH boundary
                if ( south_pe ) then
                   m = starting_pt + gs_skip
                   do ib = ls_start, ls_end
                      data( ib,k ) = mio_file_data(floc)%r_data( m+ib )
                   end do
! if (k == 1) then
!  write (6, '(a21, 10i7)') ' ==d== inter south b ', mio_mype, k, ls_start, ls_end, m, starting_pt, gs_skip, m+ls_start, m+ls_end
!  write (6, '(a21, 3i10, 2e15.8)') ' ==d== inter south b ', mio_mype, k, ib, minval(data(:,k)), maxval(data(:,k))
! end if
                end if

! Construct EAST boundary
                if ( east_pe ) then
                   m = starting_pt + ge_skip
                   do ib = le_start, le_end
                      data( ib,k ) = mio_file_data(floc)%r_data( m+ib)
                   end do
! if (k == 1) then
!  write (6, '(a21, 10i7)') ' ==d== inter east  b ', mio_mype, k, le_start, le_end, m, starting_pt, ge_skip, m+le_start, m+le_end
!  write (6, '(a21, 3i10, 2e15.8)') ' ==d== inter east  b ', mio_mype, k, ib, minval(data(:,k)), maxval(data(:,k))
!  end if
                end if

! Construct NORTH boundary
                if ( north_pe ) then
                   m = starting_pt + gn_skip
                   do ib = ln_start, ln_end
                      data( ib,k ) = mio_file_data(floc)%r_data( m+ib)
                   end do
! if (k == 1) then
!  write (6, '(a21, 10i7)') ' ==d== inter north b ', mio_mype, k, ln_start, ln_end, m, starting_pt, gn_skip, m+ln_start, m+ln_end
!  write (6, '(a21, 3i10, 2e15.8)') ' ==d== inter north b ', mio_mype, k, ib, minval(data(:,k)), maxval(data(:,k))
!  end if
                end if

! Construct WEST boundary
                if ( west_pe ) then
                   m = starting_pt + gw_skip
                   do ib = lw_start, lw_end
                      data( ib,k ) = mio_file_data(floc)%r_data( m+ib)
                   end do
! if (k == 1) then
!  write (6, '(a21, 10i7)') ' ==d== inter west  b ', mio_mype, k, lw_start, lw_end, m, starting_pt, gw_skip, m+lw_start, m+lw_end
!  write (6, '(a21, 3i10, 2e15.8)') ' ==d== inter west  b ', mio_mype, k, ib, minval(data(:,k)), maxval(data(:,k))
!  end if
                end if
             end do

!            data = reshape(mio_file_data(floc)%r_data(stored_beg_index:stored_end_index), (/dim1,dim2/))

             deallocate (loc_data)

          end if

        end subroutine mio_interpolate_var_2db_real_time_str

! ------------------------------------------------------------------------------
! Purpose: To interpolate a 3D real variable

        subroutine mio_interpolate_var_3d_real_jdatetime (fname, vname, date, time, data)

          character (*), intent(in) :: fname, vname
          integer, intent(in)       :: date, time
          real, intent(inout)       :: data(:,:,:)

          character (19) :: time_stamp

          call mio_time_format_conversion (date, time, time_stamp)

          call  mio_interpolate_var_3d_real_time_str (fname, vname, time_stamp, data)

        end subroutine mio_interpolate_var_3d_real_jdatetime

! ------------------------------------------------------------------------------
        subroutine mio_interpolate_var_3d_real_time_str (fname, vname, tstamp, data)

          character (*), intent(in) :: fname, vname, tstamp
          real, intent(out)         :: data(:,:,:)

          character(27), parameter :: pname = 'mio_interpolate_var_3d_real'

          integer :: head_time_loc, tail_time_loc, vtype, dsize, floc, vloc, &
                     head_beg_index, head_end_index,                         &
                     tail_beg_index, tail_end_index,                         &
                     stored_beg_index, stored_end_index, dim1, dim2, dim3,   &
                     loc_head, loc_tail, case
          logical :: lerror
          real    :: diff1, diff2, ratio1, ratio2

          lerror = .false.
          floc = mio_search (fname) 
          if (floc < 0) then
             write (mio_logdev, '(a19)') 'File does not exist'
             lerror = .true. 
          else
             vloc = mio_search (vname, mio_file_data(floc)%var_name, mio_file_data(floc)%nvars)
             vtype = mio_file_data(floc)%var_type(vloc)
             if (vloc < 0) then
                write (mio_logdev, '(a23)') 'Variable does not exist'
                lerror = .true. 
             else if (vtype .ne. mio_real) then
                write (mio_logdev, '(a19)') 'Wrong variable type'
                lerror = .true. 
             else
                call mio_locate_time (floc, tstamp, head_time_loc, tail_time_loc)

                if ((head_time_loc == -1) .and. (tail_time_loc == -1)) then
                   write (mio_logdev, '(a71)') 'Interpolating time is outside time interval in the file'
                   lerror = .true. 
                else if (tail_time_loc == -1) then
                   write (mio_logdev, '(a71)') 'Interpolation requires two time steps but the ending one does not exist'
                   lerror = .true. 
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, '(a19)') 'Abort in ', pname, ' due to above error'
             stop
          else
             dim1 = size(data,1)
             dim2 = size(data,2)
             dim3 = size(data,3)

             dsize = dim1 * dim2 * dim3

             loc_head = mio_file_data(floc)%head_loc(vloc)
             loc_tail = mio_file_data(floc)%tail_loc(vloc)

             if (loc_head == -1) then
                case = 1
             else if (mio_file_data(floc)%cb_tstamp(loc_head,vloc) == mio_file_data(floc)%timestamp(head_time_loc)) then
                case = 2
             else if (mio_file_data(floc)%cb_tstamp(loc_tail,vloc) == mio_file_data(floc)%timestamp(head_time_loc)) then
                case = 3
             else
                case = 4
             end if

             if ((case == 1) .or. (case == 4)) then

                mio_file_data(floc)%head_loc(vloc) = 0
                mio_file_data(floc)%tail_loc(vloc) = 1

                loc_head = mio_file_data(floc)%head_loc(vloc)
                loc_tail = mio_file_data(floc)%tail_loc(vloc)

                mio_file_data(floc)%cb_tstamp(loc_head,vloc) = mio_file_data(floc)%timestamp(head_time_loc)
                mio_file_data(floc)%cb_tstamp(loc_tail,vloc) = mio_file_data(floc)%timestamp(tail_time_loc)

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                call mio_fread (fname, vname, pname, data, mio_file_data(floc)%timestamp(head_time_loc))
                mio_file_data(floc)%r_data(head_beg_index:head_end_index) = reshape (data, (/ dsize /))

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

                call mio_fread (fname, vname, pname, data, mio_file_data(floc)%timestamp(tail_time_loc))
                mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) = reshape (data, (/ dsize /))

             else if (case == 2) then

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

             else if (case == 3) then

                loc_head = mod(loc_head+1,2)
                loc_tail = mod(loc_tail+1,2)

                mio_file_data(floc)%head_loc(vloc) = loc_head
                mio_file_data(floc)%tail_loc(vloc) = loc_tail

                mio_file_data(floc)%cb_tstamp(loc_tail,vloc) = mio_file_data(floc)%timestamp(tail_time_loc)

                call mio_fread (fname, vname, pname, data, mio_file_data(floc)%timestamp(tail_time_loc))

                head_beg_index = mio_file_data(floc)%data_index(1,loc_head,vloc)
                head_end_index = mio_file_data(floc)%data_index(2,loc_head,vloc)

                tail_beg_index = mio_file_data(floc)%data_index(1,loc_tail,vloc)
                tail_end_index = mio_file_data(floc)%data_index(2,loc_tail,vloc)

                mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) = reshape (data, (/ dsize /))

             end if

             stored_beg_index = mio_file_data(floc)%data_index(1,2,vloc)
             stored_end_index = mio_file_data(floc)%data_index(2,2,vloc)

             if (mio_file_data(floc)%cb_tstamp(2,vloc) .ne. tstamp) then
                mio_file_data(floc)%cb_tstamp(2,vloc) = tstamp

                diff1 = real(mio_time_diff (tstamp, mio_file_data(floc)%cb_tstamp(loc_head,vloc)), 4)
                diff2 = real(mio_time_diff (mio_file_data(floc)%cb_tstamp(loc_tail,vloc), tstamp), 4)

                ratio2 = diff1 / (diff1 + diff2)
                ratio1 = 1.0 - ratio2

                mio_file_data(floc)%r_data(stored_beg_index:stored_end_index) =           &
                    mio_file_data(floc)%r_data(head_beg_index:head_end_index) * ratio1    &
                  + mio_file_data(floc)%r_data(tail_beg_index:tail_end_index) * ratio2
             end if

             data = reshape(mio_file_data(floc)%r_data(stored_beg_index:stored_end_index), (/dim1,dim2,dim3 /))

          end if

        end subroutine mio_interpolate_var_3d_real_time_str

      end module mio_interpolation_module
