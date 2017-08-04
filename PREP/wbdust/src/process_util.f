	module process_util_module

        use misc_util_module

        implicit none

        contains

! ----------------------------------------------------------------------------
        subroutine process_data (infileid, outfileid, unlimited, spclist, &
                                 nspcs, var_name, var_type, var_ndims,    & 
                                 var_dimids, dim_len, dim_name, tstamp,   &  
                                 tstep, out_tstep, sc, ec, sr,   &
                                 er)

        use netcdf
        use get_env_module
        use io_util_module, only : compute_date_time

        integer, intent(in)             :: infileid, outfileid, unlimited,     &
                                           spclist(:), nspcs, var_type(:),     &
                                           var_ndims(:), var_dimids(:, :),     &
                                           dim_len(:), tstep,                  &
                                           sc, ec, sr, er
        integer, intent(inout)          :: out_tstep
        character (len = *), intent(in) :: var_name(:), dim_name(:), tstamp

        real, allocatable       :: data_real(:,:,:), data_real_c(:,:,:),   &
                                   data_real_sum(:,:,:)
        integer, allocatable    :: data_int(:,:,:), data_int_c(:,:,:),     &
                                   data_int_sum(:,:,:)
        integer :: s, t, n, nt, stat, date, time, col_size, row_size,      &
                   time_diff, jdate, jtime
        integer, save :: pre_tstep = 0, pre_out_tstep = 0
        integer, allocatable :: loc_dim(:, :)

        integer, parameter :: missing_int_value = -99999
        real, parameter :: missing_real_value = -99999.0
        character (len = 19) :: loc_tstamp
        integer :: loc_date, loc_hour
        character (len = 2) :: loc_date_str, loc_hour_str

        integer :: i, j

        allocate (loc_dim(3, nspcs), stat=stat)

        col_size = ec - sc + 1
        row_size = er - sr + 1

! check col and row size against wrf domain
        do n = 1, size(dim_name)
           if (dim_name(n) .eq. 'south_north') then
              if (er .gt. dim_len(n)) then
                 print *, ' Error: template domain row is outside wrf domain'
                 stop
              end if
           else if (dim_name(n) .eq. 'west_east') then
              if (ec .gt. dim_len(n)) then
                 print *, ' Error: template domain column is outside wrf domain'
                 stop
              end if
           end if
        end do

        loc_dim = 1
        do s = 1, nspcs
           t = spclist(s)
           do n = 1, var_ndims(t)-1

              if (s .eq. 1) then
! this adjusts ioapi_3 file's TFLAG second dimension VAR to nspcs - 1
                 if (dim_name(var_dimids(n,t)) .eq. 'VAR') then
                    loc_dim(n,s) = nspcs - 1
! this is for conversion of wrf to ioapi_3
                 else if (dim_name(var_dimids(n,t)) .eq. 'DateStrLen') then
                    loc_dim(n,s) = 2
                    loc_dim(2,s) = nspcs - 1
                 else
                    loc_dim(n,s) = dim_len(var_dimids(n, t))
                 end if
              else
                 loc_dim(n,s) = dim_len(var_dimids(n, t))
              end if

           end do
        end do

        do s = 1, nspcs

           t = spclist(s)

           if (var_type(t) .eq. nf90_char) then

              allocate (data_int(loc_dim(1,s), loc_dim(2,s), 1),  stat=stat)
              call compute_date_time (tstamp, date, time)

              data_int(1,:,1) = date
              data_int(2,:,1) = time

              stat = nf90_put_var(outfileid, s, data_int,                    &
                                  start = (/ 1, 1, out_tstep /),             &
                                  count = (/ loc_dim(1,s), loc_dim(2,s), 1 /))

              deallocate (data_int)

           else if (var_type(t) .eq. nf90_float) then

              allocate (data_real(loc_dim(1,s), loc_dim(2,s), loc_dim(3,s)),        &
                        stat=stat)

              if (var_ndims(t) .eq. 1) then


                 stat = nf90_put_var(outfileid, s, data_real(:,:,:),          &
                                     start = (/ 1, 1, 1, out_tstep /),        &
                                     count = (/ 1, 1, 1 /))

              else if (var_ndims(t) .eq. 2) then


                 stat = nf90_put_var(outfileid, s, data_real(:,:,:),          &
                                     start = (/ 1, 1, 1, out_tstep /),        &
                                     count = (/ loc_dim(1,s) /))

              else if (var_ndims(t) .eq. 3) then

                 stat = nf90_get_var(infileid, t, data_real(:,:,:),              &
                                     start = (/ 1, 1, tstep /),                  &
                                     count = (/ loc_dim(1,s), loc_dim(2,s) /))

                 allocate (data_real_c(col_size, row_size, 1), stat=stat)
                 data_real_c(1:col_size, 1:row_size,:) = data_real(sc:ec, sr:er, :)
                 stat = nf90_put_var(outfileid, s, data_real_c,                  &
                                     start = (/ 1, 1, 1, out_tstep /),           &
                                     count = (/ col_size, row_size, 1 /))
                 deallocate (data_real_c)

              else if (var_ndims(t) .eq. 4) then

                 stat = nf90_get_var(infileid, t, data_real(:,:,:),              &
                                     start = (/ 1, 1, 1, tstep /),               &
                                     count = (/ loc_dim(1,s), loc_dim(2,s),      &
                                                loc_dim(3,s) /))

                 allocate (data_real_c(col_size, row_size, loc_dim(3,s)), stat=stat)

                 data_real_c = data_real(sc:ec, sr:er, :)
                 stat = nf90_put_var(outfileid, s, data_real_c(:,:,:),           &
                                     start = (/ 1, 1, 1, out_tstep /),           &
                                     count = (/ col_size, row_size,              &
                                                loc_dim(3,s) /))
                 deallocate (data_real_c)

              end if

              deallocate (data_real)

           else if (var_type(t) .eq. nf90_int) then

              allocate (data_int(loc_dim(1,s), loc_dim(2,s), loc_dim(3,s)),         &
                        stat=stat)

              if (var_ndims(t) .eq. 1) then

                 stat = nf90_get_var(infileid, t, data_int(:,:,:),               &
                                     start = (/ tstep /),                        &
                                     count = (/ 1 /))

                 stat = nf90_put_var(outfileid, s, data_int(:,:,:),              &
                                     start = (/ 1, 1, 1, out_tstep /),           &
                                     count = (/ 1, 1, 1 /))

              else if (var_ndims(t) .eq. 2) then

                 stat = nf90_get_var(infileid, t, data_int(:,:,:),               &
                                     start = (/ 1, tstep /),                     &
                                     count = (/ loc_dim(1,s) /))

                 stat = nf90_put_var(outfileid, s, data_int(:,:,:),              &
                                     start = (/ 1, 1, 1, out_tstep /),           &
                                     count = (/ loc_dim(1,s) /))

              else if (var_ndims(t) .eq. 3) then
                 stat = nf90_get_var(infileid, t, data_int(:,:,:),               &
                                     start = (/ 1, 1, tstep /),                  &
                                     count = (/ loc_dim(1,s), loc_dim(2,s) /))

                 allocate (data_int_c(col_size, row_size, 1), stat=stat)
                 data_int_c = data_int(sc:ec, sr:er, :)

                 stat = nf90_put_var(outfileid, s, data_int_c(:,:,:),            &
                                     start = (/ 1, 1, 1, out_tstep /),           &
                                     count = (/ col_size, row_size, 1 /))

                 deallocate (data_int_c)
              else if (var_ndims(t) .eq. 4) then
                 stat = nf90_get_var(infileid, t, data_int(:,:,:),               &
                                     start = (/ 1, 1, 1, tstep /),               &
                                     count = (/ loc_dim(1,s), loc_dim(2,s),      &
                                                   loc_dim(3,s) /))

                 allocate (data_int_c(col_size, row_size, loc_dim(3,s)), stat=stat)

                 data_int_c = data_int(sc:ec, sr:er, :)

                 stat = nf90_put_var(outfileid, s, data_int_c(:,:,:),            &
                                     start = (/ 1, 1, 1, out_tstep /),           &
                                     count = (/ col_size, row_size,              &
                                                loc_dim(3,s) /))
                 deallocate (data_real_c)
              end if

              deallocate (data_int)

           end if
        end do
        out_tstep = out_tstep + 1

        out_tstep = out_tstep - 1
        pre_tstep = tstep
        deallocate (loc_dim)

        end subroutine process_data

        end module process_util_module
