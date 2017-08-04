        module misc_util_module

        use get_env_module

        implicit none

        contains

! ----------------------------------------------------------------------------
        subroutine compute_date_time (tstamp, date, time)

        character (len = 19), intent(in) :: tstamp
        integer, intent(out)             :: date, time

        integer :: year, month, day, hour, min, sec

        if (len(trim(tstamp)) .eq. 19) then

           read (tstamp, '(i4, 1x, i2, 1x, i2, 3(1x, i2))') &
                 year, month, day, hour, min, sec

           date = julian_date(year, month, day) + year * 1000

           time = hour * 10000 + min * 100 + sec
        else   ! ioapi date time format
           read (tstamp, '(i7, 1x, i6)') date, time
        end if

        end subroutine compute_date_time

! ----------------------------------------------------------------------------
        subroutine compute_year_diff (syear, eyear, factor, diff)

        integer, intent(in)    :: syear, eyear, factor
        integer, intent(inout) :: diff

        integer :: loc_syear, loc_diff

        loc_syear = syear
        loc_diff = 0
        do while (loc_syear .lt. eyear)

           if (mod(loc_syear, 4) .ne. 0) then
              loc_diff = loc_diff + 31536000
           else if (mod(loc_syear, 400) .eq. 0) then
              loc_diff = loc_diff + 31622400
           else if (mod(loc_syear, 100) .eq. 0) then
              loc_diff = loc_diff + 31536000
           else
              loc_diff = loc_diff + 31622400
           endif

           loc_syear = loc_syear + 1
        end do

        diff = diff + factor * loc_diff

        end subroutine compute_year_diff

! ----------------------------------------------------------------------------
        logical function leap_year (year)

        integer, intent(in) :: year

        logical :: temp_leap_year

        if (mod(year, 4) .ne. 0) then
           temp_leap_year = .false.
        else if (mod(year, 400) .eq. 0) then
           temp_leap_year = .true.
        else if (mod(year, 100) .eq. 0) then
           temp_leap_year = .false.
        else
           temp_leap_year = .true.
        endif

        leap_year = temp_leap_year

        end function leap_year

! ----------------------------------------------------------------------------
        integer function julian_date (year, month, day)

        integer, intent(in) :: year, month, day

        integer, parameter :: num_day(12, 2) = reshape                                &
                              ( (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,    &
                                   31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /), &
                                (/ 12, 2 /) )

        integer :: loc_julian_date, pos, i

        if (leap_year(year)) then
           pos = 2
        else
           pos = 1
        end if

        loc_julian_date = day
        do i = 1, month - 1
           loc_julian_date = loc_julian_date + num_day(i, pos)
        end do

        julian_date = loc_julian_date

        end function julian_date

! ----------------------------------------------------------------------------
        integer function date_time_diff (sdate, stime, edate, etime)

        integer, intent(in) :: sdate, stime, edate, etime

        integer :: syear, eyear, day_diff, hour_diff, min_diff, sec_diff, diff

        day_diff  = mod(edate, 1000) - mod(sdate, 1000)
        hour_diff = etime / 10000 - stime / 10000
        min_diff  = mod(etime/100, 100) - mod(stime/100, 100)
        sec_diff  = mod(etime, 100) - mod(stime, 100)

        diff = 60 * ( 60 * ( 24 * day_diff + hour_diff ) + min_diff ) + sec_diff

        syear = sdate / 1000
        eyear = edate / 1000

        if (syear .lt. eyear) then
           call compute_year_diff (syear, eyear, 1, diff)
        else
           call compute_year_diff (eyear, syear, -1, diff)
        end if

        date_time_diff = diff

        end function date_time_diff

! ----------------------------------------------------------------------------
        subroutine determine_time_step (sstep, estep, n, max_step, ss, es)

        integer, intent(in)  :: sstep(4), estep(4), n, max_step
        integer, intent(out) :: ss, es

        if (sstep(2) .eq. estep(2)) then
           ss = sstep(1)
           es = estep(1)
        else
           if (n .eq. sstep(2)) then
              ss = sstep(1)
              es = max_step
           else if (n .eq. estep(2)) then
              ss = 1
              es = estep(1)
           else
              ss = 1
              es = max_step
           end if
        end if

        end subroutine determine_time_step

! ----------------------------------------------------------------------------
        subroutine retrieve_dimension_information (fileid, dim_name, dim_len, ndim)

        use netcdf

        integer, intent(in) :: fileid, ndim
        character (len = *), intent(out) :: dim_name(:)
        integer, intent(out) :: dim_len(:)

        integer :: n, stat

        do n = 1, ndim
           stat = nf90_inquire_dimension (fileid, n, dim_name(n), dim_len(n))
        end do

        end subroutine retrieve_dimension_information

! ----------------------------------------------------------------------------
        subroutine retrieve_variable_information (fileid, var_name, var_type, var_ndims, &
                                                  var_dimids, var_att, nvar)

        use netcdf
        use type_def_module

        integer, intent(in) :: fileid, nvar
        character (len = *), intent(out) :: var_name(:)
        integer, intent(out) :: var_type(:), var_ndims(:), var_dimids(:,:)
        type(var_att_record), intent(out) :: var_att(:)

        integer :: i, n, stat, var_id, t

        do n = 1, nvar
           stat = nf90_inquire_variable (fileid, n, var_name(n), var_type(n), &
                                         var_ndims(n), var_dimids(:,n), var_att(n)%num_var_att)

           if (var_att(n)%num_var_att .gt. 0) then

              do t = 1, var_att(n)%num_var_att
                 stat = nf90_inq_varid (fileid, var_name(n), var_id)

                 stat = nf90_inq_attname (fileid, var_id, t, &
                                          var_att(n)%var_att_record_array(t)%att_name)

                 stat = nf90_inquire_attribute (fileid, var_id, &
                         var_att(n)%var_att_record_array(t)%att_name,        &
                         var_att(n)%var_att_record_array(t)%att_type,        &
                         var_att(n)%var_att_record_array(t)%att_len)

                 if (var_att(n)%var_att_record_array(t)%att_type .eq. nf90_char) then
                    stat = nf90_get_att (fileid, var_id, var_att(n)%var_att_record_array(t)%att_name, &
                                         var_att(n)%var_att_record_array(t)%att_value_char)

! nedcdf puts null character instead of blank character, this makes ioapi_3 not compatible
                    do i = 1, len(trim(var_att(n)%var_att_record_array(t)%att_value_char))
                       if (iachar(var_att(n)%var_att_record_array(t)%att_value_char(i:i)) .eq. 0) then
                          var_att(n)%var_att_record_array(t)%att_value_char(i:i) = ' '
                       end if
                    end do
                 else if (var_att(n)%var_att_record_array(t)%att_type .eq. nf90_int) then
                    stat = nf90_get_att (fileid, var_id, var_att(n)%var_att_record_array(t)%att_name, &
                                         var_att(n)%var_att_record_array(t)%att_value_int)
                 else if (var_att(n)%var_att_record_array(t)%att_type .eq. nf90_float) then
                    stat = nf90_get_att (fileid, var_id, var_att(n)%var_att_record_array(t)%att_name, &
                                         var_att(n)%var_att_record_array(t)%att_value_float)
                 end if
              end do

           end if
        end do

        end subroutine retrieve_variable_information

! ----------------------------------------------------------------------------
        subroutine retrieve_attribute_information (fileid,                   &
                                                   global_att, n_global_att, &
                                                   supplement_data_int,      &
                                                   supplement_data_real)

        use netcdf
        use type_def_module

        integer, intent(in) :: fileid, n_global_att
        type(att_value_record), intent(out) :: global_att(:)
        integer, intent(out) :: supplement_data_int (:, :)
        real, intent(out) :: supplement_data_real (:, :)

        integer :: n, stat, att_type, att_len
        integer :: num_supplement_data_int = 0
        integer :: num_supplement_data_real = 0

           do n = 1, n_global_att
              stat = nf90_inq_attname (fileid, nf90_global, n, global_att(n)%att_name)

              stat = nf90_inquire_attribute (fileid, nf90_global, &
                                             global_att(n)%att_name, att_type, att_len)
              global_att(n)%att_type = att_type
              global_att(n)%att_len = att_len
              if (att_type .eq. nf90_char) then
                 stat = nf90_get_att (fileid, nf90_global, global_att(n)%att_name, &
                                      global_att(n)%att_value_char)
              else if (att_type .eq. nf90_int) then
                 if (att_len .gt. 1) then
                    num_supplement_data_int = num_supplement_data_int + 1
                    stat = nf90_get_att (fileid, nf90_global, global_att(n)%att_name, &
                                         supplement_data_int(:,num_supplement_data_int))
                    global_att(n)%att_value_int_link = num_supplement_data_int
                 else
                    stat = nf90_get_att (fileid, nf90_global, global_att(n)%att_name, &
                                         global_att(n)%att_value_int)
                 end if
              else if (att_type .eq. nf90_float) then

                 if (att_len .gt. 1) then
                    num_supplement_data_real = num_supplement_data_real + 1
                    stat = nf90_get_att (fileid, nf90_global, global_att(n)%att_name, &
                                         supplement_data_real(:,num_supplement_data_real))
                    global_att(n)%att_value_float_link = num_supplement_data_real
                 else
                    stat = nf90_get_att (fileid, nf90_global, global_att(n)%att_name, &
                                         global_att(n)%att_value_float)
                 end if
              else if (att_type .eq. nf90_double) then
                 stat = nf90_get_att (fileid, nf90_global, global_att(n)%att_name, &
                                      global_att(n)%att_value_double)
              end if
           end do

        end subroutine retrieve_attribute_information

        end module misc_util_module
