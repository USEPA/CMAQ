! Purpose: A collection of utility functions tailored for MIO

      module mio_time_util_func_module

        use mio_global_data_module

        implicit none

        ! to compute the time interval
        interface mio_time_diff
          module procedure  mio_time_diff_time_str,    &
                            mio_time_diff_jdate_time,  &
                            mio_time_diff_time
        end interface

        ! to convert time between time statmp and julian format
        interface mio_time_format_conversion
          module procedure mio_timestamp_to_julian,   &
                           mio_julian_to_timestamp
        end interface

        ! to alter time w.r.t. a given adjustment
        interface mio_date_time_adjustment
          module procedure mio_date_time_adjustment_julian_sec,       &
                           mio_date_time_adjustment_non_julian_sec,   &
                           mio_date_time_adjustment_julian_time,      &
                           mio_date_time_adjustment_non_julian_time
        end interface

        interface mio_locate_time
          module procedure mio_locate_jdate_time,   &
                           mio_locate_time_str
        end interface

        private :: mio_time2sec,                           &
                   mio_leap_year,                          &
                   mio_date_time_backward_adjustment_sec,  &
                   mio_date_time_forward_adjustment_sec,   &
                   mio_date_time_backward_adjustment_time, &
                   mio_date_time_forward_adjustment_time

        contains

! ------------------------------------------------------------------------------
! Purpose: To convert time in hhmmss format into seconds

        integer function mio_time2sec (time)

          integer, intent(in) :: time    ! in hhmmss format
          integer :: neg_time
          integer :: hr, min, sec

          if (time .gt. 0) then
             hr = time / 10000
             min = mod(time/100, 100)
             sec = mod(time, 100)
             mio_time2sec = hr * 3600 + min * 60 + sec
          else
             neg_time = abs(time)
             hr = neg_time / 10000
             min = mod(neg_time/100, 100)
             sec = mod(neg_time, 100)
             mio_time2sec = -1*(hr * 3600 + min * 60 + sec)
          end if

        end function mio_time2sec

! ------------------------------------------------------------------------------
! Purpose: To determine a year is leaped year or not
        logical function mio_leap_year (year)

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

          mio_leap_year = temp_leap_year

        end function mio_leap_year

! ------------------------------------------------------------------------------
! purpose: To convert timestamp in YYYY:MM:DD_HH:MM:SS format to Julian date and time format

        subroutine mio_timestamp_to_julian (timestamp, date, time)

          character (*), intent(in) :: timestamp
          integer, intent(out)      :: date, time

          integer :: year, month, day, numday(12), loc_date,   &
                     i, hour, minute, second
          integer, parameter :: numday_leap_year(12) =         &
                                (/ 31, 29, 31, 30, 31, 30,     &
                                   31, 31, 30, 31, 30, 31 /)
          integer, parameter :: numday_non_leap_year(12) =     &
                                (/ 31, 28, 31, 30, 31, 30,     &
                                   31, 31, 30, 31, 30, 31 /)

          read (timestamp(1:10), '(i4, 2(1x, i2))') year, month, day
          read (timestamp(12:19), '(i2, 1x, i2, 1x, i2)') hour, minute, second

          time = hour * 10000 + minute * 100 + second

          if (mio_leap_year(year)) then
             numday = numday_leap_year
          else
             numday = numday_non_leap_year
          endif

          loc_date = 0
          do i = 1, month-1
             loc_date = loc_date + numday(i)
          end do
          loc_date = loc_date + day

          date = year * 1000 + loc_date

        end subroutine mio_timestamp_to_julian

! ------------------------------------------------------------------------------
! purpose: To convert time in Julian date and time format to timestamp YYYY:MM:DD_HH:MM:SS format

        subroutine mio_julian_to_timestamp (in_date, in_time, timestamp, flag)

          integer, intent(in)           :: in_date, in_time
          character (*), intent(out)    :: timestamp
          integer, intent(in), optional :: flag        ! a dummy argument to fool compiler for not 
                                                       ! able distinguishing routines in the
                                                       ! mio_time_format_conversion interface block

          integer :: year, month, day, hh, mm, ss

          call mio_julian_to_calendar (in_date, year, month, day)

          mm = in_time / 100
          hh = mm / 100
          mm = mod(mm,100)
          ss = mod(in_time, 100)

          write (timestamp, '(i4, a1, i2.2, a1, i2.2, a1, i2.2, 2(a1, i2.2))')  &
                 year, '-', month, '-', day, '_', hh, ':', mm, ':', ss

        end subroutine mio_julian_to_timestamp

! ------------------------------------------------------------------------------
! purpose: To convert Julian date to calendar date

        subroutine mio_julian_to_calendar (julian_date, year, month, day)

          integer, intent(in)  :: julian_date
          integer, intent(out) :: year, month, day

          integer, parameter :: num_days (12, 2) = &
              reshape ((/ 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365,                &
                          31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 /), (/12, 2/))

          integer :: ind, t_date, i
          logical :: found

          year = julian_date / 1000
          t_date = mod(julian_date, 1000)

          if (mio_leap_year(year)) then
             ind = 2
          else
             ind = 1
          end if

          found = .false.
          i = 12
          do while ((.not. found) .and. (i > 0))
             if (t_date > num_days(i, ind)) then
                found = .true.
             else
                i = i - 1
             end if
          end do

          month = i + 1
          if (i == 0) then
             day = t_date
          else
             day = t_date - num_days(i, ind)
          end if

        end subroutine mio_julian_to_calendar

! ------------------------------------------------------------------------------
! Purpose: Base on the given timestamp argument, advance to the next immeidate 
!          or nsteps step when this optional argument is provided, in the file.
!          If nsteps is a negative number, it moves backward.

        subroutine mio_nextime (fname, timestamp, nsteps)

          use mio_search_module
          use mio_global_data_module

          character (*), intent(in)    :: fname
          character (*), intent(inout) :: timestamp
          integer, optional, intent(in) :: nsteps

          integer :: floc, loc_nsteps, cur_step, next_step

          floc = mio_search (fname)

          if (present(nsteps)) then
             loc_nsteps = nsteps
          else
             loc_nsteps = 1
          end if

          cur_step = mio_search (timestamp, mio_file_data(floc)%timestamp, mio_file_data(floc)%nsteps)
          next_step = cur_step + loc_nsteps

          if ((next_step .lt. 1) .or. (next_step .gt. mio_file_data(floc)%nsteps)) then
             write (mio_logdev, *) ' Abort: in mio_nextime routine due to resulting timestamp is outside '
             write (mio_logdev, *) '        the range in the file ', trim(fname)
             stop
          else
             timestamp = mio_file_data(floc)%timestamp(next_step)
          end if

        end subroutine mio_nextime

! ------------------------------------------------------------------------------
! Purpose: Convert date in year, month and day form to Julian date

        integer function mio_julian_day (year, month, day)

          integer, intent(in) :: year, month, day

          integer, parameter :: numday(12) =                                       &
                                (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

          integer :: i, temp_julian

          i = 1
          temp_julian= 0
          do while (i < month)
             temp_julian= temp_julian+ numday(i)
             i = i + 1
          end do
          temp_julian= temp_julian + day

          if (mio_leap_year(year) .and. (month > 2)) then
             temp_julian= temp_julian + 1
          end if

          mio_julian_day = temp_julian

        end function mio_julian_day

! ------------------------------------------------------------------------------
! Purpose: Computer the time interval between date1 and date2, both are in
!          YYYY-MM-DD_HH:MM:SS format, and return in second or hhmmss
!          format when argument flag is provided.

        integer recursive function mio_time_diff_time_str (date1, date2, flag) result (time)

          character (*), intent(in)     :: date1, date2   ! YYYY-MM-DD_HH:MM:SS format
          integer, intent(in), optional :: flag           ! output in hhmmss format

          integer :: year1, month1, day1, hour1, minute1, sec1, jdate1
          integer :: year2, month2, day2, hour2, minute2, sec2, jdate2
          integer :: yr, loc_result
          character (20) :: loc_date

          if (date2 .gt. date1) then
             loc_result = mio_time_diff_time_str (date2, date1)
          else
             read (date1, '(i4, 5(1x, i2))') year1, month1, day1, hour1, minute1, sec1
             read (date2, '(i4, 5(1x, i2))') year2, month2, day2, hour2, minute2, sec2

             if (year1 .eq. year2) then
                jdate1 = mio_julian_day (year1, month1, day1)
                jdate2 = mio_julian_day (year2, month2, day2)

                loc_result = (jdate2 - jdate1) * 86400 -    &
                                 ((hour1 - hour2) * 3600 +    &
                                  (minute1 - minute2) * 60 +  &
                                  sec1 - sec2)
             else
                loc_result = 0
                do yr = year1, year2
                   if (yr .eq. year1) then
                      write (loc_date, '(i4, a15)') year1, "-12-31:23:59:59"
! + 1 adjustment is for the difference between 12-31:23:59:59 and 01-01:00:00:00
                      loc_result = loc_result + mio_time_diff_time_str (date1, loc_date) + 1
                   else if (yr .eq. year2) then
                      write (loc_date, '(i4, a15)') year2, "-01-01:00:00:00"
                      loc_result = loc_result + mio_time_diff_time_str (loc_date, date2)
                   else
                      if (mio_leap_year (yr)) then
                         loc_result = loc_result + 31622400
                      else
                         loc_result = loc_result + 31536000
                      end if
                   end if
                end do
             end if
          end if

          if (present(flag)) then
             hour1      = loc_result / 3600
             minute1    = (loc_result - hour1 * 3600) / 60
             sec1       = loc_result - hour1 * 3600 - minute1 * 60
             loc_result = hour1 * 10000 + minute1 * 100 + sec1
          end if

          if (date1 .gt. date2) then
             time = loc_result * (-1)
          else
             time = loc_result
          end if

        end function mio_time_diff_time_str

! ------------------------------------------------------------------------------
! Purpose: Computer the time interval between time1 and time2, both are in
!          hhmmss format, and return in second or in hhmmss format when argument 
!          flag is provided.

        integer function mio_time_diff_time (time1, time2, flag) result (time)

          integer, intent(in) :: time1, time2    ! in hhmmss format
          integer, intent(in), optional :: flag

          integer :: hh, mm, ss, result

          result = mio_time2sec(time1) - mio_time2sec(time2)

          if (present(flag)) then
             ss = mod(result, 60)
             mm = mod(result/60, 60)
             hh = result / 3600
             time = hh * 10000 + mm * 100 + ss
          else
             time = result
          end if

        end function mio_time_diff_time

! ------------------------------------------------------------------------------
! Purpose: Computer the time interval between date1 time1 and date2 time2, each
!          date is in Julian format and each time is in hhmmss format, and
!          return in second or in hhmmss format when argument flag is provided.

        integer function mio_time_diff_jdate_time (date1, time1, date2, time2, flag) result (time)

          integer, intent(in) :: date1, date2    ! in Julian format
          integer, intent(in) :: time1, time2    ! in hhmmss format
          integer, intent(in), optional :: flag

          character (20) :: loc_date_time1, loc_date_time2

          call mio_time_format_conversion (date1, time1, loc_date_time1)
          call mio_time_format_conversion (date2, time2, loc_date_time2)

          if (present(flag)) then
             time = mio_time_diff_time_str (loc_date_time1, loc_date_time2, flag)
          else
             time = mio_time_diff_time_str (loc_date_time1, loc_date_time2)
          end if

        end function mio_time_diff_jdate_time

! ------------------------------------------------------------------------------
! Purpose: Adjust date and time w.r.t. given adjustment and return the
!          result in the form of hhmmss

        subroutine mio_date_time_adjustment_julian_sec (date, time, adjustment, flag)

          integer, intent(inout) :: date, time
          integer, intent(in)    :: adjustment, flag

          if (adjustment >= 0) then
             call mio_date_time_forward_adjustment_sec (date, time, adjustment)
          else
             call mio_date_time_backward_adjustment_sec (date, time, -adjustment)
          end if

        end subroutine mio_date_time_adjustment_julian_sec

! ------------------------------------------------------------------------------
! Purpose: Adjust time_stamp w.r.t. given adjustment and return the
!          result in the form of hhmmss.

        subroutine mio_date_time_adjustment_non_julian_sec (time_stamp, adjustment, flag)

          character (*), intent(inout) :: time_stamp
          integer, intent(in)          :: adjustment, flag

          integer :: tflag(2)

          call mio_time_format_conversion (time_stamp, tflag(1), tflag(2))

          if (adjustment >= 0) then
             call mio_date_time_forward_adjustment_sec (tflag(1), tflag(2), adjustment)
          else
             call mio_date_time_backward_adjustment_sec (tflag(1), tflag(2), -adjustment)
          end if

          call mio_time_format_conversion (tflag(1), tflag(2), time_stamp)

        end subroutine mio_date_time_adjustment_non_julian_sec

! ------------------------------------------------------------------------------
! Purpose: Adjust date and time w.r.t. given adjustment and return the
!          result in the form of seconds.

        subroutine mio_date_time_adjustment_julian_time (date, time, adjustment)

          integer, intent(inout) :: date, time
          integer, intent(in)    :: adjustment

          if (adjustment >= 0) then
             call mio_date_time_forward_adjustment_time (date, time, adjustment)
          else
             call mio_date_time_backward_adjustment_time (date, time, -adjustment)
          end if

        end subroutine mio_date_time_adjustment_julian_time

! ------------------------------------------------------------------------------
! Purpose: Adjust time_stamp w.r.t. given adjustment and return the
!          result in the form of seconds.

        subroutine mio_date_time_adjustment_non_julian_time (time_stamp, adjustment)

          character (*), intent(inout) :: time_stamp
          integer, intent(in)          :: adjustment

          integer :: tflag(2)

          call mio_time_format_conversion (time_stamp, tflag(1), tflag(2))

          if (adjustment >= 0) then
             call mio_date_time_forward_adjustment_time (tflag(1), tflag(2), adjustment)
          else
             call mio_date_time_backward_adjustment_time (tflag(1), tflag(2), -adjustment)
          end if

          call mio_time_format_conversion (tflag(1), tflag(2), time_stamp)

        end subroutine mio_date_time_adjustment_non_julian_time

! ------------------------------------------------------------------------------
! Purpose: Backwardly adjust date and time with adjustment in seconds

        subroutine mio_date_time_backward_adjustment_sec (date, time, adjustment)

          integer, intent(inout) :: date        ! date in julian date format
          integer, intent(inout) :: time        ! time in hhmmss format
          integer, intent(in   ) :: adjustment  ! amount of adjustment in second

          integer :: loc_year, loc_date, loc_sec, subtract_hr, subtract_min,  &
                     subtract_sec, temp, hr, min, sec, num_days

          loc_year = date / 1000
          loc_date = mod(date, 1000)

          num_days = adjustment / 86400
          loc_sec = mod(adjustment, 86400)
          loc_date = loc_date - num_days

          do while (loc_date <= 0)
             loc_year = loc_year - 1
             if (mio_leap_year(loc_year)) then
                loc_date = loc_date + 366
             else
                loc_date = loc_date + 365
             end if
          end do

          subtract_hr  = loc_sec / 3600
          temp         = mod(loc_sec, 3600)
          subtract_sec = mod(temp, 60)
          subtract_min = temp / 60

          hr = time / 10000
          sec = mod(time, 100)
          min = mod(time/100, 100)

          sec = sec - subtract_sec

          if (sec .lt. 0) then
             min = min - 1 - subtract_min
             sec = sec + 60
          else
             min = min - subtract_min
          end if

          if (min .lt. 0) then
             hr = hr - 1 - subtract_hr
             min = min + 60
          else
             hr = hr - subtract_hr
          end if

          if (hr .lt. 0) then
             loc_date = loc_date - 1
             hr = hr + 24

             if (loc_date == 0) then
                loc_year = loc_year - 1
                if (mio_leap_year(loc_year)) then
                   loc_date = 366
                else
                   loc_date = 365
                end if
             end if
          end if

          date = loc_year * 1000 + loc_date
          time = hr * 10000 + min * 100 + sec

        end subroutine mio_date_time_backward_adjustment_sec

! ------------------------------------------------------------------------------
! Purpose: Forwardly adjust date and time with adjustment in seconds

        subroutine mio_date_time_forward_adjustment_sec (date, time, adjustment)

          integer, intent(inout) :: date        ! date in julian date format
          integer, intent(inout) :: time        ! time in hhmmss format
          integer, intent(in   ) :: adjustment  ! amount of adjustment in second

          integer :: loc_year, loc_date, loc_sec, add_hr, add_min,  &
                     add_sec, temp, hr, min, sec, num_days, yr_ndays
          logical :: done

          loc_year = date / 1000
          loc_date = mod(date, 1000)

          num_days = adjustment / 86400
          loc_sec = mod(adjustment, 86400)
          loc_date = loc_date + num_days

          done = .false.
          do while (.not. done)
             if (mio_leap_year(loc_year)) then
                yr_ndays = 366
             else
                yr_ndays = 365
             end if
             if (loc_date > yr_ndays) then
                loc_date = loc_date - yr_ndays
                loc_year = loc_year + 1
             else
                done = .true.
             end if
          end do

          add_hr  = loc_sec / 3600
          temp    = mod(loc_sec, 3600)
          add_sec = mod(temp, 60)
          add_min = temp / 60

          hr = time / 10000
          sec = mod(time, 100)
          min = mod(time/100, 100)

          sec = sec + add_sec

          if (sec >= 60) then
             min = min + 1 + add_min
             sec = sec - 60
          else
             min = min + add_min
          end if

          if (min >= 60) then
             hr = hr + 1 + add_hr
             min = min - 60
          else
             hr = hr + add_hr
          end if

          if (hr >= 24) then
             loc_date = loc_date + 1
             hr = hr - 24

             if (mio_leap_year(loc_year)) then
                yr_ndays = 366
             else
                yr_ndays = 365
             end if
             if (loc_date > yr_ndays) then
                loc_year = loc_year + 1
                loc_date = 1
             end if
          end if

          date = loc_year * 1000 + loc_date
          time = hr * 10000 + min * 100 + sec

        end subroutine mio_date_time_forward_adjustment_sec

! ------------------------------------------------------------------------------
! Purpose: Backwardly adjust date and time with adjustment in hhmmss format

        subroutine mio_date_time_backward_adjustment_time (date, time, adjustment)

          integer, intent(inout) :: date        ! date in julian date format
          integer, intent(inout) :: time        ! time in hhmmss format
          integer, intent(in   ) :: adjustment  ! amount of adjustment in hhmmss format

          integer :: loc_year, loc_date,                    &
                     hh, mm, ss, hour, min, sec, num_days

          loc_year = date / 1000
          loc_date = mod(date, 1000)

          ss = mod(adjustment, 100)
          mm = mod(adjustment/100, 100)
          hh = adjustment / 10000

          num_days = hh / 24
          hh = hh - num_days * 24
          loc_date = loc_date - num_days

          do while (loc_date <= 0)
             loc_year = loc_year - 1
             if (mio_leap_year(loc_year)) then
                loc_date = loc_date + 366
             else
                loc_date = loc_date + 365
             end if
          end do

          hour = time / 10000
          sec  = mod(time, 100)
          min  = mod(time/100, 100)

          sec = sec - ss

          if (sec .lt. 0) then
             min = min - 1 - mm
             sec = sec + 60
          else
             min = min - mm
          end if

          if (min .lt. 0) then
             hour = hour - 1 - hh
             min = min + 60
          else
             hour = hour - hh
          end if

          if (hour .lt. 0) then
             loc_date = loc_date - 1
             hour = hour + 24

             if (loc_date == 0) then
                loc_year = loc_year - 1
                if (mio_leap_year(loc_year)) then
                   loc_date = 366
                else
                   loc_date = 365
                end if
             end if
          end if

          date = loc_year * 1000 + loc_date
          time = hour * 10000 + min * 100 + sec

        end subroutine mio_date_time_backward_adjustment_time

! ------------------------------------------------------------------------------
! Purpose: Forwardly adjust date and time with adjustment in hhmmss format

        subroutine mio_date_time_forward_adjustment_time (date, time, adjustment)

          integer, intent(inout) :: date        ! date in julian date format
          integer, intent(inout) :: time        ! time in hhmmss format
          integer, intent(in   ) :: adjustment  ! amount of adjustment in hhmmss format

          integer :: loc_year, loc_date, hh, mm, ss,      &
                     hour, min, sec, num_days, yr_ndays
          logical :: done

          loc_year = date / 1000
          loc_date = mod(date, 1000)

          ss = mod(adjustment, 100)
          mm = mod(adjustment/100, 100)
          hh = adjustment / 10000

          num_days = hh / 24
          hh = hh - num_days * 24
          loc_date = loc_date + num_days

          done = .false.
          do while (.not. done)
             if (mio_leap_year(loc_year)) then
                yr_ndays = 366
             else
                yr_ndays = 365
             end if
             if (loc_date > yr_ndays) then
                loc_date = loc_date - yr_ndays
                loc_year = loc_year + 1
             else
                done = .true.
             end if
          end do

          hour = time / 10000
          sec  = mod(time, 100)
          min  = mod(time/100, 100)

          sec = sec + ss

          if (sec >= 60) then
             min = min + 1 + mm
             sec = sec - 60
          else
             min = min + mm
          end if

          if (min >= 60) then
             hour = hour + 1 + hh
             min = min - 60
          else
             hour = hour + hh
          end if

          if (hour >= 24) then
             loc_date = loc_date + 1
             hour = hour - 24

             if (mio_leap_year(loc_year)) then
                yr_ndays = 366
             else
                yr_ndays = 365
             end if
             if (loc_date > yr_ndays) then
                loc_year = loc_year + 1
                loc_date = 1
             end if
          end if

          date = loc_year * 1000 + loc_date
          time = hour * 10000 + min * 100 + sec

        end subroutine mio_date_time_forward_adjustment_time

! ------------------------------------------------------------------------------
        subroutine mio_locate_jdate_time (fnum, date, time, beg_loc, end_loc)

          integer, intent (in)  :: fnum, date, time
          integer, intent (out) :: beg_loc, end_loc

          logical :: found = .false.
          integer :: loc
          character (mio_max_time_length) :: ttime_stamp

          call mio_time_format_conversion (date, time, ttime_stamp)

          beg_loc = -1
          end_loc = -1
          loc = 0
          do while ((.not. found) .and. (loc < mio_file_data(fnum)%nsteps))
             loc = loc + 1
             if (ttime_stamp == mio_file_data(fnum)%timestamp(loc)) then
                found = .true.
                beg_loc = loc
                end_loc = loc + 1
                if (end_loc > mio_file_data(fnum)%nsteps) then
                   end_loc = -1
                end if
             else if (ttime_stamp < mio_file_data(fnum)%timestamp(loc)) then
                found = .true.
                beg_loc = loc - 1
                end_loc = loc
             end if
          end do

        end subroutine mio_locate_jdate_time

! ------------------------------------------------------------------------------
        subroutine mio_locate_time_str (fnum, timestamp, beg_loc, end_loc)

          integer, intent (in)     :: fnum
          character(*), intent(in) :: timestamp
          integer, intent (out)    :: beg_loc, end_loc

          logical :: found
          integer :: loc

          beg_loc = -1
          end_loc = -1
          found = .false.
          loc = 0
          do while ((.not. found) .and. (loc < mio_file_data(fnum)%nsteps))
             loc = loc + 1
             if (timestamp == mio_file_data(fnum)%timestamp(loc)) then
                found = .true.
                beg_loc = loc
                end_loc = loc + 1
                if (end_loc > mio_file_data(fnum)%nsteps) then
                   end_loc = -1
                end if
             else if (timestamp < mio_file_data(fnum)%timestamp(loc)) then
                found = .true.
                beg_loc = loc - 1
                end_loc = loc
             end if
          end do

        end subroutine mio_locate_time_str

      end module mio_time_util_func_module
