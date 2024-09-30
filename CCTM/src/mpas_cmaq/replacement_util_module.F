        module util_module

          implicit none

          private :: quicksort

          interface secsdiff
            module procedure secsdiff_jdate_jtime,
     &                       secsdiff_time_str,
     &                       secsdiff_jtime
          end interface

          interface index1
            module procedure index1_char,
     &                       index1_int
          end interface

          contains

! -----------------------------------------------------------
          logical function leap_year (year)

            integer :: year

            if (mod(year, 4) .ne. 0) then
               leap_year = .false.
            else if (mod(year, 400) .eq. 0) then
               leap_year = .true.
            else if (mod(year, 100) .eq. 0) then
               leap_year = .false.
            else
               leap_year = .true.
            endif
 
          end function leap_year

! -----------------------------------------------------------
          subroutine daymon (jdate, mon, day)

            implicit none

            integer, intent(in) :: jdate
            integer, intent(out) :: mon, day

            integer, parameter :: nonleapyear (12) = (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
            integer, parameter :: leapyear (12)    = (/ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

            integer :: lyear, lmon, lday, ndays(12)

            lyear = jdate / 1000
            lday = mod(jdate, 1000)

            if (leap_year(lyear)) then
               ndays = leapyear
            else
               ndays = nonleapyear
            end if

            lmon = 1
            do while (lday > ndays(lmon))
               lday = lday - ndays(lmon)
               lmon = lmon + 1
            end do

            mon = lmon
            day = lday

          end subroutine daymon

! ----------------------------------------------------------
          integer function julian (year, month, day)

            implicit none

            integer, intent(in) :: year, month, day

            integer, parameter :: numday(12) =
     &                            (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

            integer :: i, temp_julian

            i = 1
            temp_julian= 0
            do while (i < month)
              temp_julian= temp_julian+ numday(i)
              i = i + 1
            end do
            temp_julian= temp_julian+ day

            if (leap_year(year) .and. (month > 2)) then
               temp_julian= temp_julian+ 1
            end if

            julian = temp_julian

          end function julian

! ----------------------------------------------------------
          integer function num_julian_day (year, month, day)

            implicit none

            integer, intent(in) :: year, month, day

            integer, parameter :: numday(12) =
     &                            (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

            integer :: i, temp_julian

            i = 1
            temp_julian= 0
            do while (i < month)
              temp_julian= temp_julian+ numday(i)
              i = i + 1
            end do
            temp_julian= temp_julian + day

            if (leap_year(year) .and. (month > 2)) then
               temp_julian= temp_julian+ 1
            end if

            num_julian_day = temp_julian

          end function num_julian_day

! -----------------------------------------------------------
          integer function wkday ( jdate )

! based on Zeller's Rule, Monday = 1, Tuseday = 2, ... Sunday = 7

            integer, intent(in) :: jdate

            integer :: f, c, d, year, day

            year = jdate / 1000
            day  = mod(jdate, 1000)

! compute with respect to the first day of a year
            d = mod(year-1, 100)
            c = (year-1) / 100

! add day - 1 to calculate w.r.t. the actual day
!           f = 1 + ((13 * 11 - 1) / 5) + d + d / 4 + c / 4 - 2 * c + day - 1
            f = 28 + d + d / 4 + c / 4 - 2 * c + day

! to adjust the situation that Sunday = 7
            f = mod(f, 7)
            if (f .eq. 0) then
               f = 7
            end if

! to adjust the situation when f is negative
            if (f .lt. 0) then
               f = 1 + mod(f+6, 7)
            end if

            wkday = f

          end function wkday

! ------------------------------------------------------
          logical function isdstime(jdate)

! <  2007: first Sunday in April and the last Sunday in October
! >= 2007: secnd Sunday in March and the first Sunday in November

            integer, intent(in) :: jdate

            integer, parameter :: cut_off_year = 2006

            integer :: year, day, check_date, adj, check_date_wkday
            logical :: lower, loc_isdstime

            year = jdate / 1000
            day  = mod(jdate, 1000)

            if (day .le. 122) then
               if (year .le. cut_off_year) then
                  check_date = 91         ! 4/1
               else
                  check_date = 60         ! 3/1
               end if
               lower = .true.
            else
               if (year .le. cut_off_year) then
                  check_date = 304        ! 10/31
               else
                  check_date = 305        ! 11/1
               end if
               lower = .false.
            end if

            if (leap_year(year)) then
               check_date = check_date + 1
            end if

            check_date = check_date + year * 1000

            check_date_wkday = wkday(check_date)

            if (lower) then
               if (year .le. cut_off_year) then               ! first Sunday in April
                  if (check_date_wkday .eq. 7) then
                     adj = 0
                  else
                     adj = 7 - check_date_wkday
                  end if
               else                                           ! second Sunday in March
                  if (check_date_wkday .eq. 7) then
                     adj = 7
                  else
                     adj = 14 - check_date_wkday
                  end if
               end if
               loc_isdstime = (jdate .ge. check_date + adj)
            else
               if (year .le. cut_off_year) then               ! last Sunday in October
                  if (check_date_wkday .eq. 7) then
                     adj = 0
                  else
                     adj = - check_date_wkday
                  end if
               else                                           ! first Sunday in November
                  if (check_date_wkday .eq. 7) then
                     adj = 0
                  else
                     adj = 7 - check_date_wkday
                  end if
               end if
               loc_isdstime = (jdate .lt. check_date + adj)
            end if

            isdstime = loc_isdstime

          end function isdstime

! ------------------------------------------------------------------------------
          subroutine upcase (string)

            character (len = *), intent(inout) :: string

            integer, parameter :: lcase_a_pos = ichar('a')
            integer, parameter :: ucase_a_pos = ichar('A')
            integer, parameter :: diff = ucase_a_pos - lcase_a_pos

            character, parameter :: letter_a = 'a'
            character, parameter :: letter_z   = 'z'

            integer :: str_len, i

            str_len = len_trim(string)

            do i = 1, str_len
               if (letter_a .le. string(i:i) .and. (string(i:i) .le. letter_z)) then
                  string(i:i) = char(ichar(string(i:i)) + diff)
               end if
            end do

          end subroutine upcase

! ------------------------------------------------------------------------------
          character*14 function mmddyy (jdate)

            integer, intent(in) :: jdate

            integer, parameter :: numdays (12) = (/ 31, 28 ,31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
            character (len = 4) :: month_name (12)  = (/ 'Jan.', 'Feb.', 'Mar.', 'Apr.', 'May ', 'Jun.', 
     &                                                 'Jul.', 'Aug.', 'Sep.', 'Oct.', 'Nov.', 'Dec.' /)
                                               
            integer :: year, day, numjdays(12), i, j
            logical :: done

            year = jdate / 1000
            day  = mod(jdate, 1000)

! compute number of jdays
            numjdays(1) = 1
            numjdays(2) = 32
            if (leap_year(year)) then
               numjdays(3) = numjdays(2) + 29
            else
               numjdays(3) = numjdays(2) + 28
            end if
            do i = 4, 12
               numjdays(i) = numjdays(i-1) + numdays(i-1)
            end do

            i = 1
            done = .false.
            do while ((i < 13) .and. (.not. done))
               if (numjdays(i) .le. day) then
                  i = i + 1
               else
                  done = .true.
               end if
            end do

            if (i .gt. 1) then
               j = i - 1
            end if

            day = day + 1 - numjdays(j)

            if (day .lt. 10) then
               write (mmddyy, '(a4, 1x, i1, a1, 1x, i4)') month_name(j), day, ',', year
            else
               write (mmddyy, '(a4, 1x, i2, a1, 1x, i4)') month_name(j), day, ',', year
            end if

          end function mmddyy

! ------------------------------------------------------------------------------
          character*10 function hhmmss (jtime)

            integer, intent(in) :: jtime

            integer :: hour, min, sec

            hour = jtime / 10000
            min  = mod(jtime, 10000) / 100
            sec  = mod(jtime,100)

            if (hour .lt. 10) then
               write (hhmmss, '(i1, a1, i2.2, a1, i2.2)') hour, ':', min, ':', sec
            else
               write (hhmmss, '(i2, a1, i2.2, a1, i2.2)') hour, ':', min, ':', sec
            end if

          end function hhmmss

! ------------------------------------------------------------------------------
          character*24 function dt2str (jdate, jtime)

            integer, intent(in) :: jdate, jtime

            dt2str = hhmmss(jtime) // mmddyy(jdate)

          end function dt2str

! ------------------------------------------------------------------------------
          real function yr2day (year)

            integer, intent(in) :: year

            if (leap_year(year)) then
               yr2day = 1.0/366.0
            else
               yr2day = 1.0/365.0
            end if

          end function yr2day

! ------------------------------------------------------------------------------
          subroutine nextime (jdate, jtime, dtime)

            integer, intent(inout) :: jdate, jtime
            integer, intent(in)    :: dtime

            integer :: yr, day, hr, min, sec, hr_adj, min_adj, sec_adj, ndays
            logical :: done

            yr = jdate / 1000
            day = mod(jdate, 1000)

            hr  = jtime / 10000 
            min = mod(jtime, 10000) / 100
            sec = mod(jtime, 100)

            hr_adj  = dtime / 10000 
            min_adj = mod(dtime, 10000) / 100
            sec_adj = mod(dtime, 100)

            if (dtime .ge. 0) then
               sec = sec + sec_adj
               if (sec .gt. 59) then
                  min = min + 1
                  sec = sec - 60
               end if
               min = min + min_adj
               if (min .gt. 59) then
                  hr = hr + 1
                  min = min - 60
               end if
               hr = hr + hr_adj
               day = day + hr / 24
               hr  = mod(hr, 24)

               done  = .false.
               do while (.not. done)
                  if (leap_year(yr)) then
                     ndays = 366
                  else
                     ndays = 365
                  end if 
                  if (day .le. ndays) then
                     done = .true.
                  else
                     day = day - ndays
                     yr = yr + 1
                  end if   
               end do     
            else
               sec_adj = sec_adj * (-1)
               min_adj = min_adj * (-1)
               hr_adj  = hr_adj  * (-1)

               sec = sec - sec_adj
               if (sec .lt. 0) then
                  sec = 60 + sec
                  min = min - 1
               end if
               min = min - min_adj
               if (min .lt. 0) then
                  min = 60 + min
                  hr = hr - 1
               end if

               hr  = hr - hr_adj
               day = day + hr / 24
               hr  = mod(hr, 24)

               if (hr .lt. 0) then
                  hr = 24 + hr
                  day = day - 1
               end if
 
               do while (day .le. 0)
                  if (day .eq. 0) then
                     day = ndays
                  else
                     yr = yr - 1

                     if (leap_year(yr)) then
                        ndays = 366
                     else
                        ndays = 365
                     end if 

                     if (day .gt. ndays) then
                        day = day + ndays
                     else
                        day = ndays + day
                     end if   
                  end if   
               end do     
            end if

            jdate = yr * 1000 + day
            jtime = hr * 10000 + min * 100 + sec

          end subroutine nextime

! ------------------------------------------------------------------------------
          integer function index1_char (name, n, list)

            character (*), intent(in) :: name
            character (*), intent(in) :: list(:)
            integer, intent(in)       :: n

            integer :: i
            logical :: found

            i = 0
            found = .false.
            do while ((.not. found) .and. (i < n))
               i = i + 1
               if (name == list(i)) then
                  found = .true.
               end if
            end do

            if (.not. found) then
               index1_char = 0
            else
               index1_char = i
            end if

          end function index1_char

! ------------------------------------------------------------------------------
          integer function index1_int (name, n, list)

            integer, intent(in) :: name
            integer, intent(in) :: list(:)
            integer, intent(in) :: n

            integer :: i
            logical :: found

            i = 0
            found = .false.
            do while ((.not. found) .and. (i < n))
               i = i + 1
               if (name == list(i)) then
                  found = .true.
               end if
            end do

            if (.not. found) then
               index1_int = 0
            else
               index1_int = i
            end if

          end function index1_int

! ------------------------------------------------------------------------------
          integer function junit()

            integer, parameter :: bottom = 20
            integer, save :: top = 99
            logical :: found, status
            integer :: i

            found = .false.
            i = top
            junit = -1
            do while ((.not. found) .and. ( i .ge. bottom))
               inquire (unit = i, opened = status)
               if (status) then
                  i = i - 1
               else
                  found = .true.
                  junit = i
                  top = i - 1
               end if
            end do
        
          end function junit

! ------------------------------------------------------------------------------
          integer recursive function secsdiff_jdate_jtime (jdate1, jtime1, jdate2, jtime2) result (return_value)

            integer, intent(in) :: jdate1, jtime1, jdate2, jtime2

            integer :: year1, day1, hour1, minute1, sec1,
     &                 year2, day2, hour2, minute2, sec2
            integer :: yr, loc_secsdiff, loc_date, loc_time
            logical :: reverse

            if ((jdate1 .gt. jdate2) .or.
     &          (jdate1 .eq. jdate2) .and. (jtime1 .gt. jtime2)) then
               reverse = .true.
               loc_secsdiff = secsdiff_jdate_jtime (jdate2, jtime2, jdate1, jtime1)
            else
               reverse = .false.

               year1 = jdate1 / 1000
               year2 = jdate2 / 1000

               if (year1 .eq. year2) then
                  hour1   = jtime1 / 10000
                  hour2   = jtime2 / 10000
                  minute1 = mod(jtime1, 10000) / 100
                  minute2 = mod(jtime2, 10000) / 100
                  sec1    = mod(jtime1, 100)
                  sec2    = mod(jtime2, 100)

                  loc_secsdiff = (jdate2 - jdate1) * 86400 -
     &                            ((hour1 - hour2) * 3600 +
     &                             (minute1 - minute2) * 60 +
     &                             sec1 - sec2)
               else
                  loc_secsdiff = 0
                  do yr = year1, year2
                     if (yr .eq. year1) then
                        loc_date = year1 * 1000 + num_julian_day (year1, 12, 31)
                        loc_time = 235959

! + 1 adjustment is for the difference between 12-31:23:59:59 and 01-01:00:00:00
                        loc_secsdiff = loc_secsdiff + secsdiff_jdate_jtime (jdate1, jtime1, loc_date, loc_time) + 1
                     else if (yr .eq. year2) then
                        loc_date = year2 * 1000 + 1
                        loc_time = 0
                        loc_secsdiff = loc_secsdiff + secsdiff_jdate_jtime (loc_date, loc_time, jdate2, jtime2)
                     else
                        if (leap_year (yr)) then
                           loc_secsdiff = loc_secsdiff + 31622400
                        else
                           loc_secsdiff = loc_secsdiff + 31536000
                        end if
                     end if
                  end do
               end if
            end if

            if (reverse) then
               return_value = loc_secsdiff * (-1)
            else
               return_value = loc_secsdiff
            end if

          end function secsdiff_jdate_jtime

! -----------------------------------------------------------
          integer recursive function secsdiff_time_str (date1, date2, flag) result (return_value)

            implicit none

            character (20), intent(in) :: date1, date2
            integer, intent(in), optional :: flag      ! output in hhmmss format

            integer :: year1, month1, day1, hour1, minute1, sec1, jdate1
            integer :: year2, month2, day2, hour2, minute2, sec2, jdate2
            integer :: yr, loc_secsdiff
            character (20) :: loc_date

            if (date1 .gt. date2) then
               loc_secsdiff = secsdiff_time_str (date2, date1)
            else
               read (date1, '(i4, 5(1x, i2))') year1, month1, day1, hour1, minute1, sec1
               read (date2, '(i4, 5(1x, i2))') year2, month2, day2, hour2, minute2, sec2

               if (year1 .eq. year2) then
                  jdate1 = num_julian_day (year1, month1, day1)
                  jdate2 = num_julian_day (year2, month2, day2)

                  loc_secsdiff = (jdate2 - jdate1) * 86400 -
     &                           ((hour1 - hour2) * 3600 +
     &                            (minute1 - minute2) * 60 +
     &                            sec1 - sec2)
               else
                  loc_secsdiff = 0
                  do yr = year1, year2
                     if (yr .eq. year1) then
                        write (loc_date, '(i4, a15)') year1, "-12-31:23:59:59"
! + 1 adjustment is for the difference between 12-31:23:59:59 and 01-01:00:00:00
                        loc_secsdiff = loc_secsdiff + secsdiff_time_str (date1, loc_date) + 1
                     else if (yr .eq. year2) then
                        write (loc_date, '(i4, a15)') year2, "-01-01:00:00:00"
                        loc_secsdiff = loc_secsdiff + secsdiff_time_str (loc_date, date2)
                     else
                        if (leap_year (yr)) then
                           loc_secsdiff = loc_secsdiff + 31622400
                        else
                           loc_secsdiff = loc_secsdiff + 31536000
                        end if
                     end if
                  end do
               end if
            end if

            if (present(flag)) then
               hour1        = loc_secsdiff / 3600
               minute1      = (loc_secsdiff - hour1 * 3600) / 60
               sec1         = loc_secsdiff - hour1 * 3600 - minute1 * 60
               loc_secsdiff = hour1 * 10000 + minute1 * 100 + sec1
            end if

            if (date1 .gt. date2) then
               return_value = loc_secsdiff * (-1)
            else
               return_value = loc_secsdiff
            end if

          end function secsdiff_time_str

! ------------------------------------------------------------------------------
          integer function time2sec (time)

            integer, intent(in) :: time    ! in hhmmss format
            integer :: neg_time
            integer :: time_in_sec, hr, min, sec

            if (time .gt. 0) then
               hr = time / 10000
               min = mod(time/100, 100)
               sec = mod(time, 100)
               time2sec = hr * 3600 + min * 60 + sec
            else
               neg_time = abs(time)
               hr = neg_time / 10000
               min = mod(neg_time/100, 100)
               sec = mod(neg_time, 100)
               time2sec = -1*(hr * 3600 + min * 60 + sec)
            end if

          end function time2sec

! -------------------------------------------------------------------------
          integer function secsdiff_jtime (time1, time2)

            integer, intent(in) :: time1, time2    ! in hhmmss format

            secsdiff_jtime = time2sec(time1) - time2sec(time2)

          end function secsdiff_jtime

! -------------------------------------------------------------------------
          subroutine data_time_minus_sec (date, time, minus_sec)

            integer, intent(inout) :: date        ! date in julian date format
            integer, intent(inout) :: time        ! time in hhmmss format
            integer, intent(in   ) :: minus_sec

            integer :: loc_sec, subtract_hr, subtract_min, subtract_sec,
     &                 temp, hr, min, sec, num_days

            if (minus_sec .ge. 86400) then
               num_days = minus_sec / 86400
               loc_sec = mod(minus_sec, 86400)
               date = date - num_days
            else
               loc_sec = minus_sec
            end if

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
               date = date - 1
               hr = hr + 24
            end if

            time = hr * 10000 + min * 100 + sec

          end subroutine data_time_minus_sec

! -----------------------------------------------------------
          character (2) function crlf()

            character (2) :: temp

            temp(1:1) = char(13)
            temp(2:2) = char(10)

            crlf = temp

          end function crlf

! ------------------------------------------------------------------------------
          integer function getefile ( lname, rdonly, fmtflag, caller )

            use get_env_module

            character*(*), intent(in) :: lname          !  logical file name
            logical      , intent(in) :: rdonly         !  TRUE iff file is input-only
            logical      , intent(in) :: fmtflag        !  TRUE iff file should be formatted
            character*(*), intent(in) :: caller         !  caller-name for logging

            logical :: exist
            integer :: funit, stat
            character (11)  :: status_str, fmt_str
            character (500) :: fname

            call get_env (fname, lname, ' ')

            inquire (file=fname, exist=exist, iostat=stat)

            if (stat .ne. 0) then
               print *, 'Error: inquiring about ', trim(lname)
               getefile = -1
            else

               if (.not. exist) then
                  print *, trim(lname), ' does not exist '
                  getefile = -1
               else
                  funit = junit()
                  if (fmtflag) then
                     fmt_str = 'formatted'
                  else
                     fmt_str = 'unformatted'
                  end if 
                  if (rdonly) then
                     status_str = 'old'
                  else
                     status_str = 'unknown'
                  end if 

                  open (unit   = funit,
     &                  file   = fname,
     &                  form   = fmt_str,
     &                  status = status_str,
     &                  iostat = stat)

                  if (stat .ne. 0) then
                     print *, ' Error: not able to open file ', trim(lname)
                     getefile = -1
                  else
                     getefile = funit
                  end if
               end if
            end if
 
          end function getefile

! ------------------------------------------------------------------------------
          integer function sec2time ( secs )

            integer, intent(in) :: secs

            integer :: sec, min, hour, temp, loc_secs

            if (secs .lt. 0) then
               loc_secs = - secs
            else
               loc_secs = secs
            end if

            hour = loc_secs / 3600
            temp = mod(loc_secs, 3600)
            sec = mod(temp, 60)
            min = temp / 60

            sec2time = sign(hour * 10000 + min * 100 + sec, secs)

          end function sec2time

! ------------------------------------------------------------------------------
          real function str2real( string )

            character(*), intent(in) :: string

            read (string, *) str2real

          end function str2real

! ------------------------------------------------------------------------------
          integer function lblank ( string )

            character*(*), intent(in) :: string

            integer :: str_len, i
            logical :: found

            str_len = len(string)

            found = .false.
            i = 1
            do while ((.not. found) .and. (i .le. str_len))
               if (string(i:i) .ne. ' ') then
                  found = .true.
               else
                  i = i + 1
               end if
            end do

            lblank = i - 1

          end function lblank

! ------------------------------------------------------------------------------
          subroutine sortic (n, index, name)

            character (16), intent(in) :: name(:)
            integer, intent(in)        :: n
            integer, intent(inout)     :: index(:)

            call quicksort(n, name, index)

          end subroutine sortic

! -------------------------------------------------------------------------
          integer function quicksort_partition (n, name, index)

            character (16), intent(in) :: name(:)
            integer, intent(in) :: n
            integer, intent(inout) :: index(:)

            character (16) :: pivot_value
            integer :: p, q, open, temp

            pivot_value = name(index(1))
            temp = index(1)
            p = 1
            q = n
            open = 1
            do while (p .lt. q)
               if (open .eq. p) then
                  if (name(index(q)) .le. pivot_value) then
                     index(open) = index(q)
                     open = q
                     p = p + 1
                  else
                     q = q - 1
                  end if
               else
                  if (name(index(p)) .gt. pivot_value) then
                     index(open) = index(p)
                     open = p
                     q = q - 1
                  else
                     p = p + 1
                  end if
               end if
            end do

            index(p) = temp
            quicksort_partition = p

          end function quicksort_partition

! -------------------------------------------------------------------------
          recursive subroutine quicksort (n, name, index)

            implicit none

            character (16), intent(in) :: name(:)
            integer, intent(in) :: n
            integer, intent(inout) :: index(:)

            integer :: pindex

            if (n .gt. 1) then
               pindex = quicksort_partition (n, name, index)

               call quicksort (pindex-1, name, index(1:pindex-1))
               call quicksort (n-pindex, name, index(pindex+1:n))
            end if

          end subroutine quicksort

! ------------------------------------------------------------------------------
 
          integer function promptffile (prompt, rdonly, fmtted, default, caller)

            character*(*), intent(in) :: prompt         !  prompt for user
            logical      , intent(in) :: rdonly         !  TRUE iff file is input-only
            logical      , intent(in) :: fmtted         !  TRUE iff file should be formatted
            character*(*), intent(in) :: default        !  default logical file name
            character*(*), intent(in) :: caller         !  caller-name for logging messages

            integer :: fnum

            fnum = getefile ( default, rdonly, fmtted, caller )

            promptffile = fnum

          end function promptffile

! -------------------------------------------------------------------------
          subroutine date_time_minus_sec (date, time, minus_sec)

            integer, intent(inout) :: date        ! date in julian date format
            integer, intent(inout) :: time        ! time in hhmmss format
            integer, intent(in   ) :: minus_sec

            integer :: loc_year, loc_date, loc_sec, subtract_hr, subtract_min,
     &                 subtract_sec, temp, hr, min, sec, num_days

            loc_year = date / 1000
            loc_date = mod(date, 1000)

            num_days = minus_sec / 86400
            loc_sec = mod(minus_sec, 86400)
            loc_date = loc_date - num_days

            do while (loc_date <= 0)
               loc_year = loc_year - 1
               if (leap_year(loc_year)) then
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
               date = date - 1
               hr = hr + 24
               if (loc_date == 0) then
                  loc_year = loc_year - 1
                  if (leap_year(loc_year)) then
                     loc_date = 366
                  else
                     loc_date = 365
                  end if
               end if
            end if

            date = loc_year * 1000 + loc_date
            time = hr * 10000 + min * 100 + sec

          end subroutine date_time_minus_sec

! ------------------------------------------------------------------------------
          logical function currstep (jdate, jtime,
     &                               sdate, stime, tstep,
     &                               cdate, ctime )

            integer, intent(in)  :: jdate, jtime, sdate, stime, tstep
            integer, intent(out) :: cdate, ctime

            integer :: sec_diff, tsec, mod_sec

            if ((jdate .lt. sdate) .or.
     &          ((jdate .eq. sdate) .and. (jtime .lt. stime))) then
               currstep = .false.
               cdate = 0
               ctime = 0
            else
               currstep = .true.
               sec_diff = secsdiff (sdate, stime, jdate, jtime)
               tsec = time2sec(tstep)

               mod_sec = mod(sec_diff, tsec)
               cdate = jdate
               ctime = jtime
               call date_time_minus_sec (cdate, ctime, mod_sec)
            end if

          end function currstep

! ------------------------------------------------------------------------------
          integer function findc (key, n, list)

            character*(*), intent(in) :: key
            integer, intent(in)       :: n
            character*(*), intent(in) :: list(n)

            integer :: top, bot, mid
            logical :: done

            findc = -1
            top = 1
            bot = n
            done = .false.
            do while (.not. done)
               mid = (top + bot) / 2
               if (top .le. bot) then
                  if (key == list(mid)) then
                     done = .true.
                     findc = mid
                  else if (key .lt. list(mid)) then
                     bot = mid - 1
                  else
                     top = mid + 1
                  end if
               else
                  done = .true.
               end if
            end do

          end function findc

! ------------------------------------------------------------------------------

          real function poly (pt, x, fx, n)

            implicit none

            real, intent(in)    :: pt, x(n+1), fx(n+1)
            integer, intent(in) :: n

            real, allocatable :: fd(:,:)
            real :: prod, result
            integer :: stat, i, j

            allocate (fd(n, n), stat=stat)

            do i = 1, n
               fd(i,1) = (fx(i+1) - fx(i)) / (x(i+1) - x(i))
            end do

            do j = 2, n
               do i = 1, n-j+1
                  fd(i,j) = (fd(i+1,j-1) - fd(i,j-1)) / (x(j+i) - x(i))
               end do
            end do

            result = fx(1)
            prod = 1
            do j = 1, n
               prod = prod * (pt - x(j))
               result = result + prod * fd(1,j)
            end do

            poly = result
            deallocate (fd)

          end function poly

! -------------------------------------------------------------------------

        INTEGER FUNCTION INIT3 ( )

        use universal_data_module

      IMPLICIT NONE

!       INTEGER, EXTERNAL :: INITLOG3, JUNIT
!       EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons

        CHARACTER*16, PARAMETER:: SCENFILE = 'SCENFILE'
        CHARACTER*64, PARAMETER :: NOTICE( 17 ) = (/
     &'                                                              ',
     &'This program uses the EPA-AREAL/MCNC-EnvPgms/BAMS Models-3    ',
     &'I/O Applications Programming Interface, [I/O API] which is    ',
     &'built on top of the netCDF I/O library (Copyright 1993, 1996  ',
     &'University Corporation for Atmospheric Research/Unidata       ',
     &'Program) and the PVM parallel-programming library (from       ',
     &'Oak Ridge National Laboratory).  Copyright (C) 1992-2002 MCNC ',
     &'and Carlie J. Coats, Jr., and 2003-2006 Baron Advanced        ',
     &'Meteorological Systems, LLC and released under the GNU LGPL   ',
     &'License, version 2.1.  See URL                                ',
     &'                                                              ',
     &'    http://www.baronams.com/products/ioapi/LGPL.txt           ',
     &'                                                              ',
     &'for conditions of use.                                        ',
     &'                                                              ',
     &'Library release tag: $JDate: 2010068 $                        ',
     &'                                                              '
     &   /)


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*512   EQNAME
        INTEGER         I, J, IOST, IDEV
        LOGICAL         AFLAG

        CHARACTER *80 VARVER
        CHARACTER *80 NCFVER
        CHARACTER *80, PARAMETER :: IOAPILIBVER =
     &             '$Id: @(#) ioapi library version 3.1'
     &             // ' $'

        logical, save :: finit3 = .false.

C***********************************************************************
C   begin body of subroutine  INIT3

#if defined(__sgi)    || defined(__mips__)

        !!==========>  *HACK*  to deal with SGI v7.4  compilers, that
        !!             do not follow industry standards with respect
        !!             to how to make sure that BLOCK DATA modules are
        !!             properly linked into an executable:

        CALL INITBLK3

#endif

        IF ( FINIT3 ) THEN
            INIT3 = LOGDEV
            RETURN           !  M3 I/O already set up
        END IF

        LOGDEV = INITLOG3( 'INIT3' )

        WRITE (LOGDEV, '( 5X, A )' ) ' '

	CALL FLUSH( LOGDEV )

        FINIT3 = .TRUE.
        INIT3  = LOGDEV

        RETURN


C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '*** ERROR ABORT in subroutine INIT3 ***',
     &            /5X , A , I3, :,
     &            /5X , A , A , // )

91020   FORMAT ( //5X , '>>> WARNING in subroutine INIT3 <<<',
     &            /5X , A , I3, :,
     &            /5X , A , A , // )

91030   FORMAT ( //5X , '>>> WARNING in subroutine INIT3 <<<',
     &            /5X , A , I3, :,
     &            /5X , A , A , // )


        END FUNCTION INIT3

C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#ifdef   AVOID_FLUSH

        SUBROUTINE FLUSH( IDEV )
        IMPLICIT NONE
        INTEGER IDEV
        RETURN
        END

#endif

! ------------------------------------------------------------------------------

        INTEGER FUNCTION INITLOG3 ( CALLER )

         use universal_data_module
         use get_env_module

      IMPLICIT NONE

C...........   ARGUMENT and its description:

        CHARACTER*(*), INTENT(IN   ) :: CALLER

C...........   EXTERNAL FUNCTIONS and their descriptions:

!       INTEGER, EXTERNAL :: JUNIT
!       EXTERNAL          :: INITBLK3


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*512   EQNAME
        CHARACTER*256   MESG            !  message/warning buffer

        INTEGER         L, IOST

C...........   SAVED LOCAL VARIABLES and their descriptions:
C...........   NOTE:  the ANSI standard requires the use of SAVE statements
C...........   for variables which must retain their values from call to call.

        CHARACTER*16, PARAMETER :: LOGFILE = 'LOGFILE'


C***********************************************************************
C   begin body of subroutine  INITLOG

#if defined(__sgi)    || defined(__mips__)

        !!==========>  *HACK*  to deal with SGI v7.4 or later compilers,
        !!             that do not follow industry standards with respect
        !!             to how to make sure that BLOCK DATA modules are
        !!             properly linked into an executable:

        CALL INITBLK3

#endif

        IF ( LOGDEV .LT. 0 ) THEN

                LOGDEV = JUNIT()
                OPEN ( UNIT    =  LOGDEV,
     &                 IOSTAT  =  IOST,
     &                 FILE    =  mylogfile_name,
     &                 STATUS  =  'NEW',
     &                 ACCESS  =  'SEQUENTIAL')

            IF ( IOST .NE. 0 ) THEN
                MESG =TRIM( CALLER ) // '/INITLOG3'
                WRITE( *, '( /, 5X, 3 A )' )
     &                    '***  ERROR in ', TRIM( MESG ), '  ***'
                WRITE( *, '( 5X, A, I10 )' )
     &                    'Error opening log file on unit', LOGDEV,
     &                    'I/O STATUS =', IOST
                WRITE( *, '( 5X, 2 A, / )' )
     &                    'File: ', TRIM( EQNAME )
                stop
            END IF

        END IF          !  if logdev < 0

        INITLOG3 = LOGDEV

        RETURN

        END FUNCTION INITLOG3

        end module util_module
