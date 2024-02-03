
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

!------------------------------------------------------------------------!
! This module contains utility functions to support centralized I/O 
! implementation

! Revision History:
!  02/01/19, D. Wong: initial implementation
!  04/23/19, D. Wong: added two new subroutines: leap_year and
!                     julian_to_mpas_date_time
!  08/01/19, D. Wong: modified code to work with two-way model
!  11/20/19, F. Sidi: Modified time to sec to handle negative numbers
!  03/05/20, D. Wong: Expanded CIO functionalities to MPAS as well
!  07/07/20, D. Wong: Formulated a robust routine to compute JDATE1 - JDATE2
!                     and JDATE + NDAYS
!  01/12/21, D. Wong: Modified character declaration legnth from 20 to
!                     * for intent(out) mpas_time_stamp in subroutine
!                     julian_to_mpas_date_time and put in some error
!                     checking for jdate
!------------------------------------------------------------------------!

      module centralized_io_util_module

        implicit none

        interface time_diff
          module procedure time_diff_i,
     &                     time_diff_str
        end interface

        interface binary_search
          module procedure binary_search_char,
     &                     binary_search_int
        end interface

        interface quicksort
          module procedure quicksort_c1d,
     &                     quicksort_c2d
        end interface

        interface search
          module procedure search_c,
     &                     search_i
        end interface

        contains

! ----------------------------------------------------------------------
! this is for mpas extracting layer distribution information store in an emission file

        subroutine ext_layer_info (buf, layer, bot, top, factor)

          implicit none

          character (*), intent(in) :: buf
          integer, intent(out) :: bot(:), top(:), layer
          real, intent(out) :: factor(:)

          integer :: index(5), loc, str_len, n, start, t_lay
          logical :: zero

          zero = .true.
          str_len = len_trim(buf)
          start = 27
          do while (start .lt. str_len)
             loc = start
             n = 1
             index(1) = loc
             do while (n < 5)
                loc = loc + 1
                if (buf(loc:loc) == ',') then
                   n = n + 1
                   index(n) = loc
                else if (buf(loc:loc) == ';') then
                   n = n + 1
                   index(n) = loc - 1
                else if (loc == str_len) then
                   n = n + 1
                   index(n) = loc + 1
                end if
             end do

             start = loc + 1

             read (buf(index(1):index(2)-1), *) t_lay
             read (buf(index(2)+1:index(3)-1), *) bot(t_lay)
             read (buf(index(3)+1:index(4)-1), *) top(t_lay)
             read (buf(index(4)+1:index(5)), *) factor(t_lay)

             if (factor(t_lay) .gt. 0) then
                zero = .false.
             else if ((factor(t_lay) .eq. 0) .and. (.not. zero)) then
                start = str_len + 1
             end if 

          end do

          layer = t_lay - 1  

        end subroutine ext_layer_info

! ----------------------------------------------------------------------
        subroutine cal_distribution (bottom, top, atmo_height, factor, layer, 
     &                               num_dist_layers, new_factor)

          implicit none

          integer, intent(in) :: bottom(:), top(:), layer
          real, intent(in) :: atmo_height(:), factor(:)
          integer, intent(out) :: num_dist_layers
          real, intent(out) :: new_factor(:)

          integer  :: e_lay, a_lay, n_lay, state, stat, i, j, k, lvl
          real :: ratio, remaining, diff
          integer, allocatable :: distribution(:,:)
          real, allocatable :: distribution_factor(:,:)

          allocate (distribution(0:5, size(atmo_height)),
     &              distribution_factor(5, size(atmo_height)),
     &              stat=stat)

          distribution = 0
          distribution_factor = 0.0
          remaining = 0.0
          e_lay = 1
          a_lay = 1
          n_lay = 0
          remaining = 1.0
          ratio = 0.0
          if (top(1) < atmo_height(1)) then
             state = 0    ! considering distribution height < model height
          else
             state = 1    ! considering distribution height > model height
          end if

          do while (e_lay .le. layer)

             if (top(e_lay) < atmo_height(a_lay)) then

                n_lay = n_lay + 1 
                distribution(n_lay, a_lay) = e_lay
                if (state == 0) then
                   remaining = 1.0
                end if
                distribution_factor(n_lay, a_lay) = remaining
                distribution(0, a_lay) = n_lay
                e_lay = e_lay + 1
                state = 0

             else

                if (state == 0) then
                   diff = atmo_height(a_lay) - bottom(e_lay)
                   ratio = diff / (top(e_lay) - bottom(e_lay))
                   remaining = 1.0 - ratio
                else
                   if (a_lay == 1) then
                      ratio = atmo_height(1) / top(e_lay)
                      remaining = 1.0 - ratio
                   else
                      diff = atmo_height(a_lay) - atmo_height(a_lay-1)
                      ratio = diff / (top(e_lay) - bottom(e_lay))
                      remaining = remaining - ratio
                   end if
                end if
                n_lay = n_lay + 1 
                distribution(n_lay, a_lay) = e_lay
                distribution_factor(n_lay, a_lay) = ratio
                distribution(0, a_lay) = n_lay

                a_lay = a_lay + 1
                n_lay = 0
                state = 1
             end if 

          end do

          if (remaining > 0.0) then
             distribution(n_lay, a_lay) = e_lay - 1
             distribution_factor(n_lay, a_lay) = remaining
             distribution(0, a_lay) = n_lay
          end if

          num_dist_layers = a_lay

          new_factor = 0.0
          do k = 1, a_lay
             do j = 1, distribution(0, k)
                lvl = distribution(j, k)
                new_factor(k) = new_factor(k) + distribution_factor(j, k) * factor(lvl)
             end do
          end do

          deallocate (distribution, distribution_factor)

        end subroutine cal_distribution

! -------------------------------------------------------------------------
        recursive subroutine quicksort_c1d (name, begin, end)

          character (*), intent(out) :: name(:)
          integer, intent(in)         :: begin, end

          integer        :: i, j
          character (50) :: str1, str2
          logical        :: done

          str1 = name( (begin + end) / 2 )
          i = begin
          j = end
          done = .false.
          do while (.not. done)
             do while (name(i) < str1)
                i = i + 1
             end do
             do while (str1 < name(j))
                j = j - 1
             end do
             if (i .ge. j) then
                done = .true.
             else
                str2 = name(i)
                name(i) = name(j)
                name(j) = str2
                i = i + 1
                j = j - 1
             end if
          end do
          if (begin < i-1) call quicksort(name, begin, i-1)
          if (j+1 < end)   call quicksort(name, j+1, end)

        end subroutine quicksort_c1d

! -------------------------------------------------------------------------
        recursive subroutine quicksort_c2d (name, begin, end)

          character (*), intent(out) :: name(:,:)
          integer, intent(in)         :: begin, end

          integer        :: i, j, dsize
          character (50) :: str1, str2(3)
          logical        :: done

          dsize = size(name,2)
          str1 = name( (begin + end) / 2, 1 )
          i = begin
          j = end
          done = .false.
          do while (.not. done)
             do while (name(i,1) < str1)
                i = i + 1
             end do
             do while (str1 < name(j, 1))
                j = j - 1
             end do
             if (i .ge. j) then
                done = .true.
             else
                str2(1:dsize) = name(i,:)  
                name(i,:) = name(j,:)
                name(j,:) = str2(1:dsize)
                i = i + 1
                j = j - 1
             end if
          end do
          if (begin < i-1) call quicksort(name, begin, i-1)
          if (j+1 < end)   call quicksort(name, j+1, end)

        end subroutine quicksort_c2d

! -------------------------------------------------------------------------
        function binary_search_char (name, list, n) result (loc)

         use RUNTIME_VARS

          character (*), intent(in) :: name, list(:)
          integer, intent(in)        :: n
          integer :: loc

          logical :: found
          integer :: mid_loc, start_loc, end_loc

          start_loc = 1
          end_loc   = n
          found = .false.
          do while ((start_loc .le. end_loc) .and. (.not. found))
             mid_loc = start_loc + (end_loc - start_loc) / 2
             if (name .lt. list(mid_loc)) then
                end_loc = mid_loc - 1
             else if (name .gt. list(mid_loc)) then
                start_loc = mid_loc + 1
             else
                found = .true.
             end if
          end do

          if (found) then
             loc = mid_loc
          else
             loc = -1
          end if

        end function binary_search_char

! -------------------------------------------------------------------------
        function binary_search_int (name, list, n) result (loc)

          integer, intent(in) :: name, list(:)
          integer, intent(in) :: n
          integer :: loc

          logical :: found
          integer :: mid_loc, start_loc, end_loc

          start_loc = 1
          end_loc   = n
          found = .false.
          do while ((start_loc .le. end_loc) .and. (.not. found))
             mid_loc = start_loc + (end_loc - start_loc) / 2
             if (name .lt. list(mid_loc)) then
                end_loc = mid_loc - 1
             else if (name .gt. list(mid_loc)) then
                start_loc = mid_loc + 1
             else
                found = .true.
             end if
          end do

          if (found) then
             loc = mid_loc
          else
             loc = -1
          end if

        end function binary_search_int

! -------------------------------------------------------------------------
        function search_c (name, list, n) result (loc)

          character (*), intent(in) :: name, list(:)
          integer, intent(in)        :: n
          integer :: loc

          logical :: found
          integer :: lloc

          lloc = 0
          found = .false.
          do while ((lloc .lt. n) .and. (.not. found))
             lloc = lloc + 1
             if (name .eq. list(lloc)) then
                found = .true.
             end if
          end do

          if (found) then
             loc = lloc
          else
             loc = -1
          end if

        end function search_c

! -------------------------------------------------------------------------
        function search_i (data, list, n) result (loc)

          integer, intent(in) :: data, list(:)
          integer, intent(in) :: n
          integer :: loc

          logical :: found
          integer :: lloc

          lloc = 0
          found = .false.
          do while ((lloc .lt. n) .and. (.not. found))
             lloc = lloc + 1
             if (data .eq. list(lloc)) then
                found = .true.
             end if
          end do

          if (found) then
             loc = lloc
          else
             loc = -1
          end if

        end function search_i

! -------------------------------------------------------------------------
        integer function time_to_sec (time)

          integer, intent(in) :: time    ! in hhmmss format
          integer :: neg_time
          integer :: time_in_sec, hr, min, sec

          if (time .gt. 0) then
             hr = time / 10000
             min = mod(time/100, 100)
             sec = mod(time, 100)
             time_to_sec = hr * 3600 + min * 60 + sec
          else
             neg_time = abs(time)
             hr = neg_time / 10000
             min = mod(neg_time/100, 100)
             sec = mod(neg_time, 100)
             time_to_sec = -1*(hr * 3600 + min * 60 + sec)
          end if
          
        end function time_to_sec

! -------------------------------------------------------------------------
        integer function time_diff_i (time1, time2)

          integer, intent(in) :: time1, time2    ! in hhmmss format

          time_diff_i = time_to_sec(time1) - time_to_sec(time2)

        end function time_diff_i

! -------------------------------------------------------------------------
        integer function time_diff_str (time1, time2)

          character (64), intent(in) :: time1, time2    ! in yyyy-mm-dd_hh:mm:ss format

          integer :: hr1, min1, sec1, hr2, min2, sec2, hr, min, sec, diff, str_len

! for current implementation, assume yyyy-mm-dd are the same

          str_len = len_trim(time1) 
          read (time1(str_len-7:str_len), '(i2, 1x, i2, 1x, i2)') hr1, min1, sec1
          str_len = len_trim(time2) 
          read (time2(str_len-7:str_len), '(i2, 1x, i2, 1x, i2)') hr2, min2, sec2
     
          diff = (hr1 - hr2) * 3600 + (min1 - min2) * 60 + (sec1 - sec2)

          hr = diff / 3600
          diff = diff - hr * 3600

          min = diff / 60

          sec = diff - min * 60 

          time_diff_str = hr * 10000 + min * 100 + sec

        end function time_diff_str

! --------------------------------------------------------------------------------
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

! -------------------------------------------------------------------------
        subroutine julian_to_mpas_date_time (jdate, jtime, mpas_time_stamp)

          integer, intent(in)  :: jdate, jtime
          character (*), intent(out) :: mpas_time_stamp

          integer, parameter :: num_days (12, 2) =
     &        reshape ((/ 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365,
     &                    31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 /), (/12, 2/))

          integer :: ind, t_date, i, year, month, day, hour, minute, sec
          logical :: found

          year = jdate / 1000
          t_date = mod(jdate, 1000)

          if (leap_year(year)) then
             if (t_date > 366) then
                write (6, *) ' Error: JDATE > 366 for a leap year', jdate
                stop
             end if
             ind = 2
          else
             if (t_date > 365) then
                write (6, *) ' Error: JDATE > 365 for a non leap year', jdate
                stop
             end if
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

          hour   = jtime / 10000
          minute = mod(jtime, 10000) / 100
          sec    = mod(jtime, 100)

          write (mpas_time_stamp, '(i4, 5(a1, i2.2))')
     $          year, '-', month, '-', day, '_', hour, ':', minute, ':', sec

        end subroutine julian_to_mpas_date_time

! -------------------------------------------------------------------------
        subroutine mpas_date_time_to_julian (mpas_time_stamp, jdate, jtime)

          character (20), intent(in) :: mpas_time_stamp
          integer, intent(out)       :: jdate, jtime

          integer, parameter :: num_days (12, 2) =
     &        reshape ((/ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334,
     &                    0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 /), (/12, 2/))

          integer :: ind, year, month, day, hour, minute, sec

          read (mpas_time_stamp, '(i4, 5(1x, i2))') year, month, day, hour, minute, sec

          if (leap_year(year)) then
             ind = 2
          else
             ind = 1
          end if

          jdate = year * 1000 + num_days(month, ind) + day
          jtime = hour * 10000 + minute * 100 + sec

        end subroutine mpas_date_time_to_julian

!--------------------------------------------------------------------------
       integer function next_day (jday)

! This function determermins the next day for time interpolation 
          implicit none

          integer, intent(in) :: jday
          integer year, day

          day  = MOD(jday,1000)
          year = INT(jday/1000)

          If( day .LT. 365 ) Then
             next_day = jday+1
          Else
             If( MOD(year,4) .Eq. 0 .And. MOD(year,100) .Ne. 0 ) Then
! Leap Year        
                If( day .Eq. 365 ) Then
                   next_day = jday + 1
                Else
                   next_day = (INT(jday/1000)+1)*1000+1
                End If
             Else If(MOD(year,400) .Eq. 0 ) Then
! also a leap year, e.g. 2000 but not 2100
                If( day .Eq. 365 ) Then
                   next_day = jday + 1
                Else
                   next_day = (INT(jday/1000)+1)*1000+1
                End If
             Else
! not a leap year
                next_day = (INT(jday/1000)+1)*1000+1
             End If
          End If

       end function next_day

      end module centralized_io_util_module
