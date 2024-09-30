! Purpose: A collection of utility functions tailored for MIO

      module mio_util_func_module

        implicit none

        ! to extract a specific type of information from a string
        interface mio_extract_string
          module procedure mio_extract_string_1,   &
                           mio_extract_string_2
        end interface

        contains

! --------------------------------------------------------------------------------
! Purpose: Given a string, extract each component and report number of components.

        subroutine mio_extract_string_1 (in_str, out_str, n_items)

          use mio_parameter_module

          character (*), intent(in)      :: in_str
          character (*), intent(out)     :: out_str(:)
          integer, intent(out), optional :: n_items

          integer :: str_len, s, e, loc_n_items, state
          logical :: found_comment

          str_len = len_trim(in_str)

          e = 0
          state = 0
          out_str = ' '
          loc_n_items = 0
          found_comment = .false.
          do while ((e < str_len) .and. (.not. found_comment))
             e = e + 1
             if (in_str(e:e) == '#') then
                found_comment = .true.
             else
                if ((state == 0) .and. (in_str(e:e) .ne. ' ')) then
                   state = 1
                   s = e
                end if

                if (((state == 1) .and. (in_str(e:e) .eq. ' ')) .or.  (e == str_len)) then
                   loc_n_items = loc_n_items + 1
                   state = 0
                   if (e == str_len) then
                      out_str(loc_n_items) = in_str(s:e)
                   else
                      out_str(loc_n_items) = in_str(s:e-1)
                   end if
                end if
             end if
          end do

          if (present(n_items)) then
             n_items = loc_n_items
          end if

        end subroutine mio_extract_string_1

! --------------------------------------------------------------------------------
! Purpose: Given a string, extract each component and report number of components.

        subroutine mio_extract_string_2 (in_str, n, out_str)

          use mio_parameter_module
          use mio_file_template_module, only : n_dim_names

          character (*), intent(in)  :: in_str
          integer, intent(out)       :: n
          character (*), intent(out) :: out_str(:)

          character (mio_max_filename_len) :: loc_str, myfmt
          logical :: found_comment

          integer :: str_len, lexical_start_pos(2*n_dim_names+1), n_lexicals, p1, p2, p3
          logical :: blank

          loc_str = adjustl(in_str) 
          str_len = len_trim(loc_str)

          ! keep only one blank in between any two lexical terms
          p1 = 0
          p2 = 0
          found_comment = .false.
          blank = .true.
          n_lexicals = 0
          do while ((p2 < str_len) .and. (.not. found_comment))
             p2 = p2 + 1
             if (loc_str(p2:p2) .ne. ' ') then
                if (loc_str(p2:p2) == '#') then
                   found_comment = .true.
                   do p3 = p1+1, str_len
                      loc_str(p3:p3) = ' '
                   end do
                else
                   p1 = p1 + 1
                   loc_str(p1:p1) = loc_str(p2:p2)
                   if (blank) then
                      n_lexicals = n_lexicals + 1
                      blank = .false.
                      lexical_start_pos(n_lexicals) = p1
                   end if
                end if
             else if (loc_str(p2:p2) == ' ') then
                if (.not. blank) then
                   p1 = p1 + 1
                   loc_str(p1:p1) = loc_str(p2:p2)
                   blank = .true.
                end if
             end if
          end do

          str_len = len_trim(loc_str)
          out_str = ' '

          ! extract information accordingly
          if (n_lexicals == 1) then
             read (loc_str(1:str_len), *) n
          else if (n_lexicals == 2) then
             read (loc_str, *) n, out_str(1)
          else
             read (loc_str(1:lexical_start_pos(2)-1), *) n
             write (out_str(1), *) n_lexicals - 1         ! a number indicates creating a brand new file
             write (myfmt, '(a2, i3.3, a1)') '(a', str_len-lexical_start_pos(2)+1, ')'
             read (loc_str(lexical_start_pos(2):str_len), myfmt) out_str(2)
          end if

        end subroutine mio_extract_string_2

! --------------------------------------------------------------------------------
! Purpose: To convert upper case of English alphabet to lower case

        character (1000) function mio_to_lower_case (str)

          character (*), intent(in) :: str

          character (1000) :: t_str
          integer :: str_len, i

          str_len = len_trim(str)
          t_str = ' '
          mio_to_lower_case = ' '
          do i = 1, str_len
             if ('A' .le. str(i:i) .and. (str(i:i) .le. 'Z')) then
                t_str(i:i) = char(ichar('a') + ichar(str(i:i)) - ichar('A'))
             else
                t_str(i:i) = str(i:i)
             end if
          end do

          mio_to_lower_case(1:str_len) = t_str(1:str_len)

        end function mio_to_lower_case

      end module mio_util_func_module
