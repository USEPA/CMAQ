! Purpose: Search a particular object in a list or a file in a list of
!          file names and return the location

      module mio_search_module

        use mio_global_data_module

        implicit none

        interface mio_search
          module procedure mio_search_fname,      &
                           mio_search_item_char,  &
                           mio_search_item_int
        end interface

        contains

        integer function mio_search_fname (fname)

          character (*), intent(in) :: fname

          integer :: i
          logical :: found

          i = 0
          found = .false.
          do while ((.not. found) .and. (i < mio_nfiles))
             i = i + 1
!  write (6, *) ' ==d== search ', i, '==', trim(fname), '==', trim(mio_file_data(i)%filename), '=='
             if (fname == mio_file_data(i)%filename) then
                found = .true.
             end if
          end do

          if (.not. found) then
             mio_search_fname = -1
          else
             mio_search_fname = i
          end if

        end function mio_search_fname

! ----------------------------------------------------------------------------
        integer function mio_search_item_char (name, list, n)

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
             mio_search_item_char = -1
          else
             mio_search_item_char = i
          end if

        end function mio_search_item_char

! ----------------------------------------------------------------------------
        integer function mio_search_item_int (name, list, n)

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
             mio_search_item_int = -1
          else
             mio_search_item_int = i
          end if

        end function mio_search_item_int

      end module mio_search_module
