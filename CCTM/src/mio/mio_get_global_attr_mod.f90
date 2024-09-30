! Purpose: To retrieve a specific global attribute's value bases on its
!          data type and size, integer, integer array, real, real array,
!          double precision, and character type

       module mio_get_global_attr_module

         use mio_global_data_module
         use mio_search_module

         implicit none

         interface mio_get_global_attr
           module procedure mio_get_global_attr_int,         &
                            mio_get_global_attr_int_array,   &
                            mio_get_global_attr_real,        &
                            mio_get_global_attr_real_array,  &
                            mio_get_global_attr_double,      &
                            mio_get_global_attr_char
         end interface

         contains

! ---------------------------------------------------------------------------
         subroutine mio_get_global_attr_int (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
           integer, intent(out)      :: value

           integer :: m, loc, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
              write (mio_logdev, *) ' due to file, ', trim(infile), ' does not exist'
              lerror = .true.
           else
              m = mio_search (attr_name,                              &
                              mio_file_data(fnum)%glo_att_name,       &
                              mio_file_data(fnum)%n_global_atts) 

              if (m .gt. 0) then
                 if (mio_file_data(fnum)%glo_att_type(m) .eq. nf90_int) then
                    loc = mio_file_data(fnum)%glo_att_irange(2*m-1)
                    value = mio_file_data(fnum)%glo_att_ival(loc)
                 else
                    write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_get_global_attr_int due to error stated above'
              stop
           end if

         end subroutine mio_get_global_attr_int

! ---------------------------------------------------------------------------
         subroutine mio_get_global_attr_int_array (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
! the first number in return value is the number of elmemnts, n in the
! array and the actual value stores in the location starting from 2
           integer, intent(out)      :: value(:)

           integer :: m, s, e, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
              write (mio_logdev, *) ' due to file, ', trim(infile), ' does not exist'
              lerror = .true.
           else
              m = mio_search (attr_name,                              &
                              mio_file_data(fnum)%glo_att_name,       &
                              mio_file_data(fnum)%n_global_atts) 

              if (m .gt. 0) then
                 if (mio_file_data(fnum)%glo_att_type(m) .eq. nf90_int) then
                    s = mio_file_data(fnum)%glo_att_irange(2*m-1)
                    e = mio_file_data(fnum)%glo_att_irange(2*m)
                    value(1) = e - s + 1
                    value(2:e-s+2) = mio_file_data(fnum)%glo_att_ival(s:e)
                 else
                    write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_get_global_attr_int_array due to error stated above'
              stop
           end if

         end subroutine mio_get_global_attr_int_array

! ---------------------------------------------------------------------------
         subroutine mio_get_global_attr_real (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
           real, intent(out)         :: value

           integer :: m, loc, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
              write (mio_logdev, *) ' due to file, ', trim(infile), ' does not exist'
              lerror = .true.
           else
              m = mio_search (attr_name,                              &
                              mio_file_data(fnum)%glo_att_name,       &
                              mio_file_data(fnum)%n_global_atts) 

              if (m .gt. 0) then
                 if (mio_file_data(fnum)%glo_att_type(m) .eq. nf90_float) then
                    loc = mio_file_data(fnum)%glo_att_rrange(2*m-1)
                    value = mio_file_data(fnum)%glo_att_rval(loc)
                 else
                    write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_get_global_attr_real due to error stated above'
              stop
           end if

         end subroutine mio_get_global_attr_real

! ---------------------------------------------------------------------------
         subroutine mio_get_global_attr_real_array (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
! the first number in return value is the number of elmemnts, n in the
! array and the actual value stores in the location starting from 2
           real, intent(out)         :: value(:)

           integer :: m, s, e, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
              write (mio_logdev, *) ' due to file, ', trim(infile), ' does not exist'
              lerror = .true.
           else
              m = mio_search (attr_name,                              &
                              mio_file_data(fnum)%glo_att_name,       &
                              mio_file_data(fnum)%n_global_atts) 

              if (m .gt. 0) then
                 if (mio_file_data(fnum)%glo_att_type(m) .eq. nf90_float) then
                    s = mio_file_data(fnum)%glo_att_rrange(2*m-1)
                    e = mio_file_data(fnum)%glo_att_rrange(2*m)
                    value(1) = e - s + 1
                    value(2:e-s+2) = mio_file_data(fnum)%glo_att_rval(s:e)
                 else
                    write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_get_global_attr_real_array due to error stated above'
              stop
           end if

         end subroutine mio_get_global_attr_real_array

! ---------------------------------------------------------------------------
         subroutine mio_get_global_attr_double (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
           real*8, intent(out)       :: value

           integer :: m, loc, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
              write (mio_logdev, *) ' due to file, ', trim(infile), ' does not exist'
              lerror = .true.
           else
              m = mio_search (attr_name,                              &
                              mio_file_data(fnum)%glo_att_name,       &
                              mio_file_data(fnum)%n_global_atts) 

              if (m .gt. 0) then
                 if (mio_file_data(fnum)%glo_att_type(m) .eq. nf90_double) then
                    loc = mio_file_data(fnum)%glo_att_drange(2*m-1)
                    value = mio_file_data(fnum)%glo_att_dval(loc)
                 else
                    write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_get_global_attr_double due to error stated above'
              stop
           end if

         end subroutine mio_get_global_attr_double

! ---------------------------------------------------------------------------
         subroutine mio_get_global_attr_char (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
           character(*), intent(out) :: value

           integer :: m, s, e, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
              write (mio_logdev, *) ' due to file, ', trim(infile), ' does not exist'
              lerror = .true.
           else
              m = mio_search (attr_name,                              &
                              mio_file_data(fnum)%glo_att_name,       &
                              mio_file_data(fnum)%n_global_atts) 

              if (m .gt. 0) then
                 if (mio_file_data(fnum)%glo_att_type(m) .eq. nf90_char) then
                    s = mio_file_data(fnum)%glo_att_crange(2*m-1)
                    e = mio_file_data(fnum)%glo_att_crange(2*m)
                    value = mio_file_data(fnum)%glo_att_cval(s:e)
                 else
                    write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to access global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_get_global_attr_char due to error stated above'
              stop
           end if

         end subroutine mio_get_global_attr_char

       end module mio_get_global_attr_module
