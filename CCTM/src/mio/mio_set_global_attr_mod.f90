! Purpose: To alter the value of a specific global attibute

       module mio_set_global_attr_module

         use mio_global_data_module
         use mio_search_module

         implicit none

         interface mio_set_global_attr
           module procedure mio_set_global_attr_int,                &  ! an integer type
                            mio_set_global_attr_int_array,          &  ! an integer array
                            mio_set_global_attr_real,               &  ! a real type
                            mio_set_global_attr_real_array,         &  ! a real array
                            mio_set_global_attr_real_array_subset,  &  ! a subset from a source file
                            mio_set_global_attr_double,             &  ! a double precision type
                            mio_set_global_attr_char                   ! a character type
         end interface

         contains

! ---------------------------------------------------------------------------
         subroutine mio_set_global_attr_int (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
           integer, intent(in)       :: value

           integer :: m, loc, stat, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
              write (mio_logdev, *) ' due to file, ', trim(infile), ' does not exist'
              lerror = .true.
           else
              m = mio_search (attr_name,                              &
                              mio_file_data(fnum)%glo_att_name,       &
                              mio_file_data(fnum)%n_global_atts) 

              if (m .gt. 0) then
                 if (mio_file_data(fnum)%glo_att_type(m) .eq. nf90_int) then
                    loc = mio_file_data(fnum)%glo_att_irange(2*m-1)
                    mio_file_data(fnum)%glo_att_ival(loc) = value

                    stat = nf90_put_att (mio_file_data(fnum)%fileid,          &
                                         nf90_global,                         &
                                         mio_file_data(fnum)%glo_att_name(m), &
                                         value)
                    if (stat .ne. 0) then
                       write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                       write (mio_logdev, *) ' due to ', trim(nf90_strerror(stat))
                       lerror = .true.
                    end if
                 else
                    write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_set_global_attr_int due to error stated above'
              stop
           end if

         end subroutine mio_set_global_attr_int

! ---------------------------------------------------------------------------
         subroutine mio_set_global_attr_int_array (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
           integer, intent(in)       :: value(:)

           integer :: m, s, e, stat, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
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
                    mio_file_data(fnum)%glo_att_ival(s:e) = value

                    stat = nf90_put_att (mio_file_data(fnum)%fileid,          &
                                         nf90_global,                         &
                                         mio_file_data(fnum)%glo_att_name(m), &
                                         value)
                    if (stat .ne. 0) then
                       write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                       write (mio_logdev, *) ' due to ', trim(nf90_strerror(stat))
                       lerror = .true.
                    end if
                 else
                    write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_set_global_attr_int_array due to error stated above'
              stop
           end if

         end subroutine mio_set_global_attr_int_array

! ---------------------------------------------------------------------------
         subroutine mio_set_global_attr_real (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
           real, intent(in)          :: value

           integer :: m, loc, stat, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
              write (mio_logdev, *) ' due to file, ', trim(infile), ' does not exist'
              lerror = .true.
           else
              m = mio_search (attr_name,                              &
                              mio_file_data(fnum)%glo_att_name,       &
                              mio_file_data(fnum)%n_global_atts) 

              if (m .gt. 0) then
                 if (mio_file_data(fnum)%glo_att_type(m) .eq. nf90_float) then
                    loc = mio_file_data(fnum)%glo_att_rrange(2*m-1)
                    mio_file_data(fnum)%glo_att_rval(loc) = value

                    stat = nf90_put_att (mio_file_data(fnum)%fileid,          &
                                         nf90_global,                         &
                                         mio_file_data(fnum)%glo_att_name(m), &
                                         value)
                    if (stat .ne. 0) then
                       write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                       write (mio_logdev, *) ' due to ', trim(nf90_strerror(stat))
                       lerror = .true.
                    end if
                 else
                    write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_set_global_attr_real due to error stated above'
              stop
           end if

         end subroutine mio_set_global_attr_real

! ---------------------------------------------------------------------------
         subroutine mio_set_global_attr_real_array (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
           real, intent(in)          :: value(:)

           integer :: m, s, e, stat, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
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
                    mio_file_data(fnum)%glo_att_rval(s:e) = value

                    stat = nf90_put_att (mio_file_data(fnum)%fileid,          &
                                         nf90_global,                         &
                                         mio_file_data(fnum)%glo_att_name(m), &
                                         value)
                    if (stat .ne. 0) then
                       write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                       write (mio_logdev, *) ' due to ', trim(nf90_strerror(stat))
                       lerror = .true.
                    end if
                 else
                    write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_set_global_attr_real_array due to error stated above'
              stop
           end if

         end subroutine mio_set_global_attr_real_array

! ---------------------------------------------------------------------------
         subroutine mio_set_global_attr_real_array_subset (infile, attr_name, source_file, begin, end)

           character (*), intent(in) :: infile, source_file
           character (*), intent(in) :: attr_name
           integer, intent(in)       :: begin, end

           integer :: m, s, e, c, stat, size, fnum, sfnum
           logical :: lerror = .false.
           real, allocatable :: value(:)

           fnum  = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
              write (mio_logdev, *) ' due to file, ', trim(infile), ' does not exist'
              lerror = .true.
           else
              sfnum  = mio_search (source_file)
              if (sfnum < 0) then
                 write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to file, ', trim(source_file), ' does not exist'
                 lerror = .true.
              else

                 m = mio_search (attr_name,                              &
                                 mio_file_data(fnum)%glo_att_name,       &
                                 mio_file_data(fnum)%n_global_atts) 

                 if (m .gt. 0) then
                    if (mio_file_data(fnum)%glo_att_type(m) .eq. nf90_float) then
                       s = mio_file_data(fnum)%glo_att_rrange(2*m-1)
                       e = mio_file_data(fnum)%glo_att_rrange(2*m)

                       allocate (value(e-s+1), stat=stat)

                       stat = nf90_get_att (mio_file_data(sfnum)%fileid,          &
                                            nf90_global,                          &
                                            mio_file_data(sfnum)%glo_att_name(m), &
                                            value)

                       if (stat .ne. 0) then
                          write (mio_logdev, *) ' Failed to get global attribute ', trim(attr_name)
                          write (mio_logdev, *) ' due to ', trim(nf90_strerror(stat))
                          lerror = .true.
                       end if

                       stat = nf90_put_att (mio_file_data(fnum)%fileid,           &
                                            nf90_global,                          &
                                            mio_file_data(fnum)%glo_att_name(m),  &
                                            value(begin:end+1))

                       size = end - begin + 1

                       mio_file_data(fnum)%glo_att_rval(s:s+size) = value(begin:end+1)
                       mio_file_data(fnum)%glo_att_rrange(2*m-1)  = s
                       mio_file_data(fnum)%glo_att_rrange(2*m)    = s + size
                       
                       deallocate (value)

                       if (stat .ne. 0) then
                          write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                          write (mio_logdev, *) ' due to ', trim(nf90_strerror(stat))
                          lerror = .true.
                       end if

                    else
                       write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                       write (mio_logdev, *) ' due to mismatch global attribute data type'
                       lerror = .true.
                    end if
                 else
                    write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to non existence'
                    lerror = .true.
                 end if
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_set_global_attr_real_array due to error stated above'
              stop
           end if

         end subroutine mio_set_global_attr_real_array_subset

! ---------------------------------------------------------------------------
         subroutine mio_set_global_attr_double (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
           real*8, intent(in)        :: value

           integer :: m, loc, stat, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
              write (mio_logdev, *) ' due to file, ', trim(infile), ' does not exist'
              lerror = .true.
           else
              m = mio_search (attr_name,                              &
                              mio_file_data(fnum)%glo_att_name,       &
                              mio_file_data(fnum)%n_global_atts) 

              if (m .gt. 0) then
                 if (mio_file_data(fnum)%glo_att_type(m) .eq. nf90_double) then
                    loc = mio_file_data(fnum)%glo_att_drange(2*m-1)
                    mio_file_data(fnum)%glo_att_dval(loc) = value

                    stat = nf90_put_att (mio_file_data(fnum)%fileid,          &
                                         nf90_global,                         &
                                         mio_file_data(fnum)%glo_att_name(m), &
                                         value)
                    if (stat .ne. 0) then
                       write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                       write (mio_logdev, *) ' due to ', trim(nf90_strerror(stat))
                       lerror = .true.
                    end if
                 else
                    write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_set_global_attr_double due to error stated above'
              stop
           end if

         end subroutine mio_set_global_attr_double

! ---------------------------------------------------------------------------
         subroutine mio_set_global_attr_char (infile, attr_name, value)

           character (*), intent(in) :: infile
           character (*), intent(in) :: attr_name
           character(*), intent(in)  :: value

           integer :: m, s, e, stat, fnum
           logical :: lerror = .false.

           fnum = mio_search (infile)

           if (fnum < 0) then
              write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
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
                    mio_file_data(fnum)%glo_att_cval(s:e) = value

                    stat = nf90_put_att (mio_file_data(fnum)%fileid,          &
                                         nf90_global,                         &
                                         mio_file_data(fnum)%glo_att_name(m), &
                                         value)
                    if (stat .ne. 0) then
                       write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                       write (mio_logdev, *) ' due to ', trim(nf90_strerror(stat))
                       lerror = .true.
                    end if
                 else
                    write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                    write (mio_logdev, *) ' due to mismatch global attribute data type'
                    lerror = .true.
                 end if
              else
                 write (mio_logdev, *) ' Failed to set global attribute ', trim(attr_name)
                 write (mio_logdev, *) ' due to non existence'
                 lerror = .true.
              end if
           end if

           if (lerror) then
              write (mio_logdev, *) ' Abort in routine mio_set_global_attr_char due to error stated above'
              stop
           end if

         end subroutine mio_set_global_attr_char

       end module mio_set_global_attr_module
