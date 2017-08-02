	module extract_util_module

        use data_module

        implicit none

        contains

! ----------------------------------------------------------------------------
        subroutine ext_subset_list (filedata, spc, var_name, nvar, &
                                    spclist, nspcs)

        use get_env_module

        type(file_record), intent(inout) :: filedata(:)
        logical, intent(in)              :: spc
        character (len = *), intent(in)  :: var_name(:)
        integer, intent(in)              :: nvar
        integer, intent(inout)           :: spclist(:), nspcs

        character (len = 2560) :: list
        integer :: m, n, var_list_index, stat, tnspcs, loc_len
        character (len = 200) :: tspclist(200), buf
        logical :: done, found

        if (.not. spc) then
           nspcs = nvar
           do n = 1, nvar
              spclist(n) = n
           end do
        else
           spclist = -1

           var_list_index = 0
           do n = 1, n_global_att
              if ((global_att(n)%att_type .eq. nf90_char) .and.  &
                  (global_att(n)%att_name .eq. 'VAR-LIST')) then
                  var_list_index = n
                  global_att(var_list_index)%att_value_char = ''
              end if
           end do

           call get_env (list, 'spc_list', ' ')

! the first variable is always time and will keep it
           tnspcs = 1
           tspclist(tnspcs) = var_name(1)

           done = .false.
           m = len(trim(list))
           n = 0
           loc_len = 0
           do while (.not. done)
              read (list(1+n:m), *, iostat=stat) buf
              if (stat .ne. 0) then
                 done = .true.
              else
                 tnspcs = tnspcs + 1
                 tspclist(tnspcs) = buf

                 if (var_list_index .gt. 0) then      ! this is for ioapi_3 file var-list
                    loc_len = loc_len + ioapi_3_str_len    
                    global_att(var_list_index)%att_len = loc_len
                    global_att(var_list_index)%att_value_char(loc_len-15:loc_len) = tspclist(tnspcs)
                 end if

                 n = len(trim(buf))+1+n
                 if (n .gt. m) then
                    done = .true.
                 end if
              end if
           end do

           m = 0
           do while (m .lt. tnspcs)
              m = m + 1
              buf = tspclist(m)
              found = .false.
              n = 0
              do while ((.not. found) .and. (n .lt. nvar))
                 n = n + 1
                 if (trim(buf) .eq. trim(var_name(n))) then
                    found = .true.
                    spclist(m) = n
                 end if
              end do
              if (.not. found) then
                 print *, ' '
                 print *, ' Invalid species: ', buf
                 print *, ' Program terminates' 
                 print *, ' '
                 stop 
              end if
           end do
           nspcs = tnspcs
        end if

        end subroutine ext_subset_list

! ----------------------------------------------------------------------------
        subroutine extract_wrf_time_stamp (filedata, num_file, skip, ntsteps, &
                                           dim, tstamp,    &
                                           sstep, estep,                      &
                                           different_starting_time, flag)

        use netcdf

        type(file_record), intent(in)     :: filedata(:)
        integer, intent(in)               :: num_file, skip, dim, ntsteps(:)
        character (len = 19), intent(out) :: tstamp(:,:)
        integer, intent(out)              :: sstep(4), estep(4)
        integer, intent(in), optional     :: flag
        logical, intent(out)              :: different_starting_time

        character (len = 50) :: loc_str1, loc_str2
        integer :: n, t, i, stat, loc_sstep(2), loc_estep(2), loc_flag, time_variable
        character :: char

        if (present(flag)) then
           loc_flag = flag
        else
           loc_flag = 0
        end if

        if (n_global_att == 0) then
           time_variable = 2
        else
           time_variable = 1
        end if

        do n = 1, num_file, skip
           do t = 1, ntsteps(n)
              do i = 1, dim
                 stat = nf90_get_var(filedata(n)%fileid, time_variable, char, &
                                     start = (/ i, t /), count = (/ 1, 1 /))
                 loc_str1(i:i) = char
              end do
              tstamp(t,n) = loc_str1(1:19)

              if (present(flag)) then     !  reading in 2nd file's time stamp for dif operation 
                 if (      (ntsteps(n) .ne. ntsteps(n+1))  &
                     .and. (.not. different_starting_time)) then
                    print *, ' Error: Number of time stamps are different in the files:'
                    print *, '        file1 - ', ntsteps(n)
                    print *, '        file2 - ', ntsteps(n+1)
                    stop
                 end if

                 do i = 1, dim
                    stat = nf90_get_var(filedata(n+1)%fileid, 1, char, &
                                        start = (/ i, t /), count = (/ 1, 1 /))
                    loc_str2(i:i) = char
                 end do

                 tstamp(t,n+1) = loc_str2(1:19)

                 if (tstamp(t,n) .ne. tstamp(t,n+1)) then
                    print *, ' Error: Time stamps are different in the files:'
                    print *, '        file1 - ', trim(loc_str1)
                    print *, '        file2 - ', trim(loc_str2)
                    stop
                 end if
              end if
           end do
        end do

! check subset time step 
        sstep(1) = 1
        sstep(2) = 1
        estep(1) = ntsteps(1)
        estep(2) = 1

        different_starting_time = (tstamp(1,1) .ne. tstamp(1,2))

        end subroutine extract_wrf_time_stamp

	end module extract_util_module
