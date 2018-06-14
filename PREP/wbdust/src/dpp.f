! ndim         - number of dimensions (1st one is number of time steps)
! nvar         - number of variables
! n_global_att - number of global attributes
! unlimited    - id of the unlimited dimension (-1 means none)
! var_name     - variable name
! var_type     - variable type (1 - byte, 2 - char, 3 - short, 4 - int, 5 - float, 6 - double)
! var_ndim     - number of dimensions of each variable
! dim_name     - dimension name
! dim_len      - size of each dimension in dim_name
! var_dimids   - variable dimension's id w.r.t. dim_name
! filedata     - information which includes attribute, about each file

! ntsteps      - number of time steps in each file
! spclist      - subset of variables
! ounit        - out text file unit number
! n, t, tt     - loop index
! stat         - error status
! nspcs        - number of variables
! skip         - skip, for the dif and app cases, a pair of file is used so skip = 2
! ss, es       - starting and ending time step of a particular file
! out_tstep    - output time step
! num_file     - number of input files
! sstep, estep - starting and ending time step in input files
!                contains the time stamp index (1) and file number (2) base of first file
!                contains the time stamp index (3) and file number (4) base of second file
!                this address a case that time t1 in file one could be n steps and in file 
!                two it could be m steps
! num_supplement_data_int  - number of supplemental integer attribute
! num_supplement_data_real - number of supplemental float attribute

! oname        - output file name
! tstamp       - time stamp
! old_tstamp   - previous time stamp (this is for to omit the duplicate time step)
! option       - type of option (default is subset, other options: dif, ddump)

! spc          - a subset of variable is used
! arm-stime    - ARM's data start time
! arm-sdate    - ARM's data start date
! mode         - file output mode
! ncd_64bit_offset - ncd_64bit_offset flag

        program dpp

        use netcdf
        use type_def_module
        use data_module
        use misc_util_module
        use extract_util_module
        use process_util_module
        use io_util_module
        use get_env_module

        implicit none

        character (len = max_len) :: oname
        character (len = 19), allocatable :: tstamp(:,:)
        character (len = 19)              :: old_tstamp = ' '
        character (len = 26) :: infile1
        integer :: arm_sdate, arm_stime, mode, convert

        type(file_record), allocatable :: filedata(:)

        integer, allocatable :: ntsteps(:), spclist(:)
        integer :: ounit, n, t, tt, stat, nspcs,  &
                   skip, ss, es, tstep,        &
                   out_tstep, num_file,  &
                   sstep(4), estep(4), t2,              & 
                   num_supplement_data_int  = 0,        &
                   num_supplement_data_real = 0,        &
                   sc, ec, sr, er,     &
                   global_x_dim,        &
                   global_y_dim, wrf_x_dim, wrf_y_dim,  &
                   cur_date, cur_time, pre_date,        &
                   pre_time, num_missing,               &
                   temp_diff

        logical :: spc, different_starting_time, &
                   ncd_64bit_offset

! ***********************************************************************        

        call get_env (ncd_64bit_offset, 'ncd_64bit_offset', .false.)
        call get_env (convert, 'convert', 0)
        call get_env (spc, 'spc', .false.)
        call get_env (infile1, 'infile1', ' ')

        num_file = 2
        skip = 1

        allocate (filedata(num_file), ntsteps(num_file),                                    &
                  supplement_data_int(100, 100), supplement_data_real(100, 100), stat=stat)

        filedata(1)%filename = infile1
        call open_file (filedata(1)%filename, nf90_nowrite, filedata(1)%fileid)
        filedata(1)%io_type = 'i'

        stat = nf90_inquire (filedata(1)%fileid, ndim, nvar, n_global_att, unlimited)

! retrieve dimension information
        allocate (dim_name(ndim), dim_len(ndim, num_file), stat=stat)
        call retrieve_dimension_information (filedata(1)%fileid, dim_name, dim_len(:,1), ndim)

        if (dim_name(1) == 'TSTEP') then  ! ioapi_3 data set
           global_x_dim = dim_len(6,1)
           global_y_dim = dim_len(5,1)
        else if (dim_name(1) == 'Time') then  ! wrf data set
           do n = 1, ndim
              if (dim_name(n) == 'south_north') then
                 wrf_y_dim = n
              else if (dim_name(n) == 'west_east') then
                 wrf_x_dim = n
              end if
           end do
 
           global_x_dim = dim_len(wrf_x_dim,1)
           global_y_dim = dim_len(wrf_y_dim,1)
        end if

! retrieve variable information
        allocate (var_name(nvar), var_type(nvar), var_ndims(nvar),  &
                  var_dimids(ndim, nvar), var_att(nvar), stat=stat)

        call retrieve_variable_information (filedata(1)%fileid, var_name, var_type, &
                                            var_ndims, var_dimids, var_att, nvar)

! retrieve attribute information
        allocate (global_att(n_global_att), stat=stat)

        call retrieve_attribute_information (filedata(1)%fileid, global_att, n_global_att, &
                                             supplement_data_int, supplement_data_real)

        allocate (spclist(nvar), stat=stat)
        call ext_subset_list (filedata, spc, var_name, nvar, spclist, nspcs)

        ntsteps = 0
        wrf_nlays = 0
! obtain number of time steps info in each file
        do n = num_file - 1, 1, -1
           do t = 1, ndim
              stat = nf90_inquire_dimension (filedata(n)%fileid, t, dim_name(t), dim_len(t,n))
              if (trim(dim_name(t)) == 'bottom_top') then
                 wrf_nlays = dim_len(t,n)
              end if
           end do
              ntsteps(n) = dim_len(1,n)
        end do

! get time stamp information
        allocate (tstamp(maxval(ntsteps), num_file), stat=stat)
        call extract_wrf_time_stamp (filedata, num_file-1, skip, ntsteps,  &
                                     dim_len(2,1), tstamp, sstep, estep,   &
                                     different_starting_time)

        call get_env (oname, 'ofile', ' ')

        filedata(num_file)%filename = oname


        if (ncd_64bit_offset) then
           mode = ior (nf90_noclobber, nf90_64bit_offset)
        else
           mode = nf90_noclobber
        end if

        call create_file (oname, mode, filedata, num_file,              &
                          spclist, nspcs, ndim, dim_name, dim_len,      &
                          var_name, var_type, var_ndims, var_dimids,    &
                          var_att, unlimited, n_global_att, global_att, &
                          sstep, tstamp, sc, ec, sr, er,      &
                          tstep, convert, wrf_nlays)

        filedata(num_file)%io_type = 'o'

        out_tstep = 0
        do n = sstep(2), estep(2), skip
           call determine_time_step (sstep, estep, n, ntsteps(n), ss, es)

           do t = ss, es
              if (old_tstamp .ne. tstamp(t, n)) then
                 num_missing = 0
                 out_tstep = out_tstep + 1
                 num_missing = 0

                 call process_data (filedata(n)%fileid, filedata(num_file)%fileid, &
                                    unlimited, spclist, nspcs, var_name, var_type, &
                                    var_ndims, var_dimids, dim_len(:,n), dim_name, &
                                    tstamp(t, n), t, out_tstep,            &
                                    sc, ec, sr, er )
              end if
              old_tstamp = tstamp(t, n)
           end do
        end do

! close files
        do n = 1, num_file
           stat = nf90_close (filedata(n)%fileid)
        end do

        deallocate (filedata, ntsteps, dim_name, dim_len,      &
                    var_name, var_type, var_ndims, var_dimids, &
                    var_att, global_att, spclist, tstamp,      &
                    supplement_data_int, supplement_data_real  )

        end program dpp
