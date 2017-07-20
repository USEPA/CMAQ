        module io_util_module

        use misc_util_module

        implicit none

        contains

! ----------------------------------------------------------------------------
        subroutine create_file (fname, mode, filedata, num_file, spclist, &
                                nspcs, ndim, dim_name, dim_len, var_name, &
                                var_type, var_ndims, var_dimids, var_att, &
                                unlimited, n_global_att, global_att,      &
                                sstep, tstamp, sc, ec, sr, er,            &
                                tstep, convert, wrf_nlays)

        use type_def_module
        use netcdf
        use misc_util_module
        use get_env_module

        character (len = *), intent(in)     :: fname
        integer, intent(in)                 :: mode
        type(file_record), intent(inout)    :: filedata(:)
        integer, intent(in)                 :: num_file
        integer, intent(in)                 :: spclist(:)
        integer, intent(in)                 :: nspcs, ndim, unlimited, n_global_att
        character (len = *), intent(in)     :: dim_name(:)
        integer, intent(in)                 :: dim_len(:,:)
        character (len = *), intent(in)     :: var_name(:)
        integer, intent(in)                 :: var_type(:), var_ndims(:), var_dimids(:,:)
        type(var_att_record), intent(in)    :: var_att(:)
        type(att_value_record), intent(in)  :: global_att(:)
        integer, intent(in)                 :: sstep(4)
        character (len = 19), intent(in)    :: tstamp(:,:)
        integer, intent(out)                :: sc, ec, sr, er
        integer, intent(out)                :: tstep
        integer, intent(in)                 :: convert, wrf_nlays

        real*8, parameter :: pi = .14159265358979324d0
        real,   parameter :: pi180 = real(pi / 1.8d2)

        character (len = 3), parameter :: znw_str = 'ZNW'
        real, allocatable    :: lvl_data(:)
        integer :: znw_id

        integer, parameter :: num_projection_variable = 9
        character (len = 12), parameter :: projection_variable_list(num_projection_variable)         &
                                           = (/ 'CEN_LAT',      'CEN_LON',     &
                                                'TRUELAT1',     'TRUELAT2',    &
                                                'MOAD_CEN_LAT', 'STAND_LON',   &
                                                'DX', 'DY', 'MAP_PROJ' /)
        character (len = 12), parameter :: variable_att_list(2)                &
                                           = (/ 'units',      'description' /)

        type(var_att_record), allocatable   :: template_var_att(:)
        type(att_value_record), allocatable :: template_global_att(:)

        character (len = 5000) :: v_list
        character (len = 500) :: template_file_name, cur_date_time
        character (len = ioapi_3_str_len), allocatable :: template_dim_name(:),  &
                                                          template_var_name(:)

        integer :: template_file_id, stat, template_ndim, template_nvar,         &
                   template_n_global_att, template_unlimited, year, month, date, &
                   hour, minute, second, date1, time1, date2, time2, time_diff,  &
                   loc_tstep, t, i, j, loc_nvar, nlays, dsize, &
                   time_interval, start_spc, transfer_spc,                       &
                   projection_variable_mapping(num_projection_variable),         &
                   gl_ncols, gl_nrows, variable_att_list_mapping(2),             &
                   map_projection, gl_wrf_ncols, gl_wrf_nrows

        integer, allocatable :: template_dim_len(:), template_var_type(:),       &
                                template_var_ndims(:), template_var_dimids(:,:), &
                                template_supplement_data_int (:, :),             &
                                template_spclist(:)

        real, allocatable :: template_supplement_data_real (:, :)
        real :: wrf_lc_ref_lat, met_ref_lat, dx, dy, cntrx, cntry, xorig, yorig, &
                xtemp, ytemp, met_cone_fac, fac, met_tru1, met_tru2,             &
                met_proj_clon, met_proj_clat, met_cen_lat, met_cen_lon
        real*8 :: xxx, yyy, met_p_alp_d, met_p_bet_d, met_p_gam_d
        logical :: found_lay = .false.

        call get_env (cur_date_time, 'cur_date_time', ' ')
        call get_env (wrf_lc_ref_lat, 'wrf_lc_ref_lat', 0.0)

        if (convert == 1) then
           call get_env (template_file_name, 'template', ' ')
           call open_file (template_file_name, nf90_nowrite, template_file_id)

           stat = nf90_inquire (template_file_id, template_ndim, template_nvar, &
                                template_n_global_att, template_unlimited)
        else
            template_ndim = 6
            template_nvar = 0
            template_n_global_att = 33
        end if

! retrieve dimension information
        allocate (template_dim_name(template_ndim), &
                  template_dim_len(template_ndim), stat=stat)

        if (convert == 1) then
           call retrieve_dimension_information (template_file_id, template_dim_name, &
                                                template_dim_len, template_ndim)
        else
           template_dim_name(1) = 'TSTEP'
           template_dim_len(1)  = 25
           template_dim_name(2) = 'DATE-TIME'
           template_dim_len(2)  = 2
           template_dim_name(3) = 'LAY'
           template_dim_name(4) = 'VAR'
           template_dim_name(5) = 'ROW'
           template_dim_name(6) = 'COL'
        end if

        loc_nvar = max(template_nvar, nspcs)

! retrieve variable information
        allocate (template_var_name(loc_nvar),                    &
                  template_var_type(loc_nvar),                    &
                  template_var_ndims(loc_nvar),                   &
                  template_var_dimids(template_ndim, loc_nvar),   &
                  template_var_att(loc_nvar),                     &
                  template_spclist(loc_nvar),                     &
                  stat=stat)

        if (convert == 1) then
           call retrieve_variable_information (template_file_id, template_var_name,   &
                                               template_var_type, template_var_ndims, &
                                               template_var_dimids, template_var_att, &
                                               template_nvar)
        else
           template_var_name(1) = 'TFLAG'
           template_var_type(1) = nf90_int
           template_var_ndims(1) = 3
           template_var_dimids(1, 1) = 2
           template_var_dimids(2, 1) = 4
           template_var_dimids(3, 1) = 1

           do i = 2, nspcs
              do j = 1, var_ndims(spclist(i))
                 if (dim_name(var_dimids(j,spclist(i))) == 'west_east') then
                    template_dim_len(6) = dim_len(var_dimids(j,spclist(i)),1)
                    gl_ncols = template_dim_len(6)
                    gl_wrf_ncols = dim_len(var_dimids(j,spclist(i)),1) + 1
                 else if (dim_name(var_dimids(j,spclist(i))) == 'south_north') then
                    template_dim_len(5) = dim_len(var_dimids(j,spclist(i)),1)
                    gl_nrows = template_dim_len(5)
                    gl_wrf_nrows = dim_len(var_dimids(j,spclist(i)),1) + 1
                 end if
              end do
           end do
        end if

! in wrf data, TFLAG does not have any unit or var_desc info
        template_var_att(1)%num_var_att = 3
        template_var_att(1)%var_att_record_array(1)%att_value_char = '<YYYYDDD,HHMMSS>'
        template_var_att(1)%var_att_record_array(1)%att_name = 'units'
        template_var_att(1)%var_att_record_array(1)%att_type = nf90_char
        template_var_att(1)%var_att_record_array(2)%att_value_char = 'TFLAG'
        template_var_att(1)%var_att_record_array(2)%att_name = 'long_name'
        template_var_att(1)%var_att_record_array(2)%att_type = nf90_char
        template_var_att(1)%var_att_record_array(3)%att_value_char = 'Timestep-valid flags:  (1) YYYYDDD or (2) HHMMSS'
        template_var_att(1)%var_att_record_array(3)%att_name = 'var_desc'
        template_var_att(1)%var_att_record_array(3)%att_type = nf90_char

! check variable dimension consistency, ioapi_3 file requires all variables have the same
! dimensionality and number of dimensions
        if (n_global_att == 0) then
           start_spc = 2          ! for GOES dataset
           transfer_spc = 7       ! for GOES dataset
        else
           start_spc = 3
           transfer_spc = 2
        end if

        do i = start_spc, nspcs
           if (var_ndims(spclist(2)) .ne. var_ndims(spclist(i))) then
              print *, ' Error :: number of dimensions amoung variables are not the same', &
                       var_ndims(spclist(2)), var_ndims(spclist(i))
              print *, ' Error: variable number of dimension inconsistency '
              stop
           end if

           do j = 1, var_ndims(spclist(2))
              if (var_dimids(j,spclist(2)) .ne. var_dimids(j,spclist(i))) then
                 print *, ' Error :: dimension id amoung variables are not the same', &
                       var_dimids(j,spclist(2)), var_dimids(j,spclist(i))
                 print *, ' Error: variable id inconsistency '
                 stop
              end if
           end do
        end do

        do i = 1, 2
           variable_att_list_mapping(i) = locate (variable_att_list(i),                         &
                                                  var_att(2)%var_att_record_array(:)%att_name,  &
                                                  var_att(2)%num_var_att)
        end do

! transfer variable informat from wrf to ioapi
        template_spclist(1) = 1
        do i = 2, nspcs

           t = spclist(i)

           template_spclist(i) = i
           template_var_name(i) = var_name(t)

           template_var_att(i)%num_var_att = 3

           if (convert == 1) then
              do j = 1, template_var_att(i)%num_var_att
                 template_var_att(i)%var_att_record_array(j)%att_name =     &
                   template_var_att(2)%var_att_record_array(j)%att_name
                 template_var_att(i)%var_att_record_array(j)%att_type =     &
                   template_var_att(2)%var_att_record_array(j)%att_type
              end do
           else
              template_var_att(i)%var_att_record_array(1)%att_name = 'long_name'
              template_var_att(i)%var_att_record_array(1)%att_type = nf90_char
              template_var_att(i)%var_att_record_array(2)%att_name = 'units'
              template_var_att(i)%var_att_record_array(2)%att_type = nf90_char
              template_var_att(i)%var_att_record_array(3)%att_name = 'var_desc'
              template_var_att(i)%var_att_record_array(3)%att_type = nf90_char
           end if

           template_var_att(i)%var_att_record_array(1)%att_len =         &
             len(trim(var_name(t)))
           template_var_att(i)%var_att_record_array(1)%att_value_char =  &
             trim(var_name(t))

           template_var_att(i)%var_att_record_array(2)%att_len =                           &
             var_att(t)%var_att_record_array(variable_att_list_mapping(1))%att_len
           template_var_att(i)%var_att_record_array(2)%att_value_char =                    &
             var_att(t)%var_att_record_array(variable_att_list_mapping(1))%att_value_char

           template_var_att(i)%var_att_record_array(3)%att_len =                           &
             var_att(t)%var_att_record_array(variable_att_list_mapping(2))%att_len
           template_var_att(i)%var_att_record_array(3)%att_value_char =                    &
             var_att(t)%var_att_record_array(variable_att_list_mapping(2))%att_value_char

           template_var_type(i) = var_type(t)

           dsize = min (template_ndim, size(var_dimids,1))
           template_var_dimids(1:dsize,i) = var_dimids(1:dsize,t) 

! to compensate missing layer field in wrf 2d data when it is converted to 3d ioapi_3 data
           if (var_ndims(t) .eq. 3) then
              template_var_dimids(4,i) = var_dimids(3,t) 
              template_var_dimids(3,i) = -1
           end if

           template_var_ndims(i) = 4

! adjust dimension id accordingly
           do j = 1, template_var_ndims(i)
              if (template_var_dimids(j,i) == -1) then
                    template_var_dimids(j,i) = 3
              else
                 if (dim_name(template_var_dimids(j,i)) .eq. 'west_east') then
                    template_var_dimids(j,i) = 6
                 else if (dim_name(template_var_dimids(j,i)) .eq. 'south_north') then
                    template_var_dimids(j,i) = 5
                 else if ((dim_name(template_var_dimids(j,i)) .eq. 'bottom_top') .or. &
                          (dim_name(template_var_dimids(j,i)) .eq. 'bottom_top_stag')) then
                    template_var_dimids(j,i) = 3
                 end if
              end if
           end do
        end do

! retrieve global attribute information
        allocate (template_global_att(template_n_global_att),          &
                  template_supplement_data_int(100, 100),              &
                  template_supplement_data_real(100, 100), stat=stat)

        if (convert == 1) then
           call retrieve_attribute_information (template_file_id, template_global_att, &
                                                template_n_global_att,                 & 
                                                template_supplement_data_int,          &
                                                template_supplement_data_real     )
        else

           do i = 1, num_projection_variable
              projection_variable_mapping(i) = locate (projection_variable_list(i),       &
                                                       global_att%att_name, n_global_att)
           end do

           met_cen_lat    = global_att(projection_variable_mapping(1))%att_value_float
           met_cen_lon    = global_att(projection_variable_mapping(2))%att_value_float
           met_tru1       = global_att(projection_variable_mapping(3))%att_value_float
           met_tru2       = global_att(projection_variable_mapping(4))%att_value_float
           met_proj_clat  = global_att(projection_variable_mapping(5))%att_value_float
           met_proj_clon  = global_att(projection_variable_mapping(6))%att_value_float
           map_projection = global_att(projection_variable_mapping(9))%att_value_int

           template_global_att(16)%att_name = 'GDTYP'
           template_global_att(16)%att_type = 4
           template_global_att(16)%att_len = 1
           if (map_projection == 1) then
              template_global_att(16)%att_value_int = 2
           else if (map_projection == 2) then
              template_global_att(16)%att_value_int = 6
           end if

           if (map_projection == 1) then ! Lambert conformal 
              met_p_alp_d = dble(min(met_tru1, met_tru2))
              met_p_bet_d = dble(max(met_tru1, met_tru2))
              met_p_gam_d = dble(met_proj_clon)

              if ( wrf_lc_ref_lat > 0.0) then
                met_ref_lat  = wrf_lc_ref_lat
              else
                met_ref_lat  = ( met_tru1 + met_tru2 ) * 0.5
              end if

              CALL ll2xy_lam (met_cen_lat, met_cen_lon, met_tru1, met_tru2,  &
                              met_proj_clon, met_ref_lat, xxx, yyy)

           else if (map_projection == 2) then ! polar stereographic
              met_p_alp_d  = dble(sign(1.0, met_cen_lat))   ! +/-1.0 for North/South Pole
              met_p_bet_d  = dble(met_tru1)                 ! true latitude    [degrees]
              met_p_gam_d  = dble(met_proj_clon)            ! central meridian [degrees]
              met_ref_lat  = met_proj_clat                  ! not used

              CALL ll2xy_ps (met_cen_lat, met_cen_lon, met_tru1, met_proj_clon,  &
                             xxx, yyy)

           else
              print *, ' Error: Unknown map projection'
              stop
           end if

           template_global_att(17)%att_name = 'P_ALP'
           template_global_att(17)%att_type = 6
           template_global_att(17)%att_len = 1
           template_global_att(17)%att_value_double = met_p_alp_d

           template_global_att(18)%att_name = 'P_BET'
           template_global_att(18)%att_type = 6
           template_global_att(18)%att_len = 1
           template_global_att(18)%att_value_double = met_p_bet_d

           template_global_att(19)%att_name = 'P_GAM'
           template_global_att(19)%att_type = 6
           template_global_att(19)%att_len = 1
           template_global_att(19)%att_value_double = met_p_gam_d

           template_global_att(20)%att_name = 'XCENT'
           template_global_att(20)%att_type = 6
           template_global_att(20)%att_len = 1
           template_global_att(20)%att_value_double = dble(met_proj_clon)

           template_global_att(21)%att_name = 'YCENT'
           template_global_att(21)%att_type = 6
           template_global_att(21)%att_len = 1
           template_global_att(21)%att_value_double = dble(met_ref_lat)

! figure out x and y orig
!          CALL ll2xy_lam (moad_cen_lat, cen_lon, truelat1, truelat2, stand_lon, ref_lat, xxx, yyy)

           dx = global_att(projection_variable_mapping(7))%att_value_float
           dy = global_att(projection_variable_mapping(8))%att_value_float

           cntrx = float(gl_wrf_ncols - 1) / 2.0 + 1.0
           cntry = float(gl_wrf_nrows - 1) / 2.0 + 1.0

           xorig = xxx - DBLE( cntrx - FLOAT(1) ) * DBLE(dx)
           yorig = yyy - DBLE( cntry - FLOAT(1) ) * DBLE(dy)

           IF ( wrf_lc_ref_lat > -999.0 ) THEN  ! adjust XORIG and YORIG

             xtemp = xorig / 500.0
             ytemp = yorig / 500.0
             xtemp = FLOAT(NINT(xtemp))
             ytemp = FLOAT(NINT(ytemp))
             xorig = xtemp * 500.0
             yorig = ytemp * 500.0

           end if

           template_global_att(22)%att_name = 'XORIG'
           template_global_att(22)%att_type = 6
           template_global_att(22)%att_len = 1
           template_global_att(22)%att_value_double = dble(xorig)

           template_global_att(23)%att_name = 'YORIG'
           template_global_att(23)%att_type = 6
           template_global_att(23)%att_len = 1
           template_global_att(23)%att_value_double = dble(yorig)

           template_global_att(24)%att_name = 'XCELL'
           template_global_att(24)%att_type = 6
           template_global_att(24)%att_len = 1
           template_global_att(24)%att_value_double = dble(dx)

           template_global_att(25)%att_name = 'YCELL'
           template_global_att(25)%att_type = 6
           template_global_att(25)%att_len = 1
           template_global_att(25)%att_value_double = dble(dy)

           template_global_att(12)%att_name = 'NCOLS'
           template_global_att(12)%att_type = 4
           template_global_att(12)%att_len = 1
           template_global_att(12)%att_value_int = gl_ncols

           template_global_att(13)%att_name = 'NROWS'
           template_global_att(13)%att_type = 4
           template_global_att(13)%att_len = 1
           template_global_att(13)%att_value_int = gl_nrows

           template_global_att(14)%att_name = 'NLAYS'
           template_global_att(14)%att_type = 4
           template_global_att(14)%att_len = 1
           template_global_att(14)%att_value_int = 1

           template_global_att(15)%att_name = 'NVARS'
           template_global_att(15)%att_type = 4
           template_global_att(15)%att_len = 1
           template_global_att(15)%att_value_int = nspcs

           template_global_att(8)%att_name = 'SDATE'
           template_global_att(8)%att_type = 4
           template_global_att(8)%att_len = 1
           template_global_att(8)%att_value_int = 1988001

           template_global_att(9)%att_name = 'STIME'
           template_global_att(9)%att_type = 4
           template_global_att(9)%att_len = 1
           template_global_att(9)%att_value_int = 0

           template_global_att(3)%att_name = 'FTYPE'
           template_global_att(3)%att_type = 4
           template_global_att(3)%att_len = 1
           template_global_att(3)%att_value_int = 1

           template_global_att(11)%att_name = 'NTHIK'
           template_global_att(11)%att_type = 4
           template_global_att(11)%att_len = 1
           template_global_att(11)%att_value_int = 1

           template_global_att(10)%att_name = 'TSTEP'
           template_global_att(10)%att_type = 4
           template_global_att(10)%att_len = 1

           template_global_att(29)%att_name = 'GDNAM'
           template_global_att(29)%att_type = nf90_char
           template_global_att(29)%att_len = 3
           template_global_att(29)%att_value_char = 'WRF'

           template_global_att(4)%att_name = 'CDATE'
           template_global_att(4)%att_type = 4
           template_global_att(4)%att_len = 1
           template_global_att(4)%att_value_int = 2000001

           template_global_att(5)%att_name = 'CTIME'
           template_global_att(5)%att_type = 4
           template_global_att(5)%att_len = 1
           template_global_att(5)%att_value_int = 0

           template_global_att(6)%att_name = 'WDATE'
           template_global_att(6)%att_type = 4
           template_global_att(6)%att_len = 1
           template_global_att(6)%att_value_int = 2001001

           template_global_att(7)%att_name = 'WTIME'
           template_global_att(7)%att_type = 4
           template_global_att(7)%att_len = 1
           template_global_att(7)%att_value_int = 0

           template_global_att(30)%att_name = 'UPNAM'
           template_global_att(30)%att_type = nf90_char
           template_global_att(30)%att_len = 3
           template_global_att(30)%att_value_char = 'WRF'

           template_global_att(32)%att_name = 'FILEDESC'
           template_global_att(32)%att_type = nf90_char
           template_global_att(32)%att_len = 3
           template_global_att(32)%att_value_char = 'WRF'

           template_global_att(33)%att_name = 'HISTORY'
           template_global_att(33)%att_type = nf90_char
           template_global_att(33)%att_len = 1
           template_global_att(33)%att_value_char = ''

           template_global_att(1)%att_name = 'IOAPI_VERSION'
           template_global_att(1)%att_type = nf90_char
           template_global_att(1)%att_len = 5
           template_global_att(1)%att_value_char = '3.4.1'

           t = 1
           do i = 2, nspcs
              j = spclist(i)
              v_list(t:t+15) = var_name(j)
              t = t + 16
           end do

           template_global_att(31)%att_name = 'VAR-LIST'
           template_global_att(31)%att_type = nf90_char
           template_global_att(31)%att_len = 1
           template_global_att(31)%att_value_char = v_list

           template_global_att(2)%att_name = 'EXEC_ID'
           template_global_att(2)%att_type = nf90_char
           template_global_att(2)%att_len = 1
           template_global_att(2)%att_value_char = "???"

           template_global_att(26)%att_name = 'VGTYP'
           template_global_att(26)%att_type = 4
           template_global_att(26)%att_len = 1
           template_global_att(26)%att_value_int = 7

           template_global_att(27)%att_name = 'VGTOP'
           template_global_att(27)%att_type = nf90_float
           template_global_att(27)%att_len = 1
           template_global_att(27)%att_value_float = 5000.0

           template_global_att(28)%att_name = 'VGLVLS'
           template_global_att(28)%att_type = nf90_float
           template_global_att(28)%att_len = 2

        end if

! various adjustment
        template_dim_len(4) = nspcs                                               ! nvars
        template_global_att(15)%att_value_int = nspcs - 1                         ! nvars

        template_dim_len(1) = dim_len(1,1)                                        ! number of time steps

        call compute_date_time (tstamp(sstep(1), sstep(2)), date1, time1)

        template_global_att(08)%att_value_int = date1                          ! sdate
        template_global_att(09)%att_value_int = time1                          ! stime

! figure out tstep
        if (template_dim_len(1) .gt. 1) then
           if (sstep(1) .eq. size(tstamp,1)) then
              call compute_date_time (tstamp(1, sstep(2)+1), date2, time2)
           else
              call compute_date_time (tstamp(sstep(1)+1, sstep(2)), date2, time2)
           end if

           time_diff = date_time_diff(date1, time1, date2, time2)
           hour   = time_diff / 3600
           minute = mod(time_diff,3600) / 60
           second = mod(time_diff, 60)

           loc_tstep = hour * 10000 + minute * 100 + second
        else
           if ((date1 == 0) .and. (time1 == 0)) then
              loc_tstep = 0        ! set it to 0 to indicate a time-independent file
           else
              loc_tstep = 10000    ! set default be 10000 when there is one time step
           end if
        end if
 
        template_global_att(10)%att_value_int = loc_tstep                             ! tstep
        tstep = loc_tstep
        call compute_date_time (cur_date_time, date1, time1)

        template_global_att(4)%att_value_int = date1                              ! cdate
        template_global_att(5)%att_value_int = time1                              ! ctime
        template_global_att(6)%att_value_int = date1                              ! wdate
        template_global_att(7)%att_value_int = time1                              ! wtime

! to determine the particular wrf variable is a 2D or 3D one
        do j = 1, var_ndims(spclist(2))
           if ((dim_name(var_dimids(j,spclist(2))) .eq. 'bottom_top') .or. &
               (dim_name(var_dimids(j,spclist(2))) .eq. 'bottom_top_stag')) then
              found_lay = .true.
              nlays = dim_len(var_dimids(j,spclist(2)),1)
           end if
        end do

        if (found_lay) then
           template_global_att(14)%att_value_int = nlays                          ! nlays
           template_dim_len(3) = template_global_att(14)%att_value_int            ! nlays
           template_global_att(28)%att_len = wrf_nlays+1                          ! adjust ioapi vglvls
        else
           template_global_att(14)%att_value_int = 1                              ! nlays
           template_dim_len(3) = 1                                                ! nlays
           template_global_att(28)%att_len = 2                                    ! adjust ioapi vglvls
        end if

        template_global_att(30)%att_value_char = 'dpp'                            ! upnam
        template_global_att(32)%att_value_char = 'conversion from other netCDF format to ioapi_3' ! filedesc

        i = 1
        template_global_att(31)%att_len = 0
        do j = 2, nspcs
           template_global_att(31)%att_value_char(i:i+15) = var_name(spclist(j))                      ! var-list
           i = i + ioapi_3_str_len
           template_global_att(31)%att_len = template_global_att(31)%att_len + ioapi_3_str_len
        end do

        stat = nf90_create (fname, mode, filedata(num_file)%fileid)
        if (stat .ne. nf90_noerr) then
           print *, ' Error creating file ', trim(fname)
           stop
        end if

! check whether variable ZNW exist in wrf data
        stat = nf90_inq_varid (filedata(1)%fileid, znw_str, znw_id)
        if (stat == 0) then
           allocate (lvl_data(wrf_nlays+1), stat=stat)
           stat = nf90_get_var(filedata(1)%fileid, znw_id, lvl_data,   &
                               start = (/ 1 /), count = (/ wrf_nlays+1 /))

        end if

! define dimensions
        call define_dimensions (filedata(num_file)%fileid, template_dim_name, &
                                template_dim_len, template_ndim,              &
                                template_var_dimids, nspcs, unlimited)

! define variables
        call define_variables (filedata(num_file)%fileid, template_var_name,  &
                               template_var_type, template_var_dimids,        &
                               template_var_ndims, nspcs, template_spclist)

! define variable attributes

        call define_variable_attributes (filedata(num_file)%fileid,           &
                                         template_var_name, template_var_att, &
                                         nspcs, template_spclist, convert)

! define global attributes

        call define_global_attributes (filedata(num_file)%fileid,     &
                                       template_global_att,           &
                                       template_n_global_att, nspcs,  &
                                       template_supplement_data_int,  & 
                                       template_supplement_data_real, &
                                       lvl_data)

! end definition

        stat = nf90_enddef (filedata(num_file)%fileid)

! define starting and ending column and row of the ioapi domain
        sc = 1
        ec = template_dim_len(6)
        sr = 1
        er = template_dim_len(5)

        deallocate (template_dim_name, template_dim_len,     &
                    template_var_name, template_var_type,    &
                    template_var_ndims, template_var_dimids, &
                    template_var_att, template_global_att,   &
                    template_supplement_data_int,            &
                    template_supplement_data_real )

        end subroutine create_file

! ----------------------------------------------------------------------------
        integer function locate (search_string, list, n_list)

        use netcdf

        character (len = 12), intent(in)            :: search_string
        character (len = nf90_max_name), intent(in) :: list(:)
        integer, intent(in)                         :: n_list

        logical :: found
        integer :: n

        n = 0
        found = .false.
        do while ((.not. found) .and. (n < n_list))
          n = n + 1
          if (search_string == list(n)) then
             found = .true.
          end if
        end do

        if (.not. found) then
           print *, ' Error: missing in global att for ', trim(search_string)
           stop
        else
           locate = n
        end if

        end function locate

! ----------------------------------------------------------------------------
        subroutine open_file (fname, mode, fid)

        use netcdf

        character (len = *), intent(in) :: fname
        integer, intent(in)             :: mode
        integer, intent(out)            :: fid

        integer :: stat

        stat = nf90_open (fname, mode, fid)
        if (stat .ne. nf90_noerr) then
           print *, ' Error opening file ', trim(fname)
           stop
        end if

        end subroutine open_file

! ----------------------------------------------------------------------------
        subroutine define_dimensions (fileid, dim_name, dim_len, ndim, &
                                      var_dimids, nspcs, unlimited)

        use netcdf

        integer, intent(in)             :: fileid, ndim, unlimited, nspcs
        character (len = *), intent(in) :: dim_name(:)
        integer, intent(in)             :: dim_len(:), var_dimids(:,:)

        integer :: stat, n, dim_id

        do n = 1, ndim
           if (n .eq. unlimited) then
              stat = nf90_def_dim (fileid, dim_name(n), nf90_unlimited, dim_id)
           else
              if (n .eq. var_dimids(2,1)) then
                 stat = nf90_def_dim (fileid, dim_name(n), nspcs-1, dim_id)
              else
                 stat = nf90_def_dim (fileid, dim_name(n), dim_len(n), dim_id)
              end if
           end if
        end do

        end subroutine define_dimensions

! ----------------------------------------------------------------------------
        subroutine define_variables (fileid, var_name, var_type, &
                                     var_dimids, var_ndims,      &
                                     nspcs, spclist)

        use netcdf

        integer, intent(in)              :: fileid, nspcs
        character (len = *), intent(in)  :: var_name(:)
        integer, intent(in)              :: var_type(:), var_ndims(:), &
                                            var_dimids(:,:), spclist(:)

        integer :: s, t, stat, var_id

        do s = 1, nspcs
           t = spclist(s)
           stat = nf90_def_var (fileid, var_name(t), var_type(t), &
                                var_dimids(1:var_ndims(t),t), var_id)
        end do

        end subroutine define_variables

! ----------------------------------------------------------------------------
        subroutine define_variable_attributes (fileid, var_name,        &
                                               var_att, nspcs, spclist, convert)

        use type_def_module
        use netcdf

        integer, intent(in)              :: fileid, nspcs
        character (len = *), intent(in)  :: var_name(:)
        type(var_att_record), intent(in) :: var_att(:)
        integer, intent(in)              :: spclist(:)
        integer, intent(in), optional    :: convert

        integer :: s, n, t, stat, var_id

        do s = 1, nspcs
           n = spclist(s)

           do t = 1, var_att(n)%num_var_att
              stat = nf90_inq_varid (fileid, var_name(n), var_id)

              if (var_att(n)%var_att_record_array(t)%att_type .eq. nf90_char) then
                 if (present(convert)) then
                    if (convert == 1) then
                       stat = nf90_put_att (fileid, var_id,                                        &
                                            trim(var_att(n)%var_att_record_array(t)%att_name),     &
                                            trim(var_att(n)%var_att_record_array(t)%att_value_char))
                    else
                       stat = nf90_put_att (fileid, var_id,                                        &
                                            trim(var_att(n)%var_att_record_array(t)%att_name),     &
                                            var_att(n)%var_att_record_array(t)%att_value_char(1:16))
                    end if
                 else
                    stat = nf90_put_att (fileid, var_id,                                        &
                                         trim(var_att(n)%var_att_record_array(t)%att_name),     &
                                         trim(var_att(n)%var_att_record_array(t)%att_value_char))
                 end if

              else if (var_att(n)%var_att_record_array(t)%att_type .eq. nf90_int) then
                 stat = nf90_put_att (fileid, var_id,                                        &
                                      var_att(n)%var_att_record_array(t)%att_name,           &
                                      var_att(n)%var_att_record_array(t)%att_value_int)
              else if (var_att(n)%var_att_record_array(t)%att_type .eq. nf90_float) then
                 stat = nf90_put_att (fileid, var_id,                                        &
                                      var_att(n)%var_att_record_array(t)%att_name,           &
                                      var_att(n)%var_att_record_array(t)%att_value_float)
              end if
           end do
        end do

        end subroutine define_variable_attributes

! ----------------------------------------------------------------------------
        subroutine define_global_attributes (fileid, global_att, n_global_att, &
                                             nspcs, supplement_data_int,       &
                                             supplement_data_real, lvl_data)

        use type_def_module
        use netcdf

        integer, intent(in)                 :: fileid, n_global_att, nspcs
        type(att_value_record), intent(out) :: global_att(:)
        integer, intent(in)                 :: supplement_data_int (:, :)
        real, intent(in)                    :: supplement_data_real (:, :)
        real, optional, intent(in)          :: lvl_data (:)

        integer :: n, stat
        integer :: loc_num_sup_int = 0, loc_num_sup_real = 0

        do n = 1, n_global_att

           if (global_att(n)%att_type .eq. nf90_char) then

              stat = nf90_put_att (fileid, nf90_global, global_att(n)%att_name, &
                                   global_att(n)%att_value_char(1:global_att(n)%att_len))

           else if (global_att(n)%att_type .eq. nf90_int) then

              if (global_att(n)%att_len .gt. 1) then
                 loc_num_sup_int = global_att(n)%att_value_int_link
                 stat = nf90_put_att (fileid, nf90_global, global_att(n)%att_name,                 &
                                      supplement_data_int(1:global_att(n)%att_len,loc_num_sup_int))
              else if (global_att(n)%att_name .eq. "NVARS") then
                 stat = nf90_put_att (fileid, nf90_global, global_att(n)%att_name, nspcs-1)
              else
                 stat = nf90_put_att (fileid, nf90_global,                                         &
                                      global_att(n)%att_name, global_att(n)%att_value_int)
              end if

           else if (global_att(n)%att_type .eq. nf90_float) then

                 if (global_att(n)%att_len .gt. 1) then
                    loc_num_sup_real = global_att(n)%att_value_float_link
                    stat = nf90_put_att (fileid, nf90_global, global_att(n)%att_name,                 &
                                         supplement_data_real(1:global_att(n)%att_len,loc_num_sup_real))
                 else
                    stat = nf90_put_att (fileid, nf90_global, &
                                         global_att(n)%att_name, global_att(n)%att_value_float)
                 end if

           else if (global_att(n)%att_type .eq. nf90_double) then

              stat = nf90_put_att (fileid, nf90_global, &
                                   global_att(n)%att_name, global_att(n)%att_value_double)

           end if
        end do

        end subroutine define_global_attributes

! ----------------------------------------------------------------------------

SUBROUTINE ll2xy_ps (phi, lambda, phi1, lambda0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Polar Stereographic Projection
! Purpose:  Calcluates (X,Y) from origin for a given latitude-longitude pair
!           and polar stereographic projection information.
! Notes:    Adapted from equations found at http://starbase.jpl.nasa.gov/
!           mgn-v-rdrs-5-dvdr-v1.0/gvdr0001/catalog/dsmp.lbl.
! Revised:  28 Sep 2009  Original version.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  integer, parameter :: rearth = 6370000.0    ! [ m ]

  REAL(8)                      :: deg2rad ! convert degrees to radians
  REAL(8)                      :: drearth ! earth radius [m]
  REAL(8)                      :: hemi    ! +/-1 for Northern/Southern Hemis
  REAL,          INTENT(IN)    :: lambda  ! longitude [deg]
  REAL,          INTENT(IN)    :: lambda0 ! standard longitude [deg]
  REAL,          INTENT(IN)    :: phi     ! latitude [deg]
  REAL(8)                      :: phirad  ! latitude [rad]
  REAL,          INTENT(IN)    :: phi1    ! true latitude 1 [deg]
  REAL(8)                      :: phi1rad ! true latitude 1 [rad]
  REAL(8)                      :: pi
  REAL(8)                      :: piover4 ! pi/4
  REAL(8)                      :: scalefac
  REAL(8)                      :: sigma   ! image scale
  REAL(8)                      :: theta   ! polar angle
  REAL(8)                      :: tt
  REAL*8,          INTENT(OUT) :: xx      ! X-coordinate from origin
  REAL*8,          INTENT(OUT) :: yy      ! Y-coordinate from origin

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = DATAN(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

  drearth = DBLE(rearth)

!-------------------------------------------------------------------------------
! Compute image scale, SIGMA.
!-------------------------------------------------------------------------------

  hemi = DSIGN (1.0d0, DBLE(phi1))

  phi1rad = DBLE(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phirad  = DBLE(phi)  * deg2rad  ! convert PHI  from degrees to radians

!!!TLO  sigma   = (1.0d0 + DSIN(phi1rad)) / (1.0d0 + DSIN(pi))  ! at pole
  sigma   = (1.0d0 + DSIN(phi1rad)) / 2.0d0 * hemi

  scalefac = drearth / sigma

  tt = DTAN ( piover4 - phirad/2.0d0)

!-------------------------------------------------------------------------------
! Compute polar angle, THETA.
!-------------------------------------------------------------------------------

  theta = DBLE(lambda - lambda0) * deg2rad

!-------------------------------------------------------------------------------
! Compute Cartesian coordinates, XX and YY.
!-------------------------------------------------------------------------------

! xx = REAL(         2.0d0 * scalefac * tt * DSIN(theta) )
! yy = REAL( -hemi * 2.0d0 * scalefac * tt * DCOS(theta) )
  xx =         2.0d0 * scalefac * tt * DSIN(theta)
  yy = -hemi * 2.0d0 * scalefac * tt * DCOS(theta)

END SUBROUTINE ll2xy_ps

! ----------------------------------------------------------------------------
subroutine ll2xy_lam (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Lambert Conformal Projection
! Purpose:  Determines secant or tangent Lambert conformal case, and calls
!           appropriate routine.
! Revised:  03 Jun 2008  Original version.  (T. Otte)
!           26 Nov 2008  Added argument for reference latitude, PHI0.
!                        Prevent users from having tangent Lambert conformal
!                        case until it can be tested with the Spatial
!                        Allocator.  (Known problem is that the Spatial
!                        Allocator does not work properly when the
!                        reference latitude is equal to the first true
!                        latitude.  Work-around is to set reference latitude
!                        to average of true latitudes for Lambert conformal.
!                        But average of true latiudes for tangent Lambert
!                        conformal case is the first true latitude, which
!                        will result in the same problem as solution used
!                        in MCIPv3.4.)  (T. Otte)
!-------------------------------------------------------------------------------

  implicit none

  real,          intent(in)    :: lambda  ! longitude [deg]
  real,          intent(in)    :: lambda0 ! standard longitude [deg]
  real,          intent(in)    :: phi     ! latitude [deg]
  real,          intent(in)    :: phi0    ! reference latitude [deg]
  real,          intent(in)    :: phi1    ! true latitude 1 [deg]
  real,          intent(in)    :: phi2    ! true latitude 2 [deg]
  real(8),       intent(out)   :: xx      ! X-coordinate from origin
  real(8),       intent(out)   :: yy      ! Y-coordinate from origin

  real,          parameter     :: phitol  = 0.001  ! tolerance [deg]

  character*16,  parameter     :: pname   = 'LL2XY_LAM'

!-------------------------------------------------------------------------------
! Determine whether Lambert conformal is tangent or secant.
!-------------------------------------------------------------------------------

  if ( abs( phi1 - phi2 ) < phitol ) then  ! tangent case
    write (6,9000) phi1, phi2
    print *, 'Error: Lambert conformal is tangent '
    stop
  else  ! secant case
    call ll2xy_lam_sec (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)
  endif

  return

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 format (/, 1x, 70('*'),                                                  &
              /, 1x, '*** SUBROUTINE: LL2XY_LAM',                              &
              /, 1x, '***   TANGENT LAMBERT CONFORMAL PROJECTION DETECTED',    &
              /, 1x, '***   TRUE LATITUDES = ', f8.3, 2x, f8.3,                &
              /, 1x, '***   MAY NOT WORK PROPERLY IN SPATIAL ALLOCATOR',       &
              /, 1x, '***   ...PLEASE SUBMIT BUGZILLA TICKET TO INVESTIGATE',  &
              /, 1x, 70('*'))

end subroutine ll2xy_lam

!-------------------------------------------------------------------------------

subroutine ll2xy_lam_sec (phi, lambda, phi1, phi2, lambda0, phi0, xx, yy)

!-------------------------------------------------------------------------------
! Name:     Latitude-Longitude to (X,Y) for Lambert Conformal Projection
! Purpose:  Calcluates (X,Y) from origin for a given latitude-longitude pair
!           and Lambert conformal projection information for secant case.
! Notes:    Equations taken from "Map Projections: Theory and Applications"
!           by Frederick Pearson, II (1990), pp. 181-182.
! Revised:  03 Jun 2008  Original version.  (T. Otte)
!           04 Dec 2008  Added argument for reference latitude, PHI0.
!                        Changed routine so it is no longer hard-wired to
!                        have a reference latitude at the first true
!                        latitude.  (T. Otte and J. Pleim)
!           17 Sep 2009  Corrected inline comments associated with definitions
!                        of RHO and RHO0.  Corrected calculation of PSI (with
!                        no impact on results).  (T. Otte)
!-------------------------------------------------------------------------------

  implicit none

  real,          intent(in)    :: lambda  ! longitude [deg]
  real,          intent(in)    :: lambda0 ! standard longitude [deg]
  real,          intent(in)    :: phi     ! latitude [deg]
  real,          intent(in)    :: phi0    ! reference latitude [deg]
  real,          intent(in)    :: phi1    ! true latitude 1 [deg]
  real,          intent(in)    :: phi2    ! true latitude 2 [deg]
  real(8),       intent(out)   :: xx      ! X-coordinate from origin
  real(8),       intent(out)   :: yy      ! Y-coordinate from origin

  real, parameter              :: rearth = 6370000.0  ! [m]

  real(8)                      :: deg2rad ! convert degrees to radians
  real(8)                      :: dlambda ! delta lambda
  real(8)                      :: drearth ! double-precision radius of earth [m]
  real(8)                      :: phirad  ! latitude [rad]
  real(8)                      :: phi0rad ! reference latitude [rad]
  real(8)                      :: phi1rad ! true latitude 1 [rad]
  real(8)                      :: phi2rad ! true latitude 2 [rad]
  real(8)                      :: pi
  real(8)                      :: piover4 ! pi/4
  real(8)                      :: psi     ! auxiliary function
  real(8)                      :: rho     ! polar radius to latitude phi
  real(8)                      :: rho0    ! polar radius to origin
  real(8)                      :: term
  real(8)                      :: term0
  real(8)                      :: term1
  real(8)                      :: term2
  real(8)                      :: theta   ! polar angle
  real(8)                      :: sinphi0 ! cone constant

!-------------------------------------------------------------------------------
! Compute constants.
!-------------------------------------------------------------------------------

  piover4 = datan(1.0d0)
  pi      = 4.0d0 * piover4
  deg2rad = pi / 1.8d2

  drearth = dble(rearth)

!-------------------------------------------------------------------------------
! Compute cone constant, SINPHI0.
! Note:  PHI0 is the reference latitude, which is user-defined.  It is NOT
!        used in the calculation of SINPHI0, which is the cone constant.
!-------------------------------------------------------------------------------

  phi0rad = dble(phi0) * deg2rad  ! convert PHI0 from degrees to radians
  phi1rad = dble(phi1) * deg2rad  ! convert PHI1 from degrees to radians
  phi2rad = dble(phi2) * deg2rad  ! convert PHI2 from degrees to radians

  term0 = dtan (piover4 - phi0rad/2.0d0)
  term1 = dtan (piover4 - phi1rad/2.0d0)
  term2 = dtan (piover4 - phi2rad/2.0d0)

  sinphi0 = dlog ( dcos(phi1rad) / dcos(phi2rad) )
  sinphi0 = sinphi0 / dlog (term1 / term2)

!-------------------------------------------------------------------------------
! Compute polar angle, THETA.
!-------------------------------------------------------------------------------

  dlambda = dble(lambda - lambda0) * deg2rad
  theta   = dlambda * sinphi0

!-------------------------------------------------------------------------------
! Compute polar radius to origin, RHO0, where origin is at PHI0.
!-------------------------------------------------------------------------------

  psi  = drearth * dcos(phi1rad) / sinphi0 / (term1**sinphi0)
  rho0 = psi * (term0**sinphi0)

!-------------------------------------------------------------------------------
! Compute polar radius to latitude PHI, RHO.
!-------------------------------------------------------------------------------

  phirad = dble(phi) * deg2rad  ! convert PHI from degrees to radians
  term   = dtan (piover4 - phirad/2.0d0)
  rho    = psi * (term**sinphi0)

!-------------------------------------------------------------------------------
! Compute Cartesian coordinates, XX and YY.
!-------------------------------------------------------------------------------

  xx =        rho * dsin(theta)
  yy = rho0 - rho * dcos(theta)

end subroutine ll2xy_lam_sec

        end module io_util_module
