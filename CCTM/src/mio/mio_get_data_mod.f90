! Purpose: Retrieving data from a file

      module mio_get_data_module

        use mio_global_data_module, only: mio_logdev
        use netcdf

        implicit none

        interface mio_get_data
          module procedure mio_get_data_0d_real,     &  ! retrieve a real value
                           mio_get_data_1d_real,     &  ! retrieve a 1D real array
                           mio_get_data_2d_real,     &  ! retrieve a 2D real array
                           mio_get_data_3d_real,     &  ! retrieve a 3D real array
                           mio_get_data_4d_real,     &  ! retrieve a 4D real array
                           mio_get_data_0d_double,   &  ! retrieve a double precision variable
                           mio_get_data_1d_double,   &  ! retrieve a 1D double precision array
                           mio_get_data_2d_double,   &  ! retrieve a 2D double precision array
                           mio_get_data_3d_double,   &  ! retrieve a 3D double precision array
                           mio_get_data_4d_double,   &  ! retrieve a 4D double precision array
                           mio_get_data_0d_int,      &  ! retrieve an integer variable
                           mio_get_data_1d_int,      &  ! retrieve a 1D integer array
                           mio_get_data_2d_int,      &  ! retrieve a 2D integer array
                           mio_get_data_3d_int,      &  ! retrieve a 1D integer array
                           mio_get_data_char            ! retrieve a character type variable
        end interface

        contains

! --------------------------------------------------------------------------------------------
        function mio_get_data_0d_real (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real, intent(out)   :: data
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_0d_real'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_0d_real

! --------------------------------------------------------------------------------------------
        function mio_get_data_1d_real (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real, intent(out)   :: data(:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

!       write (mio_logdev, '(a20, 15i8, 20e15.8)') ' ==d== get data 1d a ', mystart, mycount, stat
!       write (mio_logdev, '(a20, 20e15.8)') ' ==d== get data 1d b ', minval(data), maxval(data), sum(data)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_1d_real'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_1d_real

! --------------------------------------------------------------------------------------------
        function mio_get_data_2d_real (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real, intent(out)   :: data(:,:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_2d_real'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_2d_real

! --------------------------------------------------------------------------------------------
        function mio_get_data_3d_real (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real, intent(out)   :: data(:,:,:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_3d_real'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_3d_real

! --------------------------------------------------------------------------------------------
        function mio_get_data_4d_real (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real, intent(out)   :: data(:,:,:,:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_4d_real'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_4d_real

! --------------------------------------------------------------------------------------------
        function mio_get_data_0d_double (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real*8, intent(out) :: data
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_0d_double'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_0d_double

! --------------------------------------------------------------------------------------------
        function mio_get_data_1d_double (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real*8, intent(out) :: data(:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_1d_double'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_1d_double

! --------------------------------------------------------------------------------------------
        function mio_get_data_2d_double (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real*8, intent(out) :: data(:,:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_2d_double'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_2d_double

! --------------------------------------------------------------------------------------------
        function mio_get_data_3d_double (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real*8, intent(out) :: data(:,:,:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_3d_double'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_3d_double

! --------------------------------------------------------------------------------------------
        function mio_get_data_4d_double (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real*8, intent(out) :: data(:,:,:,:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_4d_double'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_4d_double

! --------------------------------------------------------------------------------------------
        function mio_get_data_0d_int (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in)  :: fid, vid, mystart(5), mycount(5)
          integer, intent(out) :: data
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_0d_int'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_0d_int

! --------------------------------------------------------------------------------------------
        function mio_get_data_1d_int (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in)  :: fid, vid, mystart(5), mycount(5)
          integer, intent(out) :: data(:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_1d_int'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_1d_int

! --------------------------------------------------------------------------------------------
        function mio_get_data_2d_int (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in)  :: fid, vid, mystart(5), mycount(5)
          integer, intent(out) :: data(:,:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_2d_int'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_2d_int

! --------------------------------------------------------------------------------------------
        function mio_get_data_3d_int (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in)  :: fid, vid, mystart(5), mycount(5)
          integer, intent(out) :: data(:,:,:)
          logical :: success

          integer :: stat

          stat = nf90_get_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_3d_int'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_3d_int

! --------------------------------------------------------------------------------------------
        function mio_get_data_char (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in)       :: fid, vid, mystart(5), mycount(5)
          character(*), intent(out) :: data
          logical :: success

          integer :: n, stat, tstat

          tstat = 0
          do n = 1, mystart(1)
             stat = nf90_get_var(fid, vid,           &
                                 data(n:n),          &
                                 start=mystart,      &
                                 count=mycount)
             tstat = tstat + stat
          end do

          if (tstat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_get_data_char'
             write (mio_logdev, *) '       due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_get_data_char

      end module mio_get_data_module
