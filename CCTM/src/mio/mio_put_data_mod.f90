! Purpose: Write data to a file

      module mio_put_data_module

        use mio_global_data_module, only: mio_logdev
        use netcdf

        implicit none

        interface mio_put_data
          module procedure mio_put_data_0d_real,     &  ! a single real value
                           mio_put_data_1d_real,     &  ! a 1D real array
                           mio_put_data_2d_real,     &  ! a 2D real array
                           mio_put_data_3d_real,     &  ! a 3D real array
                           mio_put_data_4d_real,     &  ! a 4D real array
                           mio_put_data_0d_double,   &  ! a double precision value
                           mio_put_data_1d_double,   &  ! a 1D double precision array
                           mio_put_data_2d_double,   &  ! a 2D double precision array
                           mio_put_data_3d_double,   &  ! a 3D double precision array
                           mio_put_data_4d_double,   &  ! a 4D double precision array
                           mio_put_data_0d_int,      &  ! an integer value
                           mio_put_data_1d_int,      &  ! a 1D integer array
                           mio_put_data_2d_int,      &  ! a 2D integer array
                           mio_put_data_3d_int,      &  ! a 3D integer array
                           mio_put_data_char            ! a character type variable
        end interface

        contains

! --------------------------------------------------------------------------------------------
        function mio_put_data_0d_real (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real, intent(in)    :: data
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_0d_real due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_0d_real

! --------------------------------------------------------------------------------------------
        function mio_put_data_1d_real (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real, intent(in)    :: data(:)
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

!       write (mio_logdev, '(a18, 20i8)') ' ==d== put 1d b ', mystart, mycount, stat
!       write (mio_logdev, '(a18, 10e15.8)') ' ==d== put 1d c ', minval(data), maxval(data), sum(data)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_1d_real due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_1d_real

! --------------------------------------------------------------------------------------------
        function mio_put_data_2d_real (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real, intent(in)    :: data(:,:)
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_2d_real due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_2d_real

! --------------------------------------------------------------------------------------------
        function mio_put_data_3d_real (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real, intent(in)    :: data(:,:,:)
          logical :: success

          integer :: stat

!    write (6, *) ' ==d== put 3d a ', fid, vid

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             stat = nf90_sync(fid)
             if (stat .eq. 0) then
                success = .true.
             else
                write (mio_logdev, *) ' Error in routine mio_put_data_3d_real due to ', trim(nf90_strerror(stat))
                success = .false.
             end if
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_3d_real due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_3d_real

! --------------------------------------------------------------------------------------------
        function mio_put_data_4d_real (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real, intent(in)    :: data(:,:,:,:)
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_4d_real due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_4d_real

! --------------------------------------------------------------------------------------------
        function mio_put_data_0d_double (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real*8, intent(in)  :: data
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_0d_double due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_0d_double

! --------------------------------------------------------------------------------------------
        function mio_put_data_1d_double (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real*8, intent(in)  :: data(:)
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_1d_double due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_1d_double

! --------------------------------------------------------------------------------------------
        function mio_put_data_2d_double (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real*8, intent(in)  :: data(:,:)
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_2d_double due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_2d_double

! --------------------------------------------------------------------------------------------
        function mio_put_data_3d_double (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real*8, intent(in)  :: data(:,:,:)
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             stat = nf90_sync(fid)
             if (stat .eq. 0) then
                success = .true.
             else
                write (mio_logdev, *) ' Error in routine mio_put_data_3d_double due to ', trim(nf90_strerror(stat))
                success = .false.
             end if
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_3d_double due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_3d_double

! --------------------------------------------------------------------------------------------
        function mio_put_data_4d_double (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          real*8, intent(in)  :: data(:,:,:,:)
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_4d_double due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_4d_double

! --------------------------------------------------------------------------------------------
        function mio_put_data_0d_int (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          integer, intent(in) :: data
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_0d_int due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_0d_int

! --------------------------------------------------------------------------------------------
        function mio_put_data_1d_int (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          integer, intent(in) :: data(:)
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_1d_int due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_1d_int

! --------------------------------------------------------------------------------------------
        function mio_put_data_2d_int (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          integer, intent(in) :: data(:,:)
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_2d_int due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_2d_int

! --------------------------------------------------------------------------------------------
        function mio_put_data_3d_int (fid, vid, mystart, mycount, data) result (success)

          integer, intent(in) :: fid, vid, mystart(5), mycount(5)
          integer, intent(in) :: data(:,:,:)
          logical :: success

          integer :: stat

          stat = nf90_put_var(fid,              &
                              vid,              &
                              data,             &
                              start = mystart,  &
                              count = mycount)

          if (stat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_3d_int due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_3d_int

! --------------------------------------------------------------------------------------------
        function mio_put_data_char (fid, vid, mystart, mycount, data) result(success)

          integer, intent(in)      :: fid, vid, mystart(5), mycount(5)
          character(*), intent(in) :: data
          logical :: success

          integer :: n, stat, tstat

          tstat = 0
          do n = 1, mystart(1)
             stat = nf90_put_var(fid, vid,           &
                                 data(n:n),          &
                                 start=mystart,      &
                                 count=mycount)
             tstat = tstat + stat
          end do

          if (tstat .eq. 0) then
             success = .true.
          else
             write (mio_logdev, *) ' Error in routine mio_put_data_char due to ', trim(nf90_strerror(stat))
             success = .false.
          end if

        end function mio_put_data_char

      end module mio_put_data_module
