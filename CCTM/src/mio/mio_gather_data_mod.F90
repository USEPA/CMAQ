! Purpose: In the parallel implementation, data might need to assemble
!          before sending to output file

      module mio_gather_data_module

        use mio_global_data_module, only : mio_mype, mio_mype_p1,       &
                                           mio_nprocs, mio_logdev,      &
                                           mio_mpas_dmap,               &
                                           mio_parallelism
        use mio_parameter_module, only : mio_mpas_format, mio_serial

        implicit none

        integer, parameter :: tag1 = 101
        integer, parameter :: tag2 = 114

        interface mio_gather_data
          module procedure mio_gather_data_1d_real,      &
                           mio_gather_data_2d_real,      &
                           mio_gather_data_3d_real,      &
                           mio_gather_data_1d_double,    &
                           mio_gather_data_2d_double,    &
                           mio_gather_data_3d_double,    &
                           mio_gather_data_1d_int,       & 
                           mio_gather_data_2d_int,       & 
                           mio_gather_data_3d_int
        end interface

        contains

! --------------------------------------------------------------------------------------------
        subroutine mio_gather_data_1d_real (in_data, file_format, decomp, out_data)

          real, intent(in)    :: in_data(:)
          integer, intent(in) :: file_format
          logical, intent(in) :: decomp
          real, intent(out)   :: out_data(:)

#ifdef parallel
          include 'mpif.h'
          integer :: status(mpi_status_size)
#endif

          character(23), parameter :: pname = 'mio_gather_data_1d_real'

          integer :: i, pe, dsize, stat, loc_tag2
          real, allocatable :: recv_buf(:)
          logical :: lerror

          lerror = .false.

! this is for MPAS data
          if (file_format .eq. mio_mpas_format) then
             if (decomp .and. (mio_parallelism .ne. mio_serial)) then
                if (mio_mype .eq. 0) then
                   do i = 1, mio_mpas_dmap(0, mio_mype)
                      out_data(mio_mpas_dmap(i, mio_mype)) = in_data(i)
                   end do

#ifdef parallel
                   do pe = 1, mio_nprocs-1
                      dsize = mio_mpas_dmap(0, pe)
                      allocate (recv_buf(dsize), stat=stat)

                      loc_tag2 = tag2 + pe * 3
                      call mpi_recv (recv_buf, dsize, mpi_real, pe,          &
                                     loc_tag2, mpi_comm_world, status, stat)

                      if (stat .ne. 0) then
                         write (mio_logdev, *) ' Error: In routine ' // pname
                         write (mio_logdev, *) '        MPI error, not able to receive data'
                         lerror = .true.
                      end if

                      do i = 1, dsize
                         out_data(mio_mpas_dmap(i, pe)) = recv_buf(i)
                      end do
                      deallocate (recv_buf)
                   end do
                else
                   dsize = mio_mpas_dmap(0, mio_mype)
                   loc_tag2 = tag2 + mio_mype * 3
                   call mpi_send (in_data, dsize, mpi_real, 0,    &
                                  loc_tag2, mpi_comm_world, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error, not able to send data'
                      lerror = .true.
                   end if
#endif
                end if
             else
                out_data = in_data
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Abort in routine ' // pname // ' due to an error'
             stop
          end if

        end subroutine mio_gather_data_1d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_gather_data_2d_real (in_data, file_format, gtype,   &
                                            colde_pe, rowde_pe,            &
                                            ncols_pe, nrows_pe, out_data)

          real, intent(in)      :: in_data(:,:)
          character, intent(in) :: gtype
          integer, intent(in)   :: file_format
          integer, intent(in)   :: colde_pe(:, :, :)
          integer, intent(in)   :: rowde_pe(:, :, :)
          integer, intent(in)   :: ncols_pe(:, :)
          integer, intent(in)   :: nrows_pe(:, :)
          real, intent(out)     :: out_data(:,:)

#ifdef parallel
          include 'mpif.h'
          integer :: status(mpi_status_size)
#endif

          character(23), parameter :: pname = 'mio_gather_data_2d_real'

          integer :: stat, col_s, col_e, row_s, row_e, nc, nr, pe,     &
                     who, who_p1, send_size, recv_size, r, c, loc,     &
                     pos, dsize, i, first_dim, second_dim,             &
                     loc_tag1, loc_tag2
          real, allocatable :: recv_buf(:), recv_buf_mpas(:,:)
          logical :: lerror

          lerror = .false.

          if (file_format .eq. mio_mpas_format) then
             first_dim  = size(in_data, 1)
             if (mio_mype .eq. 0) then
                do i = 1, mio_mpas_dmap(0, mio_mype)
                   out_data(:, mio_mpas_dmap(i, mio_mype)) = in_data(:, i)
                end do

#ifdef parallel
                do pe = 1, mio_nprocs-1
                   second_dim = mio_mpas_dmap(0, pe)
                   dsize = first_dim * second_dim
                   allocate (recv_buf_mpas(first_dim, second_dim), stat=stat)

                   loc_tag2 = tag2 + pe * 3
                   call mpi_recv (recv_buf_mpas, dsize, mpi_real, pe,     &
                                  loc_tag2, mpi_comm_world, status, stat)
                   do i = 1, second_dim
                      out_data(:, mio_mpas_dmap(i, pe)) = recv_buf_mpas(:, i)
                   end do
                   deallocate (recv_buf_mpas)
                end do
             else
                second_dim = mio_mpas_dmap(0, mio_mype)
                dsize = first_dim * second_dim

                loc_tag2 = tag2 + mio_mype * 3
                call mpi_send (in_data(:,1:second_dim), dsize, mpi_real, 0,     &
                               loc_tag2, mpi_comm_world, stat)
#endif
             end if
          else
             if (gtype .eq. 'c') then
                pos = 1
             else if (gtype .eq. 'd') then
                pos = 2
             else
                write (mio_logdev, *) ' Error: In routine ' // pname
                write (mio_logdev, *) '        incorrect data type, only accept c or d'
                lerror = .true.
             end if

             if (.not. lerror) then
                if (mio_mype .eq. 0) then          
! for PE 0
                   col_s = colde_pe(1, mio_mype_p1, pos)
                   col_e = colde_pe(2, mio_mype_p1, pos)
                   row_s = rowde_pe(1, mio_mype_p1, pos)
                   row_e = rowde_pe(2, mio_mype_p1, pos)

                   out_data(col_s:col_e, row_s:row_e) = in_data

#ifdef parallel
! for other PEs
                   allocate (recv_buf(size(in_data,1)*size(in_data,2)), stat=stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        not able to allocate recv_buf'
                      lerror = .true.
                   end if

                   do pe = 1, mio_nprocs-1
                      loc_tag1 = tag1 + pe * 3
                      call mpi_recv (who, 1, mpi_integer, mpi_any_source,    &
                                     loc_tag1, mpi_comm_world, status, stat)

                      if (stat .ne. 0) then
                         write (mio_logdev, *) ' Error: In routine ' // pname
                         write (mio_logdev, *) '        MPI error receiving processor id WHO'
                         lerror = .true.
                      end if

                      who_p1 = who + 1
                      col_s = colde_pe(1, who_p1, pos)
                      col_e = colde_pe(2, who_p1, pos)
                      row_s = rowde_pe(1, who_p1, pos)
                      row_e = rowde_pe(2, who_p1, pos)
                      nc    = ncols_pe(who_p1, pos)
                      nr    = nrows_pe(who_p1, pos)
                      recv_size = nc * nr

                      loc_tag2 = tag2 + pe * 3
                      call mpi_recv (recv_buf, recv_size, mpi_real, who,     &
                                     loc_tag2, mpi_comm_world, status, stat)
                      if (stat .ne. 0) then
                         write (mio_logdev, *) ' Error: In routine ' // pname
                         write (mio_logdev, *) '        MPI error receiving recv_buf'
                         lerror = .true.
                      end if

                      loc = 0
                      do r = row_s, row_e
                         do c = col_s, col_e
                            loc = loc + 1
                            out_data(c, r) = recv_buf(loc)
                         end do
                      end do
                   end do
                else
                   loc_tag1 = tag1 + mio_mype * 3
                   call mpi_send (mio_mype, 1, mpi_integer, 0,    &
                                  loc_tag1, mpi_comm_world, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error sending WHO info'
                      lerror = .true.
                   end if

                   nc = ncols_pe(mio_mype_p1, pos)
                   nr = nrows_pe(mio_mype_p1, pos)
                   send_size = nc * nr

                   loc_tag2 = tag2 + mio_mype * 3
                   call mpi_send (in_data, send_size, mpi_real, 0, &
                                  loc_tag2, mpi_comm_world, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error sending in_data'
                      lerror = .true.
                   end if
#endif
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Abort in routine ' // pname // ' due to an error'
             stop
          end if

          if ((mio_mype .eq. 0) .and. allocated(recv_buf)) then
             deallocate (recv_buf)
          end if

        end subroutine mio_gather_data_2d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_gather_data_3d_real (in_data, gtype,               &
                                            colde_pe, rowde_pe,           &
                                            ncols_pe, nrows_pe, out_data)

          real, intent(in)      :: in_data(:,:,:)
          character, intent(in) :: gtype
          integer, intent(in)   :: colde_pe(:, :, :)
          integer, intent(in)   :: rowde_pe(:, :, :)
          integer, intent(in)   :: ncols_pe(:, :)
          integer, intent(in)   :: nrows_pe(:, :)
          real, intent(out)     :: out_data(:,:,:)

#ifdef parallel
          include 'mpif.h'
          integer :: status(mpi_status_size)
#endif

          character(23), parameter :: pname = 'mio_gather_data_3d_real'

          integer :: stat, col_s, col_e, row_s, row_e, nc, nr, third_dim,  &
                     pe, who, who_p1, send_size, recv_size, k, r, c, loc,  &
                     pos, loc_tag1, loc_tag2
          real, allocatable :: recv_buf(:)
          logical :: lerror

          lerror = .false.

          if (gtype .eq. 'c') then
             pos = 1
          else if (gtype .eq. 'd') then
             pos = 2
          else
             write (mio_logdev, *) ' Error: In routine ' // pname
             write (mio_logdev, *) '        incorrect data type, only accept c or d'
             lerror = .true.
          end if

          if (.not. lerror) then
             third_dim = size(in_data, 3)

             if (mio_mype .eq. 0) then          
! for PE 0
                col_s = colde_pe(1, mio_mype_p1, pos)
                col_e = colde_pe(2, mio_mype_p1, pos)
                row_s = rowde_pe(1, mio_mype_p1, pos)
                row_e = rowde_pe(2, mio_mype_p1, pos)

                out_data(col_s:col_e, row_s:row_e, :) = in_data

#ifdef parallel
! for other PEs
                allocate (recv_buf(size(in_data,1)*size(in_data,2)*size(in_data,3)), stat=stat)
                if (stat .ne. 0) then
                   write (mio_logdev, *) ' Error: In routine ' // pname
                   write (mio_logdev, *) '        not able to allocate recv_buf'
                   lerror = .true.
                end if

                do pe = 1, mio_nprocs-1
                   loc_tag1 = tag1 + pe * 3
                   call mpi_recv (who, 1, mpi_integer, mpi_any_source,    &
                                  loc_tag1, mpi_comm_world, status, stat)

                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error receiving processor id WHO'
                      lerror = .true.
                   end if

                   who_p1 = who + 1

                   col_s = colde_pe(1, who_p1, pos)
                   col_e = colde_pe(2, who_p1, pos)
                   row_s = rowde_pe(1, who_p1, pos)
                   row_e = rowde_pe(2, who_p1, pos)
                   nc    = ncols_pe(who_p1, pos)
                   nr    = nrows_pe(who_p1, pos)
                   recv_size = nc * nr * third_dim

                   loc_tag2 = tag2 + pe * 3
                   call mpi_recv (recv_buf, recv_size, mpi_real, who,     &
                                  loc_tag2, mpi_comm_world, status, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error receiving recv_buf'
                      lerror = .true.
                   end if

                   loc = 0
                   do k = 1, third_dim
                      do r = row_s, row_e
                         do c = col_s, col_e
                            loc = loc + 1
                            out_data(c, r, k) = recv_buf(loc)
                         end do
                      end do
                   end do

                end do
             else
                loc_tag1 = tag1 + mio_mype * 3
                call mpi_send (mio_mype, 1, mpi_integer, 0,     &
                               loc_tag1, mpi_comm_world, stat)
                if (stat .ne. 0) then
                   write (mio_logdev, *) ' Error: In routine ' // pname
                   write (mio_logdev, *) '        MPI error sending WHO info'
                   lerror = .true.
                end if

                nc = ncols_pe(mio_mype_p1, pos)
                nr = nrows_pe(mio_mype_p1, pos)
                send_size = nc * nr * third_dim

                loc_tag2 = tag2 + mio_mype * 3
                call mpi_send (in_data, send_size, mpi_real, 0, &
                               loc_tag2, mpi_comm_world, stat)
                if (stat .ne. 0) then
                   write (mio_logdev, *) ' Error: In routine ' // pname
                   write (mio_logdev, *) '        MPI error sending in_data'
                   lerror = .true.
                end if
#endif
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Abort in routine ' // pname // ' due to an error'
             stop
          end if

          if ((mio_mype .eq. 0) .and. allocated(recv_buf)) then
             deallocate (recv_buf)
          end if

        end subroutine mio_gather_data_3d_real

! --------------------------------------------------------------------------------------------
        subroutine mio_gather_data_1d_double (in_data, file_format, decomp, out_data)

          real*8, intent(in)  :: in_data(:)
          integer, intent(in) :: file_format
          logical, intent(in) :: decomp
          real*8, intent(out) :: out_data(:)

#ifdef parallel
          include 'mpif.h'
          integer :: status(mpi_status_size)
#endif

          character(25), parameter :: pname = 'mio_gather_data_1d_double'

          integer :: i, pe, dsize, stat, loc_tag2
          real*8, allocatable :: recv_buf(:)
          logical :: lerror

          lerror = .false.

! this is for MPAS data
          if (file_format .eq. mio_mpas_format) then
             if (decomp .and. (mio_parallelism .ne. mio_serial)) then
                if (mio_mype .eq. 0) then
                   do i = 1, mio_mpas_dmap(0, mio_mype)
                      out_data(mio_mpas_dmap(i, mio_mype)) = in_data(i)
                   end do

#ifdef parallel
                   do pe = 1, mio_nprocs-1
                      dsize = mio_mpas_dmap(0, pe)
                      allocate (recv_buf(dsize), stat=stat)

                      loc_tag2 = tag2 + pe * 3
                      call mpi_recv (recv_buf, dsize, mpi_double, pe,        &
                                     loc_tag2, mpi_comm_world, status, stat)

                      if (stat .ne. 0) then
                         write (mio_logdev, *) ' Error: In routine ' // pname
                         write (mio_logdev, *) '        MPI error, not able to receive data'
                         lerror = .true.
                      end if

                      do i = 1, dsize
                         out_data(mio_mpas_dmap(i, pe)) = recv_buf(i)
                      end do
                      deallocate (recv_buf)
                   end do
                else
                   dsize = mio_mpas_dmap(0, mio_mype)
                   loc_tag2 = tag2 + mio_mype * 3
                   call mpi_send (in_data, dsize, mpi_double, 0,  &
                                  loc_tag2, mpi_comm_world, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error, not able to send data'
                      lerror = .true.
                   end if
#endif
                end if
             else
                out_data = in_data
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Abort in routine ' // pname // ' due to an error'
             stop
          end if

        end subroutine mio_gather_data_1d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_gather_data_2d_double (in_data, file_format, gtype,   &
                                              colde_pe, rowde_pe,            &
                                              ncols_pe, nrows_pe, out_data)

          real*8, intent(in)    :: in_data(:,:)
          character, intent(in) :: gtype
          integer, intent(in)   :: file_format
          integer, intent(in)   :: colde_pe(:, :, :)
          integer, intent(in)   :: rowde_pe(:, :, :)
          integer, intent(in)   :: ncols_pe(:, :)
          integer, intent(in)   :: nrows_pe(:, :)
          real*8, intent(out)   :: out_data(:,:)

#ifdef parallel
          include 'mpif.h'
          integer :: status(mpi_status_size)
#endif

          character(25), parameter :: pname = 'mio_gather_data_2d_double'

          integer :: stat, col_s, col_e, row_s, row_e, nc, nr, pe,     &
                     who, who_p1, send_size, recv_size, r, c, loc,     &
                     pos, dsize, i, first_dim, second_dim,             &
                     loc_tag1, loc_tag2
          real*8, allocatable :: recv_buf(:), recv_buf_mpas(:,:)
          logical :: lerror

          lerror = .false.

          if (file_format .eq. mio_mpas_format) then
             first_dim  = size(in_data, 1)
             if (mio_mype .eq. 0) then
                do i = 1, mio_mpas_dmap(0, mio_mype)
                   out_data(:, mio_mpas_dmap(i, mio_mype)) = in_data(:, i)
                end do

#ifdef parallel
                do pe = 1, mio_nprocs-1
                   second_dim = mio_mpas_dmap(0, pe)
                   dsize = first_dim * second_dim
                   allocate (recv_buf_mpas(first_dim, second_dim), stat=stat)

                   loc_tag2 = tag2 + pe * 3
                   call mpi_recv (recv_buf_mpas, dsize, mpi_real, pe,     &
                                  loc_tag2, mpi_comm_world, status, stat)
                   do i = 1, second_dim
                      out_data(:, mio_mpas_dmap(i, pe)) = recv_buf_mpas(:, i)
                   end do
                   deallocate (recv_buf_mpas)
                end do
             else
                second_dim = mio_mpas_dmap(0, mio_mype)
                dsize = first_dim * second_dim

                loc_tag2 = tag2 + mio_mype * 3
                call mpi_send (in_data(:,1:second_dim), dsize, mpi_real, 0,     &
                               loc_tag2, mpi_comm_world, stat)
#endif
             end if
          else
             if (gtype .eq. 'c') then
                pos = 1
             else if (gtype .eq. 'd') then
                pos = 2
             else
                write (mio_logdev, *) ' Error: In routine ' // pname
                write (mio_logdev, *) '        incorrect data type, only accept c or d'
                lerror = .true.
             end if

             if (.not. lerror) then
                if (mio_mype .eq. 0) then          
! for PE 0
                   col_s = colde_pe(1, mio_mype_p1, pos)
                   col_e = colde_pe(2, mio_mype_p1, pos)
                   row_s = rowde_pe(1, mio_mype_p1, pos)
                   row_e = rowde_pe(2, mio_mype_p1, pos)

                   out_data(col_s:col_e, row_s:row_e) = in_data

#ifdef parallel
! for other PEs
                   allocate (recv_buf(size(in_data,1)*size(in_data,2)), stat=stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        not able to allocate recv_buf'
                      lerror = .true.
                   end if

                   do pe = 1, mio_nprocs-1
                      loc_tag1 = tag1 + pe * 3
                      call mpi_recv (who, 1, mpi_integer, mpi_any_source,    &
                                     loc_tag1, mpi_comm_world, status, stat)

                      if (stat .ne. 0) then
                         write (mio_logdev, *) ' Error: In routine ' // pname
                         write (mio_logdev, *) '        MPI error receiving processor id WHO'
                         lerror = .true.
                      end if

                      who_p1 = who + 1
                      col_s = colde_pe(1, who_p1, pos)
                      col_e = colde_pe(2, who_p1, pos)
                      row_s = rowde_pe(1, who_p1, pos)
                      row_e = rowde_pe(2, who_p1, pos)
                      nc    = ncols_pe(who_p1, pos)
                      nr    = nrows_pe(who_p1, pos)
                      recv_size = nc * nr

                      loc_tag2 = tag2 + pe * 3
                      call mpi_recv (recv_buf, recv_size, mpi_double, who,   &
                                     loc_tag2, mpi_comm_world, status, stat)
                      if (stat .ne. 0) then
                         write (mio_logdev, *) ' Error: In routine ' // pname
                         write (mio_logdev, *) '        MPI error receiving recv_buf'
                         lerror = .true.
                      end if

                      loc = 0
                      do r = row_s, row_e
                         do c = col_s, col_e
                            loc = loc + 1
                            out_data(c, r) = recv_buf(loc)
                         end do
                      end do
                   end do
                else
                   loc_tag1 = tag1 + mio_mype * 3
                   call mpi_send (mio_mype, 1, mpi_integer, 0,    &
                                  loc_tag1, mpi_comm_world, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error sending WHO info'
                      lerror = .true.
                   end if

                   nc = ncols_pe(mio_mype_p1, pos)
                   nr = nrows_pe(mio_mype_p1, pos)
                   send_size = nc * nr

                   loc_tag2 = tag2 + mio_mype * 3
                   call mpi_send (in_data, send_size, mpi_double, 0, &
                                  loc_tag2, mpi_comm_world, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error sending in_data'
                      lerror = .true.
                   end if
#endif
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Abort in routine ' // pname // ' due to an error'
             stop
          end if

          if ((mio_mype .eq. 0) .and. allocated(recv_buf)) then
             deallocate (recv_buf)
          end if

        end subroutine mio_gather_data_2d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_gather_data_3d_double (in_data, gtype,                &
                                              colde_pe, rowde_pe,            &
                                              ncols_pe, nrows_pe, out_data)

          real*8, intent(in)    :: in_data(:,:,:)
          character, intent(in) :: gtype
          integer, intent(in)   :: colde_pe(:, :, :)
          integer, intent(in)   :: rowde_pe(:, :, :)
          integer, intent(in)   :: ncols_pe(:, :)
          integer, intent(in)   :: nrows_pe(:, :)
          real*8, intent(out)   :: out_data(:,:,:)

#ifdef parallel
          include 'mpif.h'
          integer :: status(mpi_status_size)
#endif

          character(25), parameter :: pname = 'mio_gather_data_3d_double'

          integer :: stat, col_s, col_e, row_s, row_e, nc, nr, third_dim,  &
                     pe, who, who_p1, send_size, recv_size, k, r, c, loc,  &
                     pos, loc_tag1, loc_tag2
          real*8, allocatable :: recv_buf(:)
          logical :: lerror

          lerror = .false.

          if (gtype .eq. 'c') then
             pos = 1
          else if (gtype .eq. 'd') then
             pos = 2
          else
             write (mio_logdev, *) ' Error: In routine ' // pname
             write (mio_logdev, *) '        incorrect data type, only accept c or d'
             lerror = .true.
          end if

          if (.not. lerror) then
             third_dim = size(in_data, 3)

             if (mio_mype .eq. 0) then          
! for PE 0
                col_s = colde_pe(1, mio_mype_p1, pos)
                col_e = colde_pe(2, mio_mype_p1, pos)
                row_s = rowde_pe(1, mio_mype_p1, pos)
                row_e = rowde_pe(2, mio_mype_p1, pos)

                out_data(col_s:col_e, row_s:row_e, :) = in_data

#ifdef parallel
! for other PEs
                allocate (recv_buf(size(in_data,1)*size(in_data,2)*size(in_data,3)), stat=stat)
                if (stat .ne. 0) then
                   write (mio_logdev, *) ' Error: In routine ' // pname
                   write (mio_logdev, *) '        not able to allocate recv_buf'
                   lerror = .true.
                end if

                do pe = 1, mio_nprocs-1
                   loc_tag1 = tag1 + pe * 3
                   call mpi_recv (who, 1, mpi_integer, mpi_any_source,    &
                                  loc_tag1, mpi_comm_world, status, stat)

                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error receiving processor id WHO'
                      lerror = .true.
                   end if

                   who_p1 = who + 1

                   col_s = colde_pe(1, who_p1, pos)
                   col_e = colde_pe(2, who_p1, pos)
                   row_s = rowde_pe(1, who_p1, pos)
                   row_e = rowde_pe(2, who_p1, pos)
                   nc    = ncols_pe(who_p1, pos)
                   nr    = nrows_pe(who_p1, pos)
                   recv_size = nc * nr * third_dim

                   loc_tag2 = tag2 + pe * 3
                   call mpi_recv (recv_buf, recv_size, mpi_double, who,   &
                                  loc_tag2, mpi_comm_world, status, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error receiving recv_buf'
                      lerror = .true.
                   end if

                   loc = 0
                   do k = 1, third_dim
                      do r = row_s, row_e
                         do c = col_s, col_e
                            loc = loc + 1
                            out_data(c, r, k) = recv_buf(loc)
                         end do
                      end do
                   end do

                end do
             else
                loc_tag1 = tag1 + mio_mype * 3
                call mpi_send (mio_mype, 1, mpi_integer, 0,    &
                               loc_tag1, mpi_comm_world, stat)
                if (stat .ne. 0) then
                   write (mio_logdev, *) ' Error: In routine ' // pname
                   write (mio_logdev, *) '        MPI error sending WHO info'
                   lerror = .true.
                end if

                nc = ncols_pe(mio_mype_p1, pos)
                nr = nrows_pe(mio_mype_p1, pos)
                send_size = nc * nr * third_dim

                loc_tag2 = tag2 + mio_mype * 3
                call mpi_send (in_data, send_size, mpi_double, 0, &
                               loc_tag2, mpi_comm_world, stat)
                if (stat .ne. 0) then
                   write (mio_logdev, *) ' Error: In routine ' // pname
                   write (mio_logdev, *) '        MPI error sending in_data'
                   lerror = .true.
                end if
#endif
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Abort in routine ' // pname // ' due to an error'
             stop
          end if

          if ((mio_mype .eq. 0) .and. allocated(recv_buf)) then
             deallocate (recv_buf)
          end if

        end subroutine mio_gather_data_3d_double

! --------------------------------------------------------------------------------------------
        subroutine mio_gather_data_1d_int (in_data, file_format, decomp, out_data)

          integer, intent(in)  :: in_data(:)
          integer, intent(in)  :: file_format
          logical, intent(in)  :: decomp
          integer, intent(out) :: out_data(:)

#ifdef parallel
          include 'mpif.h'
          integer :: status(mpi_status_size)
#endif

          character(22), parameter :: pname = 'mio_gather_data_1d_int'

          integer :: i, pe, dsize, stat, loc_tag2
          integer, allocatable :: recv_buf(:)
          logical :: lerror

          lerror = .false.

! this is for MPAS data
          if (file_format .eq. mio_mpas_format) then
             if (decomp .and. (mio_parallelism .ne. mio_serial)) then
                if (mio_mype .eq. 0) then
                   do i = 1, mio_mpas_dmap(0, mio_mype)
                      out_data(mio_mpas_dmap(i, mio_mype)) = in_data(i)
                   end do

#ifdef parallel
                   do pe = 1, mio_nprocs-1
                      dsize = mio_mpas_dmap(0, pe)
                      allocate (recv_buf(dsize), stat=stat)

                      loc_tag2 = tag2 + pe * 3
                      call mpi_recv (recv_buf, dsize, mpi_int, pe,           &
                                     loc_tag2, mpi_comm_world, status, stat)

                      if (stat .ne. 0) then
                         write (mio_logdev, *) ' Error: In routine ' // pname
                         write (mio_logdev, *) '        MPI error, not able to receive data'
                         lerror = .true.
                      end if

                      do i = 1, dsize
                         out_data(mio_mpas_dmap(i, pe)) = recv_buf(i)
                      end do
                      deallocate (recv_buf)
                   end do
                else
                   dsize = mio_mpas_dmap(0, mio_mype)
                   loc_tag2 = tag2 + mio_mype * 3
                   call mpi_send (in_data, dsize, mpi_int, 0,     &
                                  loc_tag2, mpi_comm_world, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error, not able to send data'
                      lerror = .true.
                   end if
#endif
                end if
             else
                out_data = in_data
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Abort in routine ' // pname // ' due to an error'
             stop
          end if

        end subroutine mio_gather_data_1d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_gather_data_2d_int (in_data, file_format, gtype,   &
                                           colde_pe, rowde_pe,            &
                                           ncols_pe, nrows_pe, out_data)

          integer, intent(in)   :: in_data(:,:)
          character, intent(in) :: gtype
          integer, intent(in)   :: file_format
          integer, intent(in)   :: colde_pe(:, :, :)
          integer, intent(in)   :: rowde_pe(:, :, :)
          integer, intent(in)   :: ncols_pe(:, :)
          integer, intent(in)   :: nrows_pe(:, :)
          integer, intent(out)  :: out_data(:,:)

#ifdef parallel
          include 'mpif.h'
          integer :: status(mpi_status_size)
#endif

          character(22), parameter :: pname = 'mio_gather_data_2d_int'

          integer :: stat, col_s, col_e, row_s, row_e, nc, nr, pe,     &
                     who, who_p1, send_size, recv_size, r, c, loc,     &
                     pos, dsize, i, first_dim, second_dim,             &
                     loc_tag1, loc_tag2
          integer, allocatable :: recv_buf(:), recv_buf_mpas(:,:)
          logical :: lerror

          lerror = .false.

          if (file_format .eq. mio_mpas_format) then
             first_dim  = size(in_data, 1)
             if (mio_mype .eq. 0) then
                do i = 1, mio_mpas_dmap(0, mio_mype)
                   out_data(:, mio_mpas_dmap(i, mio_mype)) = in_data(:, i)
                end do

#ifdef parallel
                do pe = 1, mio_nprocs-1
                   second_dim = mio_mpas_dmap(0, pe)
                   dsize = first_dim * second_dim
                   allocate (recv_buf_mpas(first_dim, second_dim), stat=stat)

                   loc_tag2 = tag2 + pe * 3
                   call mpi_recv (recv_buf_mpas, dsize, mpi_real, pe,     &
                                  loc_tag2, mpi_comm_world, status, stat)
                   do i = 1, second_dim
                      out_data(:, mio_mpas_dmap(i, pe)) = recv_buf_mpas(:, i)
                   end do
                   deallocate (recv_buf_mpas)
                end do
             else
                second_dim = mio_mpas_dmap(0, mio_mype)
                dsize = first_dim * second_dim

                loc_tag2 = tag2 + mio_mype * 3
                call mpi_send (in_data(:,1:second_dim), dsize, mpi_real, 0,     &
                               loc_tag2, mpi_comm_world, stat)
#endif
             end if
          else
             if (gtype .eq. 'c') then
                pos = 1
             else if (gtype .eq. 'd') then
                pos = 2
             else
                write (mio_logdev, *) ' Error: In routine ' // pname
                write (mio_logdev, *) '        incorrect data type, only accept c or d'
                lerror = .true.
             end if

             if (.not. lerror) then
                if (mio_mype .eq. 0) then          
! for PE 0
                   col_s = colde_pe(1, mio_mype_p1, pos)
                   col_e = colde_pe(2, mio_mype_p1, pos)
                   row_s = rowde_pe(1, mio_mype_p1, pos)
                   row_e = rowde_pe(2, mio_mype_p1, pos)

                   out_data(col_s:col_e, row_s:row_e) = in_data

#ifdef parallel
! for other PEs
                   allocate (recv_buf(size(in_data,1)*size(in_data,2)), stat=stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        not able to allocate recv_buf'
                      lerror = .true.
                   end if

                   do pe = 1, mio_nprocs-1
                      loc_tag1 = tag1 + pe * 3
                      call mpi_recv (who, 1, mpi_integer, mpi_any_source,    &
                                     loc_tag1, mpi_comm_world, status, stat)

                      if (stat .ne. 0) then
                         write (mio_logdev, *) ' Error: In routine ' // pname
                         write (mio_logdev, *) '        MPI error receiving processor id WHO'
                         lerror = .true.
                      end if

                      who_p1 = who + 1
                      col_s = colde_pe(1, who_p1, pos)
                      col_e = colde_pe(2, who_p1, pos)
                      row_s = rowde_pe(1, who_p1, pos)
                      row_e = rowde_pe(2, who_p1, pos)
                      nc    = ncols_pe(who_p1, pos)
                      nr    = nrows_pe(who_p1, pos)
                      recv_size = nc * nr

                      loc_tag2 = tag2 + pe * 3
                      call mpi_recv (recv_buf, recv_size, mpi_int, who,      &
                                     loc_tag2, mpi_comm_world, status, stat)
                      if (stat .ne. 0) then
                         write (mio_logdev, *) ' Error: In routine ' // pname
                         write (mio_logdev, *) '        MPI error receiving recv_buf'
                         lerror = .true.
                      end if

                      loc = 0
                      do r = row_s, row_e
                         do c = col_s, col_e
                            loc = loc + 1
                            out_data(c, r) = recv_buf(loc)
                         end do
                      end do
                   end do
                else
                   loc_tag1 = tag1 + mio_mype * 3
                   call mpi_send (mio_mype, 1, mpi_integer, 0,    &
                                  loc_tag1, mpi_comm_world, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error sending WHO info'
                      lerror = .true.
                   end if

                   nc = ncols_pe(mio_mype_p1, pos)
                   nr = nrows_pe(mio_mype_p1, pos)
                   send_size = nc * nr

                   loc_tag2 = tag2 + mio_mype * 3
                   call mpi_send (in_data, send_size, mpi_int, 0, &
                                  loc_tag2, mpi_comm_world, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error sending in_data'
                      lerror = .true.
                   end if
#endif
                end if
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Abort in routine ' // pname // ' due to an error'
             stop
          end if

          if ((mio_mype .eq. 0) .and. allocated(recv_buf)) then
             deallocate (recv_buf)
          end if

        end subroutine mio_gather_data_2d_int

! --------------------------------------------------------------------------------------------
        subroutine mio_gather_data_3d_int (in_data, gtype,                &
                                           colde_pe, rowde_pe,            &
                                           ncols_pe, nrows_pe, out_data)

          integer, intent(in)   :: in_data(:,:,:)
          character, intent(in) :: gtype
          integer, intent(in)   :: colde_pe(:, :, :)
          integer, intent(in)   :: rowde_pe(:, :, :)
          integer, intent(in)   :: ncols_pe(:, :)
          integer, intent(in)   :: nrows_pe(:, :)
          integer, intent(out)  :: out_data(:,:,:)

#ifdef parallel
          include 'mpif.h'
          integer :: status(mpi_status_size)
#endif

          character(22), parameter :: pname = 'mio_gather_data_3d_int'

          integer :: stat, col_s, col_e, row_s, row_e, nc, nr, third_dim,  &
                     pe, who, who_p1, send_size, recv_size, k, r, c, loc,  &
                     pos, loc_tag1, loc_tag2
          integer, allocatable :: recv_buf(:)
          logical :: lerror

          lerror = .false.

          if (gtype .eq. 'c') then
             pos = 1
          else if (gtype .eq. 'd') then
             pos = 2
          else
             write (mio_logdev, *) ' Error: In routine ' // pname
             write (mio_logdev, *) '        incorrect data type, only accept c or d'
             lerror = .true.
          end if

          if (.not. lerror) then
             third_dim = size(in_data, 3)

             if (mio_mype .eq. 0) then          
! for PE 0
                col_s = colde_pe(1, mio_mype_p1, pos)
                col_e = colde_pe(2, mio_mype_p1, pos)
                row_s = rowde_pe(1, mio_mype_p1, pos)
                row_e = rowde_pe(2, mio_mype_p1, pos)

                out_data(col_s:col_e, row_s:row_e, :) = in_data

#ifdef parallel
! for other PEs
                allocate (recv_buf(size(in_data,1)*size(in_data,2)*size(in_data,3)), stat=stat)
                if (stat .ne. 0) then
                   write (mio_logdev, *) ' Error: In routine ' // pname
                   write (mio_logdev, *) '        not able to allocate recv_buf'
                   lerror = .true.
                end if

                do pe = 1, mio_nprocs-1
                   loc_tag1 = tag1 + pe * 3
                   call mpi_recv (who, 1, mpi_integer, mpi_any_source,    &
                                  loc_tag1, mpi_comm_world, status, stat)

                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error receiving processor id WHO'
                      lerror = .true.
                   end if

                   who_p1 = who + 1

                   col_s = colde_pe(1, who_p1, pos)
                   col_e = colde_pe(2, who_p1, pos)
                   row_s = rowde_pe(1, who_p1, pos)
                   row_e = rowde_pe(2, who_p1, pos)
                   nc    = ncols_pe(who_p1, pos)
                   nr    = nrows_pe(who_p1, pos)
                   recv_size = nc * nr * third_dim

                   loc_tag2 = tag2 + pe * 3
                   call mpi_recv (recv_buf, recv_size, mpi_int, who,      &
                                  loc_tag2, mpi_comm_world, status, stat)
                   if (stat .ne. 0) then
                      write (mio_logdev, *) ' Error: In routine ' // pname
                      write (mio_logdev, *) '        MPI error receiving recv_buf'
                      lerror = .true.
                   end if

                   loc = 0
                   do k = 1, third_dim
                      do r = row_s, row_e
                         do c = col_s, col_e
                            loc = loc + 1
                            out_data(c, r, k) = recv_buf(loc)
                         end do
                      end do
                   end do

                end do
             else
                loc_tag2 = tag2 + mio_mype * 3
                call mpi_send (mio_mype, 1, mpi_integer, 0,    &
                               loc_tag1, mpi_comm_world, stat)
                if (stat .ne. 0) then
                   write (mio_logdev, *) ' Error: In routine ' // pname
                   write (mio_logdev, *) '        MPI error sending WHO info'
                   lerror = .true.
                end if

                nc = ncols_pe(mio_mype_p1, pos)
                nr = nrows_pe(mio_mype_p1, pos)
                send_size = nc * nr * third_dim

                loc_tag2 = tag2 + mio_mype * 3
                call mpi_send (in_data, send_size, mpi_int, 0, &
                               loc_tag2, mpi_comm_world, stat)
                if (stat .ne. 0) then
                   write (mio_logdev, *) ' Error: In routine ' // pname
                   write (mio_logdev, *) '        MPI error sending in_data'
                   lerror = .true.
                end if
#endif
             end if
          end if

          if (lerror) then
             write (mio_logdev, *) ' Abort in routine ' // pname // ' due to an error'
             stop
          end if

          if ((mio_mype .eq. 0) .and. allocated(recv_buf)) then
             deallocate (recv_buf)
          end if

        end subroutine mio_gather_data_3d_int

      end module mio_gather_data_module
