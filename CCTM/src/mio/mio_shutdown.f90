! function: shut down mio system

      subroutine mio_shutdown ()

        use mio_global_data_module

        implicit none

        integer :: n, stat

        do n = 1, mio_n_infiles
           call mio_fclose(mio_file_data(n)%filename)
        end do

        if (mio_parallelism .ne. mio_serial) then
           call mio_set_barrier
        end if

        if (mio_mype .eq. 0) then
           do n = mio_n_infiles+1, mio_nfiles
              stat = nf90_sync(mio_file_data(n)%fileid)
              if (stat .ne. nf90_noerr) then
                 write (mio_logdev, *) ' Abort in mio_shutdown routine due to'
                 write (mio_logdev, *) ' error closing file ', trim(mio_file_data(n)%filename)
                 stop
              end if

              call mio_fclose(mio_file_data(n)%filename)
           end do
        end if

        if (allocated(mio_file_data)) then
           deallocate (mio_file_data)
        end if

        close (mio_iunit)

      end subroutine mio_shutdown
