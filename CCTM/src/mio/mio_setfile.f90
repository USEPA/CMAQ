! Purpose: Set the file pointer points to an opened file so any
!          information about the file is accessible.

      subroutine mio_setfile (fname)

        use mio_global_data_module
        use mio_search_module

        implicit none

        character (*), intent(in) :: fname

        integer :: n

        n = mio_search (fname)
        if (n < 0) then
           write (mio_logdev, *) ' Abort in mio_setfile routine due to'
           write (mio_logdev, *) ' file ', trim(fname), ' does not exist'
           stop
        else
           mio_cfile = n
        end if

        mio_nbase_vars = mio_file_data(mio_cfile)%time_dim_loc
        mio_nvars      = mio_file_data(mio_cfile)%nvars
        mio_nlays      = mio_file_data(mio_cfile)%nlays

        if (mio_file_data(mio_cfile)%file_format .eq. mio_mpas_format) then  !  MPAS
           mio_nrows      = 1
           mio_nbase_vars = mio_file_data(mio_cfile)%nbvars
        else                                                                 ! for WRF and IOAPI_3
           mio_gl_ncols   = mio_file_data(mio_cfile)%gl_ncols
           mio_gl_nrows   = mio_file_data(mio_cfile)%gl_nrows
           mio_ncols      = mio_file_data(mio_cfile)%ncols
           mio_nrows      = mio_file_data(mio_cfile)%nrows
           mio_nbase_vars = -1
        end if

      end subroutine mio_setfile
