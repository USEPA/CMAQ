! Purpose: close a file

      subroutine mio_fclose (fname)

        use mio_global_data_module
        use mio_search_module

        implicit none

        character (*), intent(in) :: fname

        integer :: stat, n

        n = mio_search(fname)

        if (n < 0) then
           write (6, *) ' Warning: Could not find file ', trim(fname)
           write (6, *) '          File closing operation is not performed'
        else
           if (mio_file_data(n)%link .eq. -1) then
              stat = nf90_close (mio_file_data(n)%fileid)

              if (stat == 0) then
                 mio_file_data(n)%fileid = -1
                 deallocate (mio_file_data(n)%dim_name,             &
                             mio_file_data(n)%dim_value,            &
                             mio_file_data(n)%var_name,             &
                             mio_file_data(n)%lvar_name,            &
                             mio_file_data(n)%units,                &
                             mio_file_data(n)%var_type,             &
                             mio_file_data(n)%var_decomp,           &
                             mio_file_data(n)%var_grid_type,        &
                             mio_file_data(n)%var_id,               &
                             mio_file_data(n)%var_ndims,            &
                             mio_file_data(n)%var_dimids,           &
                             mio_file_data(n)%var_dimsize,          &
                             mio_file_data(n)%num_var_att,          &
                             mio_file_data(n)%var_att_name,         &
                             mio_file_data(n)%var_att_len,          &
                             mio_file_data(n)%var_att_type,         &
                             mio_file_data(n)%int_vatt_val,         &
                             mio_file_data(n)%real_vatt_val,        &
                             mio_file_data(n)%char_vatt_val,        &
                             mio_file_data(n)%glo_att_name,         &
                             mio_file_data(n)%glo_att_type,         &
                             mio_file_data(n)%glo_att_len,          &
                             mio_file_data(n)%glo_att_crange,       &
                             mio_file_data(n)%glo_att_irange,       &
                             mio_file_data(n)%glo_att_rrange,       &
                             mio_file_data(n)%glo_att_drange)
              else
                 write (6, *) trim(nf90_strerror(stat))
                 write (6, *) ' Warning: Not able to close file ', trim(fname)
                 write (6, *) '          File closing operation is not performed'
              end if
           end if
        end if

      end subroutine mio_fclose
