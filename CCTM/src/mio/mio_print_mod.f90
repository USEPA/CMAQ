! Purpose: print dimension, variable and global attribute information
!          for debugging purposes

        module mio_print_module

          use mio_search_module
          use mio_type_def_module
          use mio_global_data_module

          implicit none

          contains

! ----------------------------------------------------------------------------
          subroutine mio_print_dimension_information (fname)

            character (*), intent(in) :: fname

            integer :: f, n

            f = mio_search (fname)

            if (f < 0) then
               write (mio_logdev, *) ' Abort in routine mio_print_dimension_information. '
               write (mio_logdev, *) ' Cannot locate file ', trim(fname)
               stop
            else
               print *, ' ==d== dimension ', trim(fname), mio_file_data(f)%fileid
               do n = 1, mio_file_data(f)%ndims
                  print *, ' ==d==           ', n,           &
                               trim(mio_file_data(f)%dim_name(n)),  &
                               mio_file_data(f)%dim_value(n)
               end do
            end if

          end subroutine mio_print_dimension_information

! ----------------------------------------------------------------------------
          subroutine mio_print_variable_information (fname)

            character (*), intent(in) :: fname

            integer :: f, n, t

            f = mio_search (fname)

            if (f < 0) then
               write (mio_logdev, *) ' Abort in routine mio_print_variable_information. '
               write (mio_logdev, *) ' Cannot locate file ', trim(fname)
               stop
            else

               do n = 1, mio_file_data(f)%nvars

                  print *, ' ==d== var a ', n, mio_file_data(f)%fileid, trim(mio_file_data(f)%var_name(n))
                  print *, ' ==d== var   ', mio_file_data(f)%var_type(n), mio_file_data(f)%var_ndims(n)
                  write (6, '(1x, a13, 20i5)') ' ==d== var   ', mio_file_data(f)%var_dimids(:,n)
                  write (6, '(1x, a13, 20i5)') ' ==d== var   ', mio_file_data(f)%num_var_att(n)

                  if (mio_file_data(f)%num_var_att(n) .gt. 0) then

                     do t = 1, mio_file_data(f)%num_var_att(n)

                        if (mio_file_data(f)%var_att_type(t,n) .eq. nf90_char) then

                           write (6, '(1x, a13, 5i5, 1x, a, 1x a)') ' ==d== var tc',                  &
                                 n, t, mio_file_data(f)%var_id(n), mio_file_data(f)%var_att_type(t,n),        &
                                 mio_file_data(f)%var_att_len(t,n), trim(mio_file_data(f)%var_att_name(t,n)), &
                                 trim(mio_file_data(f)%char_vatt_val(t,n))

                        else if (mio_file_data(f)%var_att_type(t,n) .eq. nf90_int) then
                           write (6, '(1x, a13, 5i5, 1x, a, i5)') ' ==d== var ti',                    &
                                 n, t, mio_file_data(f)%var_id(n), mio_file_data(f)%var_att_type(t,n),        &
                                 mio_file_data(f)%var_att_len(t,n), trim(mio_file_data(f)%var_att_name(t,n)), &
                                 mio_file_data(f)%int_vatt_val(t,n)

                        else if (mio_file_data(f)%var_att_type(t,n) .eq. nf90_float) then
                           write (6, '(1x, a13, 5i5, 1x, a, f12.5)') ' ==d== var tr',                 &
                                 n, t, mio_file_data(f)%var_id(n), mio_file_data(f)%var_att_type(t,n),        &
                                 mio_file_data(f)%var_att_len(t,n), trim(mio_file_data(f)%var_att_name(t,n)), &
                                 mio_file_data(f)%real_vatt_val(t,n)

                        end if
                     end do

                  end if
               end do
            end if

          end subroutine mio_print_variable_information

! ----------------------------------------------------------------------------
          subroutine mio_print_attribute_information (fname)

            character (*), intent(in) :: fname

            integer :: f, n, s, e, start(4), leng(4)

            f = mio_search (fname)

            start = 0
            leng = 1
            do n = 1, mio_file_data(f)%n_global_atts

               if (mio_file_data(f)%glo_att_type(n) .eq. nf90_char) then

                  start(1) = start(1) + leng(1)
                  s = start(1)
                  e = s + mio_file_data(f)%glo_att_len(n) - 1
                  leng(1) = mio_file_data(f)%glo_att_len(n)

                  write (6, '(a16, i6, i5,  1x, a, 1x, 2i5, 2i7, 1x, a)') ' ==d== global c ',                      &
                        mio_file_data(f)%fileid, n, trim(mio_file_data(f)%glo_att_name(n)), mio_file_data(f)%glo_att_type(n),  &
                        mio_file_data(f)%glo_att_len(n), s, e, trim(mio_file_data(f)%glo_att_cval(s:e)), '='

               else if (mio_file_data(f)%glo_att_type(n) .eq. nf90_int) then

                  start(2) = start(2) + leng(2)
                  s = start(2)
                  e = s + mio_file_data(f)%glo_att_len(n) - 1
                  leng(2) = mio_file_data(f)%glo_att_len(n)

                  write (6, '(a16, i6, i5,  1x,a, 1x, 4i5, 100i8)') ' ==d== global i ', &
                        mio_file_data(f)%fileid, n, trim(mio_file_data(f)%glo_att_name(n)), mio_file_data(f)%glo_att_type(n),  &
                        mio_file_data(f)%glo_att_len(n), s, e, mio_file_data(f)%glo_att_ival(s:e)

               else if (mio_file_data(f)%glo_att_type(n) .eq. nf90_float) then

                  start(3) = start(3) + leng(3)
                  s = start(3)
                  e = s + mio_file_data(f)%glo_att_len(n) - 1
                  leng(3) = mio_file_data(f)%glo_att_len(n)

                  write (6, '(a16, i6, i5, 1x, a, 1x, 4i5, 100f12.5)') ' ==d== global r ', &
                        mio_file_data(f)%fileid, n, trim(mio_file_data(f)%glo_att_name(n)), mio_file_data(f)%glo_att_type(n),  &
                        mio_file_data(f)%glo_att_len(n), s, e, mio_file_data(f)%glo_att_rval(s:e)

               else if (mio_file_data(f)%glo_att_type(n) .eq. nf90_double) then

                  start(4) = start(4) + leng(4)
                  s = start(4)
                  e = s + mio_file_data(f)%glo_att_len(n) - 1
                  leng(4) = mio_file_data(f)%glo_att_len(n)

                  write (6, '(a16, i6, i5,  1x,a, 1x, 4i5, 100e20.5)') ' ==d== global d ', &
                        mio_file_data(f)%fileid, n, trim(mio_file_data(f)%glo_att_name(n)), mio_file_data(f)%glo_att_type(n),  &
                        mio_file_data(f)%glo_att_len(n), s, e, mio_file_data(f)%glo_att_dval(s:e)

               end if
            end do

          end subroutine mio_print_attribute_information

        end module mio_print_module
