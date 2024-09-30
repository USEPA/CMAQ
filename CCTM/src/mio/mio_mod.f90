      module mio_module

        use mio_global_data_module
        use mio_get_env_module
        use mio_print_module
        use mio_fread_module
        use mio_fwrite_module
        use mio_set_global_attr_module
        use mio_get_global_attr_module
        use mio_util_func_module
        use mio_fcreate_module
        use mio_parameter_module
        use mio_interpolation_module

        interface
          subroutine mio_init (npcol, nprow, ncols, nrows, logdev, ptype)
            integer, intent(in) :: npcol, nprow, ncols, nrows
            integer, intent(in), optional :: logdev
            character(*), intent(in), optional :: ptype
          end subroutine mio_init
        end interface

      end module mio_module
